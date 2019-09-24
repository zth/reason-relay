/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @emails oncall+relay
 * 
 * @format
 */
'use strict';

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

var _extends2 = _interopRequireDefault(require("@babel/runtime/helpers/extends"));

var _objectWithoutPropertiesLoose2 = _interopRequireDefault(require("@babel/runtime/helpers/objectWithoutPropertiesLoose"));

var _objectSpread2 = _interopRequireDefault(require("@babel/runtime/helpers/objectSpread"));

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

var React = require('react');

var useMemo = React.useMemo,
    useRef = React.useRef,
    useState = React.useState;

var TestRenderer = require('react-test-renderer');

var useFragmentNodesOriginal = require('../useFragmentNodes');

var ReactRelayContext = require('react-relay/ReactRelayContext');

var _require = require('relay-runtime'),
    FRAGMENT_OWNER_KEY = _require.FRAGMENT_OWNER_KEY,
    FRAGMENTS_KEY = _require.FRAGMENTS_KEY,
    ID_KEY = _require.ID_KEY,
    createOperationDescriptor = _require.createOperationDescriptor;

function captureAssertion(fn) {
  // Trick to use a Jest matcher inside another Jest matcher. `fn` contains an
  // assertion; if it throws, we capture the error and return it, so the stack
  // trace presented to the user points to the original assertion in the
  // test file.
  try {
    fn();
  } catch (error) {
    return {
      pass: false,
      message: function message() {
        return error.message;
      }
    };
  }

  return {
    pass: true
  };
}

function assertYieldsWereCleared(_scheduler) {
  var actualYields = _scheduler.unstable_clearYields();

  if (actualYields.length !== 0) {
    throw new Error('Log of yielded values is not empty. ' + 'Call expect(Scheduler).toHaveYielded(...) first.');
  }
}

function expectSchedulerToFlushAndYield(expectedYields) {
  var Scheduler = require('scheduler');

  assertYieldsWereCleared(Scheduler);
  Scheduler.unstable_flushAllWithoutAsserting();
  var actualYields = Scheduler.unstable_clearYields();
  return captureAssertion(function () {
    expect(actualYields).toEqual(expectedYields);
  });
}

describe('useFragmentNodes', function () {
  var environment;
  var createMockEnvironment;
  var disableStoreUpdates;
  var enableStoreUpdates;
  var generateAndCompile;
  var gqlSingularQuery;
  var gqlSingularFragment;
  var gqlPluralQuery;
  var gqlPluralFragment;
  var singularQuery;
  var pluralQuery;
  var singularVariables;
  var pluralVariables;
  var setEnvironment;
  var setSingularOwner;
  var setSingularFooScalar;
  var setSingularFooObject;
  var renderSingularFragment;
  var renderPluralFragment;
  var forceSingularUpdate;
  var renderSpy;
  var SingularRenderer;
  var PluralRenderer;

  function resetRenderMock() {
    renderSpy.mockClear();
  }

  function useFragmentNodes(fragmentNodes, fragmentRefs) {
    var result = useFragmentNodesOriginal(fragmentNodes, fragmentRefs, 'TestDisplayName');
    var data = result.data,
        shouldUpdateGeneration = result.shouldUpdateGeneration;
    disableStoreUpdates = result.disableStoreUpdates;
    enableStoreUpdates = result.enableStoreUpdates;
    var prevShouldUpdateGeneration = useRef(null);
    var shouldUpdate = false;

    if (prevShouldUpdateGeneration.current !== shouldUpdateGeneration) {
      shouldUpdate = true;
      prevShouldUpdateGeneration.current = shouldUpdateGeneration;
    }

    renderSpy(data, shouldUpdate);
    return [data, shouldUpdate];
  }

  function assertCall(key, expected, idx) {
    var actualData = renderSpy.mock.calls[idx][0];
    var actualShouldUpdate = renderSpy.mock.calls[idx][1];
    expect(actualData[key]).toEqual(expected.data[key]);
    expect(actualShouldUpdate).toEqual(expected.shouldUpdate);
  }

  function assertFragmentResults(key, expectedCalls) {
    // This ensures that useEffect runs
    TestRenderer.act(function () {
      return jest.runAllImmediates();
    });
    expect(renderSpy).toBeCalledTimes(expectedCalls.length);
    expectedCalls.forEach(function (expected, idx) {
      return assertCall(key, expected, idx);
    });
    renderSpy.mockClear();
  }

  function expectSingularFragmentResults(expectedCalls) {
    assertFragmentResults('user', expectedCalls);
  }

  function expectPluralFragmentResults(expectedCalls) {
    assertFragmentResults('users', expectedCalls);
  }

  function createFragmentRef(id, owner) {
    var _ref2;

    return _ref2 = {}, (0, _defineProperty2["default"])(_ref2, ID_KEY, id), (0, _defineProperty2["default"])(_ref2, FRAGMENTS_KEY, {
      NestedUserFragment: {}
    }), (0, _defineProperty2["default"])(_ref2, FRAGMENT_OWNER_KEY, owner.request), _ref2;
  }

  beforeEach(function () {
    // Set up mocks
    jest.resetModules();
    jest.spyOn(console, 'warn').mockImplementationOnce(function () {});
    jest.mock("fbjs/lib/warning");
    jest.mock('scheduler', function () {
      return jest.requireActual('scheduler/unstable_mock');
    });
    jest.mock('fbjs/lib/ExecutionEnvironment', function () {
      return {
        canUseDOM: function canUseDOM() {
          return true;
        }
      };
    });
    renderSpy = jest.fn();

    var _require2 = require('relay-test-utils-internal');

    createMockEnvironment = _require2.createMockEnvironment;
    generateAndCompile = _require2.generateAndCompile;
    // Set up environment and base data
    environment = createMockEnvironment();
    var generated = generateAndCompile("\n        fragment NestedUserFragment on User {\n          username\n        }\n\n        fragment UserFragment on User  {\n          id\n          name\n          profile_picture(scale: $scale) {\n            uri\n          }\n          ...NestedUserFragment\n        }\n\n        fragment UsersFragment on User @relay(plural: true) {\n          id\n          name\n          profile_picture(scale: $scale) {\n            uri\n          }\n          ...NestedUserFragment\n        }\n\n        query UsersQuery($ids: [ID!]!, $scale: Int!) {\n          nodes(ids: $ids) {\n            ...UsersFragment\n          }\n        }\n\n        query UserQuery($id: ID!, $scale: Int!) {\n          node(id: $id) {\n            ...UserFragment\n          }\n        }\n    ");
    singularVariables = {
      id: '1',
      scale: 16
    };
    pluralVariables = {
      ids: ['1', '2'],
      scale: 16
    };
    gqlSingularQuery = generated.UserQuery;
    gqlSingularFragment = generated.UserFragment;
    gqlPluralQuery = generated.UsersQuery;
    gqlPluralFragment = generated.UsersFragment;
    singularQuery = createOperationDescriptor(gqlSingularQuery, singularVariables);
    pluralQuery = createOperationDescriptor(gqlPluralQuery, pluralVariables);
    environment.commitPayload(singularQuery, {
      node: {
        __typename: 'User',
        id: '1',
        name: 'Alice',
        username: 'useralice',
        profile_picture: null
      }
    });
    environment.commitPayload(pluralQuery, {
      nodes: [{
        __typename: 'User',
        id: '1',
        name: 'Alice',
        username: 'useralice',
        profile_picture: null
      }, {
        __typename: 'User',
        id: '2',
        name: 'Bob',
        username: 'userbob',
        profile_picture: null
      }]
    }); // Set up renderers

    SingularRenderer = function SingularRenderer(props) {
      return null;
    };

    PluralRenderer = function PluralRenderer(props) {
      return null;
    };

    var SingularContainer = function SingularContainer(props) {
      var _ref3;

      // We need a render a component to run a Hook
      var _useState = useState(props.owner),
          owner = _useState[0],
          _setOwner = _useState[1];

      var _useState2 = useState(0),
          _ = _useState2[0],
          _setCount = _useState2[1];

      var _useState3 = useState({
        fooScalar: props.fooScalar
      }),
          fooScalar = _useState3[0].fooScalar,
          _setFooScalar = _useState3[1];

      var _useState4 = useState({
        fooObject: props.fooObject
      }),
          fooObject = _useState4[0].fooObject,
          _setFooObject = _useState4[1];

      var userRef = props.hasOwnProperty('userRef') ? props.userRef : (_ref3 = {}, (0, _defineProperty2["default"])(_ref3, ID_KEY, owner.request.variables.id), (0, _defineProperty2["default"])(_ref3, FRAGMENTS_KEY, {
        UserFragment: {}
      }), (0, _defineProperty2["default"])(_ref3, FRAGMENT_OWNER_KEY, owner.request), _ref3);
      setSingularOwner = _setOwner;
      setSingularFooScalar = _setFooScalar;
      setSingularFooObject = _setFooObject;

      forceSingularUpdate = function forceSingularUpdate() {
        return _setCount(function (count) {
          return count + 1;
        });
      };

      var fragmentRefs = {
        user: userRef
      }; // Pass extra props resembling component props

      if (fooScalar != null) {
        fragmentRefs = (0, _objectSpread2["default"])({}, fragmentRefs, {
          fooScalar: fooScalar
        });
      }

      if (fooObject != null) {
        fragmentRefs = (0, _objectSpread2["default"])({}, fragmentRefs, {
          fooObject: fooObject
        });
      }

      var _useFragmentNodes = useFragmentNodes({
        user: gqlSingularFragment
      }, fragmentRefs),
          userData = _useFragmentNodes[0];

      return React.createElement(SingularRenderer, {
        user: userData.user
      });
    };

    var PluralContainer = function PluralContainer(props) {
      // We need a render a component to run a Hook
      var owner = props.owner;
      var usersRef = props.hasOwnProperty('usersRef') ? props.usersRef : owner.request.variables.ids.map(function (id) {
        var _ref4;

        return _ref4 = {}, (0, _defineProperty2["default"])(_ref4, ID_KEY, id), (0, _defineProperty2["default"])(_ref4, FRAGMENTS_KEY, {
          UsersFragment: {}
        }), (0, _defineProperty2["default"])(_ref4, FRAGMENT_OWNER_KEY, owner.request), _ref4;
      });

      var _useFragmentNodes2 = useFragmentNodes({
        users: gqlPluralFragment
      }, {
        users: usersRef
      }),
          usersData = _useFragmentNodes2[0];

      return React.createElement(PluralRenderer, {
        users: usersData.users
      });
    };

    var ContextProvider = function ContextProvider(_ref5) {
      var children = _ref5.children;

      var _useState5 = useState(environment),
          env = _useState5[0],
          _setEnv = _useState5[1]; // TODO(T39494051) - We set empty variables in relay context to make
      // Flow happy, but useFragmentNodes does not use them, instead it uses
      // the variables from the fragment owner.


      var relayContext = useMemo(function () {
        return {
          environment: env,
          variables: {}
        };
      }, [env]);
      setEnvironment = _setEnv;
      return React.createElement(ReactRelayContext.Provider, {
        value: relayContext
      }, children);
    };

    renderSingularFragment = function renderSingularFragment(args) {
      var _args;

      var _ref6 = (_args = args) !== null && _args !== void 0 ? _args : {},
          _ref6$isConcurrent = _ref6.isConcurrent,
          isConcurrent = _ref6$isConcurrent === void 0 ? false : _ref6$isConcurrent,
          props = (0, _objectWithoutPropertiesLoose2["default"])(_ref6, ["isConcurrent"]);

      return TestRenderer.create(React.createElement(React.Suspense, {
        fallback: "Singular Fallback"
      }, React.createElement(ContextProvider, null, React.createElement(SingularContainer, (0, _extends2["default"])({
        owner: singularQuery
      }, props)))), {
        unstable_isConcurrent: isConcurrent
      });
    };

    renderPluralFragment = function renderPluralFragment(args) {
      var _args2;

      var _ref7 = (_args2 = args) !== null && _args2 !== void 0 ? _args2 : {},
          _ref7$isConcurrent = _ref7.isConcurrent,
          isConcurrent = _ref7$isConcurrent === void 0 ? false : _ref7$isConcurrent,
          props = (0, _objectWithoutPropertiesLoose2["default"])(_ref7, ["isConcurrent"]);

      return TestRenderer.create(React.createElement(React.Suspense, {
        fallback: "Plural Fallback"
      }, React.createElement(ContextProvider, null, React.createElement(PluralContainer, (0, _extends2["default"])({
        owner: pluralQuery
      }, props)))), {
        unstable_isConcurrent: isConcurrent
      });
    };
  });
  afterEach(function () {
    environment.mockClear();
    renderSpy.mockClear();
  });
  it('should render singular fragment without error when data is available', function () {
    renderSingularFragment();
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      shouldUpdate: true
    }]);
  });
  it('should render singular fragment without error when ref is null', function () {
    renderSingularFragment({
      userRef: null
    });
    expectSingularFragmentResults([{
      data: {
        user: null
      },
      shouldUpdate: true
    }]);
  });
  it('should render singular fragment without error when ref is undefined', function () {
    renderSingularFragment({
      userRef: undefined
    });
    expectSingularFragmentResults([{
      data: {
        user: null
      },
      shouldUpdate: true
    }]);
  });
  it('should render plural fragment without error when data is available', function () {
    renderPluralFragment();
    expectPluralFragmentResults([{
      data: {
        users: [(0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', pluralQuery)), (0, _objectSpread2["default"])({
          id: '2',
          name: 'Bob',
          profile_picture: null
        }, createFragmentRef('2', pluralQuery))]
      },
      shouldUpdate: true
    }]);
  });
  it('should render plural fragment without error when plural field is empty', function () {
    renderPluralFragment({
      usersRef: []
    });
    expectPluralFragmentResults([{
      data: {
        users: []
      },
      shouldUpdate: true
    }]);
  });
  it('should update when fragment data changes', function () {
    renderSingularFragment();
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      shouldUpdate: true
    }]);
    environment.commitPayload(singularQuery, {
      node: {
        __typename: 'User',
        id: '1',
        // Update name
        name: 'Alice in Wonderland'
      }
    });
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          // Assert that name is updated
          name: 'Alice in Wonderland',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      shouldUpdate: true
    }]);
  });
  it('should re-read and resubscribe to fragment when environment changes', function () {
    renderSingularFragment();
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      shouldUpdate: true
    }]);
    var newEnvironment = createMockEnvironment();
    newEnvironment.commitPayload(singularQuery, {
      node: {
        __typename: 'User',
        id: '1',
        name: 'Alice in a different env',
        profile_picture: null
      }
    });
    setEnvironment(newEnvironment);
    var expectedUser = (0, _objectSpread2["default"])({
      id: '1',
      name: 'Alice in a different env',
      profile_picture: null
    }, createFragmentRef('1', singularQuery));
    expectSingularFragmentResults([{
      data: {
        user: expectedUser
      },
      shouldUpdate: true
    }, {
      data: {
        user: expectedUser
      },
      shouldUpdate: false
    }]);
    newEnvironment.commitPayload(singularQuery, {
      node: {
        __typename: 'User',
        id: '1',
        // Update name
        name: 'Alice in Wonderland'
      }
    });
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          // Assert that name is updated
          name: 'Alice in Wonderland',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      shouldUpdate: true
    }]);
  });
  it('should re-read and resubscribe to fragment when fragment pointers change', function () {
    renderSingularFragment();
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      shouldUpdate: true
    }]);
    var newVariables = (0, _objectSpread2["default"])({}, singularVariables, {
      id: '200'
    });
    var newQuery = createOperationDescriptor(gqlSingularQuery, newVariables);
    environment.commitPayload(newQuery, {
      node: {
        __typename: 'User',
        id: '200',
        name: 'Foo',
        profile_picture: null,
        username: 'userfoo'
      }
    });
    setSingularOwner(newQuery);
    var expectedUser = (0, _objectSpread2["default"])({
      // Assert updated data
      id: '200',
      name: 'Foo',
      profile_picture: null
    }, createFragmentRef('200', newQuery));
    expectSingularFragmentResults([{
      data: {
        user: expectedUser
      },
      shouldUpdate: true
    }, {
      data: {
        user: expectedUser
      },
      shouldUpdate: false
    }]);
    environment.commitPayload(newQuery, {
      node: {
        __typename: 'User',
        id: '200',
        // Update name
        name: 'Foo Updated'
      }
    });
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '200',
          // Assert that name is updated
          name: 'Foo Updated',
          profile_picture: null
        }, createFragmentRef('200', newQuery))
      },
      shouldUpdate: true
    }]);
  });
  it('should render correct data when changing fragment refs multiple times', function () {
    // Render component with data for ID 1
    renderSingularFragment();
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      shouldUpdate: true
    }]); // Update fragment refs to render data for ID 200

    var newVariables = (0, _objectSpread2["default"])({}, singularVariables, {
      id: '200'
    });
    var newQuery = createOperationDescriptor(gqlSingularQuery, newVariables);
    environment.commitPayload(newQuery, {
      node: {
        __typename: 'User',
        id: '200',
        name: 'Foo',
        username: 'userfoo',
        profile_picture: null
      }
    });
    setSingularOwner(newQuery);
    var expectedUser = (0, _objectSpread2["default"])({
      // Assert updated data
      id: '200',
      name: 'Foo',
      profile_picture: null
    }, createFragmentRef('200', newQuery));
    expectSingularFragmentResults([{
      data: {
        user: expectedUser
      },
      shouldUpdate: true
    }, {
      data: {
        user: expectedUser
      },
      shouldUpdate: false
    }]); // Udpate data for ID 1

    environment.commitPayload(singularQuery, {
      node: {
        __typename: 'User',
        id: '1',
        // Update name
        name: 'Alice in Wonderland'
      }
    }); // Switch back to rendering data for ID 1

    setSingularOwner(singularQuery); // We expect to see the latest data

    expectedUser = (0, _objectSpread2["default"])({
      // Assert updated data
      id: '1',
      name: 'Alice in Wonderland',
      profile_picture: null
    }, createFragmentRef('1', singularQuery));
    expectSingularFragmentResults([{
      data: {
        user: expectedUser
      },
      shouldUpdate: true
    }, {
      data: {
        user: expectedUser
      },
      shouldUpdate: false
    }]); // Assert it correctly subscribes to new data

    environment.commitPayload(singularQuery, {
      node: {
        __typename: 'User',
        id: '1',
        // Update name
        name: 'Alice Updated'
      }
    });
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          // Assert anme is updated
          name: 'Alice Updated',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      shouldUpdate: true
    }]);
  }); // TODO(T52977316) Re-enable concurent mode tests

  it.skip('should ignore updates to initially rendered data when fragment pointers change', function () {
    var Scheduler = require('scheduler');

    var YieldChild = function YieldChild(props) {
      // NOTE the unstable_yield method will move to the static renderer.
      // When React sync runs we need to update this.
      Scheduler.unstable_yieldValue(props.children);
      return props.children;
    };

    var YieldyUserComponent = function YieldyUserComponent(_ref8) {
      var user = _ref8.user;
      return React.createElement(React.Fragment, null, React.createElement(YieldChild, null, "Hey user,"), React.createElement(YieldChild, null, user.name), React.createElement(YieldChild, null, "with id ", user.id, "!"));
    }; // Assert initial render


    SingularRenderer = YieldyUserComponent;
    renderSingularFragment({
      isConcurrent: true
    });
    expectSchedulerToFlushAndYield(['Hey user,', 'Alice', ['with id ', '1', '!']]);
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      shouldUpdate: true
    }]);
    var newVariables = (0, _objectSpread2["default"])({}, singularVariables, {
      id: '200'
    });
    var newQuery = createOperationDescriptor(gqlSingularQuery, newVariables);
    environment.commitPayload(newQuery, {
      node: {
        __typename: 'User',
        id: '200',
        name: 'Foo',
        username: 'userfoo',
        profile_picture: null
      }
    }); // Pass new fragment ref that points to new ID 200

    setSingularOwner(newQuery); // Flush some of the changes, but don't commit

    expectSchedulerToFlushAndYield(['Hey user,', 'Foo']); // In Concurrent mode component gets rendered even if not committed
    // so we reset our mock here

    resetRenderMock(); // Trigger an update for initially rendered data while second
    // render is in progress

    environment.commitPayload(singularQuery, {
      node: {
        __typename: 'User',
        id: '1',
        name: 'Alice in Wonderland'
      }
    }); // Assert the component renders the data from newQuery/newVariables,
    // ignoring any updates triggered while render was in progress

    expectSchedulerToFlushAndYield([['with id ', '200', '!'], 'Hey user,', 'Foo', ['with id ', '200', '!']]);
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '200',
          name: 'Foo',
          profile_picture: null
        }, createFragmentRef('200', newQuery))
      },
      shouldUpdate: true
    }]); // Update latest rendered data

    environment.commitPayload(newQuery, {
      node: {
        __typename: 'User',
        id: '200',
        // Update name
        name: 'Foo Updated'
      }
    });
    expectSchedulerToFlushAndYield(['Hey user,', 'Foo Updated', ['with id ', '200', '!']]);
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '200',
          // Assert name is updated
          name: 'Foo Updated',
          profile_picture: null
        }, createFragmentRef('200', newQuery))
      },
      shouldUpdate: true
    }]);
  });
  it('should re-read and resubscribe to fragment when variables change', function () {
    renderSingularFragment();
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      shouldUpdate: true
    }]);
    var newVariables = (0, _objectSpread2["default"])({}, singularVariables, {
      id: '1',
      scale: 32
    });
    var newQuery = createOperationDescriptor(gqlSingularQuery, newVariables);
    environment.commitPayload(newQuery, {
      node: {
        __typename: 'User',
        id: '1',
        name: 'Alice',
        profile_picture: {
          uri: 'uri32'
        },
        username: 'useralice'
      }
    });
    setSingularOwner(newQuery);
    var expectedUser = (0, _objectSpread2["default"])({
      id: '1',
      name: 'Alice',
      profile_picture: {
        // Asset updated uri
        uri: 'uri32'
      }
    }, createFragmentRef('1', newQuery));
    expectSingularFragmentResults([{
      data: {
        user: expectedUser
      },
      shouldUpdate: true
    }, {
      data: {
        user: expectedUser
      },
      shouldUpdate: false
    }]);
    environment.commitPayload(newQuery, {
      node: {
        __typename: 'User',
        id: '1',
        // Update name
        name: 'Alice in Wonderland'
      }
    });
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          // Assert that name is updated
          name: 'Alice in Wonderland',
          profile_picture: {
            uri: 'uri32'
          }
        }, createFragmentRef('1', newQuery))
      },
      shouldUpdate: true
    }]);
  }); // TODO(T52977316) Re-enable concurent mode tests

  it.skip('should ignore updates to initially rendered data when variables change', function () {
    var Scheduler = require('scheduler');

    var YieldChild = function YieldChild(props) {
      Scheduler.unstable_yieldValue(props.children);
      return props.children;
    };

    var YieldyUserComponent = function YieldyUserComponent(_ref9) {
      var _ref, _user$profile_picture;

      var user = _ref9.user;
      return React.createElement(React.Fragment, null, React.createElement(YieldChild, null, "Hey user,"), React.createElement(YieldChild, null, (_ref = (_user$profile_picture = user.profile_picture) === null || _user$profile_picture === void 0 ? void 0 : _user$profile_picture.uri) !== null && _ref !== void 0 ? _ref : 'no uri'), React.createElement(YieldChild, null, "with id ", user.id, "!"));
    }; // Assert initial render


    SingularRenderer = YieldyUserComponent;
    renderSingularFragment({
      isConcurrent: true
    });
    expectSchedulerToFlushAndYield(['Hey user,', 'no uri', ['with id ', '1', '!']]);
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      shouldUpdate: true
    }]);
    var newVariables = (0, _objectSpread2["default"])({}, singularVariables, {
      id: '1',
      scale: 32
    });
    var newQuery = createOperationDescriptor(gqlSingularQuery, newVariables);
    environment.commitPayload(newQuery, {
      node: {
        __typename: 'User',
        id: '1',
        name: 'Alice',
        username: 'useralice',
        profile_picture: {
          uri: 'uri32'
        }
      }
    }); // Pass new fragment ref which contains newVariables

    setSingularOwner(newQuery); // Flush some of the changes, but don't commit

    expectSchedulerToFlushAndYield(['Hey user,', 'uri32']); // In Concurrent mode component gets rendered even if not committed
    // so we reset our mock here

    resetRenderMock(); // Trigger an update for initially rendered data while second
    // render is in progress

    environment.commitPayload(singularQuery, {
      node: {
        __typename: 'User',
        id: '1',
        // Update name
        name: 'Alice',
        // Update profile_picture value
        profile_picture: {
          uri: 'uri16'
        }
      }
    }); // Assert the component renders the data from newQuery/newVariables,
    // ignoring any updates triggered while render was in progress

    expectSchedulerToFlushAndYield([['with id ', '1', '!'], 'Hey user,', 'uri32', ['with id ', '1', '!']]);
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: {
            uri: 'uri32'
          }
        }, createFragmentRef('1', newQuery))
      },
      shouldUpdate: true
    }]); // Update latest rendered data

    environment.commitPayload(newQuery, {
      node: {
        __typename: 'User',
        id: '1',
        // Update name
        name: 'Alice latest update'
      }
    });
    expectSchedulerToFlushAndYield(['Hey user,', 'uri32', ['with id ', '1', '!']]);
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          // Assert name is updated
          name: 'Alice latest update',
          profile_picture: {
            uri: 'uri32'
          }
        }, createFragmentRef('1', newQuery))
      },
      shouldUpdate: true
    }]);
  });
  it('should NOT update if fragment refs dont change', function () {
    renderSingularFragment();
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      shouldUpdate: true
    }]); // Force a re-render with the exact same fragment refs

    forceSingularUpdate();
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      // Assert that update to consuming component wont be triggered
      shouldUpdate: false
    }]);
  });
  it('should NOT update even if fragment ref changes but doesnt point to a different ID', function () {
    renderSingularFragment();
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      shouldUpdate: true
    }]); // Setting a new owner with the same query/variables will cause new
    // fragment refs that point to the same IDs to be passed

    var newOwner = createOperationDescriptor(gqlSingularQuery, singularVariables);
    setSingularOwner(newOwner);
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      // Assert that update to consuming component wont be triggered
      shouldUpdate: false
    }]);
  });
  it('should NOT update if scalar props dont change', function () {
    renderSingularFragment({
      fooScalar: false
    });
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      shouldUpdate: true
    }]); // Re-render with same scalar value

    setSingularFooScalar({
      fooScalar: false
    });
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      // Assert that update to consuming component wont be triggered
      shouldUpdate: false
    }]);
  });
  it('should update if scalar props change', function () {
    renderSingularFragment({
      fooScalar: false
    });
    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', singularQuery))
      },
      shouldUpdate: true
    }]); // Re-render with different scalar value

    setSingularFooScalar({
      fooScalar: true
    });
    var expectedUser = (0, _objectSpread2["default"])({
      id: '1',
      name: 'Alice',
      profile_picture: null
    }, createFragmentRef('1', singularQuery));
    expectSingularFragmentResults([{
      data: {
        user: expectedUser
      },
      // Assert that consuming component knows it needs to update
      shouldUpdate: true
    }, {
      data: {
        user: expectedUser
      },
      shouldUpdate: false
    }]);
  });
  it('should update even if non-scalar props dont change', function () {
    var fooObject = {};
    var expectedUser = (0, _objectSpread2["default"])({
      id: '1',
      name: 'Alice',
      profile_picture: null
    }, createFragmentRef('1', singularQuery));
    renderSingularFragment({
      fooObject: fooObject
    });
    expectSingularFragmentResults([{
      data: {
        user: expectedUser
      },
      shouldUpdate: true
    }]); // Re-render with the exact same non-scalar prop

    setSingularFooObject({
      fooObject: fooObject
    });
    expectSingularFragmentResults([{
      data: {
        user: expectedUser
      },
      // Assert that consuming component knows it needs to update
      shouldUpdate: true
    }]);
  });
  it('should update if non-scalar props change', function () {
    var expectedUser = (0, _objectSpread2["default"])({
      id: '1',
      name: 'Alice',
      profile_picture: null
    }, createFragmentRef('1', singularQuery));
    renderSingularFragment({
      fooObject: {}
    });
    expectSingularFragmentResults([{
      data: {
        user: expectedUser
      },
      shouldUpdate: true
    }]); // Re-render with different non-scalar value

    setSingularFooObject({
      fooObject: {}
    });
    expectSingularFragmentResults([{
      data: {
        user: expectedUser
      },
      // Assert that consuming component knows it needs to update
      shouldUpdate: true
    }, {
      data: {
        user: expectedUser
      },
      shouldUpdate: true
    }]);
  });
  it('should throw a promise if if data is missing for fragment and request is in flight', function () {
    // This prevents console.error output in the test, which is expected
    jest.spyOn(console, 'error').mockImplementationOnce(function () {});
    jest.spyOn(require('relay-runtime').__internal, 'getPromiseForRequestInFlight').mockImplementationOnce(function () {
      return Promise.resolve();
    });
    var missingDataVariables = (0, _objectSpread2["default"])({}, singularVariables, {
      id: '4'
    });
    var missingDataQuery = createOperationDescriptor(gqlSingularQuery, missingDataVariables); // Commit a payload with name and profile_picture are missing

    environment.commitPayload(missingDataQuery, {
      node: {
        __typename: 'User',
        id: '4'
      }
    });
    var renderer = renderSingularFragment({
      owner: missingDataQuery
    });
    expect(renderer.toJSON()).toEqual('Singular Fallback');
  });
  it('should throw an error if fragment reference is non-null but read-out data is null', function () {
    // Clearing the data in the environment will make it so the fragment ref
    // we pass to useFragmentNodes points to data that does not exist; we expect
    // an error to be thrown in this case.
    environment.getStore().getSource().clear();

    var warning = require("fbjs/lib/warning"); // $FlowFixMe


    warning.mockClear();
    renderSingularFragment();
    expect(warning).toBeCalledTimes(2); // $FlowFixMe

    var _warning$mock$calls$ = warning.mock.calls[1],
        _ = _warning$mock$calls$[0],
        warningMessage = _warning$mock$calls$[1];
    expect(warningMessage.startsWith('Relay: Expected to have been able to read non-null data for fragment `%s`')).toEqual(true); // $FlowFixMe

    warning.mockClear();
  });
  it('should warn if data is missing and there are no pending requests', function () {
    // This prevents console.error output in the test, which is expected
    jest.spyOn(console, 'error').mockImplementationOnce(function () {});

    var warning = require("fbjs/lib/warning");

    var missingDataVariables = (0, _objectSpread2["default"])({}, singularVariables, {
      id: '4'
    });
    var missingDataQuery = createOperationDescriptor(gqlSingularQuery, missingDataVariables); // Commit a payload where name is missing.

    environment.commitPayload(missingDataQuery, {
      node: {
        __typename: 'User',
        id: '4'
      }
    }); // $FlowFixMe

    warning.mockClear();
    TestRenderer.act(function () {
      renderSingularFragment({
        owner: missingDataQuery
      });
    }); // Assert warning message

    expect(warning).toHaveBeenCalledTimes(1); // $FlowFixMe

    var _warning$mock$calls$2 = warning.mock.calls[0],
        _ = _warning$mock$calls$2[0],
        warningMessage = _warning$mock$calls$2[1],
        warningArgs = _warning$mock$calls$2.slice(2);

    expect(warningMessage.startsWith('Relay: Tried reading fragment `%s` ' + 'declared in `%s`, but it has ' + 'missing data and its parent query `%s` is not being fetched.')).toEqual(true);
    expect(warningArgs).toEqual(['UserFragment', 'TestDisplayName', 'UserQuery', 'UserQuery']); // Assert render output with missing data

    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '4',
          name: undefined,
          profile_picture: undefined
        }, createFragmentRef('4', missingDataQuery))
      },
      shouldUpdate: true
    }]);
  });
  it('should subscribe for updates even if there is missing data', function () {
    // This prevents console.error output in the test, which is expected
    jest.spyOn(console, 'error').mockImplementationOnce(function () {});

    var warning = require("fbjs/lib/warning");

    var missingDataVariables = (0, _objectSpread2["default"])({}, singularVariables, {
      id: '4'
    });
    var missingDataQuery = createOperationDescriptor(gqlSingularQuery, missingDataVariables); // Commit a payload where name is missing.

    environment.commitPayload(missingDataQuery, {
      node: {
        __typename: 'User',
        id: '4'
      }
    }); // $FlowFixMe

    warning.mockClear();
    renderSingularFragment({
      owner: missingDataQuery
    }); // Assert render output with missing data

    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '4',
          name: undefined,
          profile_picture: undefined
        }, createFragmentRef('4', missingDataQuery))
      },
      shouldUpdate: true
    }]); // Commit a payload with updated name.

    environment.commitPayload(missingDataQuery, {
      node: {
        __typename: 'User',
        id: '4',
        name: 'Mark'
      }
    }); // Assert render output with updated data

    expectSingularFragmentResults([{
      data: {
        user: (0, _objectSpread2["default"])({
          id: '4',
          name: 'Mark',
          profile_picture: undefined
        }, createFragmentRef('4', missingDataQuery))
      },
      shouldUpdate: true
    }]);
  });
  describe('disableStoreUpdates', function () {
    it('does not listen to store updates after disableStoreUpdates is called', function () {
      renderSingularFragment();
      expectSingularFragmentResults([{
        data: {
          user: (0, _objectSpread2["default"])({
            id: '1',
            name: 'Alice',
            profile_picture: null
          }, createFragmentRef('1', singularQuery))
        },
        shouldUpdate: true
      }]);
      disableStoreUpdates(); // Update data in the store

      environment.commitPayload(singularQuery, {
        node: {
          __typename: 'User',
          id: '1',
          name: 'Alice updated'
        }
      }); // Assert that component did not re-render

      TestRenderer.act(function () {
        return jest.runAllImmediates();
      });
      expect(renderSpy).toBeCalledTimes(0);
    });
    it('re-renders with latest data after re-enabling updates, if any updates were missed', function () {
      renderSingularFragment();
      expectSingularFragmentResults([{
        data: {
          user: (0, _objectSpread2["default"])({
            id: '1',
            name: 'Alice',
            profile_picture: null
          }, createFragmentRef('1', singularQuery))
        },
        shouldUpdate: true
      }]);
      disableStoreUpdates(); // Update data in the store

      environment.commitPayload(singularQuery, {
        node: {
          __typename: 'User',
          id: '1',
          name: 'Alice updated'
        }
      }); // Assert that component did not re-render while updates are disabled

      TestRenderer.act(function () {
        return jest.runAllImmediates();
      });
      expect(renderSpy).toBeCalledTimes(0);
      enableStoreUpdates(); // Assert that component re-renders with latest updated data

      expectSingularFragmentResults([{
        data: {
          user: (0, _objectSpread2["default"])({
            id: '1',
            name: 'Alice updated',
            profile_picture: null
          }, createFragmentRef('1', singularQuery))
        },
        shouldUpdate: true
      }]);
    });
    it('does not re-render after re-enabling updates, if no updates were missed', function () {
      renderSingularFragment();
      expectSingularFragmentResults([{
        data: {
          user: (0, _objectSpread2["default"])({
            id: '1',
            name: 'Alice',
            profile_picture: null
          }, createFragmentRef('1', singularQuery))
        },
        shouldUpdate: true
      }]);
      disableStoreUpdates();
      expect(renderSpy).toBeCalledTimes(0);
      enableStoreUpdates(); // Assert that component did not re-render after enabling updates

      TestRenderer.act(function () {
        return jest.runAllImmediates();
      });
      expect(renderSpy).toBeCalledTimes(0);
    });
    it('does not re-render after re-enabling updates, if data did not change', function () {
      renderSingularFragment();
      expectSingularFragmentResults([{
        data: {
          user: (0, _objectSpread2["default"])({
            id: '1',
            name: 'Alice',
            profile_picture: null
          }, createFragmentRef('1', singularQuery))
        },
        shouldUpdate: true
      }]);
      disableStoreUpdates();
      environment.commitPayload(singularQuery, {
        node: {
          __typename: 'User',
          id: '1',
          name: 'Alice'
        }
      });
      TestRenderer.act(function () {
        return jest.runAllImmediates();
      });
      expect(renderSpy).toBeCalledTimes(0);
      enableStoreUpdates(); // Assert that component did not re-render after enabling updates

      TestRenderer.act(function () {
        return jest.runAllImmediates();
      });
      expect(renderSpy).toBeCalledTimes(0);
    });
  });
});