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

var _objectSpread2 = _interopRequireDefault(require("@babel/runtime/helpers/objectSpread"));

var _extends2 = _interopRequireDefault(require("@babel/runtime/helpers/extends"));

var _objectWithoutPropertiesLoose2 = _interopRequireDefault(require("@babel/runtime/helpers/objectWithoutPropertiesLoose"));

var _assertThisInitialized2 = _interopRequireDefault(require("@babel/runtime/helpers/assertThisInitialized"));

var _inheritsLoose2 = _interopRequireDefault(require("@babel/runtime/helpers/inheritsLoose"));

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

var React = require('react');

var useMemo = React.useMemo,
    useState = React.useState;

var TestRenderer = require('react-test-renderer');

var invariant = require("fbjs/lib/invariant");

var useRefetchableFragmentNodeOriginal = require('../useRefetchableFragmentNode');

var ReactRelayContext = require('react-relay/ReactRelayContext');

var _require = require('relay-runtime'),
    FRAGMENT_OWNER_KEY = _require.FRAGMENT_OWNER_KEY,
    FRAGMENTS_KEY = _require.FRAGMENTS_KEY,
    ID_KEY = _require.ID_KEY,
    createOperationDescriptor = _require.createOperationDescriptor;

describe('useRefetchableFragmentNode', function () {
  var environment;
  var gqlQuery;
  var gqlQueryNestedFragment;
  var gqlRefetchQuery;
  var gqlQueryWithArgs;
  var gqlQueryWithLiteralArgs;
  var gqlRefetchQueryWithArgs;
  var gqlFragment;
  var gqlFragmentWithArgs;
  var query;
  var queryNestedFragment;
  var refetchQuery;
  var queryWithArgs;
  var queryWithLiteralArgs;
  var refetchQueryWithArgs;
  var variables;
  var variablesNestedFragment;
  var setEnvironment;
  var setOwner;
  var fetchPolicy;
  var renderPolicy;
  var createMockEnvironment;
  var generateAndCompile;
  var renderFragment;
  var forceUpdate;
  var renderSpy;
  var refetch;
  var Renderer;

  var ErrorBoundary =
  /*#__PURE__*/
  function (_React$Component) {
    (0, _inheritsLoose2["default"])(ErrorBoundary, _React$Component);

    function ErrorBoundary() {
      var _this;

      for (var _len = arguments.length, args = new Array(_len), _key = 0; _key < _len; _key++) {
        args[_key] = arguments[_key];
      }

      _this = _React$Component.call.apply(_React$Component, [this].concat(args)) || this;
      (0, _defineProperty2["default"])((0, _assertThisInitialized2["default"])(_this), "state", {
        error: null
      });
      return _this;
    }

    var _proto = ErrorBoundary.prototype;

    _proto.componentDidCatch = function componentDidCatch(error) {
      this.setState({
        error: error
      });
    };

    _proto.render = function render() {
      var _this$props = this.props,
          children = _this$props.children,
          fallback = _this$props.fallback;
      var error = this.state.error;

      if (error) {
        return React.createElement(fallback, {
          error: error
        });
      }

      return children;
    };

    return ErrorBoundary;
  }(React.Component);

  function useRefetchableFragmentNode(fragmentNode, fragmentRef) {
    var result = useRefetchableFragmentNodeOriginal(fragmentNode, fragmentRef, 'TestDisplayName');
    refetch = result.refetch;
    renderSpy(result.fragmentData, refetch);
    return result;
  }

  function assertCall(expected, idx) {
    var actualData = renderSpy.mock.calls[idx][0];
    expect(actualData).toEqual(expected.data);
  }

  function expectFragmentResults(expectedCalls) {
    // This ensures that useEffect runs
    TestRenderer.act(function () {
      return jest.runAllImmediates();
    });
    expect(renderSpy).toBeCalledTimes(expectedCalls.length);
    expectedCalls.forEach(function (expected, idx) {
      return assertCall(expected, idx);
    });
    renderSpy.mockClear();
  }

  function createFragmentRef(id, owner) {
    var _ref3;

    return _ref3 = {}, (0, _defineProperty2["default"])(_ref3, ID_KEY, id), (0, _defineProperty2["default"])(_ref3, FRAGMENTS_KEY, {
      NestedUserFragment: {}
    }), (0, _defineProperty2["default"])(_ref3, FRAGMENT_OWNER_KEY, owner.request), _ref3;
  }

  beforeEach(function () {
    var _gqlFragment$metadata, _gqlFragment$metadata2, _gqlFragmentWithArgs$, _gqlFragmentWithArgs$2;

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
    fetchPolicy = 'store-or-network';
    renderPolicy = 'partial';

    var _require2 = require('relay-test-utils-internal');

    createMockEnvironment = _require2.createMockEnvironment;
    generateAndCompile = _require2.generateAndCompile;
    // Set up environment and base data
    environment = createMockEnvironment();
    var generated = generateAndCompile("\n        fragment NestedUserFragment on User {\n          username\n        }\n\n        fragment UserFragmentWithArgs on User\n        @refetchable(queryName: \"UserFragmentWithArgsRefetchQuery\")\n        @argumentDefinitions(scaleLocal: {type: \"Float!\"}) {\n          id\n          name\n          profile_picture(scale: $scaleLocal) {\n            uri\n          }\n          ...NestedUserFragment\n        }\n\n        fragment UserFragment on User\n        @refetchable(queryName: \"UserFragmentRefetchQuery\") {\n          id\n          name\n          profile_picture(scale: $scale) {\n            uri\n          }\n          ...NestedUserFragment\n        }\n\n        query UserQuery($id: ID!, $scale: Int!) {\n          node(id: $id) {\n            ...UserFragment\n          }\n        }\n\n        query UserQueryNestedFragment($id: ID!, $scale: Int!) {\n          node(id: $id) {\n            actor {\n              ...UserFragment\n            }\n          }\n        }\n\n        query UserQueryWithArgs($id: ID!, $scale: Float!) {\n          node(id: $id) {\n            ...UserFragmentWithArgs @arguments(scaleLocal: $scale)\n          }\n        }\n\n        query UserQueryWithLiteralArgs($id: ID!) {\n          node(id: $id) {\n            ...UserFragmentWithArgs @arguments(scaleLocal: 16)\n          }\n        }\n    ");
    variables = {
      id: '1',
      scale: 16
    };
    variablesNestedFragment = {
      id: '<feedbackid>',
      scale: 16
    };
    gqlQuery = generated.UserQuery;
    gqlQueryNestedFragment = generated.UserQueryNestedFragment;
    gqlRefetchQuery = generated.UserFragmentRefetchQuery;
    gqlQueryWithArgs = generated.UserQueryWithArgs;
    gqlQueryWithLiteralArgs = generated.UserQueryWithLiteralArgs;
    gqlRefetchQueryWithArgs = generated.UserFragmentWithArgsRefetchQuery;
    gqlFragment = generated.UserFragment;
    gqlFragmentWithArgs = generated.UserFragmentWithArgs;
    !(((_gqlFragment$metadata = gqlFragment.metadata) === null || _gqlFragment$metadata === void 0 ? void 0 : (_gqlFragment$metadata2 = _gqlFragment$metadata.refetch) === null || _gqlFragment$metadata2 === void 0 ? void 0 : _gqlFragment$metadata2.operation) === '@@MODULE_START@@UserFragmentRefetchQuery.graphql@@MODULE_END@@') ? process.env.NODE_ENV !== "production" ? invariant(false, 'useRefetchableFragment-test: Expected refetchable fragment metadata to contain operation.') : invariant(false) : void 0;
    !(((_gqlFragmentWithArgs$ = gqlFragmentWithArgs.metadata) === null || _gqlFragmentWithArgs$ === void 0 ? void 0 : (_gqlFragmentWithArgs$2 = _gqlFragmentWithArgs$.refetch) === null || _gqlFragmentWithArgs$2 === void 0 ? void 0 : _gqlFragmentWithArgs$2.operation) === '@@MODULE_START@@UserFragmentWithArgsRefetchQuery.graphql@@MODULE_END@@') ? process.env.NODE_ENV !== "production" ? invariant(false, 'useRefetchableFragment-test: Expected refetchable fragment metadata to contain operation.') : invariant(false) : void 0; // Manually set the refetchable operation for the test.

    gqlFragment.metadata.refetch.operation = gqlRefetchQuery;
    gqlFragmentWithArgs.metadata.refetch.operation = gqlRefetchQueryWithArgs;
    query = createOperationDescriptor(gqlQuery, variables);
    queryNestedFragment = createOperationDescriptor(gqlQueryNestedFragment, variablesNestedFragment);
    refetchQuery = createOperationDescriptor(gqlRefetchQuery, variables);
    queryWithArgs = createOperationDescriptor(gqlQueryWithArgs, variables);
    queryWithLiteralArgs = createOperationDescriptor(gqlQueryWithLiteralArgs, {
      id: variables.id
    });
    refetchQueryWithArgs = createOperationDescriptor(gqlRefetchQueryWithArgs, variables);
    environment.commitPayload(query, {
      node: {
        __typename: 'User',
        id: '1',
        name: 'Alice',
        username: 'useralice',
        profile_picture: null
      }
    }); // Set up renderers

    Renderer = function Renderer(props) {
      return null;
    };

    var Container = function Container(props) {
      var _props$fragment;

      // We need a render a component to run a Hook
      var _useState = useState(props.owner),
          owner = _useState[0],
          _setOwner = _useState[1];

      var _useState2 = useState(0),
          _ = _useState2[0],
          _setCount = _useState2[1];

      var fragment = (_props$fragment = props.fragment) !== null && _props$fragment !== void 0 ? _props$fragment : gqlFragment;
      var artificialUserRef = useMemo(function () {
        var _owner$request$variab, _ref4;

        return _ref4 = {}, (0, _defineProperty2["default"])(_ref4, ID_KEY, (_owner$request$variab = owner.request.variables.id) !== null && _owner$request$variab !== void 0 ? _owner$request$variab : owner.request.variables.nodeID), (0, _defineProperty2["default"])(_ref4, FRAGMENTS_KEY, (0, _defineProperty2["default"])({}, fragment.name, {})), (0, _defineProperty2["default"])(_ref4, FRAGMENT_OWNER_KEY, owner.request), _ref4;
      }, [owner, fragment.name]);
      var userRef = props.hasOwnProperty('userRef') ? props.userRef : artificialUserRef;
      setOwner = _setOwner;

      forceUpdate = function forceUpdate() {
        return _setCount(function (count) {
          return count + 1;
        });
      };

      var _useRefetchableFragme = useRefetchableFragmentNode(fragment, userRef),
          userData = _useRefetchableFragme.fragmentData;

      return React.createElement(Renderer, {
        user: userData
      });
    };

    var ContextProvider = function ContextProvider(_ref5) {
      var children = _ref5.children;

      var _useState3 = useState(environment),
          env = _useState3[0],
          _setEnv = _useState3[1]; // TODO(T39494051) - We set empty variables in relay context to make
      // Flow happy, but useRefetchableFragmentNode does not use them, instead it uses
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

    renderFragment = function renderFragment(args) {
      var _args;

      var _ref6 = (_args = args) !== null && _args !== void 0 ? _args : {},
          _ref6$isConcurrent = _ref6.isConcurrent,
          isConcurrent = _ref6$isConcurrent === void 0 ? false : _ref6$isConcurrent,
          props = (0, _objectWithoutPropertiesLoose2["default"])(_ref6, ["isConcurrent"]);

      var renderer;
      TestRenderer.act(function () {
        renderer = TestRenderer.create(React.createElement(ErrorBoundary, {
          fallback: function fallback(_ref7) {
            var error = _ref7.error;
            return "Error: ".concat(error.message);
          }
        }, React.createElement(React.Suspense, {
          fallback: "Fallback"
        }, React.createElement(ContextProvider, null, React.createElement(Container, (0, _extends2["default"])({
          owner: query
        }, props))))), {
          unstable_isConcurrent: isConcurrent
        });
      });
      return renderer;
    };
  });
  afterEach(function () {
    environment.mockClear();
    renderSpy.mockClear();
  });
  describe('initial render', function () {
    // The bulk of initial render behavior is covered in useFragmentNodes-test,
    // so this suite covers the basic cases as a sanity check.
    it('should throw error if fragment is plural', function () {
      jest.spyOn(console, 'error').mockImplementationOnce(function () {});
      var generated = generateAndCompile("\n        fragment UserFragment on User @relay(plural: true) {\n          id\n        }\n      ");
      var renderer = renderFragment({
        fragment: generated.UserFragment
      });
      expect(renderer.toJSON().includes('Remove `@relay(plural: true)` from fragment')).toEqual(true);
    });
    it('should throw error if fragment is missing @refetchable directive', function () {
      jest.spyOn(console, 'error').mockImplementationOnce(function () {});
      var generated = generateAndCompile("\n        fragment UserFragment on User {\n          id\n        }\n      ");
      var renderer = renderFragment({
        fragment: generated.UserFragment
      });
      expect(renderer.toJSON().includes('Did you forget to add a @refetchable directive to the fragment?')).toEqual(true);
    });
    it('should render fragment without error when data is available', function () {
      renderFragment();
      expectFragmentResults([{
        data: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', query))
      }]);
    });
    it('should render fragment without error when ref is null', function () {
      renderFragment({
        userRef: null
      });
      expectFragmentResults([{
        data: null
      }]);
    });
    it('should render fragment without error when ref is undefined', function () {
      renderFragment({
        userRef: undefined
      });
      expectFragmentResults([{
        data: null
      }]);
    });
    it('should update when fragment data changes', function () {
      renderFragment();
      expectFragmentResults([{
        data: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', query))
      }]);
      environment.commitPayload(query, {
        node: {
          __typename: 'User',
          id: '1',
          // Update name
          name: 'Alice in Wonderland'
        }
      });
      expectFragmentResults([{
        data: (0, _objectSpread2["default"])({
          id: '1',
          // Assert that name is updated
          name: 'Alice in Wonderland',
          profile_picture: null
        }, createFragmentRef('1', query))
      }]);
    });
    it('should throw a promise if data is missing for fragment and request is in flight', function () {
      // This prevents console.error output in the test, which is expected
      jest.spyOn(console, 'error').mockImplementationOnce(function () {});
      jest.spyOn(require('relay-runtime').__internal, 'getPromiseForRequestInFlight').mockImplementationOnce(function () {
        return Promise.resolve();
      });
      var missingDataVariables = (0, _objectSpread2["default"])({}, variables, {
        id: '4'
      });
      var missingDataQuery = createOperationDescriptor(gqlQuery, missingDataVariables); // Commit a payload with name and profile_picture are missing

      environment.commitPayload(missingDataQuery, {
        node: {
          __typename: 'User',
          id: '4'
        }
      });
      var renderer = renderFragment({
        owner: missingDataQuery
      });
      expect(renderer.toJSON()).toEqual('Fallback');
    });
  });
  describe('refetch', function () {
    var release;
    beforeEach(function () {
      jest.resetModules();

      var _require3 = require('relay-test-utils-internal');

      createMockEnvironment = _require3.createMockEnvironment;
      generateAndCompile = _require3.generateAndCompile;
      release = jest.fn();
      environment.retain.mockImplementation(function () {
        return {
          dispose: release
        };
      });
    });

    function expectRequestIsInFlight(expected) {
      var _expected$gqlRefetchQ;

      var requestEnvironment = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : environment;
      expect(requestEnvironment.execute).toBeCalledTimes(expected.requestCount);
      expect(requestEnvironment.mock.isLoading((_expected$gqlRefetchQ = expected.gqlRefetchQuery) !== null && _expected$gqlRefetchQ !== void 0 ? _expected$gqlRefetchQ : gqlRefetchQuery, expected.refetchVariables, {
        force: true
      })).toEqual(expected.inFlight);
    }

    function expectFragmentIsRefetching(renderer, expected) {
      var _ref, _expected$refetchQuer;

      var env = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : environment;
      expect(renderSpy).toBeCalledTimes(0);
      renderSpy.mockClear(); // Assert refetch query was fetched

      expectRequestIsInFlight((0, _objectSpread2["default"])({}, expected, {
        inFlight: true,
        requestCount: 1
      }), env); // Assert component suspended

      expect(renderSpy).toBeCalledTimes(0);
      expect(renderer.toJSON()).toEqual('Fallback'); // Assert query is tentatively retained while component is suspended

      expect(env.retain).toBeCalledTimes(1);
      expect(env.retain.mock.calls[0][0]).toEqual((_ref = (_expected$refetchQuer = expected.refetchQuery) === null || _expected$refetchQuer === void 0 ? void 0 : _expected$refetchQuer.root) !== null && _ref !== void 0 ? _ref : refetchQuery.root);
    }

    it('does not refetch and warns if component has unmounted', function () {
      var warning = require("fbjs/lib/warning");

      var renderer = renderFragment();
      var initialUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: null
      }, createFragmentRef('1', query));
      expectFragmentResults([{
        data: initialUser
      }]);
      renderer.unmount();
      TestRenderer.act(function () {
        refetch({
          id: '4'
        });
      });
      expect(warning).toHaveBeenCalledTimes(1);
      expect( // $FlowFixMe
      warning.mock.calls[0][1].includes('Relay: Unexpected call to `refetch` on unmounted component')).toEqual(true);
      expect(environment.execute).toHaveBeenCalledTimes(0);
    });
    it('warns if fragment ref passed to useRefetchableFragmentNode() was null', function () {
      var warning = require("fbjs/lib/warning");

      renderFragment({
        userRef: null
      });
      expectFragmentResults([{
        data: null
      }]);
      TestRenderer.act(function () {
        refetch({
          id: '4'
        });
      });
      expect(warning).toHaveBeenCalledTimes(1);
      expect( // $FlowFixMe
      warning.mock.calls[0][1].includes('Relay: Unexpected call to `refetch` while using a null fragment ref')).toEqual(true);
      expect(environment.execute).toHaveBeenCalledTimes(1);
    });
    it('warns if refetch scheduled at high priority', function () {
      var warning = require("fbjs/lib/warning");

      var Scheduler = require('scheduler');

      renderFragment();
      var initialUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: null
      }, createFragmentRef('1', query));
      expectFragmentResults([{
        data: initialUser
      }]);
      TestRenderer.act(function () {
        Scheduler.unstable_runWithPriority(Scheduler.unstable_ImmediatePriority, function () {
          refetch({
            id: '4'
          });
        });
      });
      expect(warning).toHaveBeenCalledTimes(1);
      expect( // $FlowFixMe
      warning.mock.calls[0][1].includes('Relay: Unexpected call to `refetch` at a priority higher than expected')).toEqual(true);
      expect(environment.execute).toHaveBeenCalledTimes(1);
    });
    it('throws error when error occurs during refetch', function () {
      jest.spyOn(console, 'error').mockImplementationOnce(function () {});
      var callback = jest.fn();
      var renderer = renderFragment();
      var initialUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: null
      }, createFragmentRef('1', query));
      expectFragmentResults([{
        data: initialUser
      }]);
      TestRenderer.act(function () {
        refetch({
          id: '4'
        }, {
          onComplete: callback
        });
      }); // Assert that fragment is refetching with the right variables and
      // suspends upon refetch

      var refetchVariables = {
        id: '4',
        scale: 16
      };
      refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
      expectFragmentIsRefetching(renderer, {
        refetchVariables: refetchVariables,
        refetchQuery: refetchQuery
      }); // Mock network error

      environment.mock.reject(gqlRefetchQuery, new Error('Oops'));
      TestRenderer.act(function () {
        jest.runAllImmediates();
      }); // Assert error is caught in Error boundary

      expect(renderer.toJSON()).toEqual('Error: Oops');
      expect(callback).toBeCalledTimes(1);
      expect(callback.mock.calls[0][0]).toMatchObject({
        message: 'Oops'
      }); // Assert refetch query wasn't retained

      TestRenderer.act(function () {
        jest.runAllTimers();
      });
      expect(release).toBeCalledTimes(1);
      expect(environment.retain).toBeCalledTimes(1);
    });
    it('refetches new variables correctly when refetching new id', function () {
      var renderer = renderFragment();
      var initialUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: null
      }, createFragmentRef('1', query));
      expectFragmentResults([{
        data: initialUser
      }]);
      TestRenderer.act(function () {
        refetch({
          id: '4'
        });
      }); // Assert that fragment is refetching with the right variables and
      // suspends upon refetch

      var refetchVariables = {
        id: '4',
        scale: 16
      };
      refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
      expectFragmentIsRefetching(renderer, {
        refetchVariables: refetchVariables,
        refetchQuery: refetchQuery
      }); // Mock network response

      environment.mock.resolve(gqlRefetchQuery, {
        data: {
          node: {
            __typename: 'User',
            id: '4',
            name: 'Mark',
            profile_picture: {
              uri: 'scale16'
            },
            username: 'usermark'
          }
        }
      }); // Assert fragment is rendered with new data

      var refetchedUser = (0, _objectSpread2["default"])({
        id: '4',
        name: 'Mark',
        profile_picture: {
          uri: 'scale16'
        }
      }, createFragmentRef('4', refetchQuery));
      expectFragmentResults([{
        data: refetchedUser
      }, {
        data: refetchedUser
      }]); // Assert refetch query was retained

      expect(release).not.toBeCalled();
      expect(environment.retain).toBeCalledTimes(1);
      expect(environment.retain.mock.calls[0][0]).toEqual(refetchQuery.root);
    });
    it('refetches new variables correctly when refetching same id', function () {
      var renderer = renderFragment();
      var initialUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: null
      }, createFragmentRef('1', query));
      expectFragmentResults([{
        data: initialUser
      }]);
      TestRenderer.act(function () {
        refetch({
          scale: 32
        });
      }); // Assert that fragment is refetching with the right variables and
      // suspends upon refetch

      var refetchVariables = {
        id: '1',
        scale: 32
      };
      refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
      expectFragmentIsRefetching(renderer, {
        refetchVariables: refetchVariables,
        refetchQuery: refetchQuery
      }); // Mock network response

      environment.mock.resolve(gqlRefetchQuery, {
        data: {
          node: {
            __typename: 'User',
            id: '1',
            name: 'Alice',
            profile_picture: {
              uri: 'scale32'
            },
            username: 'useralice'
          }
        }
      }); // Assert fragment is rendered with new data

      var refetchedUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: {
          uri: 'scale32'
        }
      }, createFragmentRef('1', refetchQuery));
      expectFragmentResults([{
        data: refetchedUser
      }, {
        data: refetchedUser
      }]); // Assert refetch query was retained

      expect(release).not.toBeCalled();
      expect(environment.retain).toBeCalledTimes(1);
      expect(environment.retain.mock.calls[0][0]).toEqual(refetchQuery.root);
    });
    it('with correct id from refetchable fragment when using nested fragment', function () {
      var _ref2, _ref2$node;

      // Populate store with data for query using nested fragment
      environment.commitPayload(queryNestedFragment, {
        node: {
          __typename: 'Feedback',
          id: '<feedbackid>',
          actor: {
            __typename: 'User',
            id: '1',
            name: 'Alice',
            username: 'useralice',
            profile_picture: null
          }
        }
      }); // Get fragment ref for user using nested fragment

      var userRef = (_ref2 = environment.lookup(queryNestedFragment.fragment).data) === null || _ref2 === void 0 ? void 0 : (_ref2$node = _ref2.node) === null || _ref2$node === void 0 ? void 0 : _ref2$node.actor;
      var renderer = renderFragment({
        owner: queryNestedFragment,
        userRef: userRef
      });
      var initialUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: null
      }, createFragmentRef('1', queryNestedFragment));
      expectFragmentResults([{
        data: initialUser
      }]);
      TestRenderer.act(function () {
        refetch({
          scale: 32
        });
      }); // Assert that fragment is refetching with the right variables and
      // suspends upon refetch

      var refetchVariables = {
        // The id here should correspond to the user id, and not the
        // feedback id from the query variables (i.e. `<feedbackid>`)
        id: '1',
        scale: 32
      };
      refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
      expectFragmentIsRefetching(renderer, {
        refetchVariables: refetchVariables,
        refetchQuery: refetchQuery
      }); // Mock network response

      environment.mock.resolve(gqlRefetchQuery, {
        data: {
          node: {
            __typename: 'User',
            id: '1',
            name: 'Alice',
            profile_picture: {
              uri: 'scale32'
            },
            username: 'useralice'
          }
        }
      }); // Assert fragment is rendered with new data

      var refetchedUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: {
          uri: 'scale32'
        }
      }, createFragmentRef('1', refetchQuery));
      expectFragmentResults([{
        data: refetchedUser
      }, {
        data: refetchedUser
      }]); // Assert refetch query was retained

      expect(release).not.toBeCalled();
      expect(environment.retain).toBeCalledTimes(1);
      expect(environment.retain.mock.calls[0][0]).toEqual(refetchQuery.root);
    });
    it('refetches correctly when refetching multiple times', function () {
      var renderer = renderFragment();
      var initialUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: null
      }, createFragmentRef('1', query));
      expectFragmentResults([{
        data: initialUser
      }]);
      var refetchVariables = {
        id: '1',
        scale: 32
      };
      refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
      var refetchedUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: {
          uri: 'scale32'
        }
      }, createFragmentRef('1', refetchQuery));

      var doAndAssertRefetch = function doAndAssertRefetch(fragmentResults) {
        renderSpy.mockClear();
        environment.execute.mockClear();
        environment.retain.mockClear();
        release.mockClear();
        TestRenderer.act(function () {
          // We use fetchPolicy network-only to ensure the call to refetch
          // always suspends
          refetch({
            scale: 32
          }, {
            fetchPolicy: 'network-only'
          });
        }); // Assert that fragment is refetching with the right variables and
        // suspends upon refetch

        expectFragmentIsRefetching(renderer, {
          refetchVariables: refetchVariables,
          refetchQuery: refetchQuery
        }); // Mock network response

        environment.mock.resolve(gqlRefetchQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              profile_picture: {
                uri: 'scale32'
              },
              username: 'useralice'
            }
          }
        }); // Assert fragment is rendered with new data

        expectFragmentResults(fragmentResults); // Assert refetch query was retained

        expect(release).not.toBeCalled();
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(refetchQuery.root);
      }; // Refetch once


      doAndAssertRefetch([{
        data: refetchedUser
      }, {
        data: refetchedUser
      }]); // Refetch twice

      doAndAssertRefetch([{
        data: refetchedUser
      }]);
    });
    it('refetches new variables correctly when using @arguments', function () {
      var _environment$lookup$d;

      var userRef = (_environment$lookup$d = environment.lookup(queryWithArgs.fragment).data) === null || _environment$lookup$d === void 0 ? void 0 : _environment$lookup$d.node;
      var renderer = renderFragment({
        fragment: gqlFragmentWithArgs,
        userRef: userRef
      });
      var initialUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: null
      }, createFragmentRef('1', queryWithArgs));
      expectFragmentResults([{
        data: initialUser
      }]);
      TestRenderer.act(function () {
        refetch({
          scaleLocal: 32
        });
      }); // Assert that fragment is refetching with the right variables and
      // suspends upon refetch

      var refetchVariables = {
        id: '1',
        scaleLocal: 32
      };
      refetchQueryWithArgs = createOperationDescriptor(gqlRefetchQueryWithArgs, refetchVariables);
      expectFragmentIsRefetching(renderer, {
        refetchVariables: refetchVariables,
        refetchQuery: refetchQueryWithArgs,
        gqlRefetchQuery: gqlRefetchQueryWithArgs
      }); // Mock network response

      environment.mock.resolve(gqlRefetchQueryWithArgs, {
        data: {
          node: {
            __typename: 'User',
            id: '1',
            name: 'Alice',
            profile_picture: {
              uri: 'scale32'
            },
            username: 'useralice'
          }
        }
      }); // Assert fragment is rendered with new data

      var refetchedUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: {
          uri: 'scale32'
        }
      }, createFragmentRef('1', refetchQueryWithArgs));
      expectFragmentResults([{
        data: refetchedUser
      }, {
        data: refetchedUser
      }]); // Assert refetch query was retained

      expect(release).not.toBeCalled();
      expect(environment.retain).toBeCalledTimes(1);
      expect(environment.retain.mock.calls[0][0]).toEqual(refetchQueryWithArgs.root);
    });
    it('refetches new variables correctly when using @arguments with literal values', function () {
      var _environment$lookup$d2;

      var userRef = (_environment$lookup$d2 = environment.lookup(queryWithLiteralArgs.fragment).data) === null || _environment$lookup$d2 === void 0 ? void 0 : _environment$lookup$d2.node;
      var renderer = renderFragment({
        fragment: gqlFragmentWithArgs,
        userRef: userRef
      });
      var initialUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: null
      }, createFragmentRef('1', queryWithLiteralArgs));
      expectFragmentResults([{
        data: initialUser
      }]);
      TestRenderer.act(function () {
        refetch({
          id: '4'
        });
      }); // Assert that fragment is refetching with the right variables and
      // suspends upon refetch

      var refetchVariables = {
        id: '4',
        scaleLocal: 16
      };
      refetchQueryWithArgs = createOperationDescriptor(gqlRefetchQueryWithArgs, refetchVariables);
      expectFragmentIsRefetching(renderer, {
        refetchVariables: refetchVariables,
        refetchQuery: refetchQueryWithArgs,
        gqlRefetchQuery: gqlRefetchQueryWithArgs
      }); // Mock network response

      environment.mock.resolve(gqlRefetchQueryWithArgs, {
        data: {
          node: {
            __typename: 'User',
            id: '4',
            name: 'Mark',
            profile_picture: {
              uri: 'scale16'
            },
            username: 'usermark'
          }
        }
      }); // Assert fragment is rendered with new data

      var refetchedUser = (0, _objectSpread2["default"])({
        id: '4',
        name: 'Mark',
        profile_picture: {
          uri: 'scale16'
        }
      }, createFragmentRef('4', refetchQueryWithArgs));
      expectFragmentResults([{
        data: refetchedUser
      }, {
        data: refetchedUser
      }]); // Assert refetch query was retained

      expect(release).not.toBeCalled();
      expect(environment.retain).toBeCalledTimes(1);
      expect(environment.retain.mock.calls[0][0]).toEqual(refetchQueryWithArgs.root);
    });
    it('subscribes to changes in refetched data', function () {
      renderFragment();
      renderSpy.mockClear();
      TestRenderer.act(function () {
        refetch({
          id: '4'
        });
      }); // Mock network response

      environment.mock.resolve(gqlRefetchQuery, {
        data: {
          node: {
            __typename: 'User',
            id: '4',
            name: 'Mark',
            profile_picture: {
              uri: 'scale16'
            },
            username: 'usermark'
          }
        }
      }); // Assert fragment is rendered with new data

      var refetchVariables = {
        id: '4',
        scale: 16
      };
      refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
      var refetchedUser = (0, _objectSpread2["default"])({
        id: '4',
        name: 'Mark',
        profile_picture: {
          uri: 'scale16'
        }
      }, createFragmentRef('4', refetchQuery));
      expectFragmentResults([{
        data: refetchedUser
      }, {
        data: refetchedUser
      }]); // Assert refetch query was retained

      expect(release).not.toBeCalled();
      expect(environment.retain).toBeCalledTimes(1);
      expect(environment.retain.mock.calls[0][0]).toEqual(refetchQuery.root); // Update refetched data

      environment.commitPayload(refetchQuery, {
        node: {
          __typename: 'User',
          id: '4',
          name: 'Mark Updated'
        }
      }); // Assert that refetched data is updated

      expectFragmentResults([{
        data: (0, _objectSpread2["default"])({
          id: '4',
          // Name is updated
          name: 'Mark Updated',
          profile_picture: {
            uri: 'scale16'
          }
        }, createFragmentRef('4', refetchQuery))
      }]);
    });
    it('resets to parent data when environment changes', function () {
      renderFragment();
      renderSpy.mockClear();
      TestRenderer.act(function () {
        refetch({
          id: '4'
        });
      }); // Mock network response

      environment.mock.resolve(gqlRefetchQuery, {
        data: {
          node: {
            __typename: 'User',
            id: '4',
            name: 'Mark',
            profile_picture: {
              uri: 'scale16'
            },
            username: 'usermark'
          }
        }
      }); // Assert fragment is rendered with new data

      var refetchVariables = {
        id: '4',
        scale: 16
      };
      refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
      var refetchedUser = (0, _objectSpread2["default"])({
        id: '4',
        name: 'Mark',
        profile_picture: {
          uri: 'scale16'
        }
      }, createFragmentRef('4', refetchQuery));
      expectFragmentResults([{
        data: refetchedUser
      }, {
        data: refetchedUser
      }]); // Assert refetch query was retained

      expect(release).not.toBeCalled();
      expect(environment.retain).toBeCalledTimes(1);
      expect(environment.retain.mock.calls[0][0]).toEqual(refetchQuery.root); // Set new environment

      var newEnvironment = createMockEnvironment();
      newEnvironment.commitPayload(query, {
        node: {
          __typename: 'User',
          id: '1',
          name: 'Alice in a different env',
          username: 'useralice',
          profile_picture: null
        }
      });
      TestRenderer.act(function () {
        setEnvironment(newEnvironment);
      }); // Assert that parent data is rendered

      var expectedUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice in a different env',
        profile_picture: null
      }, createFragmentRef('1', query));
      expectFragmentResults([{
        data: expectedUser
      }, {
        data: expectedUser
      }, {
        data: expectedUser
      }]); // Assert refetch query was released

      expect(release).toBeCalledTimes(1);
      expect(environment.retain).toBeCalledTimes(1); // Update data in new environment

      newEnvironment.commitPayload(query, {
        node: {
          __typename: 'User',
          id: '1',
          name: 'Alice Updated'
        }
      }); // Assert that data in new environment is updated

      expectFragmentResults([{
        data: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice Updated',
          profile_picture: null
        }, createFragmentRef('1', query))
      }]);
    });
    it('resets to parent data when parent fragment ref changes', function () {
      renderFragment();
      renderSpy.mockClear();
      TestRenderer.act(function () {
        refetch({
          id: '4'
        });
      }); // Mock network response

      environment.mock.resolve(gqlRefetchQuery, {
        data: {
          node: {
            __typename: 'User',
            id: '4',
            name: 'Mark',
            profile_picture: {
              uri: 'scale16'
            },
            username: 'usermark'
          }
        }
      }); // Assert fragment is rendered with new data

      var refetchVariables = {
        id: '4',
        scale: 16
      };
      refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
      var refetchedUser = (0, _objectSpread2["default"])({
        id: '4',
        name: 'Mark',
        profile_picture: {
          uri: 'scale16'
        }
      }, createFragmentRef('4', refetchQuery));
      expectFragmentResults([{
        data: refetchedUser
      }, {
        data: refetchedUser
      }]); // Assert refetch query was retained

      expect(release).not.toBeCalled();
      expect(environment.retain).toBeCalledTimes(1);
      expect(environment.retain.mock.calls[0][0]).toEqual(refetchQuery.root); // Pass new parent fragment ref with different variables

      var newVariables = (0, _objectSpread2["default"])({}, variables, {
        scale: 32
      });
      var newQuery = createOperationDescriptor(gqlQuery, newVariables);
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
      });
      TestRenderer.act(function () {
        setOwner(newQuery);
      }); // Assert that parent data is rendered

      var expectedUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: {
          uri: 'uri32'
        }
      }, createFragmentRef('1', newQuery));
      expectFragmentResults([{
        data: expectedUser
      }, {
        data: expectedUser
      }, {
        data: expectedUser
      }]); // Assert refetch query was released

      expect(release).toBeCalledTimes(1);
      expect(environment.retain).toBeCalledTimes(1); // Update new parent data

      environment.commitPayload(newQuery, {
        node: {
          __typename: 'User',
          id: '1',
          name: 'Alice Updated'
        }
      }); // Assert that new data from parent is updated

      expectFragmentResults([{
        data: (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice Updated',
          profile_picture: {
            uri: 'uri32'
          }
        }, createFragmentRef('1', newQuery))
      }]);
    });
    it('warns if data retured has different __typename', function () {
      var renderer = renderFragment();
      var initialUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: null
      }, createFragmentRef('1', query));
      expectFragmentResults([{
        data: initialUser
      }]);
      var refetchVariables = {
        id: '1',
        scale: 32
      };
      refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
      renderSpy.mockClear();
      environment.execute.mockClear();
      environment.retain.mockClear();
      release.mockClear();
      TestRenderer.act(function () {
        refetch({
          scale: 32
        }, {
          fetchPolicy: 'network-only'
        });
      });
      expectFragmentIsRefetching(renderer, {
        refetchVariables: refetchVariables,
        refetchQuery: refetchQuery
      }); // Mock network response

      environment.mock.resolve(gqlRefetchQuery, {
        data: {
          node: {
            __typename: 'MessagingParticipant',
            id: '1',
            name: 'Alice',
            profile_picture: {
              uri: 'scale32'
            },
            username: 'useralice'
          }
        }
      });
      TestRenderer.act(function () {
        jest.runAllImmediates();
      });

      var warning = require("fbjs/lib/warning"); // $FlowFixMe


      var warningCalls = warning.mock.calls.filter(function (call) {
        return call[0] === false;
      });
      expect(warningCalls.length).toEqual(4); // the other warnings are from FragmentResource.js

      expect(warningCalls[1][1].includes('Relay: Call to `refetch` returned data with a different __typename:')).toEqual(true);
    });
    it('warns if a different id is returned', function () {
      var renderer = renderFragment();
      var initialUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: null
      }, createFragmentRef('1', query));
      expectFragmentResults([{
        data: initialUser
      }]);
      var refetchVariables = {
        id: '1',
        scale: 32
      };
      refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
      renderSpy.mockClear();
      environment.execute.mockClear();
      environment.retain.mockClear();
      release.mockClear();
      TestRenderer.act(function () {
        refetch({
          scale: 32
        }, {
          fetchPolicy: 'network-only'
        });
      });
      expectFragmentIsRefetching(renderer, {
        refetchVariables: refetchVariables,
        refetchQuery: refetchQuery
      });
      environment.mock.resolve(gqlRefetchQuery, {
        data: {
          node: {
            __typename: 'User',
            id: '2',
            name: 'Mark',
            profile_picture: {
              uri: 'scale32'
            },
            username: 'usermark'
          }
        }
      });
      TestRenderer.act(function () {
        jest.runAllImmediates();
      });

      var warning = require("fbjs/lib/warning"); // $FlowFixMe


      var warningCalls = warning.mock.calls.filter(function (call) {
        return call[0] === false;
      });
      expect(warningCalls.length).toEqual(2);
      expect(warningCalls[0][1].includes('Relay: Call to `refetch` returned a different id, expected')).toEqual(true);
    });
    it("doesn't warn if refetching on a different id than the current one in display", function () {
      renderFragment();
      var initialUser = (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: null
      }, createFragmentRef('1', query));
      expectFragmentResults([{
        data: initialUser
      }]);
      var refetchVariables = {
        id: '1',
        scale: 32
      };
      refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
      renderSpy.mockClear();
      environment.execute.mockClear();
      environment.retain.mockClear();
      release.mockClear();
      TestRenderer.act(function () {
        refetch({
          id: '2',
          scale: 32
        }, {
          fetchPolicy: 'network-only'
        });
        jest.runAllImmediates();
      });
      TestRenderer.act(function () {
        refetch({
          id: '3',
          scale: 32
        }, {
          fetchPolicy: 'network-only'
        });
      });
      environment.mock.resolve(gqlRefetchQuery, {
        data: {
          node: {
            __typename: 'User',
            id: '3',
            name: 'Mark',
            profile_picture: {
              uri: 'scale32'
            },
            username: 'usermark'
          }
        }
      });
      TestRenderer.act(function () {
        jest.runAllTimers();
      });

      var warning = require("fbjs/lib/warning");

      expect( // $FlowFixMe
      warning.mock.calls.filter(function (call) {
        return call[0] === false;
      }).length).toEqual(0);
    });
    describe('fetchPolicy', function () {
      describe('store-or-network', function () {
        beforeEach(function () {
          fetchPolicy = 'store-or-network';
        });
        describe('renderPolicy: partial', function () {
          beforeEach(function () {
            renderPolicy = 'partial';
          });
          it("doesn't start network request if refetch query is fully cached", function () {
            renderFragment();
            renderSpy.mockClear();
            TestRenderer.act(function () {
              refetch({
                id: '1'
              }, {
                fetchPolicy: fetchPolicy,
                renderPolicy: renderPolicy
              });
            }); // Assert request is not started

            var refetchVariables = (0, _objectSpread2["default"])({}, variables);
            refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
            expectRequestIsInFlight({
              inFlight: false,
              requestCount: 0,
              gqlRefetchQuery: gqlRefetchQuery,
              refetchVariables: refetchVariables
            }); // Assert component renders immediately since data is cached

            var refetchingUser = (0, _objectSpread2["default"])({
              id: '1',
              name: 'Alice',
              profile_picture: null
            }, createFragmentRef('1', refetchQuery));
            expectFragmentResults([{
              data: refetchingUser
            }, {
              data: refetchingUser
            }]);
          });
          it('starts network request if refetch query is not fully cached and suspends if fragment has missing data', function () {
            var renderer = renderFragment();
            var initialUser = (0, _objectSpread2["default"])({
              id: '1',
              name: 'Alice',
              profile_picture: null
            }, createFragmentRef('1', query));
            expectFragmentResults([{
              data: initialUser
            }]);
            TestRenderer.act(function () {
              refetch({
                id: '4'
              }, {
                fetchPolicy: fetchPolicy,
                renderPolicy: renderPolicy
              });
            }); // Assert that fragment is refetching with the right variables and
            // suspends upon refetch

            var refetchVariables = {
              id: '4',
              scale: 16
            };
            refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
            expectFragmentIsRefetching(renderer, {
              refetchVariables: refetchVariables,
              refetchQuery: refetchQuery
            }); // Mock network response

            environment.mock.resolve(gqlRefetchQuery, {
              data: {
                node: {
                  __typename: 'User',
                  id: '4',
                  name: 'Mark',
                  profile_picture: {
                    uri: 'scale16'
                  },
                  username: 'usermark'
                }
              }
            }); // Assert fragment is rendered with new data

            var refetchedUser = (0, _objectSpread2["default"])({
              id: '4',
              name: 'Mark',
              profile_picture: {
                uri: 'scale16'
              }
            }, createFragmentRef('4', refetchQuery));
            expectFragmentResults([{
              data: refetchedUser
            }, {
              data: refetchedUser
            }]);
          });
          it("starts network request if refetch query is not fully cached and doesn't suspend if fragment doesn't have missing data", function () {
            // Cache user with missing username
            var refetchVariables = {
              id: '4',
              scale: 16
            };
            refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
            environment.commitPayload(refetchQuery, {
              node: {
                __typename: 'User',
                id: '4',
                name: 'Mark',
                profile_picture: null
              }
            });
            renderFragment();
            renderSpy.mockClear();
            TestRenderer.act(function () {
              refetch({
                id: '4'
              }, {
                fetchPolicy: fetchPolicy,
                renderPolicy: renderPolicy
              });
            }); // Assert request is started

            expectRequestIsInFlight({
              inFlight: true,
              requestCount: 1,
              gqlRefetchQuery: gqlRefetchQuery,
              refetchVariables: refetchVariables
            }); // Assert component renders immediately since data is cached

            var refetchingUser = (0, _objectSpread2["default"])({
              id: '4',
              name: 'Mark',
              profile_picture: null
            }, createFragmentRef('4', refetchQuery));
            expectFragmentResults([{
              data: refetchingUser
            }, {
              data: refetchingUser
            }]);
          });
        });
        describe('renderPolicy: full', function () {
          beforeEach(function () {
            renderPolicy = 'full';
          });
          it("doesn't start network request if refetch query is fully cached", function () {
            renderFragment();
            renderSpy.mockClear();
            TestRenderer.act(function () {
              refetch({
                id: '1'
              }, {
                fetchPolicy: fetchPolicy,
                renderPolicy: renderPolicy
              });
            }); // Assert request is not started

            var refetchVariables = (0, _objectSpread2["default"])({}, variables);
            refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
            expectRequestIsInFlight({
              inFlight: false,
              requestCount: 0,
              gqlRefetchQuery: gqlRefetchQuery,
              refetchVariables: refetchVariables
            }); // Assert component renders immediately since data is cached

            var refetchingUser = (0, _objectSpread2["default"])({
              id: '1',
              name: 'Alice',
              profile_picture: null
            }, createFragmentRef('1', refetchQuery));
            expectFragmentResults([{
              data: refetchingUser
            }, {
              data: refetchingUser
            }]);
          });
          it('starts network request if refetch query is not fully cached and suspends if fragment has missing data', function () {
            var renderer = renderFragment();
            var initialUser = (0, _objectSpread2["default"])({
              id: '1',
              name: 'Alice',
              profile_picture: null
            }, createFragmentRef('1', query));
            expectFragmentResults([{
              data: initialUser
            }]);
            TestRenderer.act(function () {
              refetch({
                id: '4'
              }, {
                fetchPolicy: fetchPolicy,
                renderPolicy: renderPolicy
              });
            }); // Assert that fragment is refetching with the right variables and
            // suspends upon refetch

            var refetchVariables = {
              id: '4',
              scale: 16
            };
            refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
            expectFragmentIsRefetching(renderer, {
              refetchVariables: refetchVariables,
              refetchQuery: refetchQuery
            }); // Mock network response

            environment.mock.resolve(gqlRefetchQuery, {
              data: {
                node: {
                  __typename: 'User',
                  id: '4',
                  name: 'Mark',
                  profile_picture: {
                    uri: 'scale16'
                  },
                  username: 'usermark'
                }
              }
            }); // Assert fragment is rendered with new data

            var refetchedUser = (0, _objectSpread2["default"])({
              id: '4',
              name: 'Mark',
              profile_picture: {
                uri: 'scale16'
              }
            }, createFragmentRef('4', refetchQuery));
            expectFragmentResults([{
              data: refetchedUser
            }, {
              data: refetchedUser
            }]);
          });
          it("starts network request if refetch query is not fully cached and suspends even if fragment doesn't have missing data", function () {
            // Cache user with missing username
            var refetchVariables = {
              id: '4',
              scale: 16
            };
            refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
            environment.commitPayload(refetchQuery, {
              node: {
                __typename: 'User',
                id: '4',
                name: 'Mark',
                profile_picture: null
              }
            });
            var renderer = renderFragment();
            var initialUser = (0, _objectSpread2["default"])({
              id: '1',
              name: 'Alice',
              profile_picture: null
            }, createFragmentRef('1', query));
            expectFragmentResults([{
              data: initialUser
            }]);
            TestRenderer.act(function () {
              refetch({
                id: '4'
              }, {
                fetchPolicy: fetchPolicy,
                renderPolicy: renderPolicy
              });
            });
            expectFragmentIsRefetching(renderer, {
              refetchVariables: refetchVariables,
              refetchQuery: refetchQuery
            }); // Mock network response

            environment.mock.resolve(gqlRefetchQuery, {
              data: {
                node: {
                  __typename: 'User',
                  id: '4',
                  name: 'Mark',
                  profile_picture: {
                    uri: 'scale16'
                  },
                  username: 'usermark'
                }
              }
            }); // Assert fragment is rendered with new data

            var refetchedUser = (0, _objectSpread2["default"])({
              id: '4',
              name: 'Mark',
              profile_picture: {
                uri: 'scale16'
              }
            }, createFragmentRef('4', refetchQuery));
            expectFragmentResults([{
              data: refetchedUser
            }, {
              data: refetchedUser
            }]);
          });
        });
      });
      describe('store-and-network', function () {
        beforeEach(function () {
          fetchPolicy = 'store-and-network';
        });
        describe('renderPolicy: partial', function () {
          beforeEach(function () {
            renderPolicy = 'partial';
          });
          it('starts network request if refetch query is fully cached', function () {
            renderFragment();
            renderSpy.mockClear();
            TestRenderer.act(function () {
              refetch({
                id: '1'
              }, {
                fetchPolicy: fetchPolicy,
                renderPolicy: renderPolicy
              });
            }); // Assert request is not started

            var refetchVariables = (0, _objectSpread2["default"])({}, variables);
            refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
            expectRequestIsInFlight({
              inFlight: true,
              requestCount: 1,
              gqlRefetchQuery: gqlRefetchQuery,
              refetchVariables: refetchVariables
            }); // Assert component renders immediately since data is cached

            var refetchingUser = (0, _objectSpread2["default"])({
              id: '1',
              name: 'Alice',
              profile_picture: null
            }, createFragmentRef('1', refetchQuery));
            expectFragmentResults([{
              data: refetchingUser
            }, {
              data: refetchingUser
            }]);
          });
          it('starts network request if refetch query is not fully cached and suspends if fragment has missing data', function () {
            var renderer = renderFragment();
            var initialUser = (0, _objectSpread2["default"])({
              id: '1',
              name: 'Alice',
              profile_picture: null
            }, createFragmentRef('1', query));
            expectFragmentResults([{
              data: initialUser
            }]);
            TestRenderer.act(function () {
              refetch({
                id: '4'
              }, {
                fetchPolicy: fetchPolicy,
                renderPolicy: renderPolicy
              });
            }); // Assert that fragment is refetching with the right variables and
            // suspends upon refetch

            var refetchVariables = {
              id: '4',
              scale: 16
            };
            refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
            expectFragmentIsRefetching(renderer, {
              refetchVariables: refetchVariables,
              refetchQuery: refetchQuery
            }); // Mock network response

            environment.mock.resolve(gqlRefetchQuery, {
              data: {
                node: {
                  __typename: 'User',
                  id: '4',
                  name: 'Mark',
                  profile_picture: {
                    uri: 'scale16'
                  },
                  username: 'usermark'
                }
              }
            }); // Assert fragment is rendered with new data

            var refetchedUser = (0, _objectSpread2["default"])({
              id: '4',
              name: 'Mark',
              profile_picture: {
                uri: 'scale16'
              }
            }, createFragmentRef('4', refetchQuery));
            expectFragmentResults([{
              data: refetchedUser
            }, {
              data: refetchedUser
            }]);
          });
          it("starts network request if refetch query is not fully cached and doesn't suspend if fragment doesn't have missing data", function () {
            // Cache user with missing username
            var refetchVariables = {
              id: '4',
              scale: 16
            };
            refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
            environment.commitPayload(refetchQuery, {
              node: {
                __typename: 'User',
                id: '4',
                name: 'Mark',
                profile_picture: null
              }
            });
            renderFragment();
            renderSpy.mockClear();
            TestRenderer.act(function () {
              refetch({
                id: '4'
              }, {
                fetchPolicy: fetchPolicy,
                renderPolicy: renderPolicy
              });
            }); // Assert request is started

            expectRequestIsInFlight({
              inFlight: true,
              requestCount: 1,
              gqlRefetchQuery: gqlRefetchQuery,
              refetchVariables: refetchVariables
            }); // Assert component renders immediately since data is cached

            var refetchingUser = (0, _objectSpread2["default"])({
              id: '4',
              name: 'Mark',
              profile_picture: null
            }, createFragmentRef('4', refetchQuery));
            expectFragmentResults([{
              data: refetchingUser
            }, {
              data: refetchingUser
            }]);
          });
        });
        describe('renderPolicy: full', function () {
          beforeEach(function () {
            renderPolicy = 'full';
          });
          it('starts network request if refetch query is fully cached', function () {
            renderFragment();
            renderSpy.mockClear();
            TestRenderer.act(function () {
              refetch({
                id: '1'
              }, {
                fetchPolicy: fetchPolicy,
                renderPolicy: renderPolicy
              });
            }); // Assert request is not started

            var refetchVariables = (0, _objectSpread2["default"])({}, variables);
            refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
            expectRequestIsInFlight({
              inFlight: true,
              requestCount: 1,
              gqlRefetchQuery: gqlRefetchQuery,
              refetchVariables: refetchVariables
            }); // Assert component renders immediately since data is cached

            var refetchingUser = (0, _objectSpread2["default"])({
              id: '1',
              name: 'Alice',
              profile_picture: null
            }, createFragmentRef('1', refetchQuery));
            expectFragmentResults([{
              data: refetchingUser
            }, {
              data: refetchingUser
            }]);
          });
          it('starts network request if refetch query is not fully cached and suspends if fragment has missing data', function () {
            var renderer = renderFragment();
            var initialUser = (0, _objectSpread2["default"])({
              id: '1',
              name: 'Alice',
              profile_picture: null
            }, createFragmentRef('1', query));
            expectFragmentResults([{
              data: initialUser
            }]);
            TestRenderer.act(function () {
              refetch({
                id: '4'
              }, {
                fetchPolicy: fetchPolicy,
                renderPolicy: renderPolicy
              });
            }); // Assert that fragment is refetching with the right variables and
            // suspends upon refetch

            var refetchVariables = {
              id: '4',
              scale: 16
            };
            refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
            expectFragmentIsRefetching(renderer, {
              refetchVariables: refetchVariables,
              refetchQuery: refetchQuery
            }); // Mock network response

            environment.mock.resolve(gqlRefetchQuery, {
              data: {
                node: {
                  __typename: 'User',
                  id: '4',
                  name: 'Mark',
                  profile_picture: {
                    uri: 'scale16'
                  },
                  username: 'usermark'
                }
              }
            }); // Assert fragment is rendered with new data

            var refetchedUser = (0, _objectSpread2["default"])({
              id: '4',
              name: 'Mark',
              profile_picture: {
                uri: 'scale16'
              }
            }, createFragmentRef('4', refetchQuery));
            expectFragmentResults([{
              data: refetchedUser
            }, {
              data: refetchedUser
            }]);
          });
          it("starts network request if refetch query is not fully cached and doesn't suspend if fragment doesn't have missing data", function () {
            // Cache user with missing username
            var refetchVariables = {
              id: '4',
              scale: 16
            };
            refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
            environment.commitPayload(refetchQuery, {
              node: {
                __typename: 'User',
                id: '4',
                name: 'Mark',
                profile_picture: null
              }
            });
            var renderer = renderFragment();
            renderSpy.mockClear();
            TestRenderer.act(function () {
              refetch({
                id: '4'
              }, {
                fetchPolicy: fetchPolicy,
                renderPolicy: renderPolicy
              });
            }); // Assert component suspended

            expectFragmentIsRefetching(renderer, {
              refetchVariables: refetchVariables,
              refetchQuery: refetchQuery
            }); // Mock network response

            environment.mock.resolve(gqlRefetchQuery, {
              data: {
                node: {
                  __typename: 'User',
                  id: '4',
                  name: 'Mark',
                  profile_picture: {
                    uri: 'scale16'
                  },
                  username: 'usermark'
                }
              }
            }); // Assert fragment is rendered with new data

            var refetchedUser = (0, _objectSpread2["default"])({
              id: '4',
              name: 'Mark',
              profile_picture: {
                uri: 'scale16'
              }
            }, createFragmentRef('4', refetchQuery));
            expectFragmentResults([{
              data: refetchedUser
            }, {
              data: refetchedUser
            }]);
          });
        });
      });
      describe('network-only', function () {
        beforeEach(function () {
          fetchPolicy = 'network-only';
        });
        it('starts network request and suspends if refetch query is fully cached', function () {
          var renderer = renderFragment();
          var initialUser = (0, _objectSpread2["default"])({
            id: '1',
            name: 'Alice',
            profile_picture: null
          }, createFragmentRef('1', query));
          expectFragmentResults([{
            data: initialUser
          }]);
          TestRenderer.act(function () {
            refetch({
              id: '1'
            }, {
              fetchPolicy: fetchPolicy,
              renderPolicy: renderPolicy
            });
          }); // Assert that fragment is refetching with the right variables and
          // suspends upon refetch

          var refetchVariables = (0, _objectSpread2["default"])({}, variables);
          refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
          expectFragmentIsRefetching(renderer, {
            refetchVariables: refetchVariables,
            refetchQuery: refetchQuery
          }); // Mock network response

          environment.mock.resolve(gqlRefetchQuery, {
            data: {
              node: {
                __typename: 'User',
                id: '1',
                name: 'Alice',
                profile_picture: null,
                username: 'useralice'
              }
            }
          }); // Assert fragment is rendered with new data

          var refetchedUser = (0, _objectSpread2["default"])({}, initialUser, createFragmentRef('1', refetchQuery));
          expectFragmentResults([{
            data: refetchedUser
          }, {
            data: refetchedUser
          }]);
        });
        it('starts network request and suspends if refetch query is not fully cached', function () {
          var renderer = renderFragment();
          var initialUser = (0, _objectSpread2["default"])({
            id: '1',
            name: 'Alice',
            profile_picture: null
          }, createFragmentRef('1', query));
          expectFragmentResults([{
            data: initialUser
          }]);
          TestRenderer.act(function () {
            refetch({
              id: '4'
            }, {
              fetchPolicy: fetchPolicy,
              renderPolicy: renderPolicy
            });
          }); // Assert that fragment is refetching with the right variables and
          // suspends upon refetch

          var refetchVariables = {
            id: '4',
            scale: 16
          };
          refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
          expectFragmentIsRefetching(renderer, {
            refetchVariables: refetchVariables,
            refetchQuery: refetchQuery
          }); // Mock network response

          environment.mock.resolve(gqlRefetchQuery, {
            data: {
              node: {
                __typename: 'User',
                id: '4',
                name: 'Mark',
                profile_picture: {
                  uri: 'scale16'
                },
                username: 'usermark'
              }
            }
          }); // Assert fragment is rendered with new data

          var refetchedUser = (0, _objectSpread2["default"])({
            id: '4',
            name: 'Mark',
            profile_picture: {
              uri: 'scale16'
            }
          }, createFragmentRef('4', refetchQuery));
          expectFragmentResults([{
            data: refetchedUser
          }, {
            data: refetchedUser
          }]);
        });
      });
      describe('store-only', function () {
        beforeEach(function () {
          fetchPolicy = 'store-only';
        });
        it("doesn't start network request if refetch query is fully cached", function () {
          renderFragment();
          renderSpy.mockClear();
          TestRenderer.act(function () {
            refetch({
              id: '1'
            }, {
              fetchPolicy: fetchPolicy,
              renderPolicy: renderPolicy
            });
          }); // Assert request is not started

          var refetchVariables = (0, _objectSpread2["default"])({}, variables);
          refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
          expectRequestIsInFlight({
            inFlight: false,
            requestCount: 0,
            gqlRefetchQuery: gqlRefetchQuery,
            refetchVariables: refetchVariables
          }); // Assert component renders immediately since data is cached

          var refetchingUser = (0, _objectSpread2["default"])({
            id: '1',
            name: 'Alice',
            profile_picture: null
          }, createFragmentRef('1', refetchQuery));
          expectFragmentResults([{
            data: refetchingUser
          }, {
            data: refetchingUser
          }]);
        });
        it("doesn't start network request if refetch query is not fully cached", function () {
          renderFragment();
          renderSpy.mockClear();
          TestRenderer.act(function () {
            refetch({
              id: '4'
            }, {
              fetchPolicy: fetchPolicy,
              renderPolicy: renderPolicy
            });
          }); // Assert request is not started

          var refetchVariables = {
            id: '4',
            scale: 32
          };
          refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
          expectRequestIsInFlight({
            inFlight: false,
            requestCount: 0,
            gqlRefetchQuery: gqlRefetchQuery,
            refetchVariables: refetchVariables
          }); // Assert component renders immediately with empty daa

          expectFragmentResults([{
            data: null
          }, {
            data: null
          }]);
        });
      });
    });
    describe('disposing', function () {
      var _unsubscribe;

      var fetchPolicy = 'store-and-network';
      beforeEach(function () {
        _unsubscribe = jest.fn();
        jest.doMock('relay-runtime', function () {
          var originalRuntime = jest.requireActual('relay-runtime');
          var originalInternal = originalRuntime.__internal;
          return (0, _objectSpread2["default"])({}, originalRuntime, {
            __internal: (0, _objectSpread2["default"])({}, originalInternal, {
              fetchQuery: function fetchQuery() {
                for (var _len2 = arguments.length, args = new Array(_len2), _key2 = 0; _key2 < _len2; _key2++) {
                  args[_key2] = arguments[_key2];
                }

                var observable = originalInternal.fetchQuery.apply(originalInternal, args);
                return {
                  subscribe: function subscribe(observer) {
                    return observable.subscribe((0, _objectSpread2["default"])({}, observer, {
                      start: function start(originalSubscription) {
                        var observerStart = observer === null || observer === void 0 ? void 0 : observer.start;
                        observerStart && observerStart((0, _objectSpread2["default"])({}, originalSubscription, {
                          unsubscribe: function unsubscribe() {
                            originalSubscription.unsubscribe();

                            _unsubscribe();
                          }
                        }));
                      }
                    }));
                  }
                };
              }
            })
          });
        });
      });
      afterEach(function () {
        jest.dontMock('relay-runtime');
      });
      it('disposes ongoing request if environment changes', function () {
        renderFragment();
        renderSpy.mockClear();
        TestRenderer.act(function () {
          refetch({
            id: '1'
          }, {
            fetchPolicy: fetchPolicy,
            renderPolicy: renderPolicy
          });
        }); // Assert request is started

        var refetchVariables = {
          id: '1',
          scale: 16
        };
        refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
        expectRequestIsInFlight({
          inFlight: true,
          requestCount: 1,
          gqlRefetchQuery: gqlRefetchQuery,
          refetchVariables: refetchVariables
        }); // Component renders immediately even though request is in flight
        // since data is cached

        var refetchingUser = (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', refetchQuery));
        expectFragmentResults([{
          data: refetchingUser
        }, {
          data: refetchingUser
        }]); // Set new environment

        var newEnvironment = createMockEnvironment();
        newEnvironment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '1',
            name: 'Alice in a different env',
            username: 'useralice',
            profile_picture: null
          }
        });
        TestRenderer.act(function () {
          setEnvironment(newEnvironment);
        }); // Assert request was canceled

        expect(_unsubscribe).toBeCalledTimes(1);
        expectRequestIsInFlight({
          inFlight: false,
          requestCount: 1,
          gqlRefetchQuery: gqlRefetchQuery,
          refetchVariables: refetchVariables
        }); // Assert newly rendered data

        var expectedUser = (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice in a different env',
          profile_picture: null
        }, createFragmentRef('1', query));
        expectFragmentResults([{
          data: expectedUser
        }, {
          data: expectedUser
        }, {
          data: expectedUser
        }]);
      });
      it('disposes ongoing request if fragment ref changes', function () {
        renderFragment();
        renderSpy.mockClear();
        TestRenderer.act(function () {
          refetch({
            id: '1'
          }, {
            fetchPolicy: fetchPolicy,
            renderPolicy: renderPolicy
          });
        }); // Assert request is started

        var refetchVariables = {
          id: '1',
          scale: 16
        };
        refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
        expectRequestIsInFlight({
          inFlight: true,
          requestCount: 1,
          gqlRefetchQuery: gqlRefetchQuery,
          refetchVariables: refetchVariables
        }); // Component renders immediately even though request is in flight
        // since data is cached

        var refetchingUser = (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', refetchQuery));
        expectFragmentResults([{
          data: refetchingUser
        }, {
          data: refetchingUser
        }]); // Pass new parent fragment ref with different variables

        var newVariables = (0, _objectSpread2["default"])({}, variables, {
          scale: 32
        });
        var newQuery = createOperationDescriptor(gqlQuery, newVariables);
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
        });
        TestRenderer.act(function () {
          setOwner(newQuery);
        }); // Assert request was canceled

        expect(_unsubscribe).toBeCalledTimes(1);
        expectRequestIsInFlight({
          inFlight: false,
          requestCount: 1,
          gqlRefetchQuery: gqlRefetchQuery,
          refetchVariables: refetchVariables
        }); // Assert newly rendered data

        var expectedUser = (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: {
            uri: 'uri32'
          }
        }, createFragmentRef('1', newQuery));
        expectFragmentResults([{
          data: expectedUser
        }, {
          data: expectedUser
        }, {
          data: expectedUser
        }]);
      });
      it('disposes ongoing request if refetch is called again', function () {
        var renderer = renderFragment();
        renderSpy.mockClear();
        TestRenderer.act(function () {
          refetch({
            id: '1'
          }, {
            fetchPolicy: fetchPolicy,
            renderPolicy: renderPolicy
          });
        }); // Assert request is started

        var refetchVariables1 = {
          id: '1',
          scale: 16
        };
        var refetchQuery1 = createOperationDescriptor(gqlRefetchQuery, refetchVariables1);
        expectRequestIsInFlight({
          inFlight: true,
          requestCount: 1,
          gqlRefetchQuery: gqlRefetchQuery,
          refetchVariables: refetchVariables1
        }); // Component renders immediately even though request is in flight
        // since data is cached

        var refetchingUser = (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', refetchQuery1));
        expectFragmentResults([{
          data: refetchingUser
        }, {
          data: refetchingUser
        }]); // Call refetch a second time

        environment.execute.mockClear();
        var refetchVariables2 = {
          id: '4',
          scale: 16
        };
        TestRenderer.act(function () {
          refetch({
            id: '4'
          }, {
            fetchPolicy: fetchPolicy,
            renderPolicy: renderPolicy
          });
        }); // Assert first request was canceled

        expect(_unsubscribe).toBeCalledTimes(1);
        expectRequestIsInFlight({
          inFlight: false,
          requestCount: 1,
          gqlRefetchQuery: gqlRefetchQuery,
          refetchVariables: refetchVariables1
        }); // Assert second request is started

        expectRequestIsInFlight({
          inFlight: true,
          requestCount: 1,
          gqlRefetchQuery: gqlRefetchQuery,
          refetchVariables: refetchVariables2
        }); // Assert component suspended

        expect(renderSpy).toBeCalledTimes(0);
        expect(renderer.toJSON()).toEqual('Fallback');
      });
      it('disposes of ongoing request on unmount', function () {
        var renderer = renderFragment();
        renderSpy.mockClear();
        TestRenderer.act(function () {
          refetch({
            id: '1'
          }, {
            fetchPolicy: fetchPolicy,
            renderPolicy: renderPolicy
          });
        }); // Assert request is started

        var refetchVariables = {
          id: '1',
          scale: 16
        };
        refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
        expectRequestIsInFlight({
          inFlight: true,
          requestCount: 1,
          gqlRefetchQuery: gqlRefetchQuery,
          refetchVariables: refetchVariables
        }); // Component renders immediately even though request is in flight
        // since data is cached

        var refetchingUser = (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', refetchQuery));
        expectFragmentResults([{
          data: refetchingUser
        }, {
          data: refetchingUser
        }]);
        renderer.unmount(); // Assert request was canceled

        expect(_unsubscribe).toBeCalledTimes(1);
        expectRequestIsInFlight({
          inFlight: false,
          requestCount: 1,
          gqlRefetchQuery: gqlRefetchQuery,
          refetchVariables: refetchVariables
        });
      });
      it('disposes ongoing request if it is manually disposed', function () {
        renderFragment();
        renderSpy.mockClear();
        var disposable;
        TestRenderer.act(function () {
          disposable = refetch({
            id: '1'
          }, {
            fetchPolicy: fetchPolicy,
            renderPolicy: renderPolicy
          });
        }); // Assert request is started

        var refetchVariables = {
          id: '1',
          scale: 16
        };
        refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
        expectRequestIsInFlight({
          inFlight: true,
          requestCount: 1,
          gqlRefetchQuery: gqlRefetchQuery,
          refetchVariables: refetchVariables
        }); // Component renders immediately even though request is in flight
        // since data is cached

        var refetchingUser = (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', refetchQuery));
        expectFragmentResults([{
          data: refetchingUser
        }, {
          data: refetchingUser
        }]);
        disposable && disposable.dispose(); // Assert request was canceled

        expect(_unsubscribe).toBeCalledTimes(1);
        expectRequestIsInFlight({
          inFlight: false,
          requestCount: 1,
          gqlRefetchQuery: gqlRefetchQuery,
          refetchVariables: refetchVariables
        });
      });
    });
    describe('when id variable has a different variable name in original query', function () {
      beforeEach(function () {
        var _gqlFragment$metadata3, _gqlFragment$metadata4;

        var generated = generateAndCompile("\n            fragment NestedUserFragment on User {\n              username\n            }\n\n            fragment UserFragment on User\n            @refetchable(queryName: \"UserFragmentRefetchQuery\") {\n              id\n              name\n              profile_picture(scale: $scale) {\n                uri\n              }\n              ...NestedUserFragment\n            }\n\n            query UserQuery($nodeID: ID!, $scale: Int!) {\n              node(id: $nodeID) {\n                ...UserFragment\n              }\n            }\n          ");
        variables = {
          nodeID: '1',
          scale: 16
        };
        gqlQuery = generated.UserQuery;
        gqlRefetchQuery = generated.UserFragmentRefetchQuery;
        gqlFragment = generated.UserFragment;
        !(((_gqlFragment$metadata3 = gqlFragment.metadata) === null || _gqlFragment$metadata3 === void 0 ? void 0 : (_gqlFragment$metadata4 = _gqlFragment$metadata3.refetch) === null || _gqlFragment$metadata4 === void 0 ? void 0 : _gqlFragment$metadata4.operation) === '@@MODULE_START@@UserFragmentRefetchQuery.graphql@@MODULE_END@@') ? process.env.NODE_ENV !== "production" ? invariant(false, 'useRefetchableFragment-test: Expected refetchable fragment metadata to contain operation.') : invariant(false) : void 0; // Manually set the refetchable operation for the test.

        gqlFragment.metadata.refetch.operation = gqlRefetchQuery;
        query = createOperationDescriptor(gqlQuery, variables);
        refetchQuery = createOperationDescriptor(gqlRefetchQuery, variables);
        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '1',
            name: 'Alice',
            username: 'useralice',
            profile_picture: null
          }
        });
      });
      it('refetches new variables correctly when refetching new id', function () {
        var renderer = renderFragment();
        var initialUser = (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', query));
        expectFragmentResults([{
          data: initialUser
        }]);
        TestRenderer.act(function () {
          refetch({
            id: '4'
          });
        }); // Assert that fragment is refetching with the right variables and
        // suspends upon refetch

        var refetchVariables = {
          id: '4',
          scale: 16
        };
        refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
        expectFragmentIsRefetching(renderer, {
          refetchVariables: refetchVariables,
          refetchQuery: refetchQuery
        }); // Mock network response

        environment.mock.resolve(gqlRefetchQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '4',
              name: 'Mark',
              profile_picture: {
                uri: 'scale16'
              },
              username: 'usermark'
            }
          }
        }); // Assert fragment is rendered with new data

        var refetchedUser = (0, _objectSpread2["default"])({
          id: '4',
          name: 'Mark',
          profile_picture: {
            uri: 'scale16'
          }
        }, createFragmentRef('4', refetchQuery));
        expectFragmentResults([{
          data: refetchedUser
        }, {
          data: refetchedUser
        }]); // Assert refetch query was retained

        expect(release).not.toBeCalled();
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(refetchQuery.root);
      });
      it('refetches new variables correctly when refetching same id', function () {
        var renderer = renderFragment();
        var initialUser = (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', query));
        expectFragmentResults([{
          data: initialUser
        }]);
        TestRenderer.act(function () {
          refetch({
            scale: 32
          });
        }); // Assert that fragment is refetching with the right variables and
        // suspends upon refetch

        var refetchVariables = {
          id: '1',
          scale: 32
        };
        refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables);
        expectFragmentIsRefetching(renderer, {
          refetchVariables: refetchVariables,
          refetchQuery: refetchQuery
        }); // Mock network response

        environment.mock.resolve(gqlRefetchQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              profile_picture: {
                uri: 'scale32'
              },
              username: 'useralice'
            }
          }
        }); // Assert fragment is rendered with new data

        var refetchedUser = (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: {
            uri: 'scale32'
          }
        }, createFragmentRef('1', refetchQuery));
        expectFragmentResults([{
          data: refetchedUser
        }, {
          data: refetchedUser
        }]); // Assert refetch query was retained

        expect(release).not.toBeCalled();
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(refetchQuery.root);
      });
    });
    describe('internal environment option', function () {
      var newRelease;
      var newEnvironment;
      beforeEach(function () {
        var _require4 = require('relay-test-utils-internal');

        createMockEnvironment = _require4.createMockEnvironment;
        newEnvironment = createMockEnvironment();
        newRelease = jest.fn();
        newEnvironment.retain.mockImplementation(function () {
          return {
            dispose: newRelease
          };
        });
      });
      it('reloads new data into new environment, and renders successfully', function () {
        var renderer = renderFragment();
        var initialUser = (0, _objectSpread2["default"])({
          id: '1',
          name: 'Alice',
          profile_picture: null
        }, createFragmentRef('1', query)); // initial data on default environment

        expectFragmentResults([{
          data: initialUser
        }]);
        TestRenderer.act(function () {
          refetch({
            id: '1'
          }, {
            __environment: newEnvironment
          });
        });
        var refetchVariables = {
          id: '1',
          scale: 16
        };
        refetchQuery = createOperationDescriptor(gqlRefetchQuery, refetchVariables); // Fetch on newEnvironment

        expectFragmentIsRefetching(renderer, {
          refetchVariables: refetchVariables,
          refetchQuery: refetchQuery
        }, newEnvironment);
        newEnvironment.mock.resolve(gqlRefetchQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Mark',
              username: 'usermark',
              profile_picture: {
                uri: 'scale16'
              }
            }
          }
        });
        TestRenderer.act(function () {
          return jest.runAllImmediates();
        }); // Data should be loaded on the newEnvironment

        var dataInSource = {
          __id: '1',
          __typename: 'User',
          'profile_picture(scale:16)': {
            __ref: 'client:1:profile_picture(scale:16)'
          },
          id: '1',
          name: 'Mark',
          username: 'usermark'
        };
        var source = newEnvironment.getStore().getSource();
        expect(source.get('1')).toEqual(dataInSource); // Assert refetch query was retained

        expect(newRelease).not.toBeCalled();
        expect(newEnvironment.retain).toBeCalledTimes(1);
        expect(newEnvironment.retain.mock.calls[0][0]).toEqual(refetchQuery.root); // Should be able to use the new data if switched to new environment

        renderSpy.mockClear();
        newRelease.mockClear();
        TestRenderer.act(function () {
          setEnvironment(newEnvironment);
        }); // refetch on the same newEnvironment after switching should not be reset

        expect(release).not.toBeCalled();
        var refetchedUser = (0, _objectSpread2["default"])({
          id: '1',
          name: 'Mark',
          profile_picture: {
            uri: 'scale16'
          }
        }, createFragmentRef('1', refetchQuery));
        expectFragmentResults([{
          data: refetchedUser
        }, {
          data: refetchedUser
        }]); // Refetch on another enironment afterwards should work

        renderSpy.mockClear();
        environment.execute.mockClear();
        var anotherNewEnvironment = createMockEnvironment();
        TestRenderer.act(function () {
          return jest.runAllImmediates();
        });
        TestRenderer.act(function () {
          refetch({
            id: '1'
          }, {
            __environment: anotherNewEnvironment
          });
        });
        expectFragmentIsRefetching(renderer, {
          refetchVariables: refetchVariables,
          refetchQuery: refetchQuery
        }, anotherNewEnvironment);
        anotherNewEnvironment.mock.resolve(gqlRefetchQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Mark',
              username: 'usermark',
              profile_picture: {
                uri: 'scale16'
              }
            }
          }
        });
        expect(anotherNewEnvironment.getStore().getSource().get('1')).toEqual(dataInSource);
      });
    });
  });
});