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

var _objectSpread2 = _interopRequireDefault(require("@babel/runtime/helpers/objectSpread"));

var _objectWithoutPropertiesLoose2 = _interopRequireDefault(require("@babel/runtime/helpers/objectWithoutPropertiesLoose"));

var _assertThisInitialized2 = _interopRequireDefault(require("@babel/runtime/helpers/assertThisInitialized"));

var _inheritsLoose2 = _interopRequireDefault(require("@babel/runtime/helpers/inheritsLoose"));

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

var React = require('react');

var Scheduler = require('scheduler');

var useMemo = React.useMemo,
    useState = React.useState;

var TestRenderer = require('react-test-renderer');

var invariant = require("fbjs/lib/invariant");

var useBlockingPaginationFragmentOriginal = require('../useBlockingPaginationFragment');

var ReactRelayContext = require('react-relay/ReactRelayContext');

var _require = require('relay-runtime'),
    ConnectionHandler = _require.ConnectionHandler,
    FRAGMENT_OWNER_KEY = _require.FRAGMENT_OWNER_KEY,
    FRAGMENTS_KEY = _require.FRAGMENTS_KEY,
    ID_KEY = _require.ID_KEY,
    createOperationDescriptor = _require.createOperationDescriptor;

describe('useBlockingPaginationFragment', function () {
  var environment;
  var initialUser;
  var gqlQuery;
  var gqlQueryNestedFragment;
  var gqlQueryWithoutID;
  var gqlQueryWithLiteralArgs;
  var gqlPaginationQuery;
  var gqlFragment;
  var query;
  var queryNestedFragment;
  var queryWithoutID;
  var queryWithLiteralArgs;
  var paginationQuery;
  var variables;
  var variablesNestedFragment;
  var variablesWithoutID;
  var setEnvironment;
  var setOwner;
  var renderFragment;
  var renderSpy;
  var createMockEnvironment;
  var generateAndCompile;
  var loadNext;
  var refetch;
  var forceUpdate;
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

  function useBlockingPaginationFragment(fragmentNode, fragmentRef) {
    /* $FlowFixMe(>=0.108.0 site=www,mobile,react_native_fb,oss) This comment suppresses an error found
     * when Flow v0.108.0 was deployed. To see the error delete this comment
     * and run Flow. */
    var _useBlockingPaginatio = useBlockingPaginationFragmentOriginal(fragmentNode, // $FlowFixMe
    fragmentRef),
        data = _useBlockingPaginatio.data,
        result = (0, _objectWithoutPropertiesLoose2["default"])(_useBlockingPaginatio, ["data"]);

    loadNext = result.loadNext;
    refetch = result.refetch;
    renderSpy(data, result);
    return (0, _objectSpread2["default"])({
      data: data
    }, result);
  }

  function assertCall(expected, idx) {
    var actualData = renderSpy.mock.calls[idx][0];
    var actualResult = renderSpy.mock.calls[idx][1];
    var actualHasNext = actualResult.hasNext;
    var actualHasPrevious = actualResult.hasPrevious;
    expect(actualData).toEqual(expected.data);
    expect(actualHasNext).toEqual(expected.hasNext);
    expect(actualHasPrevious).toEqual(expected.hasPrevious);
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
    var _ref4;

    return _ref4 = {}, (0, _defineProperty2["default"])(_ref4, ID_KEY, id), (0, _defineProperty2["default"])(_ref4, FRAGMENTS_KEY, {
      NestedUserFragment: {}
    }), (0, _defineProperty2["default"])(_ref4, FRAGMENT_OWNER_KEY, owner.request), _ref4;
  }

  beforeEach(function () {
    var _gqlFragment$metadata, _gqlFragment$metadata2;

    // Set up mocks
    jest.resetModules();
    jest.spyOn(console, 'warn').mockImplementationOnce(function () {});
    jest.mock("fbjs/lib/warning");
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
    environment = createMockEnvironment({
      handlerProvider: function handlerProvider() {
        return ConnectionHandler;
      }
    });
    var generated = generateAndCompile("\n        fragment NestedUserFragment on User {\n          username\n        }\n\n        fragment UserFragment on User\n        @refetchable(queryName: \"UserFragmentPaginationQuery\")\n        @argumentDefinitions(\n          isViewerFriendLocal: {type: \"Boolean\", defaultValue: false}\n          orderby: {type: \"[String]\"}\n        ) {\n          id\n          name\n          friends(\n            after: $after,\n            first: $first,\n            before: $before,\n            last: $last,\n            orderby: $orderby,\n            isViewerFriend: $isViewerFriendLocal\n          ) @connection(key: \"UserFragment_friends\") {\n            edges {\n              node {\n                id\n                name\n                ...NestedUserFragment\n              }\n            }\n          }\n        }\n\n        query UserQuery(\n          $id: ID!\n          $after: ID\n          $first: Int\n          $before: ID\n          $last: Int\n          $orderby: [String]\n          $isViewerFriend: Boolean\n        ) {\n          node(id: $id) {\n            ...UserFragment @arguments(isViewerFriendLocal: $isViewerFriend, orderby: $orderby)\n          }\n        }\n\n        query UserQueryNestedFragment(\n          $id: ID!\n          $after: ID\n          $first: Int\n          $before: ID\n          $last: Int\n          $orderby: [String]\n          $isViewerFriend: Boolean\n        ) {\n          node(id: $id) {\n            actor {\n              ...UserFragment @arguments(isViewerFriendLocal: $isViewerFriend, orderby: $orderby)\n            }\n          }\n        }\n\n        query UserQueryWithoutID(\n          $after: ID\n          $first: Int\n          $before: ID\n          $last: Int\n          $orderby: [String]\n          $isViewerFriend: Boolean\n        ) {\n          viewer {\n            actor {\n              ...UserFragment @arguments(isViewerFriendLocal: $isViewerFriend, orderby: $orderby)\n            }\n          }\n        }\n\n        query UserQueryWithLiteralArgs(\n          $id: ID!\n          $after: ID\n          $first: Int\n          $before: ID\n          $last: Int\n          $orderby: [String]\n          $isViewerFriend: Boolean\n        ) {\n          node(id: $id) {\n            ...UserFragment @arguments(isViewerFriendLocal: true, orderby: [\"name\"])\n          }\n        }\n      ");
    variablesWithoutID = {
      after: null,
      first: 1,
      before: null,
      last: null,
      isViewerFriend: false,
      orderby: ['name']
    };
    variables = (0, _objectSpread2["default"])({}, variablesWithoutID, {
      id: '1'
    });
    variablesNestedFragment = (0, _objectSpread2["default"])({}, variablesWithoutID, {
      id: '<feedbackid>'
    });
    gqlQuery = generated.UserQuery;
    gqlQueryNestedFragment = generated.UserQueryNestedFragment;
    gqlQueryWithoutID = generated.UserQueryWithoutID;
    gqlQueryWithLiteralArgs = generated.UserQueryWithLiteralArgs;
    gqlPaginationQuery = generated.UserFragmentPaginationQuery;
    gqlFragment = generated.UserFragment;
    !(((_gqlFragment$metadata = gqlFragment.metadata) === null || _gqlFragment$metadata === void 0 ? void 0 : (_gqlFragment$metadata2 = _gqlFragment$metadata.refetch) === null || _gqlFragment$metadata2 === void 0 ? void 0 : _gqlFragment$metadata2.operation) === '@@MODULE_START@@UserFragmentPaginationQuery.graphql@@MODULE_END@@') ? process.env.NODE_ENV !== "production" ? invariant(false, 'useRefetchableFragment-test: Expected refetchable fragment metadata to contain operation.') : invariant(false) : void 0; // Manually set the refetchable operation for the test.

    gqlFragment.metadata.refetch.operation = gqlPaginationQuery;
    query = createOperationDescriptor(gqlQuery, variables);
    queryNestedFragment = createOperationDescriptor(gqlQueryNestedFragment, variablesNestedFragment);
    queryWithoutID = createOperationDescriptor(gqlQueryWithoutID, variablesWithoutID);
    queryWithLiteralArgs = createOperationDescriptor(gqlQueryWithLiteralArgs, variables);
    paginationQuery = createOperationDescriptor(gqlPaginationQuery, variables);
    environment.commitPayload(query, {
      node: {
        __typename: 'User',
        id: '1',
        name: 'Alice',
        friends: {
          edges: [{
            cursor: 'cursor:1',
            node: {
              __typename: 'User',
              id: 'node:1',
              name: 'name:node:1',
              username: 'username:node:1'
            }
          }],
          pageInfo: {
            endCursor: 'cursor:1',
            hasNextPage: true,
            hasPreviousPage: false,
            startCursor: 'cursor:1'
          }
        }
      }
    });
    environment.commitPayload(queryWithoutID, {
      viewer: {
        actor: {
          __typename: 'User',
          id: '1',
          name: 'Alice',
          friends: {
            edges: [{
              cursor: 'cursor:1',
              node: {
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1',
                username: 'username:node:1'
              }
            }],
            pageInfo: {
              endCursor: 'cursor:1',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:1'
            }
          }
        }
      }
    });
    environment.commitPayload(queryWithLiteralArgs, {
      node: {
        __typename: 'User',
        id: '1',
        name: 'Alice',
        friends: {
          edges: [{
            cursor: 'cursor:1',
            node: {
              __typename: 'User',
              id: 'node:1',
              name: 'name:node:1',
              username: 'username:node:1'
            }
          }],
          pageInfo: {
            endCursor: 'cursor:1',
            hasNextPage: true,
            hasPreviousPage: false,
            startCursor: 'cursor:1'
          }
        }
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
        var _environment$lookup$d;

        return (_environment$lookup$d = environment.lookup(owner.fragment).data) === null || _environment$lookup$d === void 0 ? void 0 : _environment$lookup$d.node;
      }, [owner]);
      var userRef = props.hasOwnProperty('userRef') ? props.userRef : artificialUserRef;
      setOwner = _setOwner;
      forceUpdate = _setCount;
      /* $FlowFixMe(>=0.108.0 site=www,mobile,react_native_fb,oss) This comment suppresses an error found
       * when Flow v0.108.0 was deployed. To see the error delete this comment
       * and run Flow. */

      var _useBlockingPaginatio2 = useBlockingPaginationFragment(fragment, userRef),
          userData = _useBlockingPaginatio2.data;

      return React.createElement(Renderer, {
        user: userData
      });
    };

    var ContextProvider = function ContextProvider(_ref5) {
      var children = _ref5.children;

      var _useState3 = useState(environment),
          env = _useState3[0],
          _setEnv = _useState3[1]; // TODO(T39494051) - We set empty variables in relay context to make
      // Flow happy, but useBlockingPaginationFragment does not use them, instead it uses
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

    initialUser = {
      id: '1',
      name: 'Alice',
      friends: {
        edges: [{
          cursor: 'cursor:1',
          node: (0, _objectSpread2["default"])({
            __typename: 'User',
            id: 'node:1',
            name: 'name:node:1'
          }, createFragmentRef('node:1', query))
        }],
        pageInfo: {
          endCursor: 'cursor:1',
          hasNextPage: true,
          hasPreviousPage: false,
          startCursor: 'cursor:1'
        }
      }
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
    it('should throw error if fragment uses stream', function () {
      jest.spyOn(console, 'error').mockImplementationOnce(function () {});
      var generated = generateAndCompile("\n        fragment UserFragment on User\n        @refetchable(queryName: \"UserFragmentPaginationQuery\") {\n          id\n          friends(\n            after: $after,\n            first: $first,\n            before: $before,\n            last: $last,\n            orderby: $orderby,\n            isViewerFriend: $isViewerFriendLocal\n          ) @stream_connection(key: \"UserFragment_friends\", initial_count: 1) {\n            edges {\n              node {\n                id\n              }\n            }\n          }\n        }\n      "); // Manually set the refetchable operation for the test.

      generated.UserFragment.metadata.refetch.operation = generated.UserFragmentPaginationQuery;
      var renderer = renderFragment({
        fragment: generated.UserFragment
      });
      expect(renderer.toJSON().includes('Use `useStreamingPaginationFragment` instead')).toEqual(true);
    });
    it('should throw error if fragment is missing @refetchable directive', function () {
      jest.spyOn(console, 'error').mockImplementationOnce(function () {});
      var generated = generateAndCompile("\n        fragment UserFragment on User {\n          id\n        }\n      ");
      var renderer = renderFragment({
        fragment: generated.UserFragment
      });
      expect(renderer.toJSON().includes('Did you forget to add a @refetchable directive to the fragment?')).toEqual(true);
    });
    it('should throw error if fragment is missing @connection directive', function () {
      jest.spyOn(console, 'error').mockImplementationOnce(function () {});
      var generated = generateAndCompile("\n        fragment UserFragment on User\n        @refetchable(queryName: \"UserFragmentRefetchQuery\") {\n          id\n        }\n      ");
      generated.UserFragment.metadata.refetch.operation = generated.UserFragmentRefetchQuery;
      var renderer = renderFragment({
        fragment: generated.UserFragment
      });
      expect(renderer.toJSON().includes('Did you forget to add a @connection directive to the connection field in the fragment?')).toEqual(true);
    });
    it('should render fragment without error when data is available', function () {
      renderFragment();
      expectFragmentResults([{
        data: initialUser,
        hasNext: true,
        hasPrevious: false
      }]);
    });
    it('should render fragment without error when ref is null', function () {
      renderFragment({
        userRef: null
      });
      expectFragmentResults([{
        data: null,
        hasNext: false,
        hasPrevious: false
      }]);
    });
    it('should render fragment without error when ref is undefined', function () {
      renderFragment({
        userRef: undefined
      });
      expectFragmentResults([{
        data: null,
        hasNext: false,
        hasPrevious: false
      }]);
    });
    it('should update when fragment data changes', function () {
      renderFragment();
      expectFragmentResults([{
        data: initialUser,
        hasNext: true,
        hasPrevious: false
      }]); // Update parent record

      environment.commitPayload(query, {
        node: {
          __typename: 'User',
          id: '1',
          // Update name
          name: 'Alice in Wonderland'
        }
      });
      expectFragmentResults([{
        data: (0, _objectSpread2["default"])({}, initialUser, {
          // Assert that name is updated
          name: 'Alice in Wonderland'
        }),
        hasNext: true,
        hasPrevious: false
      }]); // Update edge

      environment.commitPayload(query, {
        node: {
          __typename: 'User',
          id: 'node:1',
          // Update name
          name: 'name:node:1-updated'
        }
      });
      expectFragmentResults([{
        data: (0, _objectSpread2["default"])({}, initialUser, {
          name: 'Alice in Wonderland',
          friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                // Assert that name is updated
                name: 'name:node:1-updated'
              }, createFragmentRef('node:1', query))
            }]
          })
        }),
        hasNext: true,
        hasPrevious: false
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
  describe('pagination', function () {
    var runScheduledCallback = function runScheduledCallback() {};

    var release;
    beforeEach(function () {
      jest.resetModules();
      jest.doMock('scheduler', function () {
        var original = jest.requireActual('scheduler/unstable_mock');
        return (0, _objectSpread2["default"])({}, original, {
          unstable_next: function unstable_next(cb) {
            runScheduledCallback = function runScheduledCallback() {
              original.unstable_next(cb);
            };
          }
        });
      });
      release = jest.fn();
      environment.retain.mockImplementation(function () {
        return {
          dispose: release
        };
      });
    });
    afterEach(function () {
      jest.dontMock('scheduler');
    });

    function expectRequestIsInFlight(expected) {
      var _expected$gqlPaginati;

      expect(environment.execute).toBeCalledTimes(expected.requestCount);
      expect(environment.mock.isLoading((_expected$gqlPaginati = expected.gqlPaginationQuery) !== null && _expected$gqlPaginati !== void 0 ? _expected$gqlPaginati : gqlPaginationQuery, expected.paginationVariables, {
        force: true
      })).toEqual(expected.inFlight);
    }

    function expectFragmentIsLoadingMore(renderer, direction, expected) {
      expect(renderSpy).toBeCalledTimes(0);
      renderSpy.mockClear(); // Assert refetch query was fetched

      expectRequestIsInFlight((0, _objectSpread2["default"])({}, expected, {
        inFlight: true,
        requestCount: 1
      })); // Assert component suspended

      expect(renderSpy).toBeCalledTimes(0);
      expect(renderer.toJSON()).toEqual('Fallback');
    } // TODO
    // - backward pagination
    // - simultaneous pagination
    // - TODO(T41131846): Fetch/Caching policies for loadMore / when network
    //   returns or errors synchronously


    describe('loadNext', function () {
      var direction = 'forward';
      it('does not load more if component has unmounted', function () {
        var warning = require("fbjs/lib/warning");

        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        renderer.unmount();
        TestRenderer.act(function () {
          loadNext(1);
        });
        expect(warning).toHaveBeenCalledTimes(2);
        expect(warning.mock.calls[1][1].includes('Relay: Unexpected fetch on unmounted component')).toEqual(true);
        expect(environment.execute).toHaveBeenCalledTimes(0);
      });
      it('does not load more if fragment ref passed to useBlockingPaginationFragment() was null', function () {
        var warning = require("fbjs/lib/warning");

        renderFragment({
          userRef: null
        });
        expectFragmentResults([{
          data: null,
          hasNext: false,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1);
        });
        expect(warning).toHaveBeenCalledTimes(2);
        expect(warning.mock.calls[1][1].includes('Relay: Unexpected fetch while using a null fragment ref')).toEqual(true);
        expect(environment.execute).toHaveBeenCalledTimes(0);
      });
      it('does not load more if request is already in flight', function () {
        var callback = jest.fn();
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        var paginationVariables = {
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        expectFragmentIsLoadingMore(renderer, direction, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        expect(environment.execute).toBeCalledTimes(1);
        expect(callback).toBeCalledTimes(0);
        expect(renderSpy).toBeCalledTimes(0);
      });
      it('does not load more if parent query is already in flight (i.e. during streaming)', function () {
        // This prevents console.error output in the test, which is expected
        jest.spyOn(console, 'error').mockImplementationOnce(function () {});
        jest.spyOn(require('relay-runtime').__internal, 'hasRequestInFlight').mockImplementationOnce(function () {
          return true;
        });
        var callback = jest.fn();
        renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        expect(environment.execute).toBeCalledTimes(0);
        expect(callback).toBeCalledTimes(0);
        expect(renderSpy).toBeCalledTimes(0);
      });
      it('cancels load more if component unmounts', function () {
        var _unsubscribe = jest.fn();

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
        var callback = jest.fn();
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        var paginationVariables = {
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        expectFragmentIsLoadingMore(renderer, direction, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(_unsubscribe).toHaveBeenCalledTimes(0);
        TestRenderer.act(function () {
          renderer.unmount();
        });
        expect(_unsubscribe).toHaveBeenCalledTimes(1);
        expect(environment.execute).toBeCalledTimes(1);
        expect(callback).toBeCalledTimes(0);
        expect(renderSpy).toBeCalledTimes(0);
      });
      it('cancels load more if refetch is called', function () {
        var _unsubscribe2 = jest.fn();

        jest.doMock('relay-runtime', function () {
          var originalRuntime = jest.requireActual('relay-runtime');
          var originalInternal = originalRuntime.__internal;
          return (0, _objectSpread2["default"])({}, originalRuntime, {
            __internal: (0, _objectSpread2["default"])({}, originalInternal, {
              fetchQuery: function fetchQuery() {
                for (var _len3 = arguments.length, args = new Array(_len3), _key3 = 0; _key3 < _len3; _key3++) {
                  args[_key3] = arguments[_key3];
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

                            _unsubscribe2();
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
        var callback = jest.fn();
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        var paginationVariables = {
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        expectFragmentIsLoadingMore(renderer, direction, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(_unsubscribe2).toHaveBeenCalledTimes(0);
        TestRenderer.act(function () {
          refetch({
            id: '4'
          });
        });
        expect(_unsubscribe2).toHaveBeenCalledTimes(1);
        expect(environment.execute).toBeCalledTimes(2);
        expect(callback).toBeCalledTimes(0);
        expect(renderSpy).toBeCalledTimes(0);
      });
      it('warns if load more scheduled at high priority', function () {
        var warning = require("fbjs/lib/warning");

        renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          Scheduler.unstable_runWithPriority(Scheduler.unstable_ImmediatePriority, function () {
            loadNext(1);
          });
        }); // $FlowFixMe

        var calls = warning.mock.calls.filter(function (call) {
          return call[0] === false;
        });
        expect(calls.length).toEqual(1);
        expect(calls[0][1].includes('Relay: Unexpected call to `%s` at a priority higher than expected')).toEqual(true);
        expect(calls[0][2]).toEqual('loadNext');
        expect(environment.execute).toHaveBeenCalledTimes(1);
      });
      it('attempts to load more even if there are no more items to load', function () {
        environment.getStore().getSource().clear();
        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '1',
            name: 'Alice',
            friends: {
              edges: [{
                cursor: 'cursor:1',
                node: {
                  __typename: 'User',
                  id: 'node:1',
                  name: 'name:node:1',
                  username: 'username:node:1'
                }
              }],
              pageInfo: {
                endCursor: 'cursor:1',
                hasNextPage: false,
                hasPreviousPage: false,
                startCursor: 'cursor:1'
              }
            }
          }
        });
        var callback = jest.fn();
        var renderer = renderFragment();
        var expectedUser = (0, _objectSpread2["default"])({}, initialUser, {
          friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
            pageInfo: expect.objectContaining({
              hasNextPage: false
            })
          })
        });
        expectFragmentResults([{
          data: expectedUser,
          hasNext: false,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        var paginationVariables = {
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        expectFragmentIsLoadingMore(renderer, direction, {
          data: initialUser,
          hasNext: false,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(callback).toBeCalledTimes(0);
        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [],
                pageInfo: {
                  startCursor: null,
                  endCursor: null,
                  hasNextPage: null,
                  hasPreviousPage: null
                }
              }
            }
          }
        });
        expectFragmentResults([{
          data: expectedUser,
          hasNext: false,
          hasPrevious: false
        }]);
        expect(callback).toBeCalledTimes(1);
      });
      it('loads and renders next items in connection', function () {
        var callback = jest.fn();
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        var paginationVariables = {
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        expectFragmentIsLoadingMore(renderer, direction, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(callback).toBeCalledTimes(0);
        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:2',
                  node: {
                    __typename: 'User',
                    id: 'node:2',
                    name: 'name:node:2',
                    username: 'username:node:2'
                  }
                }],
                pageInfo: {
                  startCursor: 'cursor:2',
                  endCursor: 'cursor:2',
                  hasNextPage: true,
                  hasPreviousPage: true
                }
              }
            }
          }
        });
        var expectedUser = (0, _objectSpread2["default"])({}, initialUser, {
          friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1'
              }, createFragmentRef('node:1', query))
            }, {
              cursor: 'cursor:2',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:2',
                name: 'name:node:2'
              }, createFragmentRef('node:2', query))
            }],
            pageInfo: {
              endCursor: 'cursor:2',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:1'
            }
          })
        });
        expectFragmentResults([{
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }]);
        expect(callback).toBeCalledTimes(1);
      });
      it('loads more correctly using fragment variables from literal @argument values', function () {
        var expectedUser = (0, _objectSpread2["default"])({}, initialUser, {
          friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1'
              }, createFragmentRef('node:1', queryWithLiteralArgs))
            }]
          })
        });
        var callback = jest.fn();
        var renderer = renderFragment({
          owner: queryWithLiteralArgs
        });
        expectFragmentResults([{
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        var paginationVariables = {
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: true,
          orderby: ['name']
        };
        expect(paginationVariables.isViewerFriendLocal).not.toBe(variables.isViewerFriend);
        expectFragmentIsLoadingMore(renderer, direction, {
          data: expectedUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(callback).toBeCalledTimes(0);
        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:2',
                  node: {
                    __typename: 'User',
                    id: 'node:2',
                    name: 'name:node:2',
                    username: 'username:node:2'
                  }
                }],
                pageInfo: {
                  startCursor: 'cursor:2',
                  endCursor: 'cursor:2',
                  hasNextPage: true,
                  hasPreviousPage: true
                }
              }
            }
          }
        });
        expectedUser = (0, _objectSpread2["default"])({}, expectedUser, {
          friends: (0, _objectSpread2["default"])({}, expectedUser.friends, {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1'
              }, createFragmentRef('node:1', queryWithLiteralArgs))
            }, {
              cursor: 'cursor:2',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:2',
                name: 'name:node:2'
              }, createFragmentRef('node:2', queryWithLiteralArgs))
            }],
            pageInfo: {
              endCursor: 'cursor:2',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:1'
            }
          })
        });
        expectFragmentResults([{
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }]);
        expect(callback).toBeCalledTimes(1);
      });
      it('correctly loads and renders next items when paginating multiple times', function () {
        var callback = jest.fn();
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        var paginationVariables = {
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        expectFragmentIsLoadingMore(renderer, direction, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(callback).toBeCalledTimes(0);
        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:2',
                  node: {
                    __typename: 'User',
                    id: 'node:2',
                    name: 'name:node:2',
                    username: 'username:node:2'
                  }
                }],
                pageInfo: {
                  startCursor: 'cursor:2',
                  endCursor: 'cursor:2',
                  hasNextPage: true,
                  hasPreviousPage: true
                }
              }
            }
          }
        });
        var expectedUser = (0, _objectSpread2["default"])({}, initialUser, {
          friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1'
              }, createFragmentRef('node:1', query))
            }, {
              cursor: 'cursor:2',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:2',
                name: 'name:node:2'
              }, createFragmentRef('node:2', query))
            }],
            pageInfo: {
              endCursor: 'cursor:2',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:1'
            }
          })
        });
        expectFragmentResults([{
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }]);
        expect(callback).toBeCalledTimes(1); // Paginate a second time

        renderSpy.mockClear();
        callback.mockClear();
        environment.execute.mockClear();
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        paginationVariables = (0, _objectSpread2["default"])({}, paginationVariables, {
          after: 'cursor:2'
        });
        expectFragmentIsLoadingMore(renderer, direction, {
          data: expectedUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(callback).toBeCalledTimes(0);
        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:3',
                  node: {
                    __typename: 'User',
                    id: 'node:3',
                    name: 'name:node:3',
                    username: 'username:node:3'
                  }
                }],
                pageInfo: {
                  startCursor: 'cursor:3',
                  endCursor: 'cursor:3',
                  hasNextPage: true,
                  hasPreviousPage: true
                }
              }
            }
          }
        });
        expectedUser = (0, _objectSpread2["default"])({}, expectedUser, {
          friends: (0, _objectSpread2["default"])({}, expectedUser.friends, {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1'
              }, createFragmentRef('node:1', query))
            }, {
              cursor: 'cursor:2',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:2',
                name: 'name:node:2'
              }, createFragmentRef('node:2', query))
            }, {
              cursor: 'cursor:3',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:3',
                name: 'name:node:3'
              }, createFragmentRef('node:3', query))
            }],
            pageInfo: {
              endCursor: 'cursor:3',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:1'
            }
          })
        });
        expectFragmentResults([{
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }]);
        expect(callback).toBeCalledTimes(1);
      });
      it('does not suspend if pagination update is interruped before it commits (unsuspends)', function () {
        var callback = jest.fn();
        var renderer = renderFragment({
          isConcurrent: true
        });
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        loadNext(1, {
          onComplete: callback
        });
        Scheduler.unstable_flushAll();
        jest.runAllTimers();
        Scheduler.unstable_flushAll();
        var paginationVariables = {
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        expectFragmentIsLoadingMore(renderer, direction, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(callback).toBeCalledTimes(0); // Schedule a high-pri update while the component is
        // suspended on pagination

        Scheduler.unstable_runWithPriority(Scheduler.unstable_UserBlockingPriority, function () {
          forceUpdate(function (prev) {
            return prev + 1;
          });
        });
        Scheduler.unstable_flushAll(); // Assert high-pri update is rendered when initial update
        // that suspended hasn't committed
        // Assert that the avoided Suspense fallback isn't rendered

        expect(renderer.toJSON()).toEqual(null);
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]); // Assert list is updated after pagination request completes

        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:2',
                  node: {
                    __typename: 'User',
                    id: 'node:2',
                    name: 'name:node:2',
                    username: 'username:node:2'
                  }
                }],
                pageInfo: {
                  startCursor: 'cursor:2',
                  endCursor: 'cursor:2',
                  hasNextPage: true,
                  hasPreviousPage: true
                }
              }
            }
          }
        });
        var expectedUser = (0, _objectSpread2["default"])({}, initialUser, {
          friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1'
              }, createFragmentRef('node:1', query))
            }, {
              cursor: 'cursor:2',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:2',
                name: 'name:node:2'
              }, createFragmentRef('node:2', query))
            }],
            pageInfo: {
              endCursor: 'cursor:2',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:1'
            }
          })
        });
        expectFragmentResults([{
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }]);
        expect(callback).toBeCalledTimes(1);
      });
      it('updates are ignored while loading more (i.e. while suspended)', function () {
        jest.doMock('../useLoadMoreFunction');

        var useLoadMoreFunction = require('../useLoadMoreFunction'); // $FlowFixMe


        useLoadMoreFunction.mockImplementation(function () {
          for (var _len4 = arguments.length, args = new Array(_len4), _key4 = 0; _key4 < _len4; _key4++) {
            args[_key4] = arguments[_key4];
          }

          return jest.requireActual('../useLoadMoreFunction').apply(void 0, args);
        });
        var callback = jest.fn();
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        var paginationVariables = {
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        expectFragmentIsLoadingMore(renderer, direction, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(callback).toBeCalledTimes(0); // $FlowFixMe

        useLoadMoreFunction.mockClear();
        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '1',
            name: 'Alice updated'
          }
        }); // Assert that component did not re-render while suspended

        TestRenderer.act(function () {
          return jest.runAllImmediates();
        });
        expect(renderSpy).toBeCalledTimes(0);
        expect(useLoadMoreFunction).toBeCalledTimes(0);
        jest.dontMock('../useLoadMoreFunction');
      });
      it('renders with latest updated data from any updates missed while suspended for pagination', function () {
        var callback = jest.fn();
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        var paginationVariables = {
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        expectFragmentIsLoadingMore(renderer, direction, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(callback).toBeCalledTimes(0);
        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '1',
            name: 'Alice updated'
          }
        }); // Assert that component did not re-render while suspended

        TestRenderer.act(function () {
          return jest.runAllImmediates();
        });
        expect(renderSpy).toBeCalledTimes(0);
        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:2',
                  node: {
                    __typename: 'User',
                    id: 'node:2',
                    name: 'name:node:2',
                    username: 'username:node:2'
                  }
                }],
                pageInfo: {
                  startCursor: 'cursor:2',
                  endCursor: 'cursor:2',
                  hasNextPage: true,
                  hasPreviousPage: true
                }
              }
            }
          }
        });
        var expectedUser = (0, _objectSpread2["default"])({}, initialUser, {
          name: 'Alice',
          friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1'
              }, createFragmentRef('node:1', query))
            }, {
              cursor: 'cursor:2',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:2',
                name: 'name:node:2'
              }, createFragmentRef('node:2', query))
            }],
            pageInfo: {
              endCursor: 'cursor:2',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:1'
            }
          })
        });
        expectFragmentResults([{
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }]);
        expect(callback).toBeCalledTimes(1);
      });
      it('correctly updates when fragment data changes after pagination', function () {
        var callback = jest.fn();
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        var paginationVariables = {
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        expectFragmentIsLoadingMore(renderer, direction, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(callback).toBeCalledTimes(0);
        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:2',
                  node: {
                    __typename: 'User',
                    id: 'node:2',
                    name: 'name:node:2',
                    username: 'username:node:2'
                  }
                }],
                pageInfo: {
                  startCursor: 'cursor:2',
                  endCursor: 'cursor:2',
                  hasNextPage: true,
                  hasPreviousPage: true
                }
              }
            }
          }
        });
        var expectedUser = (0, _objectSpread2["default"])({}, initialUser, {
          friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1'
              }, createFragmentRef('node:1', query))
            }, {
              cursor: 'cursor:2',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:2',
                name: 'name:node:2'
              }, createFragmentRef('node:2', query))
            }],
            pageInfo: {
              endCursor: 'cursor:2',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:1'
            }
          })
        });
        expectFragmentResults([{
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }]);
        expect(callback).toBeCalledTimes(1); // Update parent record

        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '1',
            // Update name
            name: 'Alice in Wonderland'
          }
        });
        expectFragmentResults([{
          data: (0, _objectSpread2["default"])({}, initialUser, {
            // Assert that name is updated
            name: 'Alice in Wonderland'
          }),
          hasNext: true,
          hasPrevious: false
        }]); // Update edge

        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: 'node:1',
            // Update name
            name: 'name:node:1-updated'
          }
        });
        expectFragmentResults([{
          data: (0, _objectSpread2["default"])({}, initialUser, {
            name: 'Alice in Wonderland',
            friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
              edges: [{
                cursor: 'cursor:1',
                node: (0, _objectSpread2["default"])({
                  __typename: 'User',
                  id: 'node:1',
                  // Assert that name is updated
                  name: 'name:node:1-updated'
                }, createFragmentRef('node:1', query))
              }]
            })
          }),
          hasNext: true,
          hasPrevious: false
        }]);
      });
      it('loads more correctly when original variables do not include an id', function () {
        var _environment$lookup$d2;

        var callback = jest.fn();
        var viewer = (_environment$lookup$d2 = environment.lookup(queryWithoutID.fragment).data) === null || _environment$lookup$d2 === void 0 ? void 0 : _environment$lookup$d2.viewer;
        var userRef = typeof viewer === 'object' && viewer != null ? viewer === null || viewer === void 0 ? void 0 : viewer.actor : null;
        !(userRef != null) ? process.env.NODE_ENV !== "production" ? invariant(false, 'Expected to have cached test data') : invariant(false) : void 0;
        var expectedUser = (0, _objectSpread2["default"])({}, initialUser, {
          friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1'
              }, createFragmentRef('node:1', queryWithoutID))
            }]
          })
        });
        var renderer = renderFragment({
          owner: queryWithoutID,
          userRef: userRef
        });
        expectFragmentResults([{
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        var paginationVariables = {
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        expectFragmentIsLoadingMore(renderer, direction, {
          data: expectedUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(callback).toBeCalledTimes(0);
        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:2',
                  node: {
                    __typename: 'User',
                    id: 'node:2',
                    name: 'name:node:2',
                    username: 'username:node:2'
                  }
                }],
                pageInfo: {
                  startCursor: 'cursor:2',
                  endCursor: 'cursor:2',
                  hasNextPage: true,
                  hasPreviousPage: true
                }
              }
            }
          }
        });
        expectedUser = (0, _objectSpread2["default"])({}, initialUser, {
          friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1'
              }, createFragmentRef('node:1', queryWithoutID))
            }, {
              cursor: 'cursor:2',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:2',
                name: 'name:node:2'
              }, createFragmentRef('node:2', queryWithoutID))
            }],
            pageInfo: {
              endCursor: 'cursor:2',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:1'
            }
          })
        });
        expectFragmentResults([{
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }]);
        expect(callback).toBeCalledTimes(1);
      });
      it('loads more with correct id from refetchable fragment when using a nested fragment', function () {
        var _ref, _ref$node;

        var callback = jest.fn(); // Populate store with data for query using nested fragment

        environment.commitPayload(queryNestedFragment, {
          node: {
            __typename: 'Feedback',
            id: '<feedbackid>',
            actor: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:1',
                  node: {
                    __typename: 'User',
                    id: 'node:1',
                    name: 'name:node:1',
                    username: 'username:node:1'
                  }
                }],
                pageInfo: {
                  endCursor: 'cursor:1',
                  hasNextPage: true,
                  hasPreviousPage: false,
                  startCursor: 'cursor:1'
                }
              }
            }
          }
        }); // Get fragment ref for user using nested fragment

        var userRef = (_ref = environment.lookup(queryNestedFragment.fragment).data) === null || _ref === void 0 ? void 0 : (_ref$node = _ref.node) === null || _ref$node === void 0 ? void 0 : _ref$node.actor;
        initialUser = {
          id: '1',
          name: 'Alice',
          friends: {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1'
              }, createFragmentRef('node:1', queryNestedFragment))
            }],
            pageInfo: {
              endCursor: 'cursor:1',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:1'
            }
          }
        };
        var renderer = renderFragment({
          owner: queryNestedFragment,
          userRef: userRef
        });
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        var paginationVariables = {
          // The id here should correspond to the user id, and not the
          // feedback id from the query variables (i.e. `<feedbackid>`)
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        expectFragmentIsLoadingMore(renderer, direction, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(callback).toBeCalledTimes(0);
        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:2',
                  node: {
                    __typename: 'User',
                    id: 'node:2',
                    name: 'name:node:2',
                    username: 'username:node:2'
                  }
                }],
                pageInfo: {
                  startCursor: 'cursor:2',
                  endCursor: 'cursor:2',
                  hasNextPage: true,
                  hasPreviousPage: true
                }
              }
            }
          }
        });
        var expectedUser = (0, _objectSpread2["default"])({}, initialUser, {
          friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1'
              }, createFragmentRef('node:1', queryNestedFragment))
            }, {
              cursor: 'cursor:2',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:2',
                name: 'name:node:2'
              }, createFragmentRef('node:2', queryNestedFragment))
            }],
            pageInfo: {
              endCursor: 'cursor:2',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:1'
            }
          })
        });
        expectFragmentResults([{
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }]);
        expect(callback).toBeCalledTimes(1);
      });
      it('calls callback with error when error occurs during fetch', function () {
        var callback = jest.fn();
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        var paginationVariables = {
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        expectFragmentIsLoadingMore(renderer, direction, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(callback).toBeCalledTimes(0);
        var error = new Error('Oops');
        environment.mock.reject(gqlPaginationQuery, error); // We pass the error in the callback, but do not throw during render
        // since we want to continue rendering the existing items in the
        // connection

        expect(callback).toBeCalledTimes(1);
        expect(callback).toBeCalledWith(error);
      });
      it('preserves pagination request if re-rendered with same fragment ref', function () {
        var callback = jest.fn();
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        var paginationVariables = {
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        expectFragmentIsLoadingMore(renderer, direction, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(callback).toBeCalledTimes(0);
        TestRenderer.act(function () {
          setOwner((0, _objectSpread2["default"])({}, query));
        }); // Assert that request is still in flight after re-rendering
        // with new fragment ref that points to the same data.

        expectFragmentIsLoadingMore(renderer, direction, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(callback).toBeCalledTimes(0);
        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:2',
                  node: {
                    __typename: 'User',
                    id: 'node:2',
                    name: 'name:node:2',
                    username: 'username:node:2'
                  }
                }],
                pageInfo: {
                  startCursor: 'cursor:2',
                  endCursor: 'cursor:2',
                  hasNextPage: true,
                  hasPreviousPage: true
                }
              }
            }
          }
        });
        var expectedUser = (0, _objectSpread2["default"])({}, initialUser, {
          friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1'
              }, createFragmentRef('node:1', query))
            }, {
              cursor: 'cursor:2',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:2',
                name: 'name:node:2'
              }, createFragmentRef('node:2', query))
            }],
            pageInfo: {
              endCursor: 'cursor:2',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:1'
            }
          })
        });
        expectFragmentResults([{
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }]);
        expect(callback).toBeCalledTimes(1);
      });
      describe('disposing', function () {
        var _unsubscribe3;

        beforeEach(function () {
          _unsubscribe3 = jest.fn();
          jest.doMock('relay-runtime', function () {
            var originalRuntime = jest.requireActual('relay-runtime');
            var originalInternal = originalRuntime.__internal;
            return (0, _objectSpread2["default"])({}, originalRuntime, {
              __internal: (0, _objectSpread2["default"])({}, originalInternal, {
                fetchQuery: function fetchQuery() {
                  for (var _len5 = arguments.length, args = new Array(_len5), _key5 = 0; _key5 < _len5; _key5++) {
                    args[_key5] = arguments[_key5];
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

                              _unsubscribe3();
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
          var callback = jest.fn();
          var renderer = renderFragment();
          expectFragmentResults([{
            data: initialUser,
            hasNext: true,
            hasPrevious: false
          }]);
          TestRenderer.act(function () {
            loadNext(1, {
              onComplete: callback
            });
          }); // Assert request is started

          var paginationVariables = {
            id: '1',
            after: 'cursor:1',
            first: 1,
            before: null,
            last: null,
            isViewerFriendLocal: false,
            orderby: ['name']
          };
          expectFragmentIsLoadingMore(renderer, direction, {
            data: initialUser,
            hasNext: true,
            hasPrevious: false,
            paginationVariables: paginationVariables,
            gqlPaginationQuery: gqlPaginationQuery
          });
          expect(callback).toBeCalledTimes(0); // Set new environment

          var newEnvironment = createMockEnvironment({
            handlerProvider: function handlerProvider() {
              return ConnectionHandler;
            }
          });
          newEnvironment.commitPayload(query, {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice in a different environment',
              friends: {
                edges: [{
                  cursor: 'cursor:1',
                  node: {
                    __typename: 'User',
                    id: 'node:1',
                    name: 'name:node:1',
                    username: 'username:node:1'
                  }
                }],
                pageInfo: {
                  endCursor: 'cursor:1',
                  hasNextPage: true,
                  hasPreviousPage: false,
                  startCursor: 'cursor:1'
                }
              }
            }
          });
          TestRenderer.act(function () {
            setEnvironment(newEnvironment);
          }); // Assert request was canceled

          expect(_unsubscribe3).toBeCalledTimes(1);
          expectRequestIsInFlight({
            inFlight: false,
            requestCount: 1,
            gqlPaginationQuery: gqlPaginationQuery,
            paginationVariables: paginationVariables
          }); // Assert newly rendered data

          expectFragmentResults([{
            data: (0, _objectSpread2["default"])({}, initialUser, {
              name: 'Alice in a different environment'
            }),
            hasNext: true,
            hasPrevious: false
          }, {
            data: (0, _objectSpread2["default"])({}, initialUser, {
              name: 'Alice in a different environment'
            }),
            hasNext: true,
            hasPrevious: false
          }]);
        });
        it('disposes ongoing request if fragment ref changes', function () {
          var callback = jest.fn();
          var renderer = renderFragment();
          expectFragmentResults([{
            data: initialUser,
            hasNext: true,
            hasPrevious: false
          }]);
          TestRenderer.act(function () {
            loadNext(1, {
              onComplete: callback
            });
          }); // Assert request is started

          var paginationVariables = {
            id: '1',
            after: 'cursor:1',
            first: 1,
            before: null,
            last: null,
            isViewerFriendLocal: false,
            orderby: ['name']
          };
          expectFragmentIsLoadingMore(renderer, direction, {
            data: initialUser,
            hasNext: true,
            hasPrevious: false,
            paginationVariables: paginationVariables,
            gqlPaginationQuery: gqlPaginationQuery
          });
          expect(callback).toBeCalledTimes(0); // Pass new parent fragment ref with different variables

          var newVariables = (0, _objectSpread2["default"])({}, variables, {
            isViewerFriend: true
          });
          var newQuery = createOperationDescriptor(gqlQuery, newVariables);
          environment.commitPayload(newQuery, {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:1',
                  node: {
                    __typename: 'User',
                    id: 'node:1',
                    name: 'name:node:1',
                    username: 'username:node:1'
                  }
                }],
                pageInfo: {
                  endCursor: 'cursor:1',
                  hasNextPage: true,
                  hasPreviousPage: false,
                  startCursor: 'cursor:1'
                }
              }
            }
          });
          TestRenderer.act(function () {
            setOwner(newQuery);
          }); // Assert request was canceled

          expect(_unsubscribe3).toBeCalledTimes(1);
          expectRequestIsInFlight({
            inFlight: false,
            requestCount: 1,
            gqlPaginationQuery: gqlPaginationQuery,
            paginationVariables: paginationVariables
          }); // Assert newly rendered data

          var expectedUser = (0, _objectSpread2["default"])({}, initialUser, {
            friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
              edges: [{
                cursor: 'cursor:1',
                node: (0, _objectSpread2["default"])({
                  __typename: 'User',
                  id: 'node:1',
                  name: 'name:node:1'
                }, createFragmentRef('node:1', newQuery))
              }]
            })
          });
          expectFragmentResults([{
            data: expectedUser,
            hasNext: true,
            hasPrevious: false
          }, {
            data: expectedUser,
            hasNext: true,
            hasPrevious: false
          }]);
        });
        it('disposes ongoing request on unmount', function () {
          var callback = jest.fn();
          var renderer = renderFragment();
          expectFragmentResults([{
            data: initialUser,
            hasNext: true,
            hasPrevious: false
          }]);
          TestRenderer.act(function () {
            loadNext(1, {
              onComplete: callback
            });
          }); // Assert request is started

          var paginationVariables = {
            id: '1',
            after: 'cursor:1',
            first: 1,
            before: null,
            last: null,
            isViewerFriendLocal: false,
            orderby: ['name']
          };
          expectFragmentIsLoadingMore(renderer, direction, {
            data: initialUser,
            hasNext: true,
            hasPrevious: false,
            paginationVariables: paginationVariables,
            gqlPaginationQuery: gqlPaginationQuery
          });
          expect(callback).toBeCalledTimes(0);
          renderer.unmount(); // Assert request was canceled

          expect(_unsubscribe3).toBeCalledTimes(1);
          expectRequestIsInFlight({
            inFlight: false,
            requestCount: 1,
            gqlPaginationQuery: gqlPaginationQuery,
            paginationVariables: paginationVariables
          });
        });
        it('disposes ongoing request if it is manually disposed', function () {
          var callback = jest.fn();
          var renderer = renderFragment();
          expectFragmentResults([{
            data: initialUser,
            hasNext: true,
            hasPrevious: false
          }]);
          var disposable;
          TestRenderer.act(function () {
            disposable = loadNext(1, {
              onComplete: callback
            });
          }); // Assert request is started

          var paginationVariables = {
            id: '1',
            after: 'cursor:1',
            first: 1,
            before: null,
            last: null,
            isViewerFriendLocal: false,
            orderby: ['name']
          };
          expectFragmentIsLoadingMore(renderer, direction, {
            data: initialUser,
            hasNext: true,
            hasPrevious: false,
            paginationVariables: paginationVariables,
            gqlPaginationQuery: gqlPaginationQuery
          });
          expect(callback).toBeCalledTimes(0); // $FlowFixMe

          disposable.dispose(); // Assert request was canceled

          expect(_unsubscribe3).toBeCalledTimes(1);
          expectRequestIsInFlight({
            inFlight: false,
            requestCount: 1,
            gqlPaginationQuery: gqlPaginationQuery,
            paginationVariables: paginationVariables
          });
          expect(renderSpy).toHaveBeenCalledTimes(0);
        });
      });
    });
    describe('hasNext', function () {
      var direction = 'forward';
      it('returns true if it has more items', function () {
        environment.getStore().getSource().clear();
        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '1',
            name: 'Alice',
            friends: {
              edges: [{
                cursor: 'cursor:1',
                node: {
                  __typename: 'User',
                  id: 'node:1',
                  name: 'name:node:1',
                  username: 'username:node:1'
                }
              }],
              pageInfo: {
                endCursor: 'cursor:1',
                hasNextPage: true,
                hasPreviousPage: false,
                startCursor: 'cursor:1'
              }
            }
          }
        });
        renderFragment();
        expectFragmentResults([{
          data: (0, _objectSpread2["default"])({}, initialUser, {
            friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
              pageInfo: expect.objectContaining({
                hasNextPage: true
              })
            })
          }),
          // Assert hasNext is true
          hasNext: true,
          hasPrevious: false
        }]);
      });
      it('returns false if edges are null', function () {
        environment.getStore().getSource().clear();
        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '1',
            name: 'Alice',
            friends: {
              edges: null,
              pageInfo: {
                endCursor: 'cursor:1',
                hasNextPage: true,
                hasPreviousPage: false,
                startCursor: 'cursor:1'
              }
            }
          }
        });
        renderFragment();
        expectFragmentResults([{
          data: (0, _objectSpread2["default"])({}, initialUser, {
            friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
              edges: null,
              pageInfo: expect.objectContaining({
                hasNextPage: true
              })
            })
          }),
          // Assert hasNext is false
          hasNext: false,
          hasPrevious: false
        }]);
      });
      it('returns false if edges are undefined', function () {
        environment.getStore().getSource().clear();
        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '1',
            name: 'Alice',
            friends: {
              edges: undefined,
              pageInfo: {
                endCursor: 'cursor:1',
                hasNextPage: true,
                hasPreviousPage: false,
                startCursor: 'cursor:1'
              }
            }
          }
        });
        renderFragment();
        expectFragmentResults([{
          data: (0, _objectSpread2["default"])({}, initialUser, {
            friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
              edges: undefined,
              pageInfo: expect.objectContaining({
                hasNextPage: true
              })
            })
          }),
          // Assert hasNext is false
          hasNext: false,
          hasPrevious: false
        }]);
      });
      it('returns false if end cursor is null', function () {
        environment.getStore().getSource().clear();
        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '1',
            name: 'Alice',
            friends: {
              edges: [{
                cursor: 'cursor:1',
                node: {
                  __typename: 'User',
                  id: 'node:1',
                  name: 'name:node:1',
                  username: 'username:node:1'
                }
              }],
              pageInfo: {
                // endCursor is null
                endCursor: null,
                // but hasNextPage is still true
                hasNextPage: true,
                hasPreviousPage: false,
                startCursor: null
              }
            }
          }
        });
        renderFragment();
        expectFragmentResults([{
          data: (0, _objectSpread2["default"])({}, initialUser, {
            friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
              pageInfo: expect.objectContaining({
                endCursor: null,
                hasNextPage: true
              })
            })
          }),
          // Assert hasNext is false
          hasNext: false,
          hasPrevious: false
        }]);
      });
      it('returns false if end cursor is undefined', function () {
        environment.getStore().getSource().clear();
        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '1',
            name: 'Alice',
            friends: {
              edges: [{
                cursor: 'cursor:1',
                node: {
                  __typename: 'User',
                  id: 'node:1',
                  name: 'name:node:1',
                  username: 'username:node:1'
                }
              }],
              pageInfo: {
                // endCursor is undefined
                endCursor: undefined,
                // but hasNextPage is still true
                hasNextPage: true,
                hasPreviousPage: false,
                startCursor: undefined
              }
            }
          }
        });
        renderFragment();
        expectFragmentResults([{
          data: (0, _objectSpread2["default"])({}, initialUser, {
            friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
              pageInfo: expect.objectContaining({
                endCursor: null,
                hasNextPage: true
              })
            })
          }),
          // Assert hasNext is false
          hasNext: false,
          hasPrevious: false
        }]);
      });
      it('returns false if pageInfo.hasNextPage is false-ish', function () {
        environment.getStore().getSource().clear();
        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '1',
            name: 'Alice',
            friends: {
              edges: [{
                cursor: 'cursor:1',
                node: {
                  __typename: 'User',
                  id: 'node:1',
                  name: 'name:node:1',
                  username: 'username:node:1'
                }
              }],
              pageInfo: {
                endCursor: 'cursor:1',
                hasNextPage: null,
                hasPreviousPage: false,
                startCursor: 'cursor:1'
              }
            }
          }
        });
        renderFragment();
        expectFragmentResults([{
          data: (0, _objectSpread2["default"])({}, initialUser, {
            friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
              pageInfo: expect.objectContaining({
                hasNextPage: null
              })
            })
          }),
          // Assert hasNext is false
          hasNext: false,
          hasPrevious: false
        }]);
      });
      it('returns false if pageInfo.hasNextPage is false', function () {
        environment.getStore().getSource().clear();
        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '1',
            name: 'Alice',
            friends: {
              edges: [{
                cursor: 'cursor:1',
                node: {
                  __typename: 'User',
                  id: 'node:1',
                  name: 'name:node:1',
                  username: 'username:node:1'
                }
              }],
              pageInfo: {
                endCursor: 'cursor:1',
                hasNextPage: false,
                hasPreviousPage: false,
                startCursor: 'cursor:1'
              }
            }
          }
        });
        renderFragment();
        expectFragmentResults([{
          data: (0, _objectSpread2["default"])({}, initialUser, {
            friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
              pageInfo: expect.objectContaining({
                hasNextPage: false
              })
            })
          }),
          // Assert hasNext is false
          hasNext: false,
          hasPrevious: false
        }]);
      });
      it('updates after pagination if more results are avialable', function () {
        var callback = jest.fn();
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        var paginationVariables = {
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        expectFragmentIsLoadingMore(renderer, direction, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(callback).toBeCalledTimes(0);
        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:2',
                  node: {
                    __typename: 'User',
                    id: 'node:2',
                    name: 'name:node:2',
                    username: 'username:node:2'
                  }
                }],
                pageInfo: {
                  startCursor: 'cursor:2',
                  endCursor: 'cursor:2',
                  hasNextPage: true,
                  hasPreviousPage: true
                }
              }
            }
          }
        });
        var expectedUser = (0, _objectSpread2["default"])({}, initialUser, {
          friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1'
              }, createFragmentRef('node:1', query))
            }, {
              cursor: 'cursor:2',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:2',
                name: 'name:node:2'
              }, createFragmentRef('node:2', query))
            }],
            pageInfo: {
              endCursor: 'cursor:2',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:1'
            }
          })
        });
        expectFragmentResults([{
          // First update has updated connection
          data: expectedUser,
          // Assert hasNext reflects server response
          hasNext: true,
          hasPrevious: false
        }]);
        expect(callback).toBeCalledTimes(1);
      });
      it('updates after pagination if no more results are avialable', function () {
        var callback = jest.fn();
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
        });
        var paginationVariables = {
          id: '1',
          after: 'cursor:1',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        expectFragmentIsLoadingMore(renderer, direction, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        expect(callback).toBeCalledTimes(0);
        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:2',
                  node: {
                    __typename: 'User',
                    id: 'node:2',
                    name: 'name:node:2',
                    username: 'username:node:2'
                  }
                }],
                pageInfo: {
                  startCursor: 'cursor:2',
                  endCursor: 'cursor:2',
                  hasNextPage: false,
                  hasPreviousPage: true
                }
              }
            }
          }
        });
        var expectedUser = (0, _objectSpread2["default"])({}, initialUser, {
          friends: (0, _objectSpread2["default"])({}, initialUser.friends, {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1'
              }, createFragmentRef('node:1', query))
            }, {
              cursor: 'cursor:2',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:2',
                name: 'name:node:2'
              }, createFragmentRef('node:2', query))
            }],
            pageInfo: {
              endCursor: 'cursor:2',
              hasNextPage: false,
              hasPreviousPage: false,
              startCursor: 'cursor:1'
            }
          })
        });
        expectFragmentResults([{
          data: expectedUser,
          // Assert hasNext reflects server response
          hasNext: false,
          hasPrevious: false
        }]);
        expect(callback).toBeCalledTimes(1);
      });
    });
    describe('refetch', function () {
      // The bulk of refetch behavior is covered in useRefetchableFragmentNode-test,
      // so this suite covers the pagination-related test cases.
      function expectRefetchRequestIsInFlight(expected) {
        var _expected$gqlRefetchQ;

        expect(environment.execute).toBeCalledTimes(expected.requestCount);
        expect(environment.mock.isLoading((_expected$gqlRefetchQ = expected.gqlRefetchQuery) !== null && _expected$gqlRefetchQ !== void 0 ? _expected$gqlRefetchQ : gqlPaginationQuery, expected.refetchVariables, {
          force: true
        })).toEqual(expected.inFlight);
      }

      function expectFragmentIsRefetching(renderer, expected) {
        var _ref2, _expected$refetchQuer;

        expect(renderSpy).toBeCalledTimes(0);
        renderSpy.mockClear(); // Assert refetch query was fetched

        expectRefetchRequestIsInFlight((0, _objectSpread2["default"])({}, expected, {
          inFlight: true,
          requestCount: 1
        })); // Assert component suspended

        expect(renderSpy).toBeCalledTimes(0);
        expect(renderer.toJSON()).toEqual('Fallback'); // Assert query is tentatively retained while component is suspended

        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual((_ref2 = (_expected$refetchQuer = expected.refetchQuery) === null || _expected$refetchQuer === void 0 ? void 0 : _expected$refetchQuer.root) !== null && _ref2 !== void 0 ? _ref2 : paginationQuery.root);
      }

      it('refetches new variables correctly when refetching new id', function () {
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          refetch({
            id: '4'
          });
        }); // Assert that fragment is refetching with the right variables and
        // suspends upon refetch

        var refetchVariables = {
          after: null,
          first: 1,
          before: null,
          last: null,
          id: '4',
          isViewerFriendLocal: false,
          orderby: ['name']
        };
        paginationQuery = createOperationDescriptor(gqlPaginationQuery, refetchVariables);
        expectFragmentIsRefetching(renderer, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          refetchVariables: refetchVariables,
          refetchQuery: paginationQuery
        }); // Mock network response

        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '4',
              name: 'Mark',
              friends: {
                edges: [{
                  cursor: 'cursor:100',
                  node: {
                    __typename: 'User',
                    id: 'node:100',
                    name: 'name:node:100',
                    username: 'username:node:100'
                  }
                }],
                pageInfo: {
                  endCursor: 'cursor:100',
                  hasNextPage: true,
                  hasPreviousPage: false,
                  startCursor: 'cursor:100'
                }
              }
            }
          }
        }); // Assert fragment is rendered with new data

        var expectedUser = {
          id: '4',
          name: 'Mark',
          friends: {
            edges: [{
              cursor: 'cursor:100',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:100',
                name: 'name:node:100'
              }, createFragmentRef('node:100', paginationQuery))
            }],
            pageInfo: {
              endCursor: 'cursor:100',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:100'
            }
          }
        };
        expectFragmentResults([{
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }, {
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }]); // Assert refetch query was retained

        expect(release).not.toBeCalled();
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(paginationQuery.root);
      });
      it('refetches new variables correctly when refetching same id', function () {
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          refetch({
            isViewerFriendLocal: true,
            orderby: ['lastname']
          });
        }); // Assert that fragment is refetching with the right variables and
        // suspends upon refetch

        var refetchVariables = {
          after: null,
          first: 1,
          before: null,
          last: null,
          id: '1',
          isViewerFriendLocal: true,
          orderby: ['lastname']
        };
        paginationQuery = createOperationDescriptor(gqlPaginationQuery, refetchVariables);
        expectFragmentIsRefetching(renderer, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          refetchVariables: refetchVariables,
          refetchQuery: paginationQuery
        }); // Mock network response

        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:100',
                  node: {
                    __typename: 'User',
                    id: 'node:100',
                    name: 'name:node:100',
                    username: 'username:node:100'
                  }
                }],
                pageInfo: {
                  endCursor: 'cursor:100',
                  hasNextPage: true,
                  hasPreviousPage: false,
                  startCursor: 'cursor:100'
                }
              }
            }
          }
        }); // Assert fragment is rendered with new data

        var expectedUser = {
          id: '1',
          name: 'Alice',
          friends: {
            edges: [{
              cursor: 'cursor:100',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:100',
                name: 'name:node:100'
              }, createFragmentRef('node:100', paginationQuery))
            }],
            pageInfo: {
              endCursor: 'cursor:100',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:100'
            }
          }
        };
        expectFragmentResults([{
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }, {
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }]); // Assert refetch query was retained

        expect(release).not.toBeCalled();
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(paginationQuery.root);
      });
      it('refetches with correct id from refetchable fragment when using nested fragment', function () {
        var _ref3, _ref3$node;

        // Populate store with data for query using nested fragment
        environment.commitPayload(queryNestedFragment, {
          node: {
            __typename: 'Feedback',
            id: '<feedbackid>',
            actor: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:1',
                  node: {
                    __typename: 'User',
                    id: 'node:1',
                    name: 'name:node:1',
                    username: 'username:node:1'
                  }
                }],
                pageInfo: {
                  endCursor: 'cursor:1',
                  hasNextPage: true,
                  hasPreviousPage: false,
                  startCursor: 'cursor:1'
                }
              }
            }
          }
        }); // Get fragment ref for user using nested fragment

        var userRef = (_ref3 = environment.lookup(queryNestedFragment.fragment).data) === null || _ref3 === void 0 ? void 0 : (_ref3$node = _ref3.node) === null || _ref3$node === void 0 ? void 0 : _ref3$node.actor;
        initialUser = {
          id: '1',
          name: 'Alice',
          friends: {
            edges: [{
              cursor: 'cursor:1',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:1',
                name: 'name:node:1'
              }, createFragmentRef('node:1', queryNestedFragment))
            }],
            pageInfo: {
              endCursor: 'cursor:1',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:1'
            }
          }
        };
        var renderer = renderFragment({
          owner: queryNestedFragment,
          userRef: userRef
        });
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          refetch({
            isViewerFriendLocal: true,
            orderby: ['lastname']
          });
        }); // Assert that fragment is refetching with the right variables and
        // suspends upon refetch

        var refetchVariables = {
          after: null,
          first: 1,
          before: null,
          last: null,
          // The id here should correspond to the user id, and not the
          // feedback id from the query variables (i.e. `<feedbackid>`)
          id: '1',
          isViewerFriendLocal: true,
          orderby: ['lastname']
        };
        paginationQuery = createOperationDescriptor(gqlPaginationQuery, refetchVariables);
        expectFragmentIsRefetching(renderer, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          refetchVariables: refetchVariables,
          refetchQuery: paginationQuery
        }); // Mock network response

        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:100',
                  node: {
                    __typename: 'User',
                    id: 'node:100',
                    name: 'name:node:100',
                    username: 'username:node:100'
                  }
                }],
                pageInfo: {
                  endCursor: 'cursor:100',
                  hasNextPage: true,
                  hasPreviousPage: false,
                  startCursor: 'cursor:100'
                }
              }
            }
          }
        }); // Assert fragment is rendered with new data

        var expectedUser = {
          id: '1',
          name: 'Alice',
          friends: {
            edges: [{
              cursor: 'cursor:100',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:100',
                name: 'name:node:100'
              }, createFragmentRef('node:100', paginationQuery))
            }],
            pageInfo: {
              endCursor: 'cursor:100',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:100'
            }
          }
        };
        expectFragmentResults([{
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }, {
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }]); // Assert refetch query was retained

        expect(release).not.toBeCalled();
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(paginationQuery.root);
      });
      it('loads more items correctly after refetching', function () {
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          refetch({
            isViewerFriendLocal: true,
            orderby: ['lastname']
          });
        }); // Assert that fragment is refetching with the right variables and
        // suspends upon refetch

        var refetchVariables = {
          after: null,
          first: 1,
          before: null,
          last: null,
          id: '1',
          isViewerFriendLocal: true,
          orderby: ['lastname']
        };
        paginationQuery = createOperationDescriptor(gqlPaginationQuery, refetchVariables);
        expectFragmentIsRefetching(renderer, {
          data: initialUser,
          hasNext: true,
          hasPrevious: false,
          refetchVariables: refetchVariables,
          refetchQuery: paginationQuery
        }); // Mock network response

        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:100',
                  node: {
                    __typename: 'User',
                    id: 'node:100',
                    name: 'name:node:100',
                    username: 'username:node:100'
                  }
                }],
                pageInfo: {
                  endCursor: 'cursor:100',
                  hasNextPage: true,
                  hasPreviousPage: false,
                  startCursor: 'cursor:100'
                }
              }
            }
          }
        }); // Assert fragment is rendered with new data

        var expectedUser = {
          id: '1',
          name: 'Alice',
          friends: {
            edges: [{
              cursor: 'cursor:100',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:100',
                name: 'name:node:100'
              }, createFragmentRef('node:100', paginationQuery))
            }],
            pageInfo: {
              endCursor: 'cursor:100',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:100'
            }
          }
        };
        expectFragmentResults([{
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }, {
          data: expectedUser,
          hasNext: true,
          hasPrevious: false
        }]); // Assert refetch query was retained

        expect(release).not.toBeCalled();
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(paginationQuery.root); // Paginate after refetching

        environment.execute.mockClear();
        TestRenderer.act(function () {
          loadNext(1);
        });
        var paginationVariables = {
          id: '1',
          after: 'cursor:100',
          first: 1,
          before: null,
          last: null,
          isViewerFriendLocal: true,
          orderby: ['lastname']
        };
        expectFragmentIsLoadingMore(renderer, 'forward', {
          data: expectedUser,
          hasNext: true,
          hasPrevious: false,
          paginationVariables: paginationVariables,
          gqlPaginationQuery: gqlPaginationQuery
        });
        environment.mock.resolve(gqlPaginationQuery, {
          data: {
            node: {
              __typename: 'User',
              id: '1',
              name: 'Alice',
              friends: {
                edges: [{
                  cursor: 'cursor:200',
                  node: {
                    __typename: 'User',
                    id: 'node:200',
                    name: 'name:node:200',
                    username: 'username:node:200'
                  }
                }],
                pageInfo: {
                  startCursor: 'cursor:200',
                  endCursor: 'cursor:200',
                  hasNextPage: true,
                  hasPreviousPage: true
                }
              }
            }
          }
        });
        var paginatedUser = (0, _objectSpread2["default"])({}, expectedUser, {
          friends: (0, _objectSpread2["default"])({}, expectedUser.friends, {
            edges: [{
              cursor: 'cursor:100',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:100',
                name: 'name:node:100'
              }, createFragmentRef('node:100', paginationQuery))
            }, {
              cursor: 'cursor:200',
              node: (0, _objectSpread2["default"])({
                __typename: 'User',
                id: 'node:200',
                name: 'name:node:200'
              }, createFragmentRef('node:200', paginationQuery))
            }],
            pageInfo: {
              endCursor: 'cursor:200',
              hasNextPage: true,
              hasPreviousPage: false,
              startCursor: 'cursor:100'
            }
          })
        });
        expectFragmentResults([{
          // Second update sets isLoading flag back to false
          data: paginatedUser,
          hasNext: true,
          hasPrevious: false
        }]);
      });
    });
  });
});