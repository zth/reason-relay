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

var unstable_withSuspenseConfig = React.unstable_withSuspenseConfig,
    useCallback = React.useCallback,
    useMemo = React.useMemo,
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

var PAGINATION_SUSPENSE_CONFIG = {
  timeoutMs: 45 * 1000
};

function useSuspenseTransition(config) {
  var _useState = useState(false),
      isPending = _useState[0],
      setPending = _useState[1];

  var startTransition = useCallback(function (callback) {
    setPending(true);
    Scheduler.unstable_next(function () {
      unstable_withSuspenseConfig(function () {
        setPending(false);
        callback();
      }, config);
    });
  }, [config, setPending]);
  return [startTransition, isPending];
}

describe('useBlockingPaginationFragment with useSuspenseTransition', function () {
  var environment;
  var initialUser;
  var gqlQuery;
  var gqlQueryWithoutID;
  var gqlPaginationQuery;
  var gqlFragment;
  var query;
  var queryWithoutID;
  var paginationQuery;
  var variables;
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

  function useBlockingPaginationFragmentWithSuspenseTransition(fragmentNode, fragmentRef) {
    var _useSuspenseTransitio = useSuspenseTransition(PAGINATION_SUSPENSE_CONFIG),
        startTransition = _useSuspenseTransitio[0],
        isPendingNext = _useSuspenseTransitio[1];
    /* $FlowFixMe(>=0.108.0 site=www,mobile,react_native_fb,oss) This comment suppresses an error found
     * when Flow v0.108.0 was deployed. To see the error delete this comment
     * and run Flow. */


    var _useBlockingPaginatio = useBlockingPaginationFragmentOriginal(fragmentNode, // $FlowFixMe
    fragmentRef),
        data = _useBlockingPaginatio.data,
        result = (0, _objectWithoutPropertiesLoose2["default"])(_useBlockingPaginatio, ["data"]);

    loadNext = function loadNext() {
      for (var _len2 = arguments.length, args = new Array(_len2), _key2 = 0; _key2 < _len2; _key2++) {
        args[_key2] = arguments[_key2];
      }

      var disposable = {
        dispose: function dispose() {}
      };
      startTransition(function () {
        disposable = result.loadNext.apply(result, args);
      });
      return disposable;
    };

    refetch = result.refetch; // $FlowFixMe

    result.isPendingNext = isPendingNext;
    renderSpy(data, result);
    return (0, _objectSpread2["default"])({
      data: data
    }, result);
  }

  function assertCall(expected, idx) {
    var actualData = renderSpy.mock.calls[idx][0];
    var actualResult = renderSpy.mock.calls[idx][1]; // $FlowFixMe

    var actualIsNextPending = actualResult.isPendingNext;
    var actualHasNext = actualResult.hasNext;
    var actualHasPrevious = actualResult.hasPrevious;
    expect(actualData).toEqual(expected.data);
    expect(actualIsNextPending).toEqual(expected.isPendingNext);
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
    var _ref3;

    return _ref3 = {}, (0, _defineProperty2["default"])(_ref3, ID_KEY, id), (0, _defineProperty2["default"])(_ref3, FRAGMENTS_KEY, {
      NestedUserFragment: {}
    }), (0, _defineProperty2["default"])(_ref3, FRAGMENT_OWNER_KEY, owner.request), _ref3;
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
    var generated = generateAndCompile("\n        fragment NestedUserFragment on User {\n          username\n        }\n\n        fragment UserFragment on User\n        @refetchable(queryName: \"UserFragmentPaginationQuery\")\n        @argumentDefinitions(\n          isViewerFriendLocal: {type: \"Boolean\", defaultValue: false}\n          orderby: {type: \"[String]\"}\n        ) {\n          id\n          name\n          friends(\n            after: $after,\n            first: $first,\n            before: $before,\n            last: $last,\n            orderby: $orderby,\n            isViewerFriend: $isViewerFriendLocal\n          ) @connection(key: \"UserFragment_friends\") {\n            edges {\n              node {\n                id\n                name\n                ...NestedUserFragment\n              }\n            }\n          }\n        }\n\n        query UserQuery(\n          $id: ID!\n          $after: ID\n          $first: Int\n          $before: ID\n          $last: Int\n          $orderby: [String]\n          $isViewerFriend: Boolean\n        ) {\n          node(id: $id) {\n            actor {\n              ...UserFragment @arguments(isViewerFriendLocal: $isViewerFriend, orderby: $orderby)\n            }\n          }\n        }\n\n        query UserQueryWithoutID(\n          $after: ID\n          $first: Int\n          $before: ID\n          $last: Int\n          $orderby: [String]\n          $isViewerFriend: Boolean\n        ) {\n          viewer {\n            actor {\n              ...UserFragment @arguments(isViewerFriendLocal: $isViewerFriend, orderby: $orderby)\n            }\n          }\n        }\n      ");
    variablesWithoutID = {
      after: null,
      first: 1,
      before: null,
      last: null,
      isViewerFriend: false,
      orderby: ['name']
    };
    variables = (0, _objectSpread2["default"])({}, variablesWithoutID, {
      id: '<feedbackid>'
    });
    gqlQuery = generated.UserQuery;
    gqlQueryWithoutID = generated.UserQueryWithoutID;
    gqlPaginationQuery = generated.UserFragmentPaginationQuery;
    gqlFragment = generated.UserFragment;
    !(((_gqlFragment$metadata = gqlFragment.metadata) === null || _gqlFragment$metadata === void 0 ? void 0 : (_gqlFragment$metadata2 = _gqlFragment$metadata.refetch) === null || _gqlFragment$metadata2 === void 0 ? void 0 : _gqlFragment$metadata2.operation) === '@@MODULE_START@@UserFragmentPaginationQuery.graphql@@MODULE_END@@') ? process.env.NODE_ENV !== "production" ? invariant(false, 'useRefetchableFragment-test: Expected refetchable fragment metadata to contain operation.') : invariant(false) : void 0; // Manually set the refetchable operation for the test.

    gqlFragment.metadata.refetch.operation = gqlPaginationQuery;
    query = createOperationDescriptor(gqlQuery, variables);
    queryWithoutID = createOperationDescriptor(gqlQueryWithoutID, variablesWithoutID);
    paginationQuery = createOperationDescriptor(gqlPaginationQuery, variables);
    environment.commitPayload(query, {
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
    }); // Set up renderers

    Renderer = function Renderer(props) {
      return null;
    };

    var Container = function Container(props) {
      var _props$fragment;

      // We need a render a component to run a Hook
      var _useState2 = useState(props.owner),
          owner = _useState2[0],
          _setOwner = _useState2[1];

      var _useState3 = useState(0),
          _ = _useState3[0],
          _setCount = _useState3[1];

      var fragment = (_props$fragment = props.fragment) !== null && _props$fragment !== void 0 ? _props$fragment : gqlFragment;
      var artificialUserRef = useMemo(function () {
        var _ref, _ref$node;

        var snapshot = environment.lookup(owner.fragment);
        return (_ref = snapshot.data) === null || _ref === void 0 ? void 0 : (_ref$node = _ref.node) === null || _ref$node === void 0 ? void 0 : _ref$node.actor;
      }, [owner]);
      var userRef = props.hasOwnProperty('userRef') ? props.userRef : artificialUserRef;
      setOwner = _setOwner;
      forceUpdate = _setCount;

      var _useBlockingPaginatio2 = useBlockingPaginationFragmentWithSuspenseTransition(fragment,
      /* $FlowFixMe(>=0.108.0 site=www,mobile,react_native_fb,oss) This comment suppresses an error found
       * when Flow v0.108.0 was deployed. To see the error delete this comment
       * and run Flow. */
      userRef),
          userData = _useBlockingPaginatio2.data;

      return React.createElement(Renderer, {
        user: userData
      });
    };

    var ContextProvider = function ContextProvider(_ref4) {
      var children = _ref4.children;

      var _useState4 = useState(environment),
          env = _useState4[0],
          _setEnv = _useState4[1]; // TODO(T39494051) - We set empty variables in relay context to make
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

      var _ref5 = (_args = args) !== null && _args !== void 0 ? _args : {},
          _ref5$isConcurrent = _ref5.isConcurrent,
          isConcurrent = _ref5$isConcurrent === void 0 ? false : _ref5$isConcurrent,
          props = (0, _objectWithoutPropertiesLoose2["default"])(_ref5, ["isConcurrent"]);

      var renderer;
      TestRenderer.act(function () {
        renderer = TestRenderer.create(React.createElement(ErrorBoundary, {
          fallback: function fallback(_ref6) {
            var error = _ref6.error;
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
      // Assert fragment sets isPending to true
      expect(renderSpy).toBeCalledTimes(1);
      assertCall({
        data: expected.data,
        isPendingNext: direction === 'forward',
        hasNext: expected.hasNext,
        hasPrevious: expected.hasPrevious
      }, 0);
      renderSpy.mockClear(); // $FlowFixMe(site=www) batchedUpdats is not part of the public Flow types

      TestRenderer.unstable_batchedUpdates(function () {
        runScheduledCallback();
        jest.runAllImmediates();
      });
      Scheduler.unstable_flushExpired(); // Assert refetch query was fetched

      expectRequestIsInFlight((0, _objectSpread2["default"])({}, expected, {
        inFlight: true,
        requestCount: 1
      }));
    }

    describe('loadNext', function () {
      var direction = 'forward'; // Sanity check test, should already be tested in useBlockingPagination test

      it('loads and renders next items in connection', function () {
        var callback = jest.fn();
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          isPendingNext: false,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
          jest.runAllTimers();
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
          isPendingNext: false,
          hasNext: true,
          hasPrevious: false
        }]);
        expect(callback).toBeCalledTimes(1);
      });
      it('renders pending flag correctly if pagination update is interrupted before it commits (unsuspends)', function () {
        var callback = jest.fn();
        var renderer = renderFragment({
          isConcurrent: true
        });
        expectFragmentResults([{
          data: initialUser,
          isPendingNext: false,
          hasNext: true,
          hasPrevious: false
        }]);
        TestRenderer.act(function () {
          loadNext(1, {
            onComplete: callback
          });
          Scheduler.unstable_flushAll();
          jest.runAllTimers();
        });
        expect(renderer.toJSON()).toEqual(null);
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
          // Assert that isPending flag is still true
          isPendingNext: true,
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
          isPendingNext: false,
          hasNext: true,
          hasPrevious: false
        }]);
        expect(callback).toBeCalledTimes(1);
      });
      it('loads more correctly when original variables do not include an id', function () {
        var _environment$lookup$d;

        var callback = jest.fn();
        var viewer = (_environment$lookup$d = environment.lookup(queryWithoutID.fragment).data) === null || _environment$lookup$d === void 0 ? void 0 : _environment$lookup$d.viewer;
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
          isPendingNext: false,
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
          isPendingNext: false,
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
          isPendingNext: false,
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
          isPendingNext: false,
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

        expectRequestIsInFlight({
          inFlight: true,
          requestCount: 1,
          gqlPaginationQuery: gqlPaginationQuery,
          paginationVariables: paginationVariables
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
          isPendingNext: false,
          hasNext: true,
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

      it('loads more items correctly after refetching', function () {
        var renderer = renderFragment();
        expectFragmentResults([{
          data: initialUser,
          isPendingNext: false,
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
          isPendingNext: false,
          hasNext: true,
          hasPrevious: false
        }, {
          data: expectedUser,
          isPendingNext: false,
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
          data: paginatedUser,
          // Assert pending flag is set back to false
          isPendingNext: false,
          hasNext: true,
          hasPrevious: false
        }]);
      });
    });
  });
});