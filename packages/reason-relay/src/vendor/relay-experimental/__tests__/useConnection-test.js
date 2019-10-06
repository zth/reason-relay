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

var React = require('react'); // $FlowFixMe


var ReactTestRenderer = require('react-test-renderer');

var RelayEnvironmentProvider = require('../RelayEnvironmentProvider');

var useConnection = require('../useConnection');

var useFragment = require('../useFragment');

var _require = require('relay-runtime'),
    RelayFeatureFlags = _require.RelayFeatureFlags,
    ConnectionResolver_UNSTABLE = _require.ConnectionResolver_UNSTABLE,
    createOperationDescriptor = _require.createOperationDescriptor,
    Environment = _require.Environment,
    Network = _require.Network,
    Store = _require.Store,
    RecordSource = _require.RecordSource;

var _require2 = require('relay-test-utils-internal'),
    generateAndCompile = _require2.generateAndCompile;

var edgeID1 = 'client:feedback:1:comments(orderby:"date"):__connection_page(first:2,orderby:"date"):edges:0';
var edgeID2 = 'client:feedback:1:comments(orderby:"date"):__connection_page(first:2,orderby:"date"):edges:1';
var examplePayload = {
  node: {
    __typename: 'Feedback',
    id: 'feedback:1',
    comments: {
      count: 42,
      edges: [{
        cursor: 'cursor-1',
        node: {
          __typename: 'Comment',
          id: 'node-1',
          message: {
            text: 'Comment 1'
          }
        }
      }, {
        cursor: 'cursor-2',
        node: {
          __typename: 'Comment',
          id: 'node-2',
          message: {
            text: 'Comment 2'
          }
        }
      }],
      pageInfo: {
        endCursor: 'cursor-2',
        hasNextPage: true,
        hasPreviousPage: null,
        startCursor: 'cursor-1'
      }
    }
  }
};
var enableConnectionResolvers;
var environment;
var fetch;
var query;
var fragment;
var operation;
beforeEach(function () {
  enableConnectionResolvers = RelayFeatureFlags.ENABLE_CONNECTION_RESOLVERS;
  RelayFeatureFlags.ENABLE_CONNECTION_RESOLVERS = true;
  fetch = jest.fn();
  environment = new Environment({
    store: new Store(new RecordSource()),
    network: Network.create(fetch)
  });

  var _generateAndCompile = generateAndCompile("\n    query FeedbackQuery($id: ID!) {\n      node(id: $id) {\n        ...FeedbackFragment\n      }\n    }\n    fragment FeedbackFragment on Feedback @argumentDefinitions(\n      count: {type: \"Int\", defaultValue: 2},\n      cursor: {type: \"ID\"}\n      beforeCount: {type: \"Int\"},\n      beforeCursor: {type: \"ID\"}\n    ) {\n      id\n      comments(\n        after: $cursor\n        before: $beforeCursor\n        first: $count\n        last: $beforeCount\n        orderby: \"date\"\n      ) @connection_resolver(label: \"FeedbackFragment$comments\") {\n        count\n        edges {\n          cursor\n          node {\n            id\n            message { text }\n            ...CommentFragment\n          }\n        }\n        pageInfo {\n          endCursor\n          hasNextPage\n          hasPreviousPage\n          startCursor\n        }\n      }\n    }\n\n    fragment CommentFragment on Comment {\n      id\n    }\n  ");

  query = _generateAndCompile.FeedbackQuery;
  fragment = _generateAndCompile.FeedbackFragment;
  operation = createOperationDescriptor(query, {
    id: 'feedback:1'
  });
  environment.commitPayload(operation, examplePayload);
});
afterEach(function () {
  RelayFeatureFlags.ENABLE_CONNECTION_RESOLVERS = enableConnectionResolvers;
});
test('it returns the initial results of the connection', function () {
  var comments;

  function Component(props) {
    var _ref, _feedback$comments;

    var feedback = useFragment(fragment, props.feedback);
    comments = useConnection(ConnectionResolver_UNSTABLE, feedback.comments);
    return (_ref = feedback === null || feedback === void 0 ? void 0 : (_feedback$comments = feedback.comments) === null || _feedback$comments === void 0 ? void 0 : _feedback$comments.count) !== null && _ref !== void 0 ? _ref : null;
  }

  var queryData = environment.lookup(operation.fragment);
  var renderer = ReactTestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, {
    feedback: queryData.data.node
  })));
  expect(renderer.toJSON()).toEqual('42');
  expect(comments).toEqual({
    edges: [{
      __id: edgeID1,
      cursor: 'cursor-1',
      node: {
        id: 'node-1',
        message: {
          text: 'Comment 1'
        },
        __fragmentOwner: operation.request,
        __fragments: {
          CommentFragment: {}
        },
        __id: 'node-1'
      }
    }, {
      __id: edgeID2,
      cursor: 'cursor-2',
      node: {
        id: 'node-2',
        message: {
          text: 'Comment 2'
        },
        __fragmentOwner: operation.request,
        __fragments: {
          CommentFragment: {}
        },
        __id: 'node-2'
      }
    }],
    pageInfo: {
      endCursor: 'cursor-2',
      hasNextPage: true,
      hasPrevPage: null,
      startCursor: 'cursor-1'
    }
  });
});
test('it does not recompute if rerendered with the same inputs', function () {
  var lookupConnection_UNSTABLE = jest.spyOn(environment.getStore(), 'lookupConnection_UNSTABLE');
  var subscribeConnection_UNSTABLE = jest.spyOn(environment.getStore(), 'subscribeConnection_UNSTABLE');

  function Component(props) {
    var _ref2, _feedback$comments2;

    var feedback = useFragment(fragment, props.feedback);
    useConnection(ConnectionResolver_UNSTABLE, feedback.comments);
    return (_ref2 = feedback === null || feedback === void 0 ? void 0 : (_feedback$comments2 = feedback.comments) === null || _feedback$comments2 === void 0 ? void 0 : _feedback$comments2.count) !== null && _ref2 !== void 0 ? _ref2 : null;
  }

  var queryData = environment.lookup(operation.fragment);
  var renderer = ReactTestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, {
    feedback: queryData.data.node
  })));
  expect(lookupConnection_UNSTABLE).toBeCalledTimes(1);
  ReactTestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(lookupConnection_UNSTABLE).toBeCalledTimes(2);
  lookupConnection_UNSTABLE.mockClear(); // update w identical props: shouldn't re-read the connection

  renderer.update(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, {
    feedback: queryData.data.node
  })));
  ReactTestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(lookupConnection_UNSTABLE).toBeCalledTimes(0);
  expect(subscribeConnection_UNSTABLE).toBeCalledTimes(1);
});
test('it updates when the environment changes', function () {
  var nextEnvironment = new Environment({
    store: new Store(new RecordSource()),
    network: Network.create(fetch)
  });
  nextEnvironment.commitPayload(operation, {
    node: {
      __typename: 'Feedback',
      id: 'feedback:1',
      comments: {
        count: 43,
        edges: [{
          cursor: 'cursor-1',
          node: {
            __typename: 'Comment',
            id: 'node-1',
            message: {
              text: 'Comment 1 new environment'
            }
          }
        }, {
          cursor: 'cursor-2',
          node: {
            __typename: 'Comment',
            id: 'node-2',
            message: {
              text: 'Comment 2 new environment'
            }
          }
        }],
        pageInfo: {
          endCursor: 'cursor-2',
          hasNextPage: true,
          hasPreviousPage: null,
          startCursor: 'cursor-1'
        }
      }
    }
  });
  var comments;

  function Component(props) {
    var _ref3, _feedback$comments3;

    var feedback = useFragment(fragment, props.feedback);
    comments = useConnection(ConnectionResolver_UNSTABLE, feedback.comments);
    return (_ref3 = feedback === null || feedback === void 0 ? void 0 : (_feedback$comments3 = feedback.comments) === null || _feedback$comments3 === void 0 ? void 0 : _feedback$comments3.count) !== null && _ref3 !== void 0 ? _ref3 : null;
  }

  var queryData = environment.lookup(operation.fragment);
  var renderer = ReactTestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, {
    feedback: queryData.data.node
  })));
  ReactTestRenderer.act(function () {
    return jest.runAllImmediates();
  }); // update w identical props except for a new environment: should read from new
  // environment

  renderer.update(React.createElement(RelayEnvironmentProvider, {
    environment: nextEnvironment
  }, React.createElement(Component, {
    feedback: queryData.data.node
  })));
  expect(renderer.toJSON()).toEqual('43');
  expect(comments).toEqual({
    edges: [{
      __id: edgeID1,
      cursor: 'cursor-1',
      node: {
        id: 'node-1',
        message: {
          text: 'Comment 1 new environment'
        },
        __fragmentOwner: operation.request,
        __fragments: {
          CommentFragment: {}
        },
        __id: 'node-1'
      }
    }, {
      __id: edgeID2,
      cursor: 'cursor-2',
      node: {
        id: 'node-2',
        message: {
          text: 'Comment 2 new environment'
        },
        __fragmentOwner: operation.request,
        __fragments: {
          CommentFragment: {}
        },
        __id: 'node-2'
      }
    }],
    pageInfo: {
      endCursor: 'cursor-2',
      hasNextPage: true,
      hasPrevPage: null,
      startCursor: 'cursor-1'
    }
  });
});
test('it updates when the referenced connection changes (ref -> ref)', function () {
  var subscribeConnection_UNSTABLE = jest.spyOn(environment.getStore(), 'subscribeConnection_UNSTABLE');
  var nextOperation = createOperationDescriptor(query, {
    id: 'feedback:2'
  });
  environment.commitPayload(nextOperation, {
    node: {
      __typename: 'Feedback',
      id: 'feedback:2',
      comments: {
        count: 43,
        edges: [{
          cursor: 'cursor-3',
          node: {
            __typename: 'Comment',
            id: 'node-3',
            message: {
              text: 'Comment 3'
            }
          }
        }, {
          cursor: 'cursor-4',
          node: {
            __typename: 'Comment',
            id: 'node-4',
            message: {
              text: 'Comment 4'
            }
          }
        }],
        pageInfo: {
          endCursor: 'cursor-4',
          hasNextPage: true,
          hasPreviousPage: null,
          startCursor: 'cursor-3'
        }
      }
    }
  });
  var comments;

  function Component(props) {
    var _ref4, _feedback$comments4;

    var feedback = useFragment(fragment, props.feedback);
    comments = useConnection(ConnectionResolver_UNSTABLE, feedback.comments);
    return (_ref4 = feedback === null || feedback === void 0 ? void 0 : (_feedback$comments4 = feedback.comments) === null || _feedback$comments4 === void 0 ? void 0 : _feedback$comments4.count) !== null && _ref4 !== void 0 ? _ref4 : null;
  }

  var queryData = environment.lookup(operation.fragment);
  var renderer = ReactTestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, {
    feedback: queryData.data.node
  })));
  ReactTestRenderer.act(function () {
    return jest.runAllImmediates();
  }); // update w a different connection reference, should reread

  var nextQueryData = environment.lookup(nextOperation.fragment);
  renderer.update(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, {
    feedback: nextQueryData.data.node
  })));
  expect(renderer.toJSON()).toEqual('43');
  expect(comments).toEqual({
    edges: [{
      __id: edgeID1.replace('feedback:1', 'feedback:2'),
      cursor: 'cursor-3',
      node: {
        id: 'node-3',
        message: {
          text: 'Comment 3'
        },
        __fragmentOwner: nextOperation.request,
        __fragments: {
          CommentFragment: {}
        },
        __id: 'node-3'
      }
    }, {
      __id: edgeID2.replace('feedback:1', 'feedback:2'),
      cursor: 'cursor-4',
      node: {
        id: 'node-4',
        message: {
          text: 'Comment 4'
        },
        __fragmentOwner: nextOperation.request,
        __fragments: {
          CommentFragment: {}
        },
        __id: 'node-4'
      }
    }],
    pageInfo: {
      endCursor: 'cursor-4',
      hasNextPage: true,
      hasPrevPage: null,
      startCursor: 'cursor-3'
    }
  }); // Commit and fire effects

  ReactTestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(subscribeConnection_UNSTABLE).toBeCalledTimes(2);
});
test('it updates when the referenced connection changes (ref -> null)', function () {
  var subscribeConnection_UNSTABLE = jest.spyOn(environment.getStore(), 'subscribeConnection_UNSTABLE');
  var comments;

  function Component(props) {
    var _ref5, _ref6, _feedback$comments5;

    var feedback = useFragment(fragment, props.feedback);
    comments = useConnection(ConnectionResolver_UNSTABLE, (_ref5 = feedback === null || feedback === void 0 ? void 0 : feedback.comments) !== null && _ref5 !== void 0 ? _ref5 : null);
    return (_ref6 = feedback === null || feedback === void 0 ? void 0 : (_feedback$comments5 = feedback.comments) === null || _feedback$comments5 === void 0 ? void 0 : _feedback$comments5.count) !== null && _ref6 !== void 0 ? _ref6 : null;
  }

  var queryData = environment.lookup(operation.fragment);
  var renderer = ReactTestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, {
    feedback: queryData.data.node
  })));
  ReactTestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(subscribeConnection_UNSTABLE).toBeCalledTimes(1); // update w a different connection reference, should reread

  renderer.update(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, {
    feedback: null
  })));
  expect(renderer.toJSON()).toEqual(null);
  expect(comments).toBe(null); // Commit and fire effects

  ReactTestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(subscribeConnection_UNSTABLE).toBeCalledTimes(1);
});
test('it updates when the referenced connection changes (null -> ref)', function () {
  var subscribeConnection_UNSTABLE = jest.spyOn(environment.getStore(), 'subscribeConnection_UNSTABLE');
  var comments;

  function Component(props) {
    var _ref7, _ref8, _feedback$comments6;

    var feedback = useFragment(fragment, props.feedback);
    comments = useConnection(ConnectionResolver_UNSTABLE, (_ref7 = feedback === null || feedback === void 0 ? void 0 : feedback.comments) !== null && _ref7 !== void 0 ? _ref7 : null);
    return (_ref8 = feedback === null || feedback === void 0 ? void 0 : (_feedback$comments6 = feedback.comments) === null || _feedback$comments6 === void 0 ? void 0 : _feedback$comments6.count) !== null && _ref8 !== void 0 ? _ref8 : null;
  }

  var renderer = ReactTestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, {
    feedback: null
  })));
  ReactTestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(comments).toBe(null);
  expect(subscribeConnection_UNSTABLE).toBeCalledTimes(0); // update w a different connection reference, should reread

  var queryData = environment.lookup(operation.fragment);
  renderer.update(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, {
    feedback: queryData.data.node
  })));
  expect(renderer.toJSON()).toEqual('42');
  expect(comments).toEqual({
    edges: [{
      __id: edgeID1,
      cursor: 'cursor-1',
      node: {
        id: 'node-1',
        message: {
          text: 'Comment 1'
        },
        __fragmentOwner: operation.request,
        __fragments: {
          CommentFragment: {}
        },
        __id: 'node-1'
      }
    }, {
      __id: edgeID2,
      cursor: 'cursor-2',
      node: {
        id: 'node-2',
        message: {
          text: 'Comment 2'
        },
        __fragmentOwner: operation.request,
        __fragments: {
          CommentFragment: {}
        },
        __id: 'node-2'
      }
    }],
    pageInfo: {
      endCursor: 'cursor-2',
      hasNextPage: true,
      hasPrevPage: null,
      startCursor: 'cursor-1'
    }
  }); // Commit and fire effects

  ReactTestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(subscribeConnection_UNSTABLE).toBeCalledTimes(1);
});
test('it updates when the resolver changes', function () {
  var _initialComments$edge;

  var subscribeConnection_UNSTABLE = jest.spyOn(environment.getStore(), 'subscribeConnection_UNSTABLE');
  var comments;

  function Component(props) {
    var _ref9, _feedback$comments7;

    var feedback = useFragment(fragment, props.feedback);
    var resolver = props.resolver;
    comments = useConnection(resolver, feedback.comments);
    return (_ref9 = feedback === null || feedback === void 0 ? void 0 : (_feedback$comments7 = feedback.comments) === null || _feedback$comments7 === void 0 ? void 0 : _feedback$comments7.count) !== null && _ref9 !== void 0 ? _ref9 : null;
  }

  var queryData = environment.lookup(operation.fragment);
  var renderer = ReactTestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, {
    feedback: queryData.data.node,
    resolver: ConnectionResolver_UNSTABLE
  })));
  ReactTestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  var initialComments = comments; // update w a different resolver, should re-read

  var countResolver = {
    initialize: function initialize() {
      return ConnectionResolver_UNSTABLE.initialize();
    },
    reduce: function reduce(state, event) {
      var nextState = ConnectionResolver_UNSTABLE.reduce(state, event);
      return {
        edges: [nextState.edges[0]],
        pageInfo: nextState.pageInfo
      };
    }
  };
  renderer.update(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, {
    feedback: queryData.data.node,
    resolver: countResolver
  })));
  expect(renderer.toJSON()).toEqual('42');
  expect(comments).toEqual((0, _objectSpread2["default"])({}, initialComments, {
    edges: [initialComments === null || initialComments === void 0 ? void 0 : (_initialComments$edge = initialComments.edges) === null || _initialComments$edge === void 0 ? void 0 : _initialComments$edge[0]]
  })); // Commit and fire effects

  ReactTestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(subscribeConnection_UNSTABLE).toBeCalledTimes(2);
});
test('it updates if the connection changes between render and commit', function () {
  var _initialComments$edge2;

  var subscribeConnection_UNSTABLE = jest.spyOn(environment.getStore(), 'subscribeConnection_UNSTABLE');
  var comments;

  function Component(props) {
    var _ref10, _comments, _comments$edges;

    var feedback = useFragment(fragment, props.feedback);
    comments = useConnection(ConnectionResolver_UNSTABLE, feedback.comments);
    return (_ref10 = (_comments = comments) === null || _comments === void 0 ? void 0 : (_comments$edges = _comments.edges) === null || _comments$edges === void 0 ? void 0 : _comments$edges.length) !== null && _ref10 !== void 0 ? _ref10 : null;
  }

  var queryData = environment.lookup(operation.fragment);
  var renderer = ReactTestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, {
    feedback: queryData.data.node
  })));
  expect(renderer.toJSON()).toEqual('2');
  expect(comments).toEqual({
    edges: [{
      __id: edgeID1,
      cursor: 'cursor-1',
      node: {
        id: 'node-1',
        message: {
          text: 'Comment 1'
        },
        __fragmentOwner: operation.request,
        __fragments: {
          CommentFragment: {}
        },
        __id: 'node-1'
      }
    }, {
      __id: edgeID2,
      cursor: 'cursor-2',
      node: {
        id: 'node-2',
        message: {
          text: 'Comment 2'
        },
        __fragmentOwner: operation.request,
        __fragments: {
          CommentFragment: {}
        },
        __id: 'node-2'
      }
    }],
    pageInfo: {
      endCursor: 'cursor-2',
      hasNextPage: true,
      hasPrevPage: null,
      startCursor: 'cursor-1'
    }
  });
  var initialComments = comments; // commit a change *before* the component commits

  environment.commitUpdate(function (storeProxy) {
    storeProxy["delete"](edgeID2);
  });
  ReactTestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(renderer.toJSON()).toEqual('1');
  expect(comments).toEqual({
    edges: [initialComments === null || initialComments === void 0 ? void 0 : (_initialComments$edge2 = initialComments.edges) === null || _initialComments$edge2 === void 0 ? void 0 : _initialComments$edge2[0]],
    pageInfo: initialComments === null || initialComments === void 0 ? void 0 : initialComments.pageInfo
  }); // Commit and fire effects

  ReactTestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(subscribeConnection_UNSTABLE).toBeCalledTimes(1);
});
test('it updates if the connection changes after commit', function () {
  var _initialComments$edge3;

  var subscribeConnection_UNSTABLE = jest.spyOn(environment.getStore(), 'subscribeConnection_UNSTABLE');
  var comments;

  function Component(props) {
    var _ref11, _comments2, _comments2$edges;

    var feedback = useFragment(fragment, props.feedback);
    comments = useConnection(ConnectionResolver_UNSTABLE, feedback.comments);
    return (_ref11 = (_comments2 = comments) === null || _comments2 === void 0 ? void 0 : (_comments2$edges = _comments2.edges) === null || _comments2$edges === void 0 ? void 0 : _comments2$edges.length) !== null && _ref11 !== void 0 ? _ref11 : null;
  }

  var queryData = environment.lookup(operation.fragment);
  var renderer = ReactTestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, {
    feedback: queryData.data.node
  })));
  expect(renderer.toJSON()).toEqual('2');
  expect(comments).toEqual({
    edges: [{
      __id: edgeID1,
      cursor: 'cursor-1',
      node: {
        id: 'node-1',
        message: {
          text: 'Comment 1'
        },
        __fragmentOwner: operation.request,
        __fragments: {
          CommentFragment: {}
        },
        __id: 'node-1'
      }
    }, {
      __id: edgeID2,
      cursor: 'cursor-2',
      node: {
        id: 'node-2',
        message: {
          text: 'Comment 2'
        },
        __fragmentOwner: operation.request,
        __fragments: {
          CommentFragment: {}
        },
        __id: 'node-2'
      }
    }],
    pageInfo: {
      endCursor: 'cursor-2',
      hasNextPage: true,
      hasPrevPage: null,
      startCursor: 'cursor-1'
    }
  });
  var initialComments = comments; // Commit and fire effects

  ReactTestRenderer.act(function () {
    return jest.runAllImmediates();
  }); // commit a change *after* the component commits

  ReactTestRenderer.act(function () {
    environment.commitUpdate(function (storeProxy) {
      storeProxy["delete"](edgeID2);
    });
  });
  expect(renderer.toJSON()).toEqual('1');
  expect(comments).toEqual({
    edges: [initialComments === null || initialComments === void 0 ? void 0 : (_initialComments$edge3 = initialComments.edges) === null || _initialComments$edge3 === void 0 ? void 0 : _initialComments$edge3[0]],
    pageInfo: initialComments === null || initialComments === void 0 ? void 0 : initialComments.pageInfo
  }); // Commit and fire effects

  ReactTestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(subscribeConnection_UNSTABLE).toBeCalledTimes(1);
});
test('it ignores updates if the connection identity has changed', function () {
  var nextOperation = createOperationDescriptor(query, {
    id: 'feedback:2'
  });
  environment.commitPayload(nextOperation, {
    node: {
      __typename: 'Feedback',
      id: 'feedback:2',
      comments: {
        count: 43,
        edges: [{
          cursor: 'cursor-3',
          node: {
            __typename: 'Comment',
            id: 'node-3',
            message: {
              text: 'Comment 3'
            }
          }
        }, {
          cursor: 'cursor-4',
          node: {
            __typename: 'Comment',
            id: 'node-4',
            message: {
              text: 'Comment 4'
            }
          }
        }],
        pageInfo: {
          endCursor: 'cursor-4',
          hasNextPage: true,
          hasPreviousPage: null,
          startCursor: 'cursor-3'
        }
      }
    }
  });
  var subscribeConnection_UNSTABLE = jest.spyOn(environment.getStore(), 'subscribeConnection_UNSTABLE');
  var comments;

  function Component(props) {
    var _ref12, _comments3, _comments3$edges;

    var feedback = useFragment(fragment, props.feedback);
    comments = useConnection(ConnectionResolver_UNSTABLE, feedback.comments);
    return (_ref12 = (_comments3 = comments) === null || _comments3 === void 0 ? void 0 : (_comments3$edges = _comments3.edges) === null || _comments3$edges === void 0 ? void 0 : _comments3$edges.length) !== null && _ref12 !== void 0 ? _ref12 : null;
  }

  var queryData = environment.lookup(operation.fragment);
  var renderer = ReactTestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, {
    feedback: queryData.data.node
  }))); // Commit and fire effects

  ReactTestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  var initialComments = comments; // Rerender with a different connection

  var nextQueryData = environment.lookup(nextOperation.fragment);
  renderer.update(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, {
    feedback: nextQueryData.data.node
  })));
  var updatedComments = comments;
  expect(updatedComments).not.toEqual(initialComments); // commit a change to the original connection *before* running effects
  // for the re-render; the component should still be subscribed for updates
  // on the original connection but ignore them

  ReactTestRenderer.act(function () {
    environment.commitUpdate(function (storeProxy) {
      storeProxy["delete"](edgeID2);
    });
  });
  expect(comments).toBe(updatedComments);
  expect(subscribeConnection_UNSTABLE).toBeCalledTimes(2);
});