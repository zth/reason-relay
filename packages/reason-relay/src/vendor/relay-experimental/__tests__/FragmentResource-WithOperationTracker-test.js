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

jest.mock("fbjs/lib/warning");

var _require = require('../FragmentResource'),
    createFragmentResource = _require.createFragmentResource;

var _require2 = require('relay-runtime'),
    createOperationDescriptor = _require2.createOperationDescriptor,
    createReaderSelector = _require2.createReaderSelector;

var RelayOperationTracker = require('relay-runtime/store/RelayOperationTracker');

var _require3 = require('relay-test-utils'),
    MockPayloadGenerator = _require3.MockPayloadGenerator;

var _require4 = require('relay-test-utils-internal'),
    createMockEnvironment = _require4.createMockEnvironment,
    generateAndCompile = _require4.generateAndCompile;

var warning = require("fbjs/lib/warning");

describe('FragmentResource with Operation Tracker and Missing Data', function () {
  var componentName = 'TestComponent';
  var environment;
  var NodeQuery;
  var ViewerFriendsQuery;
  var FriendsPaginationQuery;
  var UserFragment;
  var PlainUserNameRenderer_name;
  var PlainUserNameRenderer_name$normalization;
  var FragmentResource;
  var operationLoader;
  var operationTracker;
  var viewerOperation;
  var nodeOperation;
  beforeEach(function () {
    operationLoader = {
      load: jest.fn(),
      get: jest.fn()
    };
    operationTracker = new RelayOperationTracker();
    environment = createMockEnvironment({
      operationTracker: operationTracker,
      operationLoader: operationLoader
    });
    var compiled = generateAndCompile("\n      query NodeQuery($id: ID!) @relay_test_operation {\n        node(id: $id) {\n          ...UserFragment\n        }\n      }\n\n      query ViewerFriendsQuery @relay_test_operation {\n        viewer {\n          actor {\n            friends(first: 1) @connection(key: \"Viewer_friends\") {\n              edges {\n                node {\n                  ...UserFragment\n                }\n              }\n            }\n          }\n        }\n      }\n\n      query FriendsPaginationQuery($id: ID!) @relay_test_operation {\n        node(id: $id) {\n          ... on User {\n            friends(first: 1) @connection(key: \"Viewer_friends\") {\n              edges {\n                node {\n                  ...UserFragment\n                }\n              }\n            }\n          }\n        }\n      }\n\n      fragment PlainUserNameRenderer_name on PlainUserNameRenderer {\n        plaintext\n        data {\n          text\n        }\n      }\n\n      fragment MarkdownUserNameRenderer_name on MarkdownUserNameRenderer {\n        markdown\n        data {\n          markup\n        }\n      }\n\n      fragment UserFragment on User {\n        id\n        name\n        nameRenderer @match {\n          ...PlainUserNameRenderer_name @module(name: \"PlainUserNameRenderer.react\")\n          ...MarkdownUserNameRenderer_name\n            @module(name: \"MarkdownUserNameRenderer.react\")\n        }\n        plainNameRenderer: nameRenderer @match {\n          ...PlainUserNameRenderer_name @module(name: \"PlainUserNameRenderer.react\")\n        }\n      }\n    ");
    NodeQuery = compiled.NodeQuery;
    ViewerFriendsQuery = compiled.ViewerFriendsQuery;
    FriendsPaginationQuery = compiled.FriendsPaginationQuery;
    PlainUserNameRenderer_name = compiled.PlainUserNameRenderer_name;
    PlainUserNameRenderer_name$normalization = compiled.PlainUserNameRenderer_name$normalization;
    UserFragment = compiled.UserFragment;
    FragmentResource = createFragmentResource(environment);
    viewerOperation = createOperationDescriptor(ViewerFriendsQuery, {});
    nodeOperation = createOperationDescriptor(NodeQuery, {
      id: 'user-id-1'
    });
    environment.execute({
      operation: viewerOperation
    }).subscribe({});
    environment.subscribe(environment.lookup(viewerOperation.fragment), jest.fn()); // This will add data to the store (but not for 3D)

    environment.mock.resolve(viewerOperation, // TODO: (alunyov) T43369419 [relay-testing] Make sure MockPayloadGenerator can generate data for @match
    MockPayloadGenerator.generate(viewerOperation, {
      Actor: function Actor() {
        return {
          id: 'viewer-id'
        };
      },
      User: function User(_, generateId) {
        return {
          id: 'user-id-1'
        };
      }
    })); // We need to subscribe to a fragment in order for OperationTracker
    // to be able to notify owners if they are affected by any pending operation

    environment.subscribe(environment.lookup(createReaderSelector(UserFragment, 'user-id-1', viewerOperation.request.variables, viewerOperation.request)), jest.fn()); // $FlowFixMe

    warning.mockClear();
  });
  it('should warn if data is missing and it is not being fetched by owner or other operations', function () {
    // At this point the viewer query is resolved but, it does not have any 3D data
    // So it should throw a waring for missing data
    var snapshot = FragmentResource.read(PlainUserNameRenderer_name, {
      __id: 'client:user-id-1:nameRenderer(supported:["PlainUserNameRenderer"])',
      __fragments: {
        PlainUserNameRenderer_name: {}
      },
      __fragmentOwner: viewerOperation.request
    }, componentName);
    expect(snapshot.data).toEqual({
      data: undefined,
      plaintext: undefined
    });
    expect(warning).toBeCalled(); // $FlowFixMe

    expect(warning.mock.calls[0][0]).toBe(false); // $FlowFixMe

    expect(warning.mock.calls[0][1]).toMatch(/it has missing data/);
  });
  it('should throw and cache promise for pending operation affecting fragment owner', function () {
    environment.execute({
      operation: nodeOperation
    }).subscribe({});
    operationLoader.load.mockImplementation(function () {
      return Promise.resolve(PlainUserNameRenderer_name$normalization);
    });
    environment.mock.nextValue(nodeOperation, {
      data: {
        node: {
          __typename: 'User',
          id: 'user-id-1',
          name: 'Alice',
          nameRenderer: {
            __typename: 'PlainUserNameRenderer',
            __module_component_UserFragment: 'PlainUserNameRenderer.react',
            __module_operation_UserFragment: 'PlainUserNameRenderer_name$normalization.graphql',
            plaintext: 'Plaintext',
            data: {
              text: 'Data Text'
            }
          },
          plainNameRenderer: {
            __typename: 'PlainUserNameRenderer',
            __module_component_UserFragment: 'PlainUserNameRenderer.react',
            __module_operation_UserFragment: 'PlainUserNameRenderer_name$normalization.graphql',
            plaintext: 'Plaintext',
            data: {
              text: 'Data Text'
            }
          }
        }
      }
    });
    expect(operationLoader.load).toBeCalledTimes(2); // Calling `complete` here will just mark network request as completed, but
    // we still need to process follow-ups with normalization ASTs by resolving
    // the operation loader promise

    environment.mock.complete(nodeOperation);
    var fragmentRef = {
      __id: 'client:user-id-1:nameRenderer(supported:["PlainUserNameRenderer"])',
      __fragments: {
        PlainUserNameRenderer_name: {}
      },
      __fragmentOwner: viewerOperation.request
    };
    var thrown = null;

    try {
      FragmentResource.read(PlainUserNameRenderer_name, fragmentRef, componentName);
    } catch (promise) {
      expect(promise).toBeInstanceOf(Promise);
      thrown = promise;
    }

    expect(thrown).not.toBe(null); // Try reading fragment a second time while affecting operation is pending

    var cached = null;

    try {
      FragmentResource.read(PlainUserNameRenderer_name, fragmentRef, componentName);
    } catch (promise) {
      expect(promise).toBeInstanceOf(Promise);
      cached = promise;
    } // Assert that promise from first read was cached


    expect(cached).toBe(thrown);
  });
  it('should read the data from the store once operation fully completed', function () {
    environment.execute({
      operation: nodeOperation
    }).subscribe({});
    operationLoader.load.mockImplementation(function () {
      return Promise.resolve(PlainUserNameRenderer_name$normalization);
    });
    environment.mock.nextValue(nodeOperation, {
      data: {
        node: {
          __typename: 'User',
          id: 'user-id-1',
          name: 'Alice',
          nameRenderer: {
            __typename: 'PlainUserNameRenderer',
            __module_component_UserFragment: 'PlainUserNameRenderer.react',
            __module_operation_UserFragment: 'PlainUserNameRenderer_name$normalization.graphql',
            plaintext: 'Plaintext',
            data: {
              text: 'Data Text'
            }
          },
          plainNameRenderer: {
            __typename: 'PlainUserNameRenderer',
            __module_component_UserFragment: 'PlainUserNameRenderer.react',
            __module_operation_UserFragment: 'PlainUserNameRenderer_name$normalization.graphql',
            plaintext: 'Plaintext',
            data: {
              text: 'Data Text'
            }
          }
        }
      }
    });
    expect(operationLoader.load).toBeCalledTimes(2);
    environment.mock.complete(nodeOperation); // To make sure promise is resolved

    jest.runAllTimers(); // $FlowFixMe

    warning.mockClear();
    var snapshot = FragmentResource.read(PlainUserNameRenderer_name, {
      __id: 'client:user-id-1:nameRenderer(supported:["PlainUserNameRenderer"])',
      __fragments: {
        PlainUserNameRenderer_name: {}
      },
      __fragmentOwner: viewerOperation.request
    }, componentName);
    expect(warning).not.toBeCalled();
    expect(snapshot.data).toEqual({
      data: {
        text: 'Data Text'
      },
      plaintext: 'Plaintext'
    });
  });
  it('should suspend on pagination query and then read the data', function () {
    var paginationOperation = createOperationDescriptor(FriendsPaginationQuery, {
      id: 'viewer-id'
    });
    environment.execute({
      operation: paginationOperation
    }).subscribe({});
    operationLoader.load.mockImplementation(function () {
      return Promise.resolve(PlainUserNameRenderer_name$normalization);
    });
    environment.mock.nextValue(paginationOperation, {
      data: {
        node: {
          __typename: 'User',
          id: 'viewer-id',
          friends: {
            edges: [{
              node: {
                __typename: 'User',
                id: 'user-id-2',
                name: 'Bob',
                nameRenderer: {
                  __typename: 'PlainUserNameRenderer',
                  __module_component_UserFragment: 'PlainUserNameRenderer.react',
                  __module_operation_UserFragment: 'PlainUserNameRenderer_name$normalization.graphql',
                  plaintext: 'Plaintext 2',
                  data: {
                    text: 'Data Text 2'
                  }
                },
                plainNameRenderer: {
                  __typename: 'PlainUserNameRenderer',
                  __module_component_UserFragment: 'PlainUserNameRenderer.react',
                  __module_operation_UserFragment: 'PlainUserNameRenderer_name$normalization.graphql',
                  plaintext: 'Plaintext 2',
                  data: {
                    text: 'Data Text 2'
                  }
                }
              }
            }]
          }
        }
      }
    });
    expect(operationLoader.load).toBeCalledTimes(2);
    var fragmentRef = {
      __id: 'client:user-id-2:nameRenderer(supported:["PlainUserNameRenderer"])',
      __fragments: {
        PlainUserNameRenderer_name: {}
      },
      __fragmentOwner: viewerOperation.request
    };
    var promiseThrown = false;

    try {
      FragmentResource.read(PlainUserNameRenderer_name, fragmentRef, componentName);
    } catch (promise) {
      expect(promise).toBeInstanceOf(Promise);
      promiseThrown = true;
    }

    expect(promiseThrown).toBe(true); // Complete the request

    environment.mock.complete(paginationOperation); // This should resolve promises

    jest.runAllTimers();
    var snapshot = FragmentResource.read(PlainUserNameRenderer_name, fragmentRef, componentName);
    expect(snapshot.data).toEqual({
      data: {
        text: 'Data Text 2'
      },
      plaintext: 'Plaintext 2'
    });
  });
});