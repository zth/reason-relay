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

jest.mock('relay-runtime', function () {
  var originalRuntime = jest.requireActual('relay-runtime');
  var originalInternal = originalRuntime.__internal;
  return (0, _objectSpread2["default"])({}, originalRuntime, {
    __internal: (0, _objectSpread2["default"])({}, originalInternal, {
      getPromiseForRequestInFlight: jest.fn()
    })
  });
});

var _require = require('../FragmentResource'),
    getFragmentResourceForEnvironment = _require.getFragmentResourceForEnvironment;

var _require2 = require('relay-runtime'),
    getPromiseForRequestInFlight = _require2.__internal.getPromiseForRequestInFlight,
    createOperationDescriptor = _require2.createOperationDescriptor,
    getFragment = _require2.getFragment;

describe('FragmentResource', function () {
  var environment;
  var query;
  var queryMissingData;
  var queryPlural;
  var FragmentResource;
  var createMockEnvironment;
  var generateAndCompile;
  var UserQuery;
  var UserFragment;
  var UserQueryMissing;
  var UserFragmentMissing;
  var UsersQuery;
  var UsersFragment;
  var variables = {
    id: '4'
  };
  var pluralVariables = {
    ids: ['4']
  };
  var componentDisplayName = 'TestComponent';
  beforeEach(function () {
    // jest.resetModules();
    var _require3 = require('relay-test-utils-internal');

    createMockEnvironment = _require3.createMockEnvironment;
    generateAndCompile = _require3.generateAndCompile;
    environment = createMockEnvironment();
    FragmentResource = getFragmentResourceForEnvironment(environment);

    var _generateAndCompile = generateAndCompile("\n        fragment UserFragment on User {\n          id\n          name\n        }\n        query UserQuery($id: ID!) {\n          node(id: $id) {\n            __typename\n            ...UserFragment\n          }\n        }\n    ");

    UserQuery = _generateAndCompile.UserQuery;
    UserFragment = _generateAndCompile.UserFragment;

    var _generateAndCompile2 = generateAndCompile("\n        fragment UserFragment on User {\n          id\n          name\n          username\n        }\n        query UserQuery($id: ID!) {\n          node(id: $id) {\n            __typename\n            ...UserFragment\n          }\n        }\n      ");

    UserQueryMissing = _generateAndCompile2.UserQuery;
    UserFragmentMissing = _generateAndCompile2.UserFragment;

    var _generateAndCompile3 = generateAndCompile("\n        fragment UsersFragment on User @relay(plural: true) {\n          id\n          name\n        }\n        query UsersQuery($ids: [ID!]!) {\n          nodes(ids: $ids) {\n            __typename\n            ...UsersFragment\n          }\n        }\n      ");

    UsersQuery = _generateAndCompile3.UsersQuery;
    UsersFragment = _generateAndCompile3.UsersFragment;
    query = createOperationDescriptor(UserQuery, variables);
    queryMissingData = createOperationDescriptor(UserQueryMissing, variables);
    queryPlural = createOperationDescriptor(UsersQuery, pluralVariables);
    environment.commitPayload(query, {
      node: {
        __typename: 'User',
        id: '4',
        name: 'Mark'
      }
    });
  });
  afterEach(function () {
    getPromiseForRequestInFlight.mockReset();
  });
  describe('read', function () {
    it('should read data for the fragment when all data is available', function () {
      var result = FragmentResource.read(getFragment(UserFragment), {
        __id: '4',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(result.data).toEqual({
        id: '4',
        name: 'Mark'
      });
    });
    it('should read data for plural fragment when all data is available', function () {
      var result = FragmentResource.read(getFragment(UsersFragment), [{
        __id: '4',
        __fragments: {
          UsersFragment: {}
        },
        __fragmentOwner: queryPlural.request
      }], componentDisplayName);
      expect(result.data).toEqual([{
        id: '4',
        name: 'Mark'
      }]);
    });
    it('should return empty array for plural fragment when plural field is empty', function () {
      var _generateAndCompile4 = generateAndCompile("\n          fragment UsersFragment on User @relay(plural: true) {\n            id\n          }\n          query UsersQuery($ids: [ID!]!) {\n            nodes(ids: $ids) {\n              __typename\n              ...UsersFragment\n            }\n          }\n        "),
          UsersFragment = _generateAndCompile4.UsersFragment;

      var result = FragmentResource.read(getFragment(UsersFragment), [], componentDisplayName);
      expect(result.data).toEqual([]);
    });
    it('should correctly read fragment data when dataID changes', function () {
      var result = FragmentResource.read(getFragment(UserFragment), {
        __id: '4',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(result.data).toEqual({
        id: '4',
        name: 'Mark'
      });
      environment.commitPayload(query, {
        node: {
          __typename: 'User',
          id: '5',
          name: 'User 5'
        }
      });
      result = FragmentResource.read(getFragment(UserFragment), {
        __id: '5',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(result.data).toEqual({
        id: '5',
        name: 'User 5'
      });
    });
    it('should correctly read fragment data when variables used by fragment change', function () {
      var _generateAndCompile5 = generateAndCompile("\n          fragment UserFragment on Query {\n            node(id: $id) {\n              __typename\n              id\n              name\n            }\n          }\n          query UserQuery($id: ID!) {\n            ...UserFragment\n          }\n        ");

      UserQuery = _generateAndCompile5.UserQuery;
      UserFragment = _generateAndCompile5.UserFragment;
      var prevVars = {
        id: '4'
      };
      query = createOperationDescriptor(UserQuery, prevVars);
      var result = FragmentResource.read(getFragment(UserFragment), {
        __id: 'client:root',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(result.data).toEqual({
        node: {
          __typename: 'User',
          id: '4',
          name: 'Mark'
        }
      });
      var nextVars = {
        id: '5'
      };
      query = createOperationDescriptor(UserQuery, nextVars);
      environment.commitPayload(query, {
        node: {
          __typename: 'User',
          id: '5',
          name: 'User 5'
        }
      });
      result = FragmentResource.read(getFragment(UserFragment), {
        __id: 'client:root',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(result.data).toEqual({
        node: {
          __typename: 'User',
          id: '5',
          name: 'User 5'
        }
      });
    });
    it('should correctly read fragment data when variables used by fragment ' + 'in @argumentDefinitions change', function () {
      var _generateAndCompile6 = generateAndCompile("\n          fragment UserFragment on Query @argumentDefinitions(id: {type: \"ID!\"}) {\n            node(id: $id) {\n              __typename\n              id\n              name\n            }\n          }\n          query UserQuery($id: ID!) {\n            ...UserFragment @arguments(id: $id)\n          }\n        ");

      UserQuery = _generateAndCompile6.UserQuery;
      UserFragment = _generateAndCompile6.UserFragment;
      var prevVars = {
        id: '4'
      };
      query = createOperationDescriptor(UserQuery, prevVars);
      var result = FragmentResource.read(getFragment(UserFragment), {
        __id: 'client:root',
        __fragments: {
          UserFragment: prevVars
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(result.data).toEqual({
        node: {
          __typename: 'User',
          id: '4',
          name: 'Mark'
        }
      });
      var nextVars = {
        id: '5'
      };
      query = createOperationDescriptor(UserQuery, nextVars);
      environment.commitPayload(query, {
        node: {
          __typename: 'User',
          id: '5',
          name: 'User 5'
        }
      });
      result = FragmentResource.read(getFragment(UserFragment), {
        __id: 'client:root',
        __fragments: {
          UserFragment: nextVars
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(result.data).toEqual({
        node: {
          __typename: 'User',
          id: '5',
          name: 'User 5'
        }
      });
    });
    it('should correctly read fragment data when fragment owner variables ' + 'change', function () {
      var _generateAndCompile7 = generateAndCompile("\n          fragment UserFragment on User {\n            id\n            name\n          }\n          query UserQuery($id: ID!, $foo: Boolean!) {\n            node(id: $id) {\n              __typename\n              ...UserFragment\n            }\n          }\n        ");

      UserQuery = _generateAndCompile7.UserQuery;
      UserFragment = _generateAndCompile7.UserFragment;
      var variablesWithFoo = {
        id: '4',
        foo: false
      };
      query = createOperationDescriptor(UserQuery, variablesWithFoo);
      var environmentSpy = jest.spyOn(environment, 'lookup');
      var result = FragmentResource.read(getFragment(UserFragment), {
        __id: '4',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(result.data).toEqual({
        id: '4',
        name: 'Mark'
      });
      var nextVars = (0, _objectSpread2["default"])({}, variablesWithFoo, {
        // Change value of $foo
        foo: true
      });
      query = createOperationDescriptor(UserQuery, nextVars);
      result = FragmentResource.read(getFragment(UserFragment), {
        __id: '4',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(result.data).toEqual({
        id: '4',
        name: 'Mark'
      }); // Even if variable $foo isn't directly used by the fragment, the cache
      // key for the fragment should still change since $foo might affect
      // descendants of this fragment; if we return a cached value, the
      // fragment ref we pass to our children might be stale.

      expect(environmentSpy).toHaveBeenCalledTimes(2);
      environmentSpy.mockRestore();
    });
    it('should return null data if fragment reference is not provided', function () {
      var result = FragmentResource.read(getFragment(UserFragment), null, componentDisplayName);
      expect(result.data).toBe(null);
    });
    it('should throw and cache promise if reading missing data and network request for parent query is in flight', function () {
      getPromiseForRequestInFlight.mockReturnValue(Promise.resolve());
      var fragmentNode = getFragment(UserFragmentMissing);
      var fragmentRef = {
        __id: '4',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: queryMissingData.request
      }; // Try reading a fragment while parent query is in flight

      var thrown = null;

      try {
        FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
      } catch (p) {
        expect(p).toBeInstanceOf(Promise);
        thrown = p;
      } // Assert that promise for request in flight is thrown


      expect(thrown).not.toBe(null); // Try reading a fragment a second time while parent query is in flight

      var cached = null;

      try {
        FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
      } catch (p) {
        expect(p).toBeInstanceOf(Promise);
        cached = p;
      } // Assert that promise from first read was cached


      expect(cached).toBe(thrown);
    });
    it('should raise a warning if data is missing and no pending requests', function () {
      jest.spyOn(console, 'error').mockImplementationOnce(function () {});
      FragmentResource.read(getFragment(UserFragmentMissing), {
        __id: '4',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: queryMissingData.request
      }, componentDisplayName);
      expect(console.error).toHaveBeenCalledTimes(1); // $FlowFixMe

      var warningMessage = console.error.mock.calls[0][0];
      expect(warningMessage.startsWith('Warning: Relay: Tried reading fragment `UserFragment` ' + 'declared in `TestComponent`, but it has ' + 'missing data and its parent query `UserQuery` is not being fetched.')).toEqual(true); // $FlowFixMe

      console.error.mockClear();
    });
  });
  describe('readSpec', function () {
    it('should read data for the fragment when all data is available', function () {
      var result = FragmentResource.readSpec({
        user: getFragment(UserFragment),
        user2: getFragment(UserFragment)
      }, {
        user: {
          __id: '4',
          __fragments: {
            UserFragment: {}
          },
          __fragmentOwner: query.request
        },
        user2: null
      }, componentDisplayName);
      expect(result.user.data).toEqual({
        id: '4',
        name: 'Mark'
      });
      expect(result.user2.data).toEqual(null);
    });
    it('should throw and cache promise if reading missing data and network request for parent query is in flight', function () {
      getPromiseForRequestInFlight.mockReturnValueOnce(Promise.resolve());
      var fragmentNodes = {
        user: getFragment(UserFragmentMissing)
      };
      var fragmentRefs = {
        user: {
          __id: '4',
          __fragments: {
            UserFragment: {}
          },
          __fragmentOwner: queryMissingData.request
        }
      }; // Try reading a fragment while parent query is in flight

      var thrown = null;

      try {
        FragmentResource.readSpec(fragmentNodes, fragmentRefs, componentDisplayName);
      } catch (p) {
        expect(p).toBeInstanceOf(Promise);
        thrown = p;
      } // Assert that promise for request in flight is thrown


      expect(thrown).not.toBe(null); // Try reading a fragment a second time while parent query is in flight

      var cached = null;

      try {
        FragmentResource.readSpec(fragmentNodes, fragmentRefs, componentDisplayName);
      } catch (p) {
        expect(p).toBeInstanceOf(Promise);
        cached = p;
      } // Assert that promise from first read was cached


      expect(cached).toBe(thrown);
    });
  });
  describe('subscribe', function () {
    var callback;
    beforeEach(function () {
      callback = jest.fn();
    });
    it('subscribes to the fragment that was `read`', function () {
      var result = FragmentResource.read(getFragment(UserFragment), {
        __id: '4',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(environment.subscribe).toHaveBeenCalledTimes(0);
      var disposable = FragmentResource.subscribe(result, callback);
      expect(environment.subscribe).toBeCalledTimes(1);
      expect(environment.subscribe.mock.dispose).toBeCalledTimes(0); // Update data

      environment.commitPayload(query, {
        node: {
          __typename: 'User',
          id: '4',
          name: 'Mark Updated'
        }
      }); // Assert that callback gets update

      expect(callback).toBeCalledTimes(1); // Assert that reading the result again will reflect the latest value

      result = FragmentResource.read(getFragment(UserFragment), {
        __id: '4',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(result.data).toEqual({
        id: '4',
        name: 'Mark Updated'
      });
      disposable.dispose();
      expect(environment.subscribe).toBeCalledTimes(1);
      expect(environment.subscribe.mock.dispose).toBeCalledTimes(1);
    });
    it('immediately notifies of data updates that were missed between calling `read` and `subscribe`', function () {
      var result = FragmentResource.read(getFragment(UserFragment), {
        __id: '4',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(environment.subscribe).toHaveBeenCalledTimes(0); // Update data once, before subscribe has been called

      environment.commitPayload(query, {
        node: {
          __typename: 'User',
          id: '4',
          name: 'Mark Updated 1'
        }
      });
      var disposable = FragmentResource.subscribe(result, callback);
      expect(environment.subscribe).toBeCalledTimes(1);
      expect(environment.subscribe.mock.dispose).toBeCalledTimes(0); // Assert that callback was immediately called

      expect(callback).toBeCalledTimes(1); // Assert that reading the result again will reflect the latest value

      result = FragmentResource.read(getFragment(UserFragment), {
        __id: '4',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(result.data).toEqual({
        id: '4',
        name: 'Mark Updated 1'
      }); // Update data again

      environment.commitPayload(query, {
        node: {
          __typename: 'User',
          id: '4',
          name: 'Mark Updated 2'
        }
      }); // Assert that callback gets update

      expect(callback).toBeCalledTimes(2); // Assert that reading the result again will reflect the latest value

      result = FragmentResource.read(getFragment(UserFragment), {
        __id: '4',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(result.data).toEqual({
        id: '4',
        name: 'Mark Updated 2'
      });
      disposable.dispose();
      expect(environment.subscribe).toBeCalledTimes(1);
      expect(environment.subscribe.mock.dispose).toBeCalledTimes(1);
    });
    it('immediately notifies of data updates that were missed between calling `read` and `subscribe` (revert to original value)', function () {
      var result = FragmentResource.read(getFragment(UserFragment), {
        __id: '4',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(environment.subscribe).toHaveBeenCalledTimes(0); // Update data once, before subscribe has been called

      environment.commitPayload(query, {
        node: {
          __typename: 'User',
          id: '4',
          name: 'Mark Updated 1'
        }
      });
      var disposable = FragmentResource.subscribe(result, callback);
      expect(environment.subscribe).toBeCalledTimes(1);
      expect(environment.subscribe.mock.dispose).toBeCalledTimes(0); // Assert that callback was immediately called

      expect(callback).toBeCalledTimes(1); // Assert that reading the result again will reflect the latest value

      result = FragmentResource.read(getFragment(UserFragment), {
        __id: '4',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(result.data).toEqual({
        id: '4',
        name: 'Mark Updated 1'
      }); // Update data again

      environment.commitPayload(query, {
        node: {
          __typename: 'User',
          id: '4',
          name: 'Mark' // original value

        }
      }); // Assert that callback gets update

      expect(callback).toBeCalledTimes(2); // Assert that reading the result again will reflect the latest value

      result = FragmentResource.read(getFragment(UserFragment), {
        __id: '4',
        __fragments: {
          UserFragment: {}
        },
        __fragmentOwner: query.request
      }, componentDisplayName);
      expect(result.data).toEqual({
        id: '4',
        name: 'Mark'
      });
      disposable.dispose();
      expect(environment.subscribe).toBeCalledTimes(1);
      expect(environment.subscribe.mock.dispose).toBeCalledTimes(1);
    });
    it("doesn't subscribe when result was null", function () {
      var result = FragmentResource.read(getFragment(UserFragment), null, componentDisplayName);
      expect(environment.subscribe).toHaveBeenCalledTimes(0);
      var disposable = FragmentResource.subscribe(result, callback);
      expect(environment.subscribe).toBeCalledTimes(0);
      expect(callback).toBeCalledTimes(0);
      disposable.dispose();
      expect(environment.subscribe).toBeCalledTimes(0);
      expect(callback).toBeCalledTimes(0);
    });
    it("doesn't subscribe when result was empty", function () {
      var result = FragmentResource.read(getFragment(UsersFragment), [], componentDisplayName);
      expect(environment.subscribe).toHaveBeenCalledTimes(0);
      var disposable = FragmentResource.subscribe(result, callback);
      expect(environment.subscribe).toBeCalledTimes(0);
      expect(callback).toBeCalledTimes(0);
      disposable.dispose();
      expect(environment.subscribe).toBeCalledTimes(0);
      expect(callback).toBeCalledTimes(0);
    });
    describe('when subscribing multiple times to the same fragment', function () {
      it('maintains subscription even if one of the fragments is disposed of', function () {
        var fragmentNode = getFragment(UserFragment);
        var fragmentRef = {
          __id: '4',
          __fragments: {
            UserFragment: {}
          },
          __fragmentOwner: query.request
        };
        var callback1 = jest.fn();
        var callback2 = jest.fn();
        var result = FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
        expect(environment.subscribe).toHaveBeenCalledTimes(0);
        var disposable1 = FragmentResource.subscribe(result, callback1);
        expect(environment.subscribe).toBeCalledTimes(1);
        expect(environment.subscribe.mock.dispose).toBeCalledTimes(0);
        var disposable2 = FragmentResource.subscribe(result, callback2);
        expect(environment.subscribe).toBeCalledTimes(2);
        expect(environment.subscribe.mock.dispose).toBeCalledTimes(0); // Update data once

        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '4',
            name: 'Mark Update 1'
          }
        }); // Assert that both callbacks receive update

        expect(callback1).toBeCalledTimes(1);
        expect(callback2).toBeCalledTimes(1); // Assert that reading the result again will reflect the latest value

        result = FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
        expect(result.data).toEqual({
          id: '4',
          name: 'Mark Update 1'
        }); // Unsubscribe the second listener

        disposable2.dispose();
        expect(environment.subscribe).toBeCalledTimes(2);
        expect(environment.subscribe.mock.dispose).toBeCalledTimes(1); // Update data again

        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '4',
            name: 'Mark Update 2'
          }
        }); // Assert that subscription that hasn't been disposed receives update

        expect(callback1).toBeCalledTimes(2); // Assert that subscription that was already disposed isn't called again

        expect(callback2).toBeCalledTimes(1); // Assert that reading the result again will reflect the latest value

        result = FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
        expect(result.data).toEqual({
          id: '4',
          name: 'Mark Update 2'
        });
        disposable1.dispose();
        expect(environment.subscribe).toBeCalledTimes(2);
      });
    });
    describe('when subscribing to plural fragment', function () {
      it('subscribes to the plural fragment that was `read`', function () {
        var fragmentNode = getFragment(UsersFragment);
        var fragmentRef = [{
          __id: '4',
          __fragments: {
            UsersFragment: {}
          },
          __fragmentOwner: queryPlural.request
        }];
        var result = FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
        expect(environment.subscribe).toHaveBeenCalledTimes(0);
        var disposable = FragmentResource.subscribe(result, callback);
        expect(environment.subscribe).toBeCalledTimes(1);
        expect(environment.subscribe.mock.dispose).toBeCalledTimes(0); // Update data

        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '4',
            name: 'Mark Updated'
          }
        }); // Assert that callback gets update

        expect(callback).toBeCalledTimes(1); // Assert that reading the result again will reflect the latest value

        result = FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
        expect(result.data).toEqual([{
          id: '4',
          name: 'Mark Updated'
        }]);
        disposable.dispose();
        expect(environment.subscribe).toBeCalledTimes(1);
        expect(environment.subscribe.mock.dispose).toBeCalledTimes(1);
      });
      it('immediately notifies of data updates that were missed between calling `read` and `subscribe`', function () {
        var fragmentNode = getFragment(UsersFragment);
        var fragmentRef = [{
          __id: '4',
          __fragments: {
            UsersFragment: {}
          },
          __fragmentOwner: queryPlural.request
        }];
        var result = FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
        expect(environment.subscribe).toHaveBeenCalledTimes(0); // Update data once, before subscribe has been called

        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '4',
            name: 'Mark Updated 1'
          }
        });
        var disposable = FragmentResource.subscribe(result, callback);
        expect(environment.subscribe).toBeCalledTimes(1);
        expect(environment.subscribe.mock.dispose).toBeCalledTimes(0); // Assert that callback was immediately called

        expect(callback).toBeCalledTimes(1); // Assert that reading the result again will reflect the latest value

        result = FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
        expect(result.data).toEqual([{
          id: '4',
          name: 'Mark Updated 1'
        }]); // Update data again

        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '4',
            name: 'Mark Updated 2'
          }
        }); // Assert that callback gets update

        expect(callback).toBeCalledTimes(2); // Assert that reading the result again will reflect the latest value

        result = FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
        expect(result.data).toEqual([{
          id: '4',
          name: 'Mark Updated 2'
        }]);
        disposable.dispose();
        expect(environment.subscribe).toBeCalledTimes(1);
        expect(environment.subscribe.mock.dispose).toBeCalledTimes(1);
      });
      it('correctly subscribes to a plural fragment with multiple records', function () {
        queryPlural = createOperationDescriptor(UsersQuery, {
          ids: ['4', '5']
        });
        environment.commitPayload(queryPlural, {
          nodes: [{
            __typename: 'User',
            id: '4',
            name: 'Mark'
          }, {
            __typename: 'User',
            id: '5',
            name: 'User 5'
          }]
        });
        var fragmentNode = getFragment(UsersFragment);
        var fragmentRef = [{
          __id: '4',
          __fragments: {
            UsersFragment: {}
          },
          __fragmentOwner: queryPlural.request
        }, {
          __id: '5',
          __fragments: {
            UsersFragment: {}
          },
          __fragmentOwner: queryPlural.request
        }];
        var result = FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
        expect(environment.subscribe).toHaveBeenCalledTimes(0);
        var disposable = FragmentResource.subscribe(result, callback);
        expect(environment.subscribe).toBeCalledTimes(2);
        expect(environment.subscribe.mock.dispose).toBeCalledTimes(0); // Update data

        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '4',
            name: 'Mark Updated'
          }
        }); // Assert that callback gets update

        expect(callback).toBeCalledTimes(1); // Assert that reading the result again will reflect the latest value

        result = FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
        expect(result.data).toEqual([{
          id: '4',
          name: 'Mark Updated'
        }, {
          id: '5',
          name: 'User 5'
        }]);
        disposable.dispose();
        expect(environment.subscribe).toBeCalledTimes(2);
        expect(environment.subscribe.mock.dispose).toBeCalledTimes(1);
      });
      it('immediately notifies of data updates that were missed between calling `read` and `subscribe` when subscribing to multiple records', function () {
        queryPlural = createOperationDescriptor(UsersQuery, {
          ids: ['4', '5']
        });
        environment.commitPayload(queryPlural, {
          nodes: [{
            __typename: 'User',
            id: '4',
            name: 'Mark'
          }, {
            __typename: 'User',
            id: '5',
            name: 'User 5'
          }, {
            __typename: 'User',
            id: '6',
            name: 'User 6'
          }]
        });
        var fragmentNode = getFragment(UsersFragment);
        var fragmentRef = [{
          __id: '4',
          __fragments: {
            UsersFragment: {}
          },
          __fragmentOwner: queryPlural.request
        }, {
          __id: '5',
          __fragments: {
            UsersFragment: {}
          },
          __fragmentOwner: queryPlural.request
        }, {
          __id: '6',
          __fragments: {
            UsersFragment: {}
          },
          __fragmentOwner: queryPlural.request
        }];
        var result = FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
        expect(environment.subscribe).toHaveBeenCalledTimes(0); // Update data once, before subscribe has been called

        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '4',
            name: 'Mark Updated 1'
          }
        });
        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '6',
            name: 'User 6 Updated'
          }
        });
        var disposable = FragmentResource.subscribe(result, callback);
        expect(environment.subscribe).toBeCalledTimes(3);
        expect(environment.subscribe.mock.dispose).toBeCalledTimes(0); // Assert that callback was immediately called

        expect(callback).toBeCalledTimes(1); // Assert that reading the result again will reflect the latest value

        result = FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
        expect(result.data).toEqual([{
          id: '4',
          name: 'Mark Updated 1'
        }, {
          id: '5',
          name: 'User 5'
        }, {
          id: '6',
          name: 'User 6 Updated'
        }]); // Update data again

        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '4',
            name: 'Mark Updated 2'
          }
        }); // Assert that callback gets update

        expect(callback).toBeCalledTimes(2); // Assert that reading the result again will reflect the latest value

        result = FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
        expect(result.data).toEqual([{
          id: '4',
          name: 'Mark Updated 2'
        }, {
          id: '5',
          name: 'User 5'
        }, {
          id: '6',
          name: 'User 6 Updated'
        }]);
        disposable.dispose();
        expect(environment.subscribe).toBeCalledTimes(3);
        expect(environment.subscribe.mock.dispose).toBeCalledTimes(1);
      });
      it('immediately notifies of data updates that were missed between calling `read` and `subscribe` when subscribing to multiple records (revert to original)', function () {
        queryPlural = createOperationDescriptor(UsersQuery, {
          ids: ['4']
        });
        environment.commitPayload(queryPlural, {
          nodes: [{
            __typename: 'User',
            id: '4',
            name: 'Mark'
          }]
        });
        var fragmentNode = getFragment(UsersFragment);
        var fragmentRef = [{
          __id: '4',
          __fragments: {
            UsersFragment: {}
          },
          __fragmentOwner: queryPlural.request
        }];
        var result = FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
        expect(environment.subscribe).toHaveBeenCalledTimes(0); // Update data once, before subscribe has been called

        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '4',
            name: 'Mark Updated 1'
          }
        });
        var disposable = FragmentResource.subscribe(result, callback);
        expect(environment.subscribe).toBeCalledTimes(1);
        expect(environment.subscribe.mock.dispose).toBeCalledTimes(0); // Assert that callback was immediately called

        expect(callback).toBeCalledTimes(1); // Assert that reading the result again will reflect the latest value

        result = FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
        expect(result.data).toEqual([{
          id: '4',
          name: 'Mark Updated 1'
        }]); // Update data again

        environment.commitPayload(query, {
          node: {
            __typename: 'User',
            id: '4',
            name: 'Mark' // revert to original

          }
        }); // Assert that callback gets update

        expect(callback).toBeCalledTimes(2); // Assert that reading the result again will reflect the latest value

        result = FragmentResource.read(fragmentNode, fragmentRef, componentDisplayName);
        expect(result.data).toEqual([{
          id: '4',
          name: 'Mark'
        }]);
        disposable.dispose();
        expect(environment.subscribe).toBeCalledTimes(1);
        expect(environment.subscribe.mock.dispose).toBeCalledTimes(1);
      });
    });
  });
  describe('subscribeSpec', function () {
    var unsubscribe;
    var callback;
    beforeEach(function () {
      unsubscribe = jest.fn();
      callback = jest.fn();
      jest.spyOn(environment, 'subscribe').mockImplementation(function () {
        return {
          dispose: unsubscribe
        };
      });
    });
    it('subscribes to the fragment spec that was `read`', function () {
      var result = FragmentResource.readSpec({
        user: getFragment(UserFragment)
      }, {
        user: {
          __id: '4',
          __fragments: {
            UserFragment: {}
          },
          __fragmentOwner: query.request
        }
      }, componentDisplayName);
      expect(environment.subscribe).toHaveBeenCalledTimes(0);
      var disposable = FragmentResource.subscribeSpec(result, callback);
      expect(unsubscribe).toBeCalledTimes(0);
      expect(environment.subscribe).toBeCalledTimes(1);
      disposable.dispose();
      expect(unsubscribe).toBeCalledTimes(1);
      expect(environment.subscribe).toBeCalledTimes(1);
    });
  });
});