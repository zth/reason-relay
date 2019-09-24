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

var _require = require('../QueryResource'),
    getQueryResourceForEnvironment = _require.getQueryResourceForEnvironment;

var _require2 = require('relay-runtime'),
    Observable = _require2.Observable,
    ROOT_ID = _require2.ROOT_ID,
    fetchQuery = _require2.__internal.fetchQuery,
    createOperationDescriptor = _require2.createOperationDescriptor;

var _require3 = require('relay-test-utils-internal'),
    createMockEnvironment = _require3.createMockEnvironment,
    generateAndCompile = _require3.generateAndCompile;

describe('QueryResource', function () {
  var environment;
  var QueryResource;
  var fetchPolicy;
  var fetchObservable;
  var fetchObservableMissingData;
  var gqlQuery;
  var query;
  var queryMissingData;
  var gqlQueryMissingData;
  var release;
  var renderPolicy;
  var variables = {
    id: '4'
  };
  beforeEach(function () {
    jest.mock('fbjs/lib/ExecutionEnvironment', function () {
      return {
        canUseDOM: function canUseDOM() {
          return true;
        }
      };
    });
    environment = createMockEnvironment();
    QueryResource = getQueryResourceForEnvironment(environment);
    gqlQuery = generateAndCompile("query UserQuery($id: ID!) {\n        node(id: $id) {\n          ... on User {\n            id\n          }\n        }\n      }\n    ").UserQuery;
    gqlQueryMissingData = generateAndCompile("query UserQuery($id: ID!) {\n        node(id: $id) {\n          ... on User {\n            id\n            name\n          }\n        }\n      }\n    ").UserQuery;
    query = createOperationDescriptor(gqlQuery, variables);
    queryMissingData = createOperationDescriptor(gqlQueryMissingData, variables);
    environment.commitPayload(query, {
      node: {
        __typename: 'User',
        id: '4'
      }
    });
    fetchObservable = fetchQuery(environment, query, {
      networkCacheConfig: {
        force: true
      }
    });
    fetchObservableMissingData = fetchQuery(environment, queryMissingData, {
      networkCacheConfig: {
        force: true
      }
    });
    release = jest.fn();
    environment.retain.mockImplementation(function () {
      return {
        dispose: release
      };
    });
    renderPolicy = 'partial';
  });
  describe('prepare', function () {
    describe('fetchPolicy: store-or-network', function () {
      beforeEach(function () {
        fetchPolicy = 'store-or-network';
      });
      describe('renderPolicy: partial', function () {
        beforeEach(function () {
          renderPolicy = 'partial';
        });
        it('should return result and not send a network request if all data is locally available', function () {
          expect(environment.check(query.root)).toEqual(true);
          var result = QueryResource.prepare(query, fetchObservable, fetchPolicy, renderPolicy);
          expect(result).toEqual({
            cacheKey: expect.any(String),
            fragmentNode: query.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: query.request
            },
            operation: query
          });
          expect(environment.execute).toBeCalledTimes(0);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should return result and send a network request if data is missing for the query', function () {
          expect(environment.check(queryMissingData.root)).toEqual(false);
          var result = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
          expect(result).toEqual({
            cacheKey: expect.any(String),
            fragmentNode: queryMissingData.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: queryMissingData.request
            },
            operation: queryMissingData
          });
          expect(environment.execute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should send a single network request when same query is read multiple times', function () {
          var result1 = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy); // Assert query is temporarily retained during call to prepare

          expect(release).toBeCalledTimes(0);
          expect(environment.retain).toBeCalledTimes(1);
          var result2 = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy); // Assert query is still temporarily retained during second call to prepare

          expect(release).toBeCalledTimes(0);
          expect(environment.retain).toBeCalledTimes(1);
          var expected = {
            cacheKey: expect.any(String),
            fragmentNode: queryMissingData.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: queryMissingData.request
            },
            operation: queryMissingData
          };
          expect(result1).toEqual(expected);
          expect(result2).toEqual(expected);
          expect(environment.execute).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should throw error if network request errors', function () {
          var thrown = false;
          var sink;
          var networkExecute = jest.fn();
          var errorFetchObservable = Observable.create(function (s) {
            networkExecute();
            sink = s;
          });
          QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);

          if (!sink) {
            throw new Error('Expect sink to be defined');
          }

          try {
            sink.error(new Error('Oops'));
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);
          } catch (e) {
            expect(e instanceof Error).toEqual(true);
            expect(e.message).toEqual('Oops');
            thrown = true;
          }

          expect(thrown).toEqual(true);
          expect(networkExecute).toBeCalledTimes(1); // Assert query is temporarily retained during call to prepare

          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should return result and send a network request if data is missing for the query and observable returns synchronously', function () {
          expect(environment.check(queryMissingData.root)).toEqual(false);
          var networkExecute = jest.fn();
          var syncFetchObservable = Observable.create(function (sink) {
            environment.commitPayload(queryMissingData, {
              node: {
                __typename: 'User',
                id: '4',
                name: 'User 4'
              }
            });
            var snapshot = environment.lookup(queryMissingData.fragment);
            networkExecute();
            sink.next(snapshot);
            sink.complete();
          });
          var result = QueryResource.prepare(queryMissingData, syncFetchObservable, fetchPolicy, renderPolicy);
          expect(result).toEqual({
            cacheKey: expect.any(String),
            fragmentNode: queryMissingData.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: queryMissingData.request
            },
            operation: queryMissingData
          });
          expect(networkExecute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should throw error if network request errors synchronously', function () {
          var thrown = false;
          var networkExecute = jest.fn();
          var errorFetchObservable = Observable.create(function (sink) {
            networkExecute();
            sink.error(new Error('Oops'));
          });

          try {
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);
          } catch (e) {
            expect(e instanceof Error).toEqual(true);
            expect(e.message).toEqual('Oops');
            thrown = true;
          }

          expect(thrown).toEqual(true);
          expect(networkExecute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        describe('when using fragments', function () {
          it('should return result and not send a network request if all data is locally available', function () {
            var _generateAndCompile = generateAndCompile("\n              fragment UserFragment on User {\n                id\n              }\n              query UserQuery($id: ID!) {\n                node(id: $id) {\n                  __typename\n                  ...UserFragment\n                }\n              }\n            "),
                UserQuery = _generateAndCompile.UserQuery;

            var queryWithFragments = createOperationDescriptor(UserQuery, variables);
            var fetchObservableWithFragments = fetchQuery(environment, queryWithFragments, {
              networkCacheConfig: {
                force: true
              }
            });
            expect(environment.check(queryWithFragments.root)).toEqual(true);
            var result = QueryResource.prepare(queryWithFragments, fetchObservableWithFragments, fetchPolicy, renderPolicy);
            expect(result).toEqual({
              cacheKey: expect.any(String),
              fragmentNode: queryWithFragments.fragment.node,
              fragmentRef: {
                __id: ROOT_ID,
                __fragments: {
                  UserQuery: variables
                },
                __fragmentOwner: queryWithFragments.request
              },
              operation: queryWithFragments
            });
            expect(environment.execute).toBeCalledTimes(0);
            expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
            // calling QueryResource.retain

            jest.runAllTimers();
            expect(release).toBeCalledTimes(1);
          });
          it('should return result and send a network request when some data is missing in fragment', function () {
            var _generateAndCompile2 = generateAndCompile("\n                fragment UserFragment on User {\n                  id\n                  username\n                }\n                query UserQuery($id: ID!) {\n                  node(id: $id) {\n                    __typename\n                    ...UserFragment\n                  }\n                }\n              "),
                UserQuery = _generateAndCompile2.UserQuery;

            var queryWithFragments = createOperationDescriptor(UserQuery, variables);
            var fetchObservableWithFragments = fetchQuery(environment, queryWithFragments, {
              networkCacheConfig: {
                force: true
              }
            });
            expect(environment.check(queryWithFragments.root)).toEqual(false);
            var result = QueryResource.prepare(queryWithFragments, fetchObservableWithFragments, fetchPolicy, renderPolicy);
            expect(result).toEqual({
              cacheKey: expect.any(String),
              fragmentNode: queryWithFragments.fragment.node,
              fragmentRef: {
                __id: ROOT_ID,
                __fragments: {
                  UserQuery: variables
                },
                __fragmentOwner: queryWithFragments.request
              },
              operation: queryWithFragments
            });
            expect(environment.execute).toBeCalledTimes(1);
            expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
            // calling QueryResource.retain

            jest.runAllTimers();
            expect(release).toBeCalledTimes(1);
          });
        });
      });
      describe('renderPolicy: full', function () {
        beforeEach(function () {
          renderPolicy = 'full';
        });
        it('should return result and not send a network request if all data is locally available', function () {
          expect(environment.check(query.root)).toEqual(true);
          var result = QueryResource.prepare(query, fetchObservable, fetchPolicy, renderPolicy);
          expect(result).toEqual({
            cacheKey: expect.any(String),
            fragmentNode: query.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: query.request
            },
            operation: query
          });
          expect(environment.execute).toBeCalledTimes(0);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should suspend and send a network request if data is missing for the query', function () {
          expect(environment.check(queryMissingData.root)).toEqual(false);
          var thrown = false;

          try {
            QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
          } catch (promise) {
            expect(typeof promise.then).toBe('function');
            thrown = true;
          }

          expect(environment.execute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1);
          expect(thrown).toBe(true); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should cache a single promise and send a single network request when same query is read multiple times', function () {
          var promise1;

          try {
            QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
          } catch (p) {
            expect(typeof p.then).toBe('function');
            promise1 = p;
          } // Assert query is temporarily retained during call to prepare


          expect(release).toBeCalledTimes(0);
          expect(environment.retain).toBeCalledTimes(1);
          var promise2;

          try {
            QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
          } catch (p) {
            expect(typeof p.then).toBe('function');
            promise2 = p;
          } // Assert query is still temporarily retained during second call to prepare


          expect(release).toBeCalledTimes(0);
          expect(environment.retain).toBeCalledTimes(1); // Assert that same promise was thrown

          expect(promise1).toBe(promise2); // Assert that network was only called once

          expect(environment.execute).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should throw error if network request errors', function () {
          var thrown = false;
          var sink;
          var networkExecute = jest.fn();
          var errorFetchObservable = Observable.create(function (s) {
            networkExecute();
            sink = s;
          });

          try {
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);
          } catch (p) {
            expect(typeof p.then).toBe('function');
          }

          if (!sink) {
            throw new Error('Expect sink to be defined');
          }

          try {
            sink.error(new Error('Oops'));
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);
          } catch (e) {
            expect(e instanceof Error).toEqual(true);
            expect(e.message).toEqual('Oops');
            thrown = true;
          }

          expect(thrown).toEqual(true);
          expect(networkExecute).toBeCalledTimes(1); // Assert query is temporarily retained during call to prepare

          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should return result and send a network request if data is missing for the query and observable returns synchronously', function () {
          expect(environment.check(queryMissingData.root)).toEqual(false);
          var networkExecute = jest.fn();
          var syncFetchObservable = Observable.create(function (sink) {
            environment.commitPayload(queryMissingData, {
              node: {
                __typename: 'User',
                id: '4',
                name: 'User 4'
              }
            });
            var snapshot = environment.lookup(queryMissingData.fragment);
            networkExecute();
            sink.next(snapshot);
            sink.complete();
          });
          var result = QueryResource.prepare(queryMissingData, syncFetchObservable, fetchPolicy, renderPolicy);
          expect(result).toEqual({
            cacheKey: expect.any(String),
            fragmentNode: queryMissingData.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: queryMissingData.request
            },
            operation: queryMissingData
          });
          expect(networkExecute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should throw error if network request errors synchronously', function () {
          var thrown = false;
          var networkExecute = jest.fn();
          var errorFetchObservable = Observable.create(function (sink) {
            networkExecute();
            sink.error(new Error('Oops'));
          });

          try {
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);
          } catch (e) {
            expect(e instanceof Error).toEqual(true);
            expect(e.message).toEqual('Oops');
            thrown = true;
          }

          expect(thrown).toEqual(true);
          expect(networkExecute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should discard old promise cache when query observable is unsubscribed, and create a new promise on a new request', function () {
          var promise1;
          var subscription;

          try {
            QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy, {
              start: function start(sub) {
                subscription = sub;
              }
            });
          } catch (p) {
            expect(typeof p.then).toBe('function');
            promise1 = p;
          }

          subscription && subscription.unsubscribe(); // Assert cache is cleared

          expect(QueryResource.getCacheEntry(queryMissingData, fetchPolicy, renderPolicy)).toBeUndefined();
          var promise2;

          try {
            QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
          } catch (p) {
            expect(typeof p.then).toBe('function');
            promise2 = p;
          } // Assert that different promises were thrown


          expect(promise1).not.toBe(promise2);
          expect(environment.execute).toBeCalledTimes(2);
        });
        describe('when using fragments', function () {
          it('should return result and not send a network request if all data is locally available', function () {
            var _generateAndCompile3 = generateAndCompile("\n              fragment UserFragment on User {\n                id\n              }\n              query UserQuery($id: ID!) {\n                node(id: $id) {\n                  __typename\n                  ...UserFragment\n                }\n              }\n            "),
                UserQuery = _generateAndCompile3.UserQuery;

            var queryWithFragments = createOperationDescriptor(UserQuery, variables);
            var fetchObservableWithFragments = fetchQuery(environment, queryWithFragments, {
              networkCacheConfig: {
                force: true
              }
            });
            expect(environment.check(queryWithFragments.root)).toEqual(true);
            var result = QueryResource.prepare(queryWithFragments, fetchObservableWithFragments, fetchPolicy, renderPolicy);
            expect(result).toEqual({
              cacheKey: expect.any(String),
              fragmentNode: queryWithFragments.fragment.node,
              fragmentRef: {
                __id: ROOT_ID,
                __fragments: {
                  UserQuery: variables
                },
                __fragmentOwner: queryWithFragments.request
              },
              operation: queryWithFragments
            });
            expect(environment.execute).toBeCalledTimes(0);
            expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
            // calling QueryResource.retain

            jest.runAllTimers();
            expect(release).toBeCalledTimes(1);
          });
          it('should suspend and send a network request when some data is missing in fragment', function () {
            var _generateAndCompile4 = generateAndCompile("\n                fragment UserFragment on User {\n                  id\n                  username\n                }\n                query UserQuery($id: ID!) {\n                  node(id: $id) {\n                    __typename\n                    ...UserFragment\n                  }\n                }\n              "),
                UserQuery = _generateAndCompile4.UserQuery;

            var queryWithFragments = createOperationDescriptor(UserQuery, variables);
            var fetchObservableWithFragments = fetchQuery(environment, queryWithFragments, {
              networkCacheConfig: {
                force: true
              }
            });
            expect(environment.check(queryWithFragments.root)).toEqual(false);
            var thrown = false;

            try {
              QueryResource.prepare(queryWithFragments, fetchObservableWithFragments, fetchPolicy, renderPolicy);
            } catch (p) {
              expect(typeof p.then).toBe('function');
              thrown = true;
            }

            expect(environment.execute).toBeCalledTimes(1);
            expect(environment.retain).toBeCalledTimes(1);
            expect(thrown).toEqual(true); // Assert that query is released after enough time has passed without
            // calling QueryResource.retain

            jest.runAllTimers();
            expect(release).toBeCalledTimes(1);
          });
        });
        describe('when using incremental data', function () {
          it('should suspend and send a network request when some data is missing in fragment', function () {
            var _generateAndCompile5 = generateAndCompile("\n                fragment UserFragment on User {\n                  id\n                  username\n                }\n                query UserQuery($id: ID!) {\n                  node(id: $id) {\n                    __typename\n                    id\n                    ...UserFragment @defer\n                  }\n                }\n              "),
                UserQuery = _generateAndCompile5.UserQuery;

            var queryWithFragments = createOperationDescriptor(UserQuery, variables);
            var fetchObservableWithFragments = fetchQuery(environment, queryWithFragments, {
              networkCacheConfig: {
                force: true
              }
            });
            expect(environment.check(queryWithFragments.root)).toEqual(false); // Should suspend until first payload is received

            var thrown = false;

            try {
              QueryResource.prepare(queryWithFragments, fetchObservableWithFragments, fetchPolicy, renderPolicy);
            } catch (p) {
              expect(typeof p.then).toBe('function');
              thrown = true;
            }

            expect(environment.execute).toBeCalledTimes(1);
            expect(environment.retain).toBeCalledTimes(1);
            expect(thrown).toEqual(true); // Resolve first payload

            environment.mock.nextValue(queryWithFragments, {
              data: {
                node: {
                  id: '4',
                  __typename: 'User'
                }
              }
            }); // Data should still be missing after first payload

            expect(environment.check(queryWithFragments.root)).toEqual(false); // Calling prepare again shouldn't suspend; the fragment with
            // the deferred data would suspend further down the tree

            var result = QueryResource.prepare(queryWithFragments, fetchObservableWithFragments, fetchPolicy, renderPolicy);
            var expectedResult = {
              cacheKey: expect.any(String),
              fragmentNode: queryWithFragments.fragment.node,
              fragmentRef: {
                __id: ROOT_ID,
                __fragments: {
                  UserQuery: variables
                },
                __fragmentOwner: queryWithFragments.request
              },
              operation: queryWithFragments
            };
            expect(result).toEqual(expectedResult);
            expect(environment.execute).toBeCalledTimes(1);
            expect(environment.retain).toBeCalledTimes(1);
            expect(thrown).toEqual(true); // Resolve deferred payload

            environment.mock.nextValue(queryWithFragments, {
              data: {
                id: '1',
                __typename: 'User',
                username: 'zuck'
              },
              label: 'UserQuery$defer$UserFragment',
              path: ['node']
            }); // Data should not be missing anymore

            expect(environment.check(queryWithFragments.root)).toEqual(true); // Calling prepare again should return same result

            var result2 = QueryResource.prepare(queryWithFragments, fetchObservableWithFragments, fetchPolicy, renderPolicy);
            expect(result2).toEqual(expectedResult); // Assert that query is released after enough time has passed without
            // calling QueryResource.retain

            jest.runAllTimers();
            expect(release).toBeCalledTimes(1);
          });
        });
      });
    });
    describe('fetchPolicy: store-and-network', function () {
      beforeEach(function () {
        fetchPolicy = 'store-and-network';
      });
      describe('renderPolicy: partial', function () {
        beforeEach(function () {
          renderPolicy = 'partial';
        });
        it('should return result and send a network request even when data is locally available', function () {
          expect(environment.check(query.root)).toEqual(true);
          var result = QueryResource.prepare(query, fetchObservable, fetchPolicy, renderPolicy);
          expect(result).toEqual({
            cacheKey: expect.any(String),
            fragmentNode: query.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: query.request
            },
            operation: query
          });
          expect(environment.execute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should return result and send a network request if data is missing for the query', function () {
          expect(environment.check(queryMissingData.root)).toEqual(false);
          var result = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
          expect(result).toEqual({
            cacheKey: expect.any(String),
            fragmentNode: queryMissingData.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: queryMissingData.request
            },
            operation: queryMissingData
          });
          expect(environment.execute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should send a single network request when same query is read multiple times', function () {
          var result1 = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
          var result2 = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
          var expected = {
            cacheKey: expect.any(String),
            fragmentNode: queryMissingData.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: queryMissingData.request
            },
            operation: queryMissingData
          };
          expect(result1).toEqual(expected);
          expect(result2).toEqual(expected);
          expect(environment.execute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should throw error if network request errors', function () {
          var thrown = false;
          var sink;
          var networkExecute = jest.fn();
          var errorFetchObservable = Observable.create(function (s) {
            networkExecute();
            sink = s;
          });

          try {
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);

            if (!sink) {
              throw new Error('Expect sink to be defined');
            }

            sink.error(new Error('Oops'));
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);
          } catch (e) {
            expect(e instanceof Error).toEqual(true);
            expect(e.message).toEqual('Oops');
            thrown = true;
          }

          expect(thrown).toEqual(true);
          expect(networkExecute).toBeCalledTimes(1); // Assert query is temporarily retained during call to prepare

          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should return result and send a network request if data is missing for the query and observable returns synchronously', function () {
          expect(environment.check(queryMissingData.root)).toEqual(false);
          var networkExecute = jest.fn();
          var syncFetchObservable = Observable.create(function (sink) {
            environment.commitPayload(queryMissingData, {
              node: {
                __typename: 'User',
                id: '4',
                name: 'User 4'
              }
            });
            var snapshot = environment.lookup(queryMissingData.fragment);
            networkExecute();
            sink.next(snapshot);
            sink.complete();
          });
          var result = QueryResource.prepare(queryMissingData, syncFetchObservable, fetchPolicy, renderPolicy);
          expect(result).toEqual({
            cacheKey: expect.any(String),
            fragmentNode: queryMissingData.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: queryMissingData.request
            },
            operation: queryMissingData
          });
          expect(networkExecute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should throw error if network request errors synchronously', function () {
          var thrown = false;
          var networkExecute = jest.fn();
          var errorFetchObservable = Observable.create(function (sink) {
            networkExecute();
            sink.error(new Error('Oops'));
          });

          try {
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);
          } catch (e) {
            expect(e instanceof Error).toEqual(true);
            expect(e.message).toEqual('Oops');
            thrown = true;
          }

          expect(thrown).toEqual(true);
          expect(networkExecute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
      });
      describe('renderPolicy: full', function () {
        beforeEach(function () {
          renderPolicy = 'full';
        });
        it('should return result and send a network request even when data is locally available', function () {
          expect(environment.check(query.root)).toEqual(true);
          var result = QueryResource.prepare(query, fetchObservable, fetchPolicy, renderPolicy);
          expect(result).toEqual({
            cacheKey: expect.any(String),
            fragmentNode: query.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: query.request
            },
            operation: query
          });
          expect(environment.execute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should suspend and send a network request if data is missing for the query', function () {
          expect(environment.check(queryMissingData.root)).toEqual(false);
          var thrown;

          try {
            QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
          } catch (p) {
            expect(typeof p.then).toEqual('function');
            thrown = true;
          }

          expect(environment.execute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1);
          expect(thrown).toEqual(true); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should cache a single promise and send a single network request when same query is read multiple times', function () {
          var promise1;

          try {
            QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
          } catch (p) {
            expect(typeof p.then).toBe('function');
            promise1 = p;
          } // Assert query is temporarily retained during call to prepare


          expect(release).toBeCalledTimes(0);
          expect(environment.retain).toBeCalledTimes(1);
          var promise2;

          try {
            QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
          } catch (p) {
            expect(typeof p.then).toBe('function');
            promise2 = p;
          } // Assert query is still temporarily retained during second call to prepare


          expect(release).toBeCalledTimes(0);
          expect(environment.retain).toBeCalledTimes(1); // Assert that same promise was thrown

          expect(promise1).toBe(promise2); // Assert that network was only called once

          expect(environment.execute).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should throw error if network request errors', function () {
          var thrown = false;
          var sink;
          var networkExecute = jest.fn();
          var errorFetchObservable = Observable.create(function (s) {
            networkExecute();
            sink = s;
          });
          551;

          try {
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);
          } catch (p) {
            expect(typeof p.then).toBe('function');
          }

          if (!sink) {
            throw new Error('Expect sink to be defined');
          }

          try {
            sink.error(new Error('Oops'));
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);
          } catch (e) {
            expect(e instanceof Error).toEqual(true);
            expect(e.message).toEqual('Oops');
            thrown = true;
          }

          expect(thrown).toEqual(true);
          expect(networkExecute).toBeCalledTimes(1); // Assert query is temporarily retained during call to prepare

          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should return result and send a network request if data is missing for the query and observable returns synchronously', function () {
          expect(environment.check(queryMissingData.root)).toEqual(false);
          var networkExecute = jest.fn();
          var syncFetchObservable = Observable.create(function (sink) {
            environment.commitPayload(queryMissingData, {
              node: {
                __typename: 'User',
                id: '4',
                name: 'User 4'
              }
            });
            var snapshot = environment.lookup(queryMissingData.fragment);
            networkExecute();
            sink.next(snapshot);
            sink.complete();
          });
          var result = QueryResource.prepare(queryMissingData, syncFetchObservable, fetchPolicy, renderPolicy);
          expect(result).toEqual({
            cacheKey: expect.any(String),
            fragmentNode: queryMissingData.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: queryMissingData.request
            },
            operation: queryMissingData
          });
          expect(networkExecute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should throw error if network request errors synchronously', function () {
          var thrown = false;
          var networkExecute = jest.fn();
          var errorFetchObservable = Observable.create(function (sink) {
            networkExecute();
            sink.error(new Error('Oops'));
          });

          try {
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);
          } catch (e) {
            expect(e instanceof Error).toEqual(true);
            expect(e.message).toEqual('Oops');
            thrown = true;
          }

          expect(thrown).toEqual(true);
          expect(networkExecute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
      });
    });
    describe('fetchPolicy: network-only', function () {
      beforeEach(function () {
        fetchPolicy = 'network-only';
      });
      describe('renderPolicy: partial', function () {
        beforeEach(function () {
          renderPolicy = 'partial';
        });
        it('should suspend and send a network request even if data is available locally', function () {
          expect(environment.check(query.root)).toEqual(true);
          var thrown = false;

          try {
            QueryResource.prepare(query, fetchObservable, fetchPolicy, renderPolicy);
          } catch (promise) {
            expect(typeof promise.then).toBe('function');
            thrown = true;
          }

          expect(environment.execute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1);
          expect(thrown).toBe(true); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should suspend and send a network request when query has missing data', function () {
          expect(environment.check(queryMissingData.root)).toEqual(false);
          var thrown = false;

          try {
            QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
          } catch (promise) {
            expect(typeof promise.then).toBe('function');
            thrown = true;
          }

          expect(environment.execute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1);
          expect(thrown).toBe(true); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should throw error if network request errors', function () {
          var thrownPromise = false;
          var thrownError = false;
          var sink;
          var networkExecute = jest.fn();
          var errorFetchObservable = Observable.create(function (s) {
            networkExecute();
            sink = s;
          });

          try {
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);
          } catch (promise) {
            expect(typeof promise.then).toBe('function');
            thrownPromise = true;
          }

          expect(thrownPromise).toEqual(true);

          if (!sink) {
            throw new Error('Expect sink to be defined');
          }

          sink.error(new Error('Oops'));

          try {
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);
          } catch (e) {
            expect(e instanceof Error).toEqual(true);
            expect(e.message).toEqual('Oops');
            thrownError = true;
          }

          expect(thrownError).toEqual(true);
          expect(networkExecute).toBeCalledTimes(1); // Assert query is temporarily retained during call to prepare

          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should return result if network observable returns synchronously', function () {
          var networkExecute = jest.fn();
          var syncFetchObservable = Observable.create(function (sink) {
            var snapshot = environment.lookup(query.fragment);
            networkExecute();
            sink.next(snapshot);
            sink.complete();
          });
          var result = QueryResource.prepare(query, syncFetchObservable, fetchPolicy, renderPolicy);
          expect(result).toEqual({
            cacheKey: expect.any(String),
            fragmentNode: query.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: query.request
            },
            operation: query
          });
          expect(networkExecute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should throw error if network request errors synchronously', function () {
          var thrown = false;
          var networkExecute = jest.fn();
          var errorFetchObservable = Observable.create(function (sink) {
            networkExecute();
            var error = new Error('Oops');
            sink.error(error);
          });

          try {
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);
          } catch (e) {
            expect(e instanceof Error).toEqual(true);
            expect(e.message).toEqual('Oops');
            thrown = true;
          }

          expect(thrown).toEqual(true);
          expect(networkExecute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
      });
      describe('renderPolicy: full', function () {
        beforeEach(function () {
          renderPolicy = 'full';
        });
        it('should suspend and send a network request even if data is available locally', function () {
          expect(environment.check(query.root)).toEqual(true);
          var thrown = false;

          try {
            QueryResource.prepare(query, fetchObservable, fetchPolicy, renderPolicy);
          } catch (promise) {
            expect(typeof promise.then).toBe('function');
            thrown = true;
          }

          expect(environment.execute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1);
          expect(thrown).toBe(true); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should suspend and send a network request when query has missing data', function () {
          expect(environment.check(queryMissingData.root)).toEqual(false);
          var thrown = false;

          try {
            QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
          } catch (promise) {
            expect(typeof promise.then).toBe('function');
            thrown = true;
          }

          expect(environment.execute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1);
          expect(thrown).toBe(true); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should throw error if network request errors', function () {
          var thrownPromise = false;
          var thrownError = false;
          var sink;
          var networkExecute = jest.fn();
          var errorFetchObservable = Observable.create(function (s) {
            networkExecute();
            sink = s;
          });

          try {
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);
          } catch (promise) {
            expect(typeof promise.then).toBe('function');
            thrownPromise = true;
          }

          expect(thrownPromise).toEqual(true);

          if (!sink) {
            throw new Error('Expect sink to be defined');
          }

          sink.error(new Error('Oops'));

          try {
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);
          } catch (e) {
            expect(e instanceof Error).toEqual(true);
            expect(e.message).toEqual('Oops');
            thrownError = true;
          }

          expect(thrownError).toEqual(true);
          expect(networkExecute).toBeCalledTimes(1); // Assert query is temporarily retained during call to prepare

          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should return result if network observable returns synchronously', function () {
          var networkExecute = jest.fn();
          var syncFetchObservable = Observable.create(function (sink) {
            var snapshot = environment.lookup(query.fragment);
            networkExecute();
            sink.next(snapshot);
            sink.complete();
          });
          var result = QueryResource.prepare(query, syncFetchObservable, fetchPolicy, renderPolicy);
          expect(result).toEqual({
            cacheKey: expect.any(String),
            fragmentNode: query.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: query.request
            },
            operation: query
          });
          expect(networkExecute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should throw error if network request errors synchronously', function () {
          var thrown = false;
          var networkExecute = jest.fn();
          var errorFetchObservable = Observable.create(function (sink) {
            networkExecute();
            var error = new Error('Oops');
            sink.error(error);
          });

          try {
            QueryResource.prepare(queryMissingData, errorFetchObservable, fetchPolicy, renderPolicy);
          } catch (e) {
            expect(e instanceof Error).toEqual(true);
            expect(e.message).toEqual('Oops');
            thrown = true;
          }

          expect(thrown).toEqual(true);
          expect(networkExecute).toBeCalledTimes(1);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
      });
    });
    describe('fetchPolicy: store-only', function () {
      beforeEach(function () {
        fetchPolicy = 'store-only';
      });
      describe('renderPolicy: partial', function () {
        beforeEach(function () {
          renderPolicy = 'partial';
        });
        it('should not send network request if data is available locally', function () {
          expect(environment.check(query.root)).toEqual(true);
          var result = QueryResource.prepare(query, fetchObservable, fetchPolicy, renderPolicy);
          expect(result).toEqual({
            cacheKey: expect.any(String),
            fragmentNode: query.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: query.request
            },
            operation: query
          });
          expect(environment.execute).toBeCalledTimes(0);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should not send network request even if data is missing', function () {
          expect(environment.check(queryMissingData.root)).toEqual(false);
          var result = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
          expect(result).toEqual({
            cacheKey: expect.any(String),
            fragmentNode: queryMissingData.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: queryMissingData.request
            },
            operation: queryMissingData
          });
          expect(environment.execute).toBeCalledTimes(0);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
      });
      describe('renderPolicy: full', function () {
        beforeEach(function () {
          renderPolicy = 'full';
        });
        it('should not send network request if data is available locally', function () {
          expect(environment.check(query.root)).toEqual(true);
          var result = QueryResource.prepare(query, fetchObservable, fetchPolicy, renderPolicy);
          expect(result).toEqual({
            cacheKey: expect.any(String),
            fragmentNode: query.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: query.request
            },
            operation: query
          });
          expect(environment.execute).toBeCalledTimes(0);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
        it('should not send network request even if data is missing', function () {
          expect(environment.check(queryMissingData.root)).toEqual(false);
          var result = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
          expect(result).toEqual({
            cacheKey: expect.any(String),
            fragmentNode: queryMissingData.fragment.node,
            fragmentRef: {
              __id: ROOT_ID,
              __fragments: {
                UserQuery: variables
              },
              __fragmentOwner: queryMissingData.request
            },
            operation: queryMissingData
          });
          expect(environment.execute).toBeCalledTimes(0);
          expect(environment.retain).toBeCalledTimes(1); // Assert that query is released after enough time has passed without
          // calling QueryResource.retain

          jest.runAllTimers();
          expect(release).toBeCalledTimes(1);
        });
      });
    });
  });
  describe('retain', function () {
    beforeEach(function () {
      fetchPolicy = 'store-or-network';
    });
    it('should permanently retain the query that was retained during `prepare`', function () {
      var result = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
      expect(environment.execute).toBeCalledTimes(1);
      expect(environment.retain).toBeCalledTimes(1);
      expect(environment.retain.mock.calls[0][0]).toEqual(queryMissingData.root); // Data retention ownership is established permanently:
      // - Temporary retain is released
      // - New permanent retain is established

      var disposable = QueryResource.retain(result);
      expect(release).toBeCalledTimes(0);
      expect(environment.retain).toBeCalledTimes(1);
      expect(environment.retain.mock.calls[0][0]).toEqual(queryMissingData.root); // Running timers won't release the query since it has been
      // permanently retained

      jest.runAllTimers();
      expect(release).toBeCalledTimes(0); // Should not clear the cache entry

      expect(QueryResource.getCacheEntry(queryMissingData, fetchPolicy, renderPolicy)).toBeDefined(); // Assert that disposing releases the query

      disposable.dispose();
      expect(release).toBeCalledTimes(1);
      expect(environment.retain).toBeCalledTimes(1);
    });
    it('should auto-release if enough time has passed before `retain` is called after `prepare`', function () {
      var result = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy);
      expect(environment.execute).toBeCalledTimes(1);
      expect(release).toBeCalledTimes(0);
      expect(environment.retain).toBeCalledTimes(1);
      expect(environment.retain.mock.calls[0][0]).toEqual(queryMissingData.root); // Running timers before calling `retain` auto-releases the query
      // retained during `read`

      jest.runAllTimers();
      expect(release).toBeCalledTimes(1); // Cache entry should be removed

      expect(QueryResource.getCacheEntry(queryMissingData, fetchPolicy, renderPolicy)).toBeUndefined(); // Calling retain after query has been auto-released should retain
      // the query again.

      var disposable = QueryResource.retain(result);
      expect(release).toBeCalledTimes(1);
      expect(environment.retain).toBeCalledTimes(2);
      expect(environment.retain.mock.calls[1][0]).toEqual(queryMissingData.root); // Assert that disposing releases the query

      disposable.dispose();
      expect(release).toBeCalledTimes(2);
      expect(environment.retain).toBeCalledTimes(2);
    });
    it("retains the query during `prepare` even if a network request wasn't started", function () {
      var result = QueryResource.prepare(query, fetchObservable, fetchPolicy, renderPolicy);
      expect(environment.execute).toBeCalledTimes(0);
      expect(release).toBeCalledTimes(0);
      expect(environment.retain).toBeCalledTimes(1);
      expect(environment.retain.mock.calls[0][0]).toEqual(query.root); // Running timers before calling `retain` auto-releases the query
      // retained during `read`

      jest.runAllTimers();
      expect(release).toBeCalledTimes(1); // Calling retain should retain the query.

      var disposable = QueryResource.retain(result);
      expect(release).toBeCalledTimes(1);
      expect(environment.retain).toBeCalledTimes(2);
      expect(environment.retain.mock.calls[1][0]).toEqual(query.root); // Assert that disposing releases the query

      disposable.dispose();
      expect(release).toBeCalledTimes(2);
      expect(environment.retain).toBeCalledTimes(2);
    });
    describe('when retaining the same query multiple times', function () {
      it('correctly retains query after temporarily retaining multiple times during render phase', function () {
        QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy); // Assert query is temporarily retained

        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(queryMissingData.root); // Assert that retain count is 1

        var cacheEntry = QueryResource.getCacheEntry(queryMissingData, fetchPolicy, renderPolicy);
        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1);
        var result = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy); // Assert query is still temporarily retained

        expect(release).toHaveBeenCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(queryMissingData.root); // Assert retain count is still 1

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1); // Assert network is only called once

        expect(environment.execute).toBeCalledTimes(1); // Permanently retain the second result, which is what would happen
        // if the second render got committed

        var disposable = QueryResource.retain(result); // Assert permanent retain is established and nothing is released

        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1); // Assert retain count is still 1

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1); // Assert that disposing correctly releases the query

        disposable.dispose();
        expect(release).toBeCalledTimes(1);
        expect(environment.retain).toBeCalledTimes(1);
      });
      it('correctly retains query after temporarily retaining multiple times during render phase and auto-release timers have expired', function () {
        QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy); // Assert query is temporarily retained

        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(queryMissingData.root); // Assert that retain count is 1

        var cacheEntry = QueryResource.getCacheEntry(queryMissingData, fetchPolicy, renderPolicy);
        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1);
        var result = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy); // Assert query is still temporarily retained

        expect(release).toHaveBeenCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(queryMissingData.root); // Assert that retain count is still 1

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1); // Assert network is only called once

        expect(environment.execute).toBeCalledTimes(1); // Permanently retain the second result, which is what would happen
        // if the second render got committed

        var disposable = QueryResource.retain(result); // Assert permanent retain is established and nothing is released

        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1); // Assert that retain count is still 1

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1); // Running timers won't release the query since it has been
        // permanently retained

        jest.runAllTimers();
        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1); // Assert that retain count is still 1

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1); // Assert that disposing correctly releases the query

        disposable.dispose();
        expect(release).toBeCalledTimes(1);
        expect(environment.retain).toBeCalledTimes(1);
      });
      it('does not temporarily retain query anymore if it has been permanently retained', function () {
        var result = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy); // Assert query is temporarily retained

        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(queryMissingData.root); // Assert that retain count is 1

        var cacheEntry = QueryResource.getCacheEntry(queryMissingData, fetchPolicy, renderPolicy);
        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1); // Assert network is called once

        expect(environment.execute).toBeCalledTimes(1); // Permanently retain the second result, which is what would happen
        // if the second render got committed

        var disposable = QueryResource.retain(result); // Assert permanent retain is established and nothing is released

        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1); // Assert that retain count remains at 1

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1); // Running timers won't release the query since it has been
        // permanently retained

        jest.runAllTimers();
        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1); // Assert that retain count remains at 1

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1);
        QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy); // Assert that the retain count remains at 1, even after
        // temporarily retaining again

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1); // Assert query is still retained

        expect(release).toHaveBeenCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(queryMissingData.root); // Assert that disposing the first disposable doesn't release the query

        disposable.dispose();
        expect(release).toBeCalledTimes(1);
        expect(environment.retain).toBeCalledTimes(1);
      });
      it("should not release the query before all callers have released it and auto-release timers haven't expired", function () {
        // NOTE: This simulates 2 separate query renderers mounting
        // simultaneously
        var result1 = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy); // Assert query is temporarily retained

        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(queryMissingData.root); // Assert that retain count is 1

        var cacheEntry = QueryResource.getCacheEntry(queryMissingData, fetchPolicy, renderPolicy);
        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1);
        var result2 = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy); // Assert query is still temporarily retained

        expect(release).toHaveBeenCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(queryMissingData.root); // Assert that retain count is still 1

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1); // Assert network is only called once

        expect(environment.execute).toBeCalledTimes(1);
        var disposable1 = QueryResource.retain(result1); // Assert permanent retain is established and nothing is released

        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1); // Assert that retain count is still 1

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1);
        var disposable2 = QueryResource.retain(result2); // Assert permanent retain is still established

        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1); // Assert that retain count is now 2

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(2); // Assert that disposing the first disposable doesn't release the query

        disposable1.dispose();
        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1);
        expect(QueryResource.getCacheEntry(queryMissingData, fetchPolicy, renderPolicy)).toBeDefined(); // Assert that disposing the last disposable fully releases the query

        disposable2.dispose();
        expect(release).toBeCalledTimes(1);
        expect(environment.retain).toBeCalledTimes(1);
        expect(QueryResource.getCacheEntry(queryMissingData, fetchPolicy, renderPolicy)).toBeUndefined();
      });
      it('should not release the query before all callers have released it and auto-release timers have expired', function () {
        // NOTE: This simulates 2 separate query renderers mounting
        // simultaneously
        var result1 = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy); // Assert query is temporarily retained

        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(queryMissingData.root); // Assert that retain count is 1

        var cacheEntry = QueryResource.getCacheEntry(queryMissingData, fetchPolicy, renderPolicy);
        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1);
        var result2 = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy); // Assert query is still temporarily retained

        expect(release).toHaveBeenCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(queryMissingData.root); // Assert that retain count is still 1

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1); // Assert network is only called once

        expect(environment.execute).toBeCalledTimes(1);
        var disposable1 = QueryResource.retain(result1); // Assert permanent retain is established and nothing is released

        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1); // Assert that retain count is still 1

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1);
        var disposable2 = QueryResource.retain(result2); // Assert permanent retain is still established

        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1); // Assert that retain count is now 2

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(2); // Running timers won't release the query since it has been
        // permanently retained

        jest.runAllTimers();
        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1); // Assert that retain count is still 2

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(2); // Assert that disposing the first disposable doesn't release the query

        disposable1.dispose();
        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1);
        expect(QueryResource.getCacheEntry(queryMissingData, fetchPolicy, renderPolicy)).toBeDefined(); // Assert that disposing the last disposable fully releases the query

        disposable2.dispose();
        expect(release).toBeCalledTimes(1);
        expect(environment.retain).toBeCalledTimes(1);
        expect(QueryResource.getCacheEntry(queryMissingData, fetchPolicy, renderPolicy)).toBeUndefined();
      });
      it('correctly retains query when releasing and re-retaining', function () {
        // NOTE: This simulates a query renderer unmounting and re-mounting
        var result1 = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy); // Assert query is temporarily retained

        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(queryMissingData.root); // Assert that retain count is 1

        var cacheEntry = QueryResource.getCacheEntry(queryMissingData, fetchPolicy, renderPolicy);
        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1); // Assert network is called

        expect(environment.execute).toBeCalledTimes(1); // Assert permanent retain is established

        var disposable1 = QueryResource.retain(result1);
        expect(release).toBeCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1); // Assert that retain count is still 1

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1); // Prepare the query again after it has been permanently retained.
        // This will happen if the query component is unmounting and re-mounting

        var result2 = QueryResource.prepare(queryMissingData, fetchObservableMissingData, fetchPolicy, renderPolicy); // Assert query is still retained

        expect(release).toHaveBeenCalledTimes(0);
        expect(environment.retain).toBeCalledTimes(1);
        expect(environment.retain.mock.calls[0][0]).toEqual(queryMissingData.root); // Assert that retain count is still 1

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1); // First disposable will be called when query component finally unmounts

        disposable1.dispose(); // Assert that query is temporarily fully released on unmount

        expect(release).toHaveBeenCalledTimes(1);
        expect(environment.retain).toBeCalledTimes(1); // Assert that retain count is now 0

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(0); // Permanently retain the query after the initial retain has been
        // disposed of. This will occur when the query component remounts.

        var disposable2 = QueryResource.retain(result2); // Assert latest temporary retain is released

        expect(release).toBeCalledTimes(1);
        expect(environment.retain).toBeCalledTimes(2); // Assert that retain count is now 1

        cacheEntry = QueryResource.getCacheEntry(queryMissingData, fetchPolicy, renderPolicy);
        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1); // Running timers won't release the query since it has been
        // permanently retained

        jest.runAllTimers();
        expect(release).toBeCalledTimes(1);
        expect(environment.retain).toBeCalledTimes(2); // Assert that retain count is still 1

        expect(cacheEntry && cacheEntry.getRetainCount()).toEqual(1); // Assert that disposing the last disposable fully releases the query

        disposable2.dispose();
        expect(release).toBeCalledTimes(2);
        expect(environment.retain).toBeCalledTimes(2);
      });
    });
  });
});