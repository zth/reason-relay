/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * 
 * @emails oncall+relay
 * @format
 */
'use strict';

var _asyncToGenerator = require("@babel/runtime/helpers/asyncToGenerator");

var fetchQuery = require('../fetchQuery');

var _require = require('relay-test-utils-internal'),
    createMockEnvironment = _require.createMockEnvironment,
    generateAndCompile = _require.generateAndCompile;

var response = {
  data: {
    node: {
      __typename: 'User',
      id: '4'
    }
  }
};
describe('fetchQuery', function () {
  var query;
  var variables;
  var environment;
  var retained = [];
  beforeEach(function () {
    retained = [];
    jest.mock('fbjs/lib/ExecutionEnvironment', function () {
      return {
        canUseDOM: function canUseDOM() {
          return true;
        }
      };
    });
    environment = createMockEnvironment();
    environment.retain.mockImplementation(function (obj) {
      var idx = retained.push(obj);
      return {
        dispose: function dispose() {
          retained = retained.filter(function (o, ii) {
            return ii === idx;
          });
        }
      };
    });
    variables = {
      id: '4'
    };
    query = generateAndCompile("query TestQuery($id: ID!) {\n          node(id: $id) {\n            id\n          }\n        }\n      ").TestQuery;
  });
  it('fetches request and does not retain data', function () {
    var calledObserver = false;
    var observer = {
      complete: function complete() {
        calledObserver = true;
        expect(retained.length).toEqual(0);
      }
    };
    var subscription = fetchQuery(environment, query, variables).subscribe(observer);
    environment.mock.nextValue(query, response);
    environment.mock.complete(query);
    subscription.unsubscribe();
    expect(calledObserver).toEqual(true);
    expect(retained.length).toEqual(0);
  });
  it('provides data snapshot on next', function () {
    var calledNext = false;
    var observer = {
      next: function next(data) {
        calledNext = true;
        expect(retained.length).toEqual(0);
        expect(data).toEqual({
          node: {
            id: '4'
          }
        });
      }
    };
    fetchQuery(environment, query, variables).subscribe(observer);
    environment.mock.nextValue(query, response);
    expect(calledNext).toEqual(true);
    environment.mock.complete(query);
    expect(retained.length).toEqual(0);
  });
  it('unsubscribes when request is disposed', function () {
    var calledNext = false;
    var calledUnsubscribe = false;
    var observer = {
      next: function next() {
        calledNext = true;
        expect(retained.length).toEqual(0);
      },
      unsubscribe: function unsubscribe() {
        calledUnsubscribe = true;
      }
    };
    var subscription = fetchQuery(environment, query, variables).subscribe(observer);
    environment.mock.nextValue(query, response);
    subscription.unsubscribe();
    expect(calledNext).toEqual(true);
    expect(calledUnsubscribe).toEqual(true);
  });
  it('handles error correctly', function () {
    var calledError = false;
    var observer = {
      error: function error(_error) {
        calledError = true;
        expect(_error.message).toEqual('Oops');
        expect(retained.length).toEqual(0);
      }
    };
    var subscription = fetchQuery(environment, query, variables).subscribe(observer);
    environment.mock.reject(query, new Error('Oops'));
    expect(calledError).toEqual(true);
    expect(retained.length).toEqual(0);
    subscription.unsubscribe();
  });
  describe('.toPromise()', function () {
    it('fetches request and does not retain query data',
    /*#__PURE__*/
    _asyncToGenerator(function* () {
      var promise = fetchQuery(environment, query, variables).toPromise();
      expect(environment.mock.isLoading(query, variables)).toEqual(true);
      environment.mock.nextValue(query, response);
      var data = yield promise;
      expect(data).toEqual({
        node: {
          id: '4'
        }
      });
      expect(environment.mock.isLoading(query, variables)).toEqual(false);
      expect(retained.length).toEqual(0);
    }));
    it('rejects when error occurs',
    /*#__PURE__*/
    _asyncToGenerator(function* () {
      var promise = fetchQuery(environment, query, variables).toPromise();
      expect(environment.mock.isLoading(query, variables)).toEqual(true);
      environment.mock.reject(query, new Error('Oops'));

      try {
        yield promise;
      } catch (error) {
        expect(error.message).toEqual('Oops');
      }

      expect(environment.mock.isLoading(query, variables)).toEqual(false);
      expect(retained.length).toEqual(0);
    }));
  });
});