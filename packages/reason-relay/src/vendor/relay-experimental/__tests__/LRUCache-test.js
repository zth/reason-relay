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

var LRUCache = require('../LRUCache');

var invariant = require("fbjs/lib/invariant");

test('LRUCache', function () {
  var testInstance = LRUCache.create(3);
  var testCases = [['', null, 'size', 0], ['', null, 'capacity', 3], ['a', 1, 'set', undefined], ['b', 2, 'set', undefined], ['c', 3, 'set', undefined], ['c', null, 'get', 3], ['d', 4, 'set', undefined], ['', null, 'size', 3], ['a', null, 'get', undefined], ['b', null, 'get', 2], ['b', null, 'delete', undefined], ['', null, 'size', 2], ['', null, 'capacity', 1], ['b', null, 'has', false], ['b', 2, 'set', undefined], ['b', null, 'has', true], ['a', 1, 'set', undefined], ['e', 5, 'set', undefined], ['f', 6, 'set', undefined], ['b', null, 'has', false], ['a', null, 'has', true], ['e', null, 'has', true], ['f', null, 'has', true]];

  for (var _i = 0, _testCases = testCases; _i < _testCases.length; _i++) {
    var testCase = _testCases[_i];
    var key = testCase[0],
        value = testCase[1],
        method = testCase[2],
        expected = testCase[3];
    var result = void 0;

    switch (method) {
      case 'set':
        if (value != null) {
          result = testInstance.set(key, value);
        }

        break;

      case 'get':
        result = testInstance.get(key);
        break;

      case 'has':
        result = testInstance.has(key);
        break;

      case 'size':
        result = testInstance.size();
        break;

      case 'delete':
        result = testInstance["delete"](key);
        break;

      case 'capacity':
        result = testInstance.capacity();
        break;

      default:
        !false ? process.env.NODE_ENV !== "production" ? invariant(false, 'Test case for method %s is not available.', method) : invariant(false) : void 0;
    }

    expect(result).toEqual(expected);
  }
});