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

var invariant = require("fbjs/lib/invariant");

function getValueAtPath(data, path) {
  var result = data;
  var _iteratorNormalCompletion = true;
  var _didIteratorError = false;
  var _iteratorError = undefined;

  try {
    for (var _iterator = path[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
      var key = _step.value;

      if (result == null) {
        return null;
      }

      if (typeof key === 'number') {
        !Array.isArray(result) ? process.env.NODE_ENV !== "production" ? invariant(false, 'Relay: Expected an array when extracting value at path. ' + "If you're seeing this, this is likely a bug in Relay.") : invariant(false) : void 0;
        result = result[key];
      } else {
        !(typeof result === 'object' && !Array.isArray(result)) ? process.env.NODE_ENV !== "production" ? invariant(false, 'Relay: Expected an object when extracting value at path. ' + "If you're seeing this, this is likely a bug in Relay.") : invariant(false) : void 0;
        result = result[key];
      }
    }
  } catch (err) {
    _didIteratorError = true;
    _iteratorError = err;
  } finally {
    try {
      if (!_iteratorNormalCompletion && _iterator["return"] != null) {
        _iterator["return"]();
      }
    } finally {
      if (_didIteratorError) {
        throw _iteratorError;
      }
    }
  }

  return result;
}

module.exports = getValueAtPath;