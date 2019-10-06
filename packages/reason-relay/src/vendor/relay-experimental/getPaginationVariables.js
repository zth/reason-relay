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

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

var _objectSpread4 = _interopRequireDefault(require("@babel/runtime/helpers/objectSpread"));

var invariant = require("fbjs/lib/invariant");

function getPaginationVariables(direction, count, cursor, baseVariables, paginationMetadata) {
  var _objectSpread3;

  var backwardMetadata = paginationMetadata.backward,
      forwardMetadata = paginationMetadata.forward;

  if (direction === 'backward') {
    var _objectSpread2;

    !(backwardMetadata != null && backwardMetadata.count != null && backwardMetadata.cursor != null) ? process.env.NODE_ENV !== "production" ? invariant(false, 'Relay: Expected backward pagination metadata to be avialable. ' + "If you're seeing this, this is likely a bug in Relay.") : invariant(false) : void 0;

    var _paginationVariables = (0, _objectSpread4["default"])({}, baseVariables, (_objectSpread2 = {}, (0, _defineProperty2["default"])(_objectSpread2, backwardMetadata.cursor, cursor), (0, _defineProperty2["default"])(_objectSpread2, backwardMetadata.count, count), _objectSpread2));

    if (forwardMetadata && forwardMetadata.cursor) {
      _paginationVariables[forwardMetadata.cursor] = null;
    }

    if (forwardMetadata && forwardMetadata.count) {
      _paginationVariables[forwardMetadata.count] = null;
    }

    return _paginationVariables;
  }

  !(forwardMetadata != null && forwardMetadata.count != null && forwardMetadata.cursor != null) ? process.env.NODE_ENV !== "production" ? invariant(false, 'Relay: Expected forward pagination metadata to be avialable. ' + "If you're seeing this, this is likely a bug in Relay.") : invariant(false) : void 0;
  var paginationVariables = (0, _objectSpread4["default"])({}, baseVariables, (_objectSpread3 = {}, (0, _defineProperty2["default"])(_objectSpread3, forwardMetadata.cursor, cursor), (0, _defineProperty2["default"])(_objectSpread3, forwardMetadata.count, count), _objectSpread3));

  if (backwardMetadata && backwardMetadata.cursor) {
    paginationVariables[backwardMetadata.cursor] = null;
  }

  if (backwardMetadata && backwardMetadata.count) {
    paginationVariables[backwardMetadata.count] = null;
  }

  return paginationVariables;
}

module.exports = getPaginationVariables;