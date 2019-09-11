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

var React = require('react');

var useMemoVariables = require('./useMemoVariables');

var _require = require('relay-runtime'),
    createOperationDescriptor = _require.createOperationDescriptor,
    getRequest = _require.getRequest;

var useMemo = React.useMemo;

function useMemoOperationDescriptor(gqlQuery, variables) {
  var _useMemoVariables = useMemoVariables(variables),
      memoVariables = _useMemoVariables[0];

  return useMemo(function () {
    return createOperationDescriptor(getRequest(gqlQuery), memoVariables);
  }, [gqlQuery, memoVariables]);
}

module.exports = useMemoOperationDescriptor;