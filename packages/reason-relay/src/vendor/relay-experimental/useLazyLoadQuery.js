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

var useLazyLoadQueryNode = require('./useLazyLoadQueryNode');

var useMemoOperationDescriptor = require('./useMemoOperationDescriptor');

function useLazyLoadQuery(gqlQuery, variables, options) {
  var query = useMemoOperationDescriptor(gqlQuery, variables);
  var data = useLazyLoadQueryNode({
    componentDisplayName: 'useLazyLoadQuery()',
    fetchKey: options === null || options === void 0 ? void 0 : options.fetchKey,
    fetchPolicy: options === null || options === void 0 ? void 0 : options.fetchPolicy,
    networkCacheConfig: options === null || options === void 0 ? void 0 : options.networkCacheConfig,
    query: query
  });
  return data;
}

module.exports = useLazyLoadQuery;