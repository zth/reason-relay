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

var useMemoOperationDescriptor = require('./useMemoOperationDescriptor');

var useQueryNode = require('./useQueryNode');

function useQuery(gqlQuery, variables, options) {
  var query = useMemoOperationDescriptor(gqlQuery, variables);
  var data = useQueryNode({
    query: query,
    fetchKey: options === null || options === void 0 ? void 0 : options.fetchKey,
    fetchPolicy: options === null || options === void 0 ? void 0 : options.fetchPolicy,
    componentDisplayName: 'useQuery()'
  });
  return data;
}

module.exports = useQuery;