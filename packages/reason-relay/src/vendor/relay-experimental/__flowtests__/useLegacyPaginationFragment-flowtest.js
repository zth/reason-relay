/**
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

var _utils = require("./utils");

var useLegacyPaginationFragment = require('../useLegacyPaginationFragment');

/* eslint-disable react-hooks/rules-of-hooks */
// Nullability of returned data type is correct
useLegacyPaginationFragment(_utils.fragmentInput, _utils.keyNonNullable);
useLegacyPaginationFragment(_utils.fragmentInput, _utils.keyNullable); // $FlowExpectedError: can't cast nullable to non-nullable

useLegacyPaginationFragment(_utils.fragmentInput, _utils.keyNullable); // $FlowExpectedError: actual type of returned data is correct

useLegacyPaginationFragment(_utils.fragmentInput, _utils.keyAnotherNonNullable); // $FlowExpectedError

useLegacyPaginationFragment(_utils.fragmentInput, _utils.keyAnotherNullable); // Refetch function options:

var _useLegacyPaginationF = useLegacyPaginationFragment(_utils.fragmentInput, _utils.keyNonNullable),
    refetch = _useLegacyPaginationF.refetch; // $FlowExpectedError: internal option


refetch(variables, {
  __environment: environment
}); // $FlowExpectedError: doesn't exist

refetch(variables, {
  NON_EXIST: 'NON_EXIST'
});