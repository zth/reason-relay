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

var useRefetchableFragment = require('../useRefetchableFragment');

/* eslint-disable react-hooks/rules-of-hooks */
// Nullability of returned data type is correct
useRefetchableFragment(_utils.fragmentInput, _utils.keyNonNullable);
useRefetchableFragment(_utils.fragmentInput, _utils.keyNullable); // $FlowExpectedError: can't cast nullable to non-nullable

useRefetchableFragment(_utils.fragmentInput, _utils.keyNullable); // $FlowExpectedError: refetch requires exact type if key is nullable

useRefetchableFragment(_utils.fragmentInput, _utils.keyNullable); // $FlowExpectedError: actual type of returned data is correct

useRefetchableFragment(_utils.fragmentInput, _utils.keyAnotherNonNullable); // $FlowExpectedError

useRefetchableFragment(_utils.fragmentInput, _utils.keyAnotherNullable); // Refetch function options:

var _useRefetchableFragme = useRefetchableFragment(_utils.fragmentInput, _utils.keyNonNullable),
    _ = _useRefetchableFragme[0],
    refetch = _useRefetchableFragme[1]; // $FlowExpectedError: internal option


refetch(variables, {
  __environment: environment
}); // $FlowExpectedError: doesn't exist

refetch(variables, {
  NON_EXIST: 'NON_EXIST'
});