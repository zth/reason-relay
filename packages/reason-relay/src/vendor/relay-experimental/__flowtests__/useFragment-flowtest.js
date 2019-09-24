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

var useFragment = require('../useFragment');

/* eslint-disable react-hooks/rules-of-hooks */
// Nullability of returned data type is correct
useFragment(_utils.fragmentInput, _utils.keyNonNullable);
useFragment(_utils.fragmentInput, _utils.keyNullable);
useFragment(_utils.fragmentInput, _utils.keyNonNullablePlural);
useFragment(_utils.fragmentInput, _utils.keyNullablePlural); // $FlowExpectedError: can't cast nullable to non-nullable

useFragment(_utils.fragmentInput, _utils.keyNullable); // $FlowExpectedError: can't cast nullable plural to non-nullable plural

useFragment(_utils.fragmentInput, _utils.keyNullablePlural); // $FlowExpectedError: actual type of returned data is correct

useFragment(_utils.fragmentInput, _utils.keyAnotherNonNullable); // $FlowExpectedError

useFragment(_utils.fragmentInput, _utils.keyAnotherNullable); // $FlowExpectedError: Key should be one of the generated types

useFragment(_utils.fragmentInput, 'INVALID_KEY');
/* eslint-enable react-hooks/rules-of-hooks */