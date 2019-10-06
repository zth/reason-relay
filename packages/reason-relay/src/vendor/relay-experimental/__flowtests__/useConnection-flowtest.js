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

var useConnection = require('../useConnection');

var _require = require('relay-runtime'),
    Resolver = _require.ConnectionResolver_UNSTABLE;

/* eslint-disable react-hooks/rules-of-hooks */
// Nullability of returned data type is correct
useConnection(Resolver, connectionReference);
useConnection(Resolver, nullableConnectionReference); // $FlowExpectedError: can't cast nullable to non-nullable

useConnection(Resolver, nullableConnectionReference); // $FlowExpectedError: actual type of returned data is correct

useConnection(Resolver, otherConnectionReference); // $FlowExpectedError

useConnection(Resolver, otherNullableConnectionReference); // $FlowExpectedError: Key should be one of the generated types

useConnection(Resolver, 'INVALID_KEY');
/* eslint-enable react-hooks/rules-of-hooks */