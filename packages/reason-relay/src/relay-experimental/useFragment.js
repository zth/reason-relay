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

var useFragmentNode = require('./useFragmentNode');

var useStaticPropWarning = require('./useStaticPropWarning');

var _require = require('relay-runtime'),
    getFragment = _require.getFragment;

function useFragment(fragmentInput, fragmentRef) {
  useStaticPropWarning(fragmentInput, 'first argument of useFragment()');
  var fragmentNode = getFragment(fragmentInput);

  var _useFragmentNode = useFragmentNode(fragmentNode, fragmentRef, 'useFragment()'),
      data = _useFragmentNode.data;

  return data;
}

module.exports = useFragment;