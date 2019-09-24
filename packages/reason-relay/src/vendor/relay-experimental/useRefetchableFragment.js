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

var useRefetchableFragmentNode = require('./useRefetchableFragmentNode');

var useStaticFragmentNodeWarning = require('./useStaticFragmentNodeWarning');

var _require = require('relay-runtime'),
    getFragment = _require.getFragment;

function useRefetchableFragment(fragmentInput, fragmentRef) {
  var fragmentNode = getFragment(fragmentInput);
  useStaticFragmentNodeWarning(fragmentNode, 'first argument of useRefetchableFragment()');

  var _useRefetchableFragme = useRefetchableFragmentNode(fragmentNode, fragmentRef, 'useRefetchableFragment()'),
      fragmentData = _useRefetchableFragme.fragmentData,
      refetch = _useRefetchableFragme.refetch; // $FlowExpectedError: Exposed options is a subset of internal options


  return [fragmentData, refetch];
}

module.exports = useRefetchableFragment;