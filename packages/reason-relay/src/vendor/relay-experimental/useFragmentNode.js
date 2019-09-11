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

var _require = require('react'),
    useMemo = _require.useMemo;

var useFragmentNodes = require('./useFragmentNodes');

function useFragmentNode(fragmentNode, fragmentRef, containerDisplayName) {
  var fragmentNodes = useMemo(function () {
    return {
      result: fragmentNode
    };
  }, [fragmentNode]);
  var fragmentRefs = useMemo(function () {
    return {
      result: fragmentRef
    };
  }, [fragmentRef]);

  var _useFragmentNodes = useFragmentNodes(fragmentNodes, fragmentRefs, containerDisplayName),
      data = _useFragmentNodes.data,
      disableStoreUpdates = _useFragmentNodes.disableStoreUpdates,
      enableStoreUpdates = _useFragmentNodes.enableStoreUpdates,
      shouldUpdateGeneration = _useFragmentNodes.shouldUpdateGeneration;

  return {
    data: data.result,
    disableStoreUpdates: disableStoreUpdates,
    enableStoreUpdates: enableStoreUpdates,
    shouldUpdateGeneration: shouldUpdateGeneration
  };
}

module.exports = useFragmentNode;