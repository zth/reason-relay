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
    useEffect = _require.useEffect,
    useRef = _require.useRef;

function useIsMountedRef() {
  var isMountedRef = useRef(true);
  useEffect(function () {
    return function () {
      isMountedRef.current = false;
    };
  }, []);
  return isMountedRef;
}

module.exports = useIsMountedRef;