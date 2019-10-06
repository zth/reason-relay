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

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

var _objectSpread2 = _interopRequireDefault(require("@babel/runtime/helpers/objectSpread"));

var getPaginationMetadata = require('./getPaginationMetadata');

var useLoadMoreFunction = require('./useLoadMoreFunction');

var useRefetchableFragmentNode = require('./useRefetchableFragmentNode');

var useStaticFragmentNodeWarning = require('./useStaticFragmentNodeWarning');

var _require = require('react'),
    useCallback = _require.useCallback,
    useState = _require.useState;

var _require2 = require('relay-runtime'),
    getFragment = _require2.getFragment,
    getFragmentIdentifier = _require2.getFragmentIdentifier;

function useLegacyPaginationFragment(fragmentInput, parentFragmentRef) {
  var fragmentNode = getFragment(fragmentInput);
  useStaticFragmentNodeWarning(fragmentNode, 'first argument of useLegacyPaginationFragment()');
  var componentDisplayName = 'useLegacyPaginationFragment()';

  var _getPaginationMetadat = getPaginationMetadata(fragmentNode, componentDisplayName),
      connectionPathInFragmentData = _getPaginationMetadat.connectionPathInFragmentData,
      fragmentRefPathInResponse = _getPaginationMetadat.fragmentRefPathInResponse,
      paginationRequest = _getPaginationMetadat.paginationRequest,
      paginationMetadata = _getPaginationMetadat.paginationMetadata;

  var _useRefetchableFragme = useRefetchableFragmentNode(fragmentNode, parentFragmentRef, componentDisplayName),
      fragmentData = _useRefetchableFragme.fragmentData,
      fragmentRef = _useRefetchableFragme.fragmentRef,
      refetch = _useRefetchableFragme.refetch;

  var fragmentIdentifier = getFragmentIdentifier(fragmentNode, fragmentRef); // Backward pagination

  var _useLoadMore = useLoadMore({
    direction: 'backward',
    fragmentNode: fragmentNode,
    fragmentRef: fragmentRef,
    fragmentIdentifier: fragmentIdentifier,
    fragmentData: fragmentData,
    connectionPathInFragmentData: connectionPathInFragmentData,
    fragmentRefPathInResponse: fragmentRefPathInResponse,
    paginationRequest: paginationRequest,
    paginationMetadata: paginationMetadata,
    componentDisplayName: componentDisplayName
  }),
      loadPrevious = _useLoadMore[0],
      hasPrevious = _useLoadMore[1],
      isLoadingPrevious = _useLoadMore[2],
      disposeFetchPrevious = _useLoadMore[3]; // Forward pagination


  var _useLoadMore2 = useLoadMore({
    direction: 'forward',
    fragmentNode: fragmentNode,
    fragmentRef: fragmentRef,
    fragmentIdentifier: fragmentIdentifier,
    fragmentData: fragmentData,
    connectionPathInFragmentData: connectionPathInFragmentData,
    fragmentRefPathInResponse: fragmentRefPathInResponse,
    paginationRequest: paginationRequest,
    paginationMetadata: paginationMetadata,
    componentDisplayName: componentDisplayName
  }),
      loadNext = _useLoadMore2[0],
      hasNext = _useLoadMore2[1],
      isLoadingNext = _useLoadMore2[2],
      disposeFetchNext = _useLoadMore2[3];

  var refetchPagination = useCallback(function (variables, options) {
    disposeFetchNext();
    disposeFetchPrevious();
    return refetch(variables, (0, _objectSpread2["default"])({}, options, {
      __environment: undefined
    }));
  }, [disposeFetchNext, disposeFetchPrevious, refetch]);
  return {
    data: fragmentData,
    loadNext: loadNext,
    loadPrevious: loadPrevious,
    hasNext: hasNext,
    hasPrevious: hasPrevious,
    isLoadingNext: isLoadingNext,
    isLoadingPrevious: isLoadingPrevious,
    refetch: refetchPagination
  };
}

function useLoadMore(args) {
  var _useState = useState(false),
      isLoadingMore = _useState[0],
      setIsLoadingMore = _useState[1];

  var observer = {
    start: function start() {
      return setIsLoadingMore(true);
    },
    complete: function complete() {
      return setIsLoadingMore(false);
    },
    error: function error() {
      return setIsLoadingMore(false);
    }
  };

  var handleReset = function handleReset() {
    return setIsLoadingMore(false);
  };

  var _useLoadMoreFunction = useLoadMoreFunction((0, _objectSpread2["default"])({}, args, {
    observer: observer,
    onReset: handleReset
  })),
      loadMore = _useLoadMoreFunction[0],
      hasMore = _useLoadMoreFunction[1],
      disposeFetch = _useLoadMoreFunction[2];

  return [loadMore, hasMore, isLoadingMore, disposeFetch];
}

module.exports = useLegacyPaginationFragment;