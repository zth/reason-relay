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
'use strict'; // flowlint untyped-import:off

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

var _objectWithoutPropertiesLoose2 = _interopRequireDefault(require("@babel/runtime/helpers/objectWithoutPropertiesLoose"));

var _objectSpread2 = _interopRequireDefault(require("@babel/runtime/helpers/objectSpread"));

var Scheduler = require('scheduler'); // flowlint untyped-import:error


var getPaginationMetadata = require('./getPaginationMetadata');

var invariant = require("fbjs/lib/invariant");

var useLoadMoreFunction = require('./useLoadMoreFunction');

var useRefetchableFragmentNode = require('./useRefetchableFragmentNode');

var useStaticPropWarning = require('./useStaticPropWarning');

var warning = require("fbjs/lib/warning");

var _require = require('react'),
    useCallback = _require.useCallback,
    useEffect = _require.useEffect,
    useRef = _require.useRef,
    useState = _require.useState;

var _require2 = require('relay-runtime'),
    getFragment = _require2.getFragment,
    getFragmentIdentifier = _require2.getFragmentIdentifier,
    getFragmentOwner = _require2.getFragmentOwner;

function useBlockingPaginationFragment(fragmentInput, parentFragmentRef) {
  var componentDisplayName = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 'useBlockingPaginationFragment()';
  useStaticPropWarning(fragmentInput, "first argument of ".concat(componentDisplayName));
  var fragmentNode = getFragment(fragmentInput);

  var _getPaginationMetadat = getPaginationMetadata(fragmentNode, componentDisplayName),
      connectionPathInFragmentData = _getPaginationMetadat.connectionPathInFragmentData,
      fragmentRefPathInResponse = _getPaginationMetadat.fragmentRefPathInResponse,
      paginationRequest = _getPaginationMetadat.paginationRequest,
      paginationMetadata = _getPaginationMetadat.paginationMetadata,
      stream = _getPaginationMetadat.stream;

  !(stream === false) ? process.env.NODE_ENV !== "production" ? invariant(false, 'Relay: @stream_connection is not compatible with `useBlockingPaginationFragment`. ' + 'Use `useStreamingPaginationFragment` instead.') : invariant(false) : void 0;

  var _useRefetchableFragme = useRefetchableFragmentNode(fragmentNode, parentFragmentRef, componentDisplayName),
      fragmentData = _useRefetchableFragme.fragmentData,
      fragmentRef = _useRefetchableFragme.fragmentRef,
      refetch = _useRefetchableFragme.refetch,
      disableStoreUpdates = _useRefetchableFragme.disableStoreUpdates,
      enableStoreUpdates = _useRefetchableFragme.enableStoreUpdates;

  var fragmentIdentifier = getFragmentIdentifier(fragmentNode, fragmentRef); // $FlowFixMe - TODO T39154660 Use FragmentPointer type instead of mixed

  var fragmentOwner = getFragmentOwner(fragmentNode, fragmentRef); // Backward pagination

  var _useLoadMore = useLoadMore({
    direction: 'backward',
    fragmentNode: fragmentNode,
    fragmentIdentifier: fragmentIdentifier,
    fragmentOwner: fragmentOwner,
    fragmentData: fragmentData,
    connectionPathInFragmentData: connectionPathInFragmentData,
    fragmentRefPathInResponse: fragmentRefPathInResponse,
    paginationRequest: paginationRequest,
    paginationMetadata: paginationMetadata,
    disableStoreUpdates: disableStoreUpdates,
    enableStoreUpdates: enableStoreUpdates,
    componentDisplayName: componentDisplayName
  }),
      loadPrevious = _useLoadMore[0],
      hasPrevious = _useLoadMore[1],
      disposeFetchPrevious = _useLoadMore[2]; // Forward pagination


  var _useLoadMore2 = useLoadMore({
    direction: 'forward',
    fragmentNode: fragmentNode,
    fragmentIdentifier: fragmentIdentifier,
    fragmentOwner: fragmentOwner,
    fragmentData: fragmentData,
    connectionPathInFragmentData: connectionPathInFragmentData,
    fragmentRefPathInResponse: fragmentRefPathInResponse,
    paginationRequest: paginationRequest,
    paginationMetadata: paginationMetadata,
    disableStoreUpdates: disableStoreUpdates,
    enableStoreUpdates: enableStoreUpdates,
    componentDisplayName: componentDisplayName
  }),
      loadNext = _useLoadMore2[0],
      hasNext = _useLoadMore2[1],
      disposeFetchNext = _useLoadMore2[2];

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
    refetch: refetchPagination
  };
}

function useLoadMore(args) {
  var disableStoreUpdates = args.disableStoreUpdates,
      enableStoreUpdates = args.enableStoreUpdates,
      loadMoreArgs = (0, _objectWithoutPropertiesLoose2["default"])(args, ["disableStoreUpdates", "enableStoreUpdates"]);

  var _useState = useState(null),
      requestPromise = _useState[0],
      setRequestPromise = _useState[1];

  var requestPromiseRef = useRef(null);
  var promiseResolveRef = useRef(null);

  var promiseResolve = function promiseResolve() {
    if (promiseResolveRef.current != null) {
      promiseResolveRef.current();
      promiseResolveRef.current = null;
    }
  };

  var handleReset = function handleReset() {
    promiseResolve();
  };

  var observer = {
    complete: promiseResolve,
    // NOTE: loadMore is a no-op if a request is already in flight, so we
    // can safely assume that `start` will only be called once while a
    // request is in flight.
    start: function start() {
      // NOTE: We disable store updates when we suspend to ensure
      // that higher-pri updates from the Relay store don't disrupt
      // any Suspense timeouts passed via withSuspenseConfig.
      disableStoreUpdates();
      var promise = new Promise(function (resolve) {
        promiseResolveRef.current = function () {
          requestPromiseRef.current = null;
          resolve();
        };
      });
      requestPromiseRef.current = promise;
      setRequestPromise(promise);
    },
    // NOTE: Since streaming is disallowed with this hook, this means that the
    // first payload will always contain the entire next page of items,
    // while subsequent paylaods will contain @defer'd payloads.
    // This allows us to unsuspend here, on the first payload, and allow
    // descendant components to suspend on their respective @defer payloads
    next: promiseResolve,
    // TODO: Handle error; we probably don't want to throw an error
    // and blow away the whole list of items.
    error: promiseResolve
  };

  var _useLoadMoreFunction = useLoadMoreFunction((0, _objectSpread2["default"])({}, loadMoreArgs, {
    observer: observer,
    onReset: handleReset
  })),
      _loadMore = _useLoadMoreFunction[0],
      hasMore = _useLoadMoreFunction[1],
      disposeFetch = _useLoadMoreFunction[2]; // NOTE: To determine if we need to suspend, we check that the promise in
  // state is the same as the promise on the ref, which ensures that we
  // wont incorrectly suspend on other higher-pri updates before the update
  // to suspend has committed.


  if (requestPromise != null && requestPromise === requestPromiseRef.current) {
    throw requestPromise;
  }

  useEffect(function () {
    if (requestPromise == null) {
      // NOTE: After suspense pagination has resolved, we re-enable store updates
      // for this fragment. This may cause the component to re-render if
      // we missed any updates to the fragment data other than the pagination update.
      enableStoreUpdates();
    } // NOTE: We know the identity of enableStoreUpdates wont change
    // eslint-disable-next-line react-hooks/exhaustive-deps

  }, [requestPromise]);
  var loadMore = useCallback(function () {
    if (Scheduler.unstable_getCurrentPriorityLevel() < Scheduler.unstable_NormalPriority) {
      process.env.NODE_ENV !== "production" ? warning(false, 'Relay: Unexpected call to `%s` at a priority higher than ' + 'expected on fragment `%s` in `%s`. It looks like you tried to ' + 'call `refetch` under a high priority update, but updates that ' + 'can cause the component to suspend should be scheduled at ' + 'normal priority. Make sure you are calling `refetch` inside ' + '`startTransition()` from the `useSuspenseTransition()` hook.', args.direction === 'forward' ? 'loadNext' : 'loadPrevious', args.fragmentNode.name, args.componentDisplayName) : void 0;
    }

    for (var _len = arguments.length, callArgs = new Array(_len), _key = 0; _key < _len; _key++) {
      callArgs[_key] = arguments[_key];
    }

    return _loadMore.apply(void 0, callArgs);
  }, [_loadMore, args.componentDisplayName, args.direction, args.fragmentNode.name]);
  return [loadMore, hasMore, disposeFetch];
}

module.exports = useBlockingPaginationFragment;