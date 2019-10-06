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

var mapObject = require("fbjs/lib/mapObject");

var useRelayEnvironment = require('./useRelayEnvironment');

var warning = require("fbjs/lib/warning");

var _require = require('./FragmentResource'),
    getFragmentResourceForEnvironment = _require.getFragmentResourceForEnvironment;

var _require2 = require('react'),
    useEffect = _require2.useEffect,
    useRef = _require2.useRef,
    useState = _require2.useState;

var _require3 = require('relay-runtime'),
    RelayProfiler = _require3.RelayProfiler,
    getFragmentSpecIdentifier = _require3.getFragmentSpecIdentifier,
    isScalarAndEqual = _require3.isScalarAndEqual;

function useFragmentNodes(fragmentNodes, props, componentDisplayName) {
  var environment = useRelayEnvironment();
  var FragmentResource = getFragmentResourceForEnvironment(environment);
  var isMountedRef = useRef(false);

  var _useState = useState(0),
      forceUpdate = _useState[1];

  var fragmentSpecIdentifier = getFragmentSpecIdentifier(fragmentNodes, props); // The values of these React refs are counters that should be incremented
  // under their respective conditions. This allows us to use the counters as
  // memoization values to indicate if computations for useMemo or useEffect
  // should be re-executed.

  var mustResubscribeGenerationRef = useRef(0);
  var shouldUpdateGenerationRef = useRef(0);
  var environmentChanged = useHasChanged(environment);
  var fragmentSpecIdentifierChanged = useHasChanged(fragmentSpecIdentifier); // If the fragment identifier changes, it means that the variables on the
  // fragment owner changed, or the fragment refs point to different records.
  // In this case, we need to resubscribe to the Relay store.

  var mustResubscribe = environmentChanged || fragmentSpecIdentifierChanged; // We mirror the props to check if they have changed between renders

  var _useState2 = useState(props),
      mirroredProps = _useState2[0],
      setMirroredProps = _useState2[1]; // `props` contains both fragment refs and regular component
  // props, so we extract here the props that aren't fragment refs.
  // TODO(T38931859) This can be simplified if we use named fragment refs


  var nonFragmentRefPropKeys = Object.keys(props).filter(function (key) {
    return !fragmentNodes.hasOwnProperty(key);
  });
  var nonFragmentRefPropsChanged = nonFragmentRefPropKeys.some(function (key) {
    return mirroredProps[key] !== props[key];
  });
  var scalarNonFragmentRefPropsChanged = nonFragmentRefPropKeys.some(function (key) {
    return !isScalarAndEqual(mirroredProps[key], props[key]);
  }); // We only want to update the component consuming this fragment under the
  // following circumstances:
  // - We receive an update from the Relay store, indicating that the data
  //   the component is directly subscribed to has changed.
  // - We need to subscribe and render /different/ data (i.e. the fragment refs
  //   now point to different records, or the context changed).
  //   Note that even if identity of the fragment ref objects changes, we
  //   don't consider them as different unless they point to a different data ID.
  // - Any props that are /not/ fragment refs have changed.
  //
  // This prevents unnecessary updates when a parent re-renders this component
  // with the same props, which is a common case when the parent updates due
  // to change in the data /it/ is subscribed to, but which doesn't affect the
  // child.

  var shouldUpdate = mustResubscribe || scalarNonFragmentRefPropsChanged;

  if (shouldUpdate) {
    shouldUpdateGenerationRef.current++;
  }

  if (mustResubscribe) {
    mustResubscribeGenerationRef.current++;
  } // Since `props` contains both fragment refs and regular props, we need to
  // ensure we keep the mirrored version in sync if non fragment ref props
  // change , to be able to compare them between renders


  if (nonFragmentRefPropsChanged) {
    setMirroredProps(props);
  } // Read fragment data; this might suspend.


  var fragmentSpecResult = FragmentResource.readSpec(fragmentNodes, props, componentDisplayName);
  var isListeningForUpdatesRef = useRef(true);

  function enableStoreUpdates() {
    isListeningForUpdatesRef.current = true;
    var didMissUpdates = FragmentResource.checkMissedUpdatesSpec(fragmentSpecResult);

    if (didMissUpdates) {
      handleDataUpdate();
    }
  }

  function disableStoreUpdates() {
    isListeningForUpdatesRef.current = false;
  }

  function handleDataUpdate() {
    var _shouldUpdateGenerati;

    if (isMountedRef.current === false || isListeningForUpdatesRef.current === false) {
      return;
    } // If we receive an update from the Relay store, we need to make sure the
    // consuming component updates.


    shouldUpdateGenerationRef.current = ((_shouldUpdateGenerati = shouldUpdateGenerationRef.current) !== null && _shouldUpdateGenerati !== void 0 ? _shouldUpdateGenerati : 0) + 1; // React bails out on noop state updates as an optimization.
    // If we want to force an update via setState, we need to pass an value.
    // The actual value can be arbitrary though, e.g. an incremented number.

    forceUpdate(function (count) {
      return count + 1;
    });
  } // Establish Relay store subscriptions in the commit phase, only if
  // rendering for the first time, or if we need to subscribe to new data


  useEffect(function () {
    isMountedRef.current = true;
    var disposable = FragmentResource.subscribeSpec(fragmentSpecResult, handleDataUpdate);
    return function () {
      // When unmounting or resubscribing to new data, clean up current
      // subscription. This will also make sure fragment data is no longer
      // cached for the so next time it its read, it will be read fresh from the
      // Relay store
      isMountedRef.current = false;
      disposable.dispose();
    }; // NOTE: We disable react-hooks-deps warning because mustResubscribeGenerationRef
    // is capturing all information about whether the effect should be re-ran.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [mustResubscribeGenerationRef.current]);
  var data = mapObject(fragmentSpecResult, function (result, key) {
    if (process.env.NODE_ENV !== "production") {
      if (props[key] != null && result.data == null) {
        var _ref, _fragmentNodes$key;

        var fragmentName = (_ref = (_fragmentNodes$key = fragmentNodes[key]) === null || _fragmentNodes$key === void 0 ? void 0 : _fragmentNodes$key.name) !== null && _ref !== void 0 ? _ref : 'Unknown fragment';
        process.env.NODE_ENV !== "production" ? warning(false, 'Relay: Expected to have been able to read non-null data for ' + 'fragment `%s` declared in ' + '`%s`, since fragment reference was non-null. ' + "Make sure that that `%s`'s parent isn't " + 'holding on to and/or passing a fragment reference for data that ' + 'has been deleted.', fragmentName, componentDisplayName, componentDisplayName) : void 0;
      }
    }

    return result.data;
  });
  return {
    // $FlowFixMe
    data: data,
    disableStoreUpdates: disableStoreUpdates,
    enableStoreUpdates: enableStoreUpdates,
    shouldUpdateGeneration: shouldUpdateGenerationRef.current
  };
}

function useHasChanged(value) {
  var _useState3 = useState(value),
      mirroredValue = _useState3[0],
      setMirroredValue = _useState3[1];

  var valueChanged = mirroredValue !== value;

  if (valueChanged) {
    setMirroredValue(value);
  }

  return valueChanged;
}

module.exports = RelayProfiler.instrument('useFragmentNodes', useFragmentNodes);