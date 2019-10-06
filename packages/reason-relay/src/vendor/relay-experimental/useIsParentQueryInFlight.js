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

var React = require('react');

var invariant = require("fbjs/lib/invariant");

var useRelayEnvironment = require('./useRelayEnvironment');

var useStaticFragmentNodeWarning = require('./useStaticFragmentNodeWarning');

var _require = require('relay-runtime'),
    getObservableForRequestInFlight = _require.__internal.getObservableForRequestInFlight,
    getFragment = _require.getFragment,
    getSelector = _require.getSelector;

var useEffect = React.useEffect,
    useState = React.useState,
    useMemo = React.useMemo;

function useIsParentQueryInFlight(fragmentInput, fragmentRef) {
  var environment = useRelayEnvironment();
  var fragmentNode = getFragment(fragmentInput);
  useStaticFragmentNodeWarning(fragmentNode, 'first argument of useIsParentQueryInFlight()');
  var observable = useMemo(function () {
    var selector = getSelector(fragmentNode, fragmentRef);

    if (selector == null) {
      return null;
    }

    !(selector.kind === 'SingularReaderSelector') ? process.env.NODE_ENV !== "production" ? invariant(false, 'useIsParentQueryInFlight: Plural fragments are not supported.') : invariant(false) : void 0;
    return getObservableForRequestInFlight(environment, selector.owner);
  }, [environment, fragmentNode, fragmentRef]);

  var _useState = useState(observable != null),
      isInFlight = _useState[0],
      setIsInFlight = _useState[1];

  useEffect(function () {
    var subscription;
    setIsInFlight(observable != null);

    if (observable != null) {
      var onCompleteOrError = function onCompleteOrError() {
        setIsInFlight(false);
      };

      subscription = observable.subscribe({
        complete: onCompleteOrError,
        error: onCompleteOrError
      });
    }

    return function () {
      if (subscription) {
        subscription.unsubscribe();
      }
    };
  }, [observable]);
  return isInFlight;
}

module.exports = useIsParentQueryInFlight;