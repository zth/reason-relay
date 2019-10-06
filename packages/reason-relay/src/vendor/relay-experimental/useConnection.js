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

var areEqual = require("fbjs/lib/areEqual");

var useRelayEnvironment = require('./useRelayEnvironment');

var _require = require('react'),
    useEffect = _require.useEffect,
    useState = _require.useState;

/**
 * Public hook for consuming the results of `@connection_resolver` and
 * `@stream_connection_resolver`. Given a resolver and a reference to a
 * connection, returns the latest results for that connection as derived by
 * that resolver.
 */
function useConnection(resolver, ref) {
  var _connectionState$refe;

  var environment = useRelayEnvironment();
  var reference = ref === null || ref === void 0 ? void 0 : ref.__connection; // Lazily initialize state, resetting if the inputs change

  var _useState = useState(function () {
    return buildConnectionState(environment, resolver, reference, null);
  }),
      connectionState = _useState[0],
      setConnectionState = _useState[1];

  if (connectionState.environment !== environment || ((_connectionState$refe = connectionState.reference) === null || _connectionState$refe === void 0 ? void 0 : _connectionState$refe.id) !== (reference === null || reference === void 0 ? void 0 : reference.id) || connectionState.resolver !== resolver) {
    setConnectionState(buildConnectionState(environment, resolver, reference, connectionState));
  }

  var connectionSnapshot = connectionState.snapshot;
  useEffect(function () {
    if (connectionSnapshot == null) {
      return;
    }

    var latestState = buildConnectionState(environment, resolver, reference, connectionState);

    if (latestState.snapshot != null && !areEqual(latestState.snapshot.state, connectionSnapshot.state)) {
      setConnectionState(latestState); // as an optimization, avoid subscribing until a render with fresh data

      return;
    }

    var store = environment.getStore();
    var disposable = store.subscribeConnection_UNSTABLE(connectionSnapshot, resolver, function (updatedSnapshot) {
      setConnectionState(function (currentConnectionState) {
        if (currentConnectionState.generation !== connectionState.generation) {
          // When the connection generation changes there can be a gap where
          // the previous generation is still subscribed (between rendering
          // w the new identity and the effect cleanup). Ignore these updates.
          return currentConnectionState;
        }

        return (0, _objectSpread2["default"])({}, currentConnectionState, {
          snapshot: updatedSnapshot
        });
      });
    });
    return function () {
      return disposable.dispose();
    }; // State.generation changes whenever environment/resolver/reference change
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [connectionState.generation]);
  return connectionSnapshot != null ? connectionSnapshot.state : connectionSnapshot;
}

function buildConnectionState(environment, resolver, reference, previousState) {
  var store = environment.getStore();
  var snapshot = reference != null ? store.lookupConnection_UNSTABLE(reference, resolver) : null;
  return {
    environment: environment,
    generation: previousState != null ? previousState.generation + 1 : 0,
    reference: reference,
    resolver: resolver,
    snapshot: snapshot
  };
}

module.exports = useConnection;