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

var ReactRelayContext = require('react-relay/ReactRelayContext');

var useMemo = React.useMemo;

function RelayEnvironmentProvider(props) {
  var children = props.children,
      environment = props.environment; // TODO(T39494051) - We're setting empty variables here to make Flow happy
  // and for backwards compatibility, while we remove variables from context
  // in favor of fragment ownershipt

  var context = useMemo(function () {
    return {
      environment: environment,
      variables: {}
    };
  }, [environment]);
  return React.createElement(ReactRelayContext.Provider, {
    value: context
  }, children);
}

module.exports = RelayEnvironmentProvider;