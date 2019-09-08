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

var getRefetchMetadata = require('./getRefetchMetadata');

var invariant = require("fbjs/lib/invariant");

function getPaginationMetadata(fragmentNode, componentDisplayName) {
  var _ref, _fragmentNode$metadat;

  var _getRefetchMetadata = getRefetchMetadata(fragmentNode, componentDisplayName),
      paginationRequest = _getRefetchMetadata.refetchableRequest,
      fragmentRefPathInResponse = _getRefetchMetadata.fragmentRefPathInResponse,
      refetchMetadata = _getRefetchMetadata.refetchMetadata;

  var paginationMetadata = refetchMetadata.connection;
  !(paginationMetadata != null) ? process.env.NODE_ENV !== "production" ? invariant(false, 'Relay: getPaginationMetadata(): Expected fragment `%s` to include a ' + 'connection when using `%s`. Did you forget to add a @connection ' + 'directive to the connection field in the fragment?', componentDisplayName, fragmentNode.name) : invariant(false) : void 0;
  var connectionPathInFragmentData = paginationMetadata.path;
  var connectionMetadata = ((_ref = (_fragmentNode$metadat = fragmentNode.metadata) === null || _fragmentNode$metadat === void 0 ? void 0 : _fragmentNode$metadat.connection) !== null && _ref !== void 0 ? _ref : [])[0];
  !(connectionMetadata != null) ? process.env.NODE_ENV !== "production" ? invariant(false, 'Relay: getPaginationMetadata(): Expected fragment `%s` to include a ' + 'connection when using `%s`. Did you forget to add a @connection ' + 'directive to the connection field in the fragment?', componentDisplayName, fragmentNode.name) : invariant(false) : void 0;
  return {
    connectionPathInFragmentData: connectionPathInFragmentData,
    fragmentRefPathInResponse: fragmentRefPathInResponse,
    paginationRequest: paginationRequest,
    paginationMetadata: paginationMetadata,
    stream: connectionMetadata.stream === true
  };
}

module.exports = getPaginationMetadata;