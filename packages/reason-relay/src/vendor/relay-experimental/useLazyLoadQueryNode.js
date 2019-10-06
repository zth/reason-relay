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

var ProfilerContext = require('./ProfilerContext');

var React = require('react');

var useFetchTrackingRef = require('./useFetchTrackingRef');

var useFragmentNode = require('./useFragmentNode');

var useRelayEnvironment = require('./useRelayEnvironment');

var _require = require('./QueryResource'),
    getQueryResourceForEnvironment = _require.getQueryResourceForEnvironment;

var _require2 = require('relay-runtime'),
    fetchQuery = _require2.__internal.fetchQuery;

var useContext = React.useContext,
    useEffect = React.useEffect;

function useLazyLoadQueryNode(args) {
  var _args$fetchObservable;

  var environment = useRelayEnvironment();
  var profilerContext = useContext(ProfilerContext);
  var QueryResource = getQueryResourceForEnvironment(environment);
  var query = args.query,
      componentDisplayName = args.componentDisplayName,
      fetchKey = args.fetchKey,
      fetchPolicy = args.fetchPolicy;
  var fetchObservable = (_args$fetchObservable = args.fetchObservable) !== null && _args$fetchObservable !== void 0 ? _args$fetchObservable : fetchQuery(environment, query, {
    networkCacheConfig: args.networkCacheConfig
  });

  var _useFetchTrackingRef = useFetchTrackingRef(),
      startFetch = _useFetchTrackingRef.startFetch,
      completeFetch = _useFetchTrackingRef.completeFetch;

  var preparedQueryResult = profilerContext.wrapPrepareQueryResource(function () {
    return QueryResource.prepare(query, fetchObservable, fetchPolicy, null, {
      start: startFetch,
      complete: completeFetch,
      error: completeFetch
    }, fetchKey);
  });
  useEffect(function () {
    var disposable = QueryResource.retain(preparedQueryResult);
    return function () {
      disposable.dispose();
    }; // NOTE: We disable react-hooks-deps warning because the `environment`
    // and `query` identities are capturing all information about whether
    // the effect should be re-ran and the query re-retained.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [environment, query]);
  var fragmentNode = preparedQueryResult.fragmentNode,
      fragmentRef = preparedQueryResult.fragmentRef;

  var _useFragmentNode = useFragmentNode(fragmentNode, fragmentRef, componentDisplayName),
      data = _useFragmentNode.data;

  return data;
}

module.exports = useLazyLoadQueryNode;