/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * 
 * @emails oncall+relay
 * @format
 */
'use strict';

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

var _objectSpread2 = _interopRequireDefault(require("@babel/runtime/helpers/objectSpread"));

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

var ExecutionEnvironment = require('fbjs/lib/ExecutionEnvironment');

var LRUCache = require('./LRUCache');

var invariant = require("fbjs/lib/invariant");

var _require = require('relay-runtime'),
    isPromise = _require.isPromise,
    RelayFeatureFlags = _require.RelayFeatureFlags;

var CACHE_CAPACITY = 1000;
var DEFAULT_FETCH_POLICY = 'store-or-network';
var DEFAULT_RENDER_POLICY = RelayFeatureFlags.ENABLE_PARTIAL_RENDERING_DEFAULT === true ? 'partial' : 'full';
var DATA_RETENTION_TIMEOUT = 30 * 1000;

function getQueryCacheKey(operation, fetchPolicy, renderPolicy) {
  return "".concat(fetchPolicy, "-").concat(renderPolicy, "-").concat(operation.request.identifier);
}

function getQueryResult(operation, cacheKey) {
  var rootFragmentRef = {
    __id: operation.fragment.dataID,
    __fragments: (0, _defineProperty2["default"])({}, operation.fragment.node.name, operation.request.variables),
    __fragmentOwner: operation.request
  };
  return {
    cacheKey: cacheKey,
    fragmentNode: operation.request.node.fragment,
    fragmentRef: rootFragmentRef,
    operation: operation
  };
}

function createCacheEntry(cacheKey, operation, value, networkSubscription, onDispose) {
  var currentValue = value;
  var retainCount = 0;
  var permanentlyRetained = false;
  var retainDisposable = null;
  var releaseTemporaryRetain = null;
  var currentNetworkSubscription = networkSubscription;

  var retain = function retain(environment) {
    retainCount++;

    if (retainCount === 1) {
      retainDisposable = environment.retain(operation.root);
    }

    return {
      dispose: function dispose() {
        retainCount = Math.max(0, retainCount - 1);

        if (retainCount === 0) {
          !(retainDisposable != null) ? process.env.NODE_ENV !== "production" ? invariant(false, 'Relay: Expected disposable to release query to be defined.' + "If you're seeing this, this is likely a bug in Relay.") : invariant(false) : void 0;
          retainDisposable.dispose();
          retainDisposable = null;
        }

        onDispose(cacheEntry);
      }
    };
  };

  var cacheEntry = {
    cacheKey: cacheKey,
    getValue: function getValue() {
      return currentValue;
    },
    setValue: function setValue(val) {
      currentValue = val;
    },
    getRetainCount: function getRetainCount() {
      return retainCount;
    },
    getNetworkSubscription: function getNetworkSubscription() {
      return currentNetworkSubscription;
    },
    setNetworkSubscription: function setNetworkSubscription(subscription) {
      if (currentNetworkSubscription != null) {
        currentNetworkSubscription.unsubscribe();
      }

      currentNetworkSubscription = subscription;
    },
    temporaryRetain: function temporaryRetain(environment) {
      // NOTE: If we're executing in a server environment, there's no need
      // to create temporary retains, since the component will never commit.
      if (!ExecutionEnvironment.canUseDOM) {
        return {
          dispose: function dispose() {}
        };
      }

      if (permanentlyRetained === true) {
        return {
          dispose: function dispose() {}
        };
      } // NOTE: temporaryRetain is called during the render phase. However,
      // given that we can't tell if this render will eventually commit or not,
      // we create a timer to autodispose of this retain in case the associated
      // component never commits.
      // If the component /does/ commit, permanentRetain will clear this timeout
      // and permanently retain the data.


      var disposable = retain(environment);
      var releaseQueryTimeout = null;

      var localReleaseTemporaryRetain = function localReleaseTemporaryRetain() {
        clearTimeout(releaseQueryTimeout);
        releaseQueryTimeout = null;
        releaseTemporaryRetain = null;
        disposable.dispose();
      };

      releaseQueryTimeout = setTimeout(localReleaseTemporaryRetain, DATA_RETENTION_TIMEOUT); // NOTE: Since temporaryRetain can be called multiple times, we release
      // the previous temporary retain after we re-establish a new one, since
      // we only ever need a single temporary retain until the permanent retain is
      // established.
      // temporaryRetain may be called multiple times by React during the render
      // phase, as well multiple times by other query components that are
      // rendering the same query/variables.

      if (releaseTemporaryRetain != null) {
        releaseTemporaryRetain();
      }

      releaseTemporaryRetain = localReleaseTemporaryRetain;
      return {
        dispose: function dispose() {
          if (permanentlyRetained === true) {
            return;
          }

          releaseTemporaryRetain && releaseTemporaryRetain();
        }
      };
    },
    permanentRetain: function permanentRetain(environment) {
      var disposable = retain(environment);

      if (releaseTemporaryRetain != null) {
        releaseTemporaryRetain();
        releaseTemporaryRetain = null;
      }

      permanentlyRetained = true;
      return {
        dispose: function dispose() {
          disposable.dispose();

          if (retainCount <= 0 && currentNetworkSubscription != null) {
            currentNetworkSubscription.unsubscribe();
          }

          permanentlyRetained = false;
        }
      };
    }
  };
  return cacheEntry;
}

var QueryResourceImpl =
/*#__PURE__*/
function () {
  function QueryResourceImpl(environment) {
    var _this = this;

    (0, _defineProperty2["default"])(this, "_clearCacheEntry", function (cacheEntry) {
      if (cacheEntry.getRetainCount() <= 0) {
        _this._cache["delete"](cacheEntry.cacheKey);
      }
    });
    this._environment = environment;
    this._cache = LRUCache.create(CACHE_CAPACITY);
  }
  /**
   * This function should be called during a Component's render function,
   * to either read an existing cached value for the query, or fetch the query
   * and suspend.
   */


  var _proto = QueryResourceImpl.prototype;

  _proto.prepare = function prepare(operation, fetchObservable, maybeFetchPolicy, maybeRenderPolicy, observer, cacheKeyBuster) {
    var _maybeFetchPolicy, _maybeRenderPolicy;

    var environment = this._environment;
    var fetchPolicy = (_maybeFetchPolicy = maybeFetchPolicy) !== null && _maybeFetchPolicy !== void 0 ? _maybeFetchPolicy : DEFAULT_FETCH_POLICY;
    var renderPolicy = (_maybeRenderPolicy = maybeRenderPolicy) !== null && _maybeRenderPolicy !== void 0 ? _maybeRenderPolicy : DEFAULT_RENDER_POLICY;
    var cacheKey = getQueryCacheKey(operation, fetchPolicy, renderPolicy);

    if (cacheKeyBuster != null) {
      cacheKey += "-".concat(cacheKeyBuster);
    } // 1. Check if there's a cached value for this operation, and reuse it if
    // it's available


    var cacheEntry = this._cache.get(cacheKey);

    var temporaryRetainDisposable = null;

    if (cacheEntry == null) {
      // 2. If a cached value isn't available, try fetching the operation.
      // fetchAndSaveQuery will update the cache with either a Promise or
      // an Error to throw, or a FragmentResource to return.
      cacheEntry = this._fetchAndSaveQuery(cacheKey, operation, fetchObservable, fetchPolicy, renderPolicy, (0, _objectSpread2["default"])({}, observer, {
        unsubscribe: function unsubscribe(subscription) {
          // 4. If the request is cancelled, make sure to dispose
          // of the temporary retain; this will ensure that a promise
          // doesn't remain unnecessarilly cached until the temporary retain
          // expires. Not clearing the temporary retain might cause the
          // query to incorrectly re-suspend.
          if (temporaryRetainDisposable != null) {
            temporaryRetainDisposable.dispose();
          }

          var observerUnsubscribe = observer === null || observer === void 0 ? void 0 : observer.unsubscribe;
          observerUnsubscribe && observerUnsubscribe(subscription);
        }
      }));
    } // 3. Temporarily retain here in render phase. When the Component reading
    // the operation is committed, we will transfer ownership of data retention
    // to the component.
    // In case the component never commits (mounts or updates) from this render,
    // this data retention hold will auto-release itself afer a timeout.


    temporaryRetainDisposable = cacheEntry.temporaryRetain(environment);
    var cachedValue = cacheEntry.getValue();

    if (isPromise(cachedValue) || cachedValue instanceof Error) {
      throw cachedValue;
    }

    return cachedValue;
  }
  /**
   * This function should be called during a Component's commit phase
   * (e.g. inside useEffect), in order to retain the operation in the Relay store
   * and transfer ownership of the operation to the component lifecycle.
   */
  ;

  _proto.retain = function retain(queryResult) {
    var environment = this._environment;
    var cacheKey = queryResult.cacheKey,
        operation = queryResult.operation;

    var cacheEntry = this._getOrCreateCacheEntry(cacheKey, operation, queryResult, null);

    var disposable = cacheEntry.permanentRetain(environment);
    return {
      dispose: function dispose() {
        disposable.dispose();
      }
    };
  };

  _proto.getCacheEntry = function getCacheEntry(operation, fetchPolicy, maybeRenderPolicy) {
    var _maybeRenderPolicy2;

    var renderPolicy = (_maybeRenderPolicy2 = maybeRenderPolicy) !== null && _maybeRenderPolicy2 !== void 0 ? _maybeRenderPolicy2 : DEFAULT_RENDER_POLICY;
    var cacheKey = getQueryCacheKey(operation, fetchPolicy, renderPolicy);
    return this._cache.get(cacheKey);
  };

  _proto._getOrCreateCacheEntry = function _getOrCreateCacheEntry(cacheKey, operation, value, networkSubscription) {
    var cacheEntry = this._cache.get(cacheKey);

    if (cacheEntry == null) {
      cacheEntry = createCacheEntry(cacheKey, operation, value, networkSubscription, this._clearCacheEntry);

      this._cache.set(cacheKey, cacheEntry);
    }

    return cacheEntry;
  };

  _proto._fetchAndSaveQuery = function _fetchAndSaveQuery(cacheKey, operation, fetchObservable, fetchPolicy, renderPolicy, observer) {
    var _this2 = this;

    var environment = this._environment; // NOTE: Running `check` will write missing data to the store using any
    // missing data handlers specified on the environment;
    // We run it here first to make the handlers get a chance to populate
    // missing data.

    var hasFullQuery = environment.check(operation.root);
    var canPartialRender = hasFullQuery || renderPolicy === 'partial';
    var shouldFetch;
    var shouldAllowRender;

    var resolveNetworkPromise = function resolveNetworkPromise() {};

    switch (fetchPolicy) {
      case 'store-only':
        {
          shouldFetch = false;
          shouldAllowRender = true;
          break;
        }

      case 'store-or-network':
        {
          shouldFetch = !hasFullQuery;
          shouldAllowRender = canPartialRender;
          break;
        }

      case 'store-and-network':
        {
          shouldFetch = true;
          shouldAllowRender = canPartialRender;
          break;
        }

      case 'network-only':
      default:
        {
          shouldFetch = true;
          shouldAllowRender = false;
          break;
        }
    } // NOTE: If this value is false, we will cache a promise for this
    // query, which means we will suspend here at this query root.
    // If it's true, we will cache the query resource and allow rendering to
    // continue.


    if (shouldAllowRender) {
      var queryResult = getQueryResult(operation, cacheKey);

      var _cacheEntry = createCacheEntry(cacheKey, operation, queryResult, null, this._clearCacheEntry);

      this._cache.set(cacheKey, _cacheEntry);
    }

    environment.__log({
      name: 'queryresource.fetch',
      operation: operation,
      fetchPolicy: fetchPolicy,
      renderPolicy: renderPolicy,
      hasFullQuery: hasFullQuery,
      shouldFetch: shouldFetch
    });

    if (shouldFetch) {
      var _queryResult = getQueryResult(operation, cacheKey);

      var networkSubscription;
      fetchObservable.subscribe({
        start: function start(subscription) {
          networkSubscription = subscription;

          var cacheEntry = _this2._cache.get(cacheKey);

          if (cacheEntry) {
            cacheEntry.setNetworkSubscription(networkSubscription);
          }

          var observerStart = observer === null || observer === void 0 ? void 0 : observer.start;
          observerStart && observerStart(subscription);
        },
        next: function next() {
          var snapshot = environment.lookup(operation.fragment);

          var cacheEntry = _this2._getOrCreateCacheEntry(cacheKey, operation, _queryResult, networkSubscription);

          cacheEntry.setValue(_queryResult);
          resolveNetworkPromise();
          var observerNext = observer === null || observer === void 0 ? void 0 : observer.next;
          observerNext && observerNext(snapshot);
        },
        error: function error(_error) {
          var cacheEntry = _this2._getOrCreateCacheEntry(cacheKey, operation, _error, networkSubscription);

          cacheEntry.setValue(_error);
          resolveNetworkPromise();
          networkSubscription = null;
          cacheEntry.setNetworkSubscription(null);
          var observerError = observer === null || observer === void 0 ? void 0 : observer.error;
          observerError && observerError(_error);
        },
        complete: function complete() {
          resolveNetworkPromise();
          networkSubscription = null;

          var cacheEntry = _this2._cache.get(cacheKey);

          if (cacheEntry) {
            cacheEntry.setNetworkSubscription(null);
          }

          var observerComplete = observer === null || observer === void 0 ? void 0 : observer.complete;
          observerComplete && observerComplete();
        },
        unsubscribe: observer === null || observer === void 0 ? void 0 : observer.unsubscribe
      });

      var _cacheEntry2 = this._cache.get(cacheKey);

      if (!_cacheEntry2) {
        var networkPromise = new Promise(function (resolve) {
          resolveNetworkPromise = resolve;
        }); // $FlowExpectedError Expando to annotate Promises.

        networkPromise.displayName = 'Relay(' + operation.fragment.node.name + ')';
        _cacheEntry2 = createCacheEntry(cacheKey, operation, networkPromise, networkSubscription, this._clearCacheEntry);

        this._cache.set(cacheKey, _cacheEntry2);
      }
    } else {
      var observerComplete = observer === null || observer === void 0 ? void 0 : observer.complete;
      observerComplete && observerComplete();
    }

    var cacheEntry = this._cache.get(cacheKey);

    !(cacheEntry != null) ? process.env.NODE_ENV !== "production" ? invariant(false, 'Relay: Expected to have cached a result when attempting to fetch query.' + "If you're seeing this, this is likely a bug in Relay.") : invariant(false) : void 0;
    return cacheEntry;
  };

  return QueryResourceImpl;
}();

function createQueryResource(environment) {
  return new QueryResourceImpl(environment);
}

var dataResources = new Map();

function getQueryResourceForEnvironment(environment) {
  var cached = dataResources.get(environment);

  if (cached) {
    return cached;
  }

  var newDataResource = createQueryResource(environment);
  dataResources.set(environment, newDataResource);
  return newDataResource;
}

module.exports = {
  createQueryResource: createQueryResource,
  getQueryResourceForEnvironment: getQueryResourceForEnvironment
};