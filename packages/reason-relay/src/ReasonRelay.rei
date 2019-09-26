type jsObj('a) = Js.t({..} as 'a);

/**
 * Abstract helper type to signify something that could not be
 * generated in a type-safe way.
 */

type any;

/**
 * The type of the actual node that Relay uses for operations.
 */
type queryNode;
type fragmentNode;
type mutationNode;
type subscriptionNode;

/**
 * Store and updaters
 */
type dataId;

let dataIdToString: dataId => string;
let makeDataId: string => dataId;

/**
 * Experimental stuff
 */

module ReactSuspenseConfig: {
  type t;
  let make:
    (
      ~timeOutMs: float,
      ~busyDelayMs: float=?,
      ~busyMinDurationMs: float=?,
      unit
    ) =>
    t;
};

let unstable_withSuspenseConfig: (unit => unit, ReactSuspenseConfig.t) => unit;

/**
 * Store helpers.
 * We modify most store primitives to return options instead of nullables.
 */
module RecordProxy: {
  type t;
  type arguments('a) = jsObj('a) constraint 'a = {..};

  let copyFieldsFrom: (t, ~sourceRecord: t) => unit;

  let getDataId: t => dataId;

  let getLinkedRecord:
    (t, ~name: string, ~arguments: option(arguments({..}))) => option(t);

  let getLinkedRecords:
    (t, ~name: string, ~arguments: option(arguments({..}))) =>
    option(array(option(t)));

  let getOrCreateLinkedRecord:
    (
      t,
      ~name: string,
      ~typeName: string,
      ~arguments: option(arguments({..}))
    ) =>
    t;

  let getType: t => string;

  let getValueString:
    (t, ~name: string, ~arguments: option(arguments({..}))) => option(string);

  let getValueStringArray:
    (t, ~name: string, ~arguments: option(arguments({..}))) =>
    option(array(option(string)));

  let getValueInt:
    (t, ~name: string, ~arguments: option(arguments({..}))) => option(int);

  let getValueIntArray:
    (t, ~name: string, ~arguments: option(arguments({..}))) =>
    option(array(option(int)));

  let getValueFloat:
    (t, ~name: string, ~arguments: option(arguments({..}))) => option(float);

  let getValueFloatArray:
    (t, ~name: string, ~arguments: option(arguments({..}))) =>
    option(array(option(float)));

  let getValueBool:
    (t, ~name: string, ~arguments: option(arguments({..}))) => option(bool);

  let getValueBoolArray:
    (t, ~name: string, ~arguments: option(arguments({..}))) =>
    option(array(option(bool)));

  let setLinkedRecord:
    (t, ~record: t, ~name: string, ~arguments: option(arguments({..}))) => t;

  let setLinkedRecords:
    (
      t,
      ~records: array(option(t)),
      ~name: string,
      ~arguments: option(arguments({..}))
    ) =>
    t;

  let setValueString:
    (t, ~value: string, ~name: string, ~arguments: option(arguments({..}))) =>
    t;

  let setValueStringArray:
    (
      t,
      ~value: array(string),
      ~name: string,
      ~arguments: option(arguments({..}))
    ) =>
    t;

  let setValueInt:
    (t, ~value: int, ~name: string, ~arguments: option(arguments({..}))) => t;

  let setValueIntArray:
    (
      t,
      ~value: array(int),
      ~name: string,
      ~arguments: option(arguments({..}))
    ) =>
    t;

  let setValueFloat:
    (t, ~value: float, ~name: string, ~arguments: option(arguments({..}))) =>
    t;

  let setValueFloatArray:
    (
      t,
      ~value: array(float),
      ~name: string,
      ~arguments: option(arguments({..}))
    ) =>
    t;

  let setValueBool:
    (t, ~value: bool, ~name: string, ~arguments: option(arguments({..}))) => t;

  let setValueBoolArray:
    (
      t,
      ~value: array(bool),
      ~name: string,
      ~arguments: option(arguments({..}))
    ) =>
    t;
};

module RecordSourceSelectorProxy: {
  type t;

  let create: (t, ~dataId: dataId, ~typeName: string) => RecordProxy.t;

  let delete: (t, ~dataId: dataId) => unit;

  let get: (t, ~dataId: dataId) => option(RecordProxy.t);

  let getRoot: t => RecordProxy.t;

  let getRootField: (t, ~fieldName: string) => option(RecordProxy.t);

  let getPluralRootField:
    (t, ~fieldName: string) => option(array(option(RecordProxy.t)));
};

module RecordSourceProxy: {
  type t;

  let create: (t, ~dataId: dataId, ~typeName: string) => RecordProxy.t;

  let delete: (t, ~dataId: dataId) => unit;

  let get: (t, ~dataId: dataId) => option(RecordProxy.t);

  let getRoot: t => RecordProxy.t;
};

module ConnectionHandler: {
  type filters('a) = jsObj('a) constraint 'a = {..};

  let getConnection:
    (~record: RecordProxy.t, ~key: string, ~filters: option(filters({..}))) =>
    option(RecordProxy.t);

  let createEdge:
    (
      ~store: RecordSourceProxy.t,
      ~connection: RecordProxy.t,
      ~node: RecordProxy.t,
      ~edgeType: string
    ) =>
    RecordProxy.t;

  let insertEdgeBefore:
    (
      ~connection: RecordProxy.t,
      ~newEdge: RecordProxy.t,
      ~cursor: option(string)
    ) =>
    unit;

  let insertEdgeAfter:
    (
      ~connection: RecordProxy.t,
      ~newEdge: RecordProxy.t,
      ~cursor: option(string)
    ) =>
    unit;

  let deleteNode: (~connection: RecordProxy.t, ~nodeId: dataId) => unit;
};

/**
 * QUERY
 */

module Disposable: {
  type t;
  let dispose: t => unit;
};

module CacheConfig: {
  type t;
  type config = {
    force: option(bool),
    poll: option(int),
    liveConfigId: option(string),
    transactionId: option(string),
  };

  let make:
    (
      ~force: option(bool),
      ~poll: option(int),
      ~liveConfigId: option(string),
      ~transactionId: option(string)
    ) =>
    t;

  let getConfig: t => config;
};

type fetchPolicy =
  | StoreOnly
  | StoreOrNetwork
  | StoreAndNetwork
  | NetworkOnly;

module type MakeUseQueryConfig = {
  type response;
  type variables;
  let query: queryNode;
};

module MakeUseQuery:
  (C: MakeUseQueryConfig) =>
   {
    type response = C.response;
    type variables = C.variables;
    let use:
      (
        ~variables: variables,
        ~fetchPolicy: fetchPolicy=?,
        ~fetchKey: string=?,
        ~networkCacheConfig: CacheConfig.t=?,
        unit
      ) =>
      response;
  };

/**
 * FRAGMENT
 */

module type MakeUseFragmentConfig = {
  type fragment;
  type fragmentRef;
  let fragmentSpec: fragmentNode;
};

module MakeUseFragment:
  (C: MakeUseFragmentConfig) => {let use: C.fragmentRef => C.fragment;};

/** Refetchable */
type refetchFn('variables) =
  (
    ~variables: 'variables,
    ~fetchPolicy: fetchPolicy=?,
    ~onComplete: option(Js.Exn.t) => unit=?,
    unit
  ) =>
  unit;

module type MakeUseRefetchableFragmentConfig = {
  type fragment;
  type variables;
  type fragmentRef;
  let fragmentSpec: fragmentNode;
};

module MakeUseRefetchableFragment:
  (C: MakeUseRefetchableFragmentConfig) =>
   {
    let useRefetchable:
      C.fragmentRef => (C.fragment, refetchFn(C.variables));
  };

/** Pagination */
module type MakeUsePaginationFragmentConfig = {
  type fragment;
  type variables;
  type fragmentRef;
  let fragmentSpec: fragmentNode;
};

type paginationLoadMoreFn =
  (~count: int, ~onComplete: option(option(Js.Exn.t) => unit)) =>
  Disposable.t;

type paginationBlockingFragmentReturn('fragmentData, 'variables) = {
  data: 'fragmentData,
  loadNext: paginationLoadMoreFn,
  loadPrevious: paginationLoadMoreFn,
  hasNext: bool,
  hasPrevious: bool,
  refetch: refetchFn('variables),
};

type paginationLegacyFragmentReturn('fragmentData, 'variables) = {
  data: 'fragmentData,
  loadNext: paginationLoadMoreFn,
  loadPrevious: paginationLoadMoreFn,
  hasNext: bool,
  hasPrevious: bool,
  isLoadingNext: bool,
  isLoadingPrevious: bool,
  refetch: refetchFn('variables),
};

module MakeUsePaginationFragment:
  (C: MakeUsePaginationFragmentConfig) =>
   {
    let useBlockingPagination:
      C.fragmentRef =>
      paginationBlockingFragmentReturn(C.fragment, C.variables);

    let useLegacyPagination:
      C.fragmentRef => paginationLegacyFragmentReturn(C.fragment, C.variables);
  };

/**
 * MUTATION
 */

module type MutationConfig = {
  type variables;
  type response;
  let node: mutationNode;
};

type updaterFn = RecordSourceSelectorProxy.t => unit;

type mutationError = {. "message": string};

type mutationState('response) =
  | Loading
  | Error(mutationError)
  | Success(option('response));

type mutationResult('response) =
  | Success('response)
  | Error(Js.Promise.error);

type useMutationConfigType('variables) = {variables: 'variables};

module MakeUseMutation:
  (C: MutationConfig) =>
   {
    let use:
      unit =>
      (
        (
          ~variables: C.variables,
          ~optimisticResponse: C.response=?,
          ~optimisticUpdater: RecordSourceSelectorProxy.t => unit=?,
          ~updater: updaterFn=?,
          unit
        ) =>
        Js.Promise.t(mutationResult(C.response)),
        mutationState(C.response),
      );
  };

/**
 * NETWORK
 */

module Observable: {
  type t;

  type sink('t) = {
    next: 't => unit,
    error: Js.Exn.t => unit,
    completed: unit => unit,
    closed: bool,
  };

  let make: (sink('t) => unit) => t;
};

module Network: {
  type t;

  type operation = {
    .
    "name": string,
    "operationKind": string,
    "text": string,
  };
  type subscribeFn =
    {
      .
      "request": operation,
      "variables": Js.Json.t,
      "cacheConfig": CacheConfig.t,
    } =>
    Observable.t;

  type fetchFunctionPromise =
    (operation, Js.Json.t, CacheConfig.t) => Js.Promise.t(Js.Json.t);

  type fetchFunctionObservable =
    (operation, Js.Json.t, CacheConfig.t) => Observable.t;

  let makePromiseBased:
    (
      ~fetchFunction: fetchFunctionPromise,
      ~subscriptionFunction: subscribeFn=?,
      unit
    ) =>
    t;

  let makeObservableBased:
    (
      ~observableFunction: fetchFunctionObservable,
      ~subscriptionFunction: subscribeFn=?,
      unit
    ) =>
    t;
};

/**
 * STORE
 */

module RecordSource: {
  type t;
  let make: unit => t;
};

module Store: {
  type t;
  let make: RecordSource.t => t;
};

/**
 * ENVIRONMENT
 */
module Environment: {
  type t;

  let make:
    (
      ~network: Network.t,
      ~store: Store.t,
      ~getDataID: (~nodeObj: 'a, ~typeName: string) => string=?,
      unit
    ) =>
    t;
};

module Context: {
  type t;
  type contextShape = {. "environment": Environment.t};
  let context: React.Context.t(option(contextShape));

  module Provider: {
    let makeProps:
      (
        ~environment: Environment.t,
        ~children: 'children,
        ~key: string=?,
        unit
      ) =>
      {
        .
        "children": 'children,
        "environment": Environment.t,
      };
    let make:
      {
        .
        "children": React.element,
        "environment": Environment.t,
      } =>
      React.element;
  };
};

exception EnvironmentNotFoundInContext;

let useEnvironmentFromContext: unit => Environment.t;

exception Mutation_failed(array(mutationError));

module MakeCommitMutation:
  (C: MutationConfig) =>
   {
    let commitMutation:
      (
        ~environment: Environment.t,
        ~variables: C.variables,
        ~optimisticUpdater: RecordSourceSelectorProxy.t => unit=?,
        ~optimisticResponse: C.response=?,
        ~updater: RecordSourceSelectorProxy.t => unit=?,
        unit
      ) =>
      Js.Promise.t(C.response);
  };

let commitLocalUpdate:
  (
    ~environment: Environment.t,
    ~updater: RecordSourceSelectorProxy.t => unit
  ) =>
  unit;

let fetchQuery:
  (Environment.t, queryNode, 'variables) => Js.Promise.t('response);

/**
 * SUBSCRIPTIONS
 */
module type SubscriptionConfig = {
  type variables;
  type response;
  let node: subscriptionNode;
};

module MakeUseSubscription:
  (C: SubscriptionConfig) =>
   {
    let subscribe:
      (
        ~environment: Environment.t,
        ~variables: C.variables,
        ~onCompleted: unit => unit=?,
        ~onError: Js.Exn.t => unit=?,
        ~onNext: C.response => unit=?,
        ~updater: updaterFn=?,
        unit
      ) =>
      Disposable.t;
  };