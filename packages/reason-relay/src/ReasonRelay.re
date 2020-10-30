type arguments;
type allFieldsMasked = {.};

type any;

type queryNode;
type fragmentNode;
type mutationNode;
type subscriptionNode;

type fragmentRefs('fragments);

type dataId;

type recordSourceRecords;

external dataIdToString: dataId => string = "%identity";
external makeDataId: string => dataId = "%identity";

external makeArguments: Js.t({..}) => arguments = "%identity";

[@bs.module "relay-runtime"]
external generateClientID:
  (~dataId: dataId, ~storageKey: string, ~index: int=?, unit) => dataId =
  "generateClientID";

[@bs.module "relay-runtime"]
external generateUniqueClientID: unit => dataId = "generateUniqueClientID";

[@bs.module "relay-runtime"]
external isClientID: dataId => bool = "isClientID";

type featureFlags = {
  [@bs.as "ENABLE_VARIABLE_CONNECTION_KEY"]
  mutable enableVariableConnectionKey: bool,
  [@bs.as "ENABLE_PARTIAL_RENDERING_DEFAULT"]
  mutable enablePartialRenderingDefault: bool,
  [@bs.as "ENABLE_RELAY_CONTAINERS_SUSPENSE"]
  mutable enableRelayContainersSuspense: bool,
  [@bs.as "ENABLE_PRECISE_TYPE_REFINEMENT"]
  mutable enablePrecisTypeRefinement: bool,
};

[@bs.module "relay-runtime"]
external relayFeatureFlags: featureFlags = "RelayFeatureFlags";
/**
 * Various helpers.
 */

// We occasionally have to remove undefined keys from objects, something I haven't figured out how to do with pure BuckleScript
let internal_cleanObjectFromUndefinedRaw = [%raw
  {|
  function (obj) {
    var newObj = {};

    Object.keys(obj).forEach(function(key) {
      if (typeof obj[key] !== 'undefined') {
        newObj[key] = obj[key];
      }
    });

    return newObj;
  }
|}
];

// Since BS compiles unit to 0, we have to convert that to an empty object when dealing with variables in order for Relay to be happy
let internal_cleanVariablesRaw = [%raw
  {|
  function (variables) {
    if (typeof variables !== "object" || variables == null) {
      return {};
    }

    return variables;
  }
|}
];

[@bs.module "./utils"]
external convertObj:
  ('a, Js.Dict.t(Js.Dict.t(Js.Dict.t(string))), 'b, 'c) => 'd =
  "traverser";

let optArrayOfNullableToOptArrayOfOpt:
  option(array(Js.Nullable.t('a))) => option(array(option('a))) =
  fun
  | None => None
  | Some(arr) => Some(arr->Belt.Array.map(Js.Nullable.toOption));

[@bs.module "relay-runtime"] external storeRootId: dataId = "ROOT_ID";
[@bs.module "relay-runtime"] external storeRootType: string = "ROOT_TYPE";

module RecordProxy = {
  type t;

  [@bs.send]
  external copyFieldsFrom: (t, ~sourceRecord: t) => unit = "copyFieldsFrom";

  [@bs.send] external getDataId: t => dataId = "getDataID";

  [@bs.send] [@bs.return nullable]
  external getLinkedRecord:
    (t, ~name: string, ~arguments: arguments=?, unit) => option(t) =
    "getLinkedRecord";

  [@bs.send] [@bs.return nullable]
  external getLinkedRecords:
    (t, string, option(arguments)) => option(array(Js.Nullable.t(t))) =
    "getLinkedRecords";

  let getLinkedRecords =
      (t, ~name, ~arguments=?, ()): option(array(option(t))) =>
    getLinkedRecords(t, name, arguments) |> optArrayOfNullableToOptArrayOfOpt;

  [@bs.send]
  external getOrCreateLinkedRecord:
    (t, ~name: string, ~typeName: string, ~arguments: arguments=?, unit) => t =
    "getOrCreateLinkedRecord";

  [@bs.send] external getType: t => string = "getType";

  [@bs.send] [@bs.return nullable]
  external getValueString:
    (t, ~name: string, ~arguments: arguments=?, unit) => option(string) =
    "getValue";

  [@bs.send] [@bs.return nullable]
  external getValueStringArray:
    (t, ~name: string, ~arguments: arguments=?, unit) =>
    option(array(option(string))) =
    "getValue";

  [@bs.send] [@bs.return nullable]
  external getValueInt:
    (t, ~name: string, ~arguments: arguments=?, unit) => option(int) =
    "getValue";

  [@bs.send] [@bs.return nullable]
  external getValueIntArray:
    (t, ~name: string, ~arguments: arguments=?, unit) =>
    option(array(option(int))) =
    "getValue";

  [@bs.send] [@bs.return nullable]
  external getValueFloat:
    (t, ~name: string, ~arguments: arguments=?, unit) => option(float) =
    "getValue";

  [@bs.send] [@bs.return nullable]
  external getValueFloatArray:
    (t, ~name: string, ~arguments: arguments=?, unit) =>
    option(array(option(float))) =
    "getValue";

  [@bs.send] [@bs.return nullable]
  external getValueBool:
    (t, ~name: string, ~arguments: arguments=?, unit) => option(bool) =
    "getValue";

  [@bs.send] [@bs.return nullable]
  external getValueBoolArray:
    (t, ~name: string, ~arguments: arguments=?, unit) =>
    option(array(option(bool))) =
    "getValue";

  [@bs.send]
  external setLinkedRecord:
    (t, ~record: t, ~name: string, ~arguments: arguments=?, unit) => t =
    "setLinkedRecord";

  [@bs.send]
  external setLinkedRecordToUndefined:
    (
      t,
      [@bs.as {json|undefined|json}] _,
      ~name: string,
      ~arguments: arguments=?,
      unit
    ) =>
    t =
    "setLinkedRecord";

  [@bs.send]
  external setLinkedRecordToNull:
    (
      t,
      [@bs.as {json|null|json}] _,
      ~name: string,
      ~arguments: arguments=?,
      unit
    ) =>
    t =
    "setLinkedRecord";

  [@bs.send]
  external setLinkedRecords:
    (
      t,
      ~records: array(option(t)),
      ~name: string,
      ~arguments: arguments=?,
      unit
    ) =>
    t =
    "setLinkedRecords";

  [@bs.send]
  external setLinkedRecordsToUndefined:
    (
      t,
      [@bs.as {json|undefined|json}] _,
      ~name: string,
      ~arguments: arguments=?,
      unit
    ) =>
    t =
    "setLinkedRecords";

  [@bs.send]
  external setLinkedRecordsToNull:
    (
      t,
      [@bs.as {json|null|json}] _,
      ~name: string,
      ~arguments: arguments=?,
      unit
    ) =>
    t =
    "setLinkedRecords";

  [@bs.send]
  external setValueToUndefined:
    (
      t,
      [@bs.as {json|undefined|json}] _,
      ~name: string,
      ~arguments: arguments=?,
      unit
    ) =>
    t =
    "setValue";

  [@bs.send]
  external setValueToNull:
    (
      t,
      [@bs.as {json|null|json}] _,
      ~name: string,
      ~arguments: arguments=?,
      unit
    ) =>
    t =
    "setValue";

  [@bs.send]
  external setValueString:
    (t, ~value: string, ~name: string, ~arguments: arguments=?, unit) => t =
    "setValue";

  [@bs.send]
  external setValueStringArray:
    (
      t,
      ~value: array(string),
      ~name: string,
      ~arguments: arguments=?,
      unit
    ) =>
    t =
    "setValue";

  [@bs.send]
  external setValueInt:
    (t, ~value: int, ~name: string, ~arguments: arguments=?, unit) => t =
    "setValue";

  [@bs.send]
  external setValueIntArray:
    (t, ~value: array(int), ~name: string, ~arguments: arguments=?, unit) => t =
    "setValue";

  [@bs.send]
  external setValueFloat:
    (t, ~value: float, ~name: string, ~arguments: arguments=?, unit) => t =
    "setValue";

  [@bs.send]
  external setValueFloatArray:
    (t, ~value: array(float), ~name: string, ~arguments: arguments=?, unit) =>
    t =
    "setValue";

  [@bs.send]
  external setValueBool:
    (t, ~value: bool, ~name: string, ~arguments: arguments=?, unit) => t =
    "setValue";

  [@bs.send]
  external setValueBoolArray:
    (t, ~value: array(bool), ~name: string, ~arguments: arguments=?, unit) =>
    t =
    "setValue";

  [@bs.send] external invalidateRecord: t => unit = "invalidateRecord";
};

module RecordSourceSelectorProxy = {
  type t;

  [@bs.send]
  external create: (t, ~dataId: dataId, ~typeName: string) => RecordProxy.t =
    "create";

  [@bs.send] external delete: (t, ~dataId: dataId) => unit = "delete";

  [@bs.send] [@bs.return nullable]
  external get: (t, ~dataId: dataId) => option(RecordProxy.t) = "get";

  [@bs.send] external getRoot: t => RecordProxy.t = "getRoot";

  [@bs.send] [@bs.return nullable]
  external getRootField: (t, ~fieldName: string) => option(RecordProxy.t) =
    "getRootField";

  [@bs.send] [@bs.return nullable]
  external getPluralRootField:
    (t, ~fieldName: string) => option(array(Js.Nullable.t(RecordProxy.t))) =
    "getPluralRootField";

  let getPluralRootField =
      (t, ~fieldName): option(array(option(RecordProxy.t))) =>
    getPluralRootField(t, ~fieldName) |> optArrayOfNullableToOptArrayOfOpt;

  [@bs.send] external invalidateStore: t => unit = "invalidateStore";
};

module RecordSourceProxy = {
  type t;

  [@bs.send]
  external create: (t, ~dataId: dataId, ~typeName: string) => RecordProxy.t =
    "create";

  [@bs.send] external delete: (t, ~dataId: dataId) => unit = "delete";

  [@bs.send] [@bs.return nullable]
  external get: (t, ~dataId: dataId) => option(RecordProxy.t) = "get";

  [@bs.send] external getRoot: t => RecordProxy.t = "getRoot";

  [@bs.send] external invalidateStore: t => unit = "invalidateStore";
};

module ConnectionHandler = {
  [@bs.module "relay-runtime"]
  [@bs.scope "ConnectionHandler"]
  [@bs.return nullable]
  external getConnection:
    (~record: RecordProxy.t, ~key: string, ~filters: arguments=?, unit) =>
    option(RecordProxy.t) =
    "getConnection";

  [@bs.module "relay-runtime"] [@bs.scope "ConnectionHandler"]
  external createEdge:
    (
      ~store: RecordSourceSelectorProxy.t,
      ~connection: RecordProxy.t,
      ~node: RecordProxy.t,
      ~edgeType: string
    ) =>
    RecordProxy.t =
    "createEdge";

  [@bs.module "relay-runtime"] [@bs.scope "ConnectionHandler"]
  external insertEdgeBefore:
    (
      ~connection: RecordProxy.t,
      ~newEdge: RecordProxy.t,
      ~cursor: string=?,
      unit
    ) =>
    unit =
    "insertEdgeBefore";

  [@bs.module "relay-runtime"] [@bs.scope "ConnectionHandler"]
  external insertEdgeAfter:
    (
      ~connection: RecordProxy.t,
      ~newEdge: RecordProxy.t,
      ~cursor: string=?,
      unit
    ) =>
    unit =
    "insertEdgeAfter";

  [@bs.module "relay-runtime"] [@bs.scope "ConnectionHandler"]
  external deleteNode: (~connection: RecordProxy.t, ~nodeId: dataId) => unit =
    "deleteNode";
};

/**
 * QUERY
 */
type operationDescriptor;

[@bs.module "relay-runtime"]
external internal_createOperationDescriptor:
  (queryNode, 'variables) => operationDescriptor =
  "createOperationDescriptor";

module Disposable = {
  type t;

  [@bs.send] external dispose: t => unit = "dispose";
};

type cacheConfig = {
  force: option(bool),
  poll: option(int),
  liveConfigId: option(string),
  transactionId: option(string),
};

/**
 * Misc
 */
module Observable = {
  type t('response);

  type subscription = {
    unsubscribe: unit => unit,
    closed: bool,
  };

  type sink('response) = {
    next: 'response => unit,
    error: Js.Exn.t => unit,
    complete: unit => unit,
    closed: bool,
  };

  type observer('response);

  [@bs.obj]
  external makeObserver:
    (
      ~start: subscription => unit=?,
      ~next: 'response => unit=?,
      ~error: Js.Exn.t => unit=?,
      ~complete: unit => unit=?,
      ~unsubscribe: subscription => unit=?,
      unit
    ) =>
    observer('response);

  [@bs.module "relay-runtime"] [@bs.scope "Observable"]
  external make: (sink('response) => option(subscription)) => t('response) =
    "create";

  [@bs.send]
  external subscribe: (t('response), observer('response)) => subscription =
    "subscribe";

  [@bs.send] external toPromise: t('t) => Promise.t('t) = "toPromise";
};

module Network = {
  type t;

  type operation = {
    text: string,
    name: string,
    operationKind: string,
  };

  type subscribeFn =
    (operation, Js.Json.t, cacheConfig) => Observable.t(Js.Json.t);

  type fetchFunctionPromise =
    (operation, Js.Json.t, cacheConfig) => Js.Promise.t(Js.Json.t);

  type fetchFunctionObservable =
    (operation, Js.Json.t, cacheConfig) => Observable.t(Js.Json.t);

  [@bs.module "relay-runtime"] [@bs.scope "Network"]
  external makePromiseBased:
    (
      ~fetchFunction: fetchFunctionPromise,
      ~subscriptionFunction: subscribeFn=?,
      unit
    ) =>
    t =
    "create";

  [@bs.module "relay-runtime"] [@bs.scope "Network"]
  external makeObservableBased:
    (
      ~observableFunction: fetchFunctionObservable,
      ~subscriptionFunction: subscribeFn=?,
      unit
    ) =>
    t =
    "create";
};

module RecordSource = {
  type t;

  [@bs.module "relay-runtime"] [@bs.new]
  external make: (~records: recordSourceRecords=?, unit) => t = "RecordSource";

  [@bs.send] external toJSON: t => recordSourceRecords = "toJSON";
};

module Store = {
  type t;

  type storeConfig = {
    gcReleaseBufferSize: option(int),
    queryCacheExpirationTime: option(int),
  };

  [@bs.module "relay-runtime"] [@bs.new]
  external make: (RecordSource.t, storeConfig) => t = "Store";

  let make =
      (~source, ~gcReleaseBufferSize=?, ~queryCacheExpirationTime=?, ()) =>
    make(source, {gcReleaseBufferSize, queryCacheExpirationTime});

  [@bs.send] external getSource: t => RecordSource.t = "getSource";
};

type renderPolicy =
  | Full
  | Partial;

let mapRenderPolicy =
  fun
  | Some(Full) => Some("full")
  | Some(Partial) => Some("partial")
  | None => None;

module Environment = {
  type t;

  type missingFieldHandlers;

  [@bs.deriving abstract]
  type environmentConfig('a) = {
    network: Network.t,
    store: Store.t,
    [@bs.optional] [@bs.as "UNSTABLE_DO_NOT_USE_getDataID"]
    getDataID: (~nodeObj: 'a, ~typeName: string) => string,
    [@bs.optional] [@bs.as "UNSTABLE_defaultRenderPolicy"]
    defaultRenderPolicy: string,
    [@bs.optional]
    treatMissingFieldsAsNull: bool,
    missingFieldHandlers,
  };

  [@bs.module "relay-runtime"] [@bs.new]
  external make: environmentConfig('a) => t = "Environment";

  let make =
      (
        ~network,
        ~store,
        ~getDataID=?,
        ~defaultRenderPolicy=?,
        ~treatMissingFieldsAsNull=?,
        (),
      ) =>
    make(
      environmentConfig(
        ~network,
        ~store,
        ~getDataID?,
        ~defaultRenderPolicy=?defaultRenderPolicy->mapRenderPolicy,
        ~treatMissingFieldsAsNull?,
        // This handler below enables automatic resolution of all cached items through the Node interface
        ~missingFieldHandlers=[%raw
          {|
            [
              {
                kind: "linked",
                handle: function(field, record, args, store) {
                  if (
                    record != null &&
                    record.__typename === require("relay-runtime").ROOT_TYPE &&
                    field.name === "node" &&
                    args.hasOwnProperty("id")
                  ) {
                    return args.id;
                  }
                }
              }
            ]
          |}
        ],
        (),
      ),
    );

  [@bs.send] external getStore: t => Store.t = "getStore";
  [@bs.send]
  external commitPayload: (t, operationDescriptor, 'payload) => unit =
    "commitPayload";
};

module Context = {
  type t;

  type contextShape = {. "environment": Environment.t};

  [@bs.module "react-relay"]
  external context: React.Context.t(option(contextShape)) =
    "ReactRelayContext";
  let provider = React.Context.provider(context);

  module Provider = {
    [@react.component]
    let make = (~environment: Environment.t, ~children) =>
      React.createElement(
        provider,
        {"value": Some({"environment": environment}), "children": children},
      );
  };
};

let internal_useConvertedValue = (convert, v) =>
  React.useMemo1(() => convert(v), [|v|]);

exception EnvironmentNotFoundInContext;

let useEnvironmentFromContext = () => {
  let context = React.useContext(Context.context);

  switch (context) {
  | Some(ctx) => ctx##environment
  | None => raise(EnvironmentNotFoundInContext)
  };
};

type fetchPolicy =
  | StoreOnly
  | StoreOrNetwork
  | StoreAndNetwork
  | NetworkOnly;

let mapFetchPolicy =
  fun
  | Some(StoreOnly) => Some("store-only")
  | Some(StoreOrNetwork) => Some("store-or-network")
  | Some(StoreAndNetwork) => Some("store-and-network")
  | Some(NetworkOnly) => Some("network-only")
  | None => None;

type fetchQueryFetchPolicy =
  | NetworkOnly
  | StoreOrNetwork;

let mapFetchQueryFetchPolicy =
  fun
  | Some(StoreOrNetwork) => Some("store-or-network")
  | Some(NetworkOnly) => Some("network-only")
  | None => None;

type fetchQueryOptions = {
  networkCacheConfig: option(cacheConfig),
  fetchPolicy: option(string),
};

[@bs.module "react-relay/hooks"]
external fetchQuery:
  (Environment.t, queryNode, 'variables, option(fetchQueryOptions)) =>
  Observable.t('response) =
  "fetchQuery";

type useQueryConfig = {
  fetchKey: option(string),
  fetchPolicy: option(string),
  [@bs.as "UNSTABLE_renderPolicy"]
  renderPolicy: option(string),
  networkCacheConfig: option(cacheConfig),
};

type loadQueryConfig = {
  fetchKey: option(string),
  fetchPolicy: option(string),
  networkCacheConfig: option(cacheConfig),
};

[@bs.module "react-relay/hooks"]
external internal_useQuery:
  (queryNode, 'variables, useQueryConfig) => 'queryResponse =
  "useLazyLoadQuery";

type useQueryLoaderOptions = {
  fetchPolicy: option(fetchPolicy),
  networkCacheConfig: option(cacheConfig),
};

[@bs.module "react-relay/hooks"]
external internal_useQueryLoader:
  queryNode =>
  (
    Js.nullable('queryRef),
    ('variables, useQueryLoaderOptions) => unit,
    unit => unit,
  ) =
  "useQueryLoader";

[@bs.module "react-relay/hooks"] [@bs.scope "loadQuery"]
external loadQuery:
  (Environment.t, queryNode, 'variables, loadQueryConfig) => 'queryResponse =
  "loadQuery";

[@bs.module "react-relay/hooks"]
external internal_usePreloadedQuery:
  (queryNode, 'token, option({. "UNSTABLE_renderPolicy": option(string)})) =>
  'queryResponse =
  "usePreloadedQuery";

module type MakeLoadQueryConfig = {
  type variables;
  type loadedQueryRef;
  type response;
  let query: queryNode;
  let convertVariables: variables => variables;
};

module MakeLoadQuery = (C: MakeLoadQueryConfig) => {
  let load:
    (
      ~environment: Environment.t,
      ~variables: C.variables,
      ~fetchPolicy: fetchPolicy=?,
      ~fetchKey: string=?,
      ~networkCacheConfig: cacheConfig=?,
      unit
    ) =>
    C.loadedQueryRef =
    (
      ~environment,
      ~variables,
      ~fetchPolicy=?,
      ~fetchKey=?,
      ~networkCacheConfig=?,
      (),
    ) =>
      loadQuery(
        environment,
        C.query,
        variables |> C.convertVariables |> internal_cleanVariablesRaw,
        {
          fetchKey,
          fetchPolicy: fetchPolicy |> mapFetchPolicy,
          networkCacheConfig,
        },
      );

  type rawPreloadToken('response) = {
    source: Js.Nullable.t(Observable.t('response)),
  };
  external tokenToRaw: C.loadedQueryRef => rawPreloadToken(C.response) =
    "%identity";

  let queryRefToObservable = token => {
    let raw = token->tokenToRaw;
    raw.source->Js.Nullable.toOption;
  };

  let queryRefToPromise = token => {
    let (promise, resolve) = Promise.pending();

    switch (token->queryRefToObservable) {
    | None => resolve(Error())
    | Some(o) =>
      let _: Observable.subscription =
        o->Observable.(
             subscribe(makeObserver(~complete=() => resolve(Ok()), ()))
           );
      ();
    };

    promise;
  };
};

/**
 * FRAGMENT
 */
[@bs.module "react-relay/hooks"]
external internal_useFragment: (fragmentNode, 'fragmentRef) => 'fragmentData =
  "useFragment";

[@bs.module "react-relay"]
external internal_readInlineData: (fragmentNode, 'fragmentRef) => 'fragmentData =
  "readInlineData";

[@bs.module "react-relay/hooks"]
external internal_useFragmentOpt:
  (fragmentNode, Js.Nullable.t('fragmentRef)) => Js.Nullable.t('fragmentData) =
  "useFragment";

/** Refetchable */
[@bs.deriving abstract]
type refetchableFnOpts = {
  [@bs.optional]
  fetchPolicy: string,
  [@bs.optional] [@bs.as "UNSTABLE_renderPolicy"]
  renderPolicy: string,
  [@bs.optional]
  onComplete: Js.Nullable.t(Js.Exn.t) => unit,
};

type refetchFnRaw('variables) =
  ('variables, refetchableFnOpts) => Disposable.t;

let internal_nullableToOptionalExnHandler =
  fun
  | None => None
  | Some(handler) =>
    Some(maybeExn => maybeExn->Js.Nullable.toOption->handler);

let makeRefetchableFnOpts =
    (~fetchPolicy=?, ~renderPolicy=?, ~onComplete=?, ()) =>
  refetchableFnOpts(
    ~fetchPolicy=?fetchPolicy->mapFetchPolicy,
    ~renderPolicy=?renderPolicy->mapRenderPolicy,
    ~onComplete=?onComplete->internal_nullableToOptionalExnHandler,
    (),
  );

let internal_makeRefetchableFnOpts = makeRefetchableFnOpts;

[@bs.module "react-relay/hooks"]
external internal_useRefetchableFragment:
  (fragmentNode, 'fragmentRef) => ('fragmentData, refetchFnRaw('variables)) =
  "useRefetchableFragment";

/** Pagination */
type paginationLoadMoreOptions = {
  onComplete: option(Js.nullable(Js.Exn.t) => unit),
};

type paginationLoadMoreFn =
  (~count: int, ~onComplete: option(Js.Exn.t) => unit=?, unit) => Disposable.t;

type paginationBlockingFragmentReturn('fragmentData, 'variables) = {
  data: 'fragmentData,
  loadNext: paginationLoadMoreFn,
  loadPrevious: paginationLoadMoreFn,
  hasNext: bool,
  hasPrevious: bool,
  refetch:
    (
      ~variables: 'variables,
      ~fetchPolicy: fetchPolicy=?,
      ~renderPolicy: renderPolicy=?,
      ~onComplete: option(Js.Exn.t) => unit=?,
      unit
    ) =>
    Disposable.t,
};

type paginationFragmentReturn('fragmentData, 'variables) = {
  data: 'fragmentData,
  loadNext: paginationLoadMoreFn,
  loadPrevious: paginationLoadMoreFn,
  hasNext: bool,
  hasPrevious: bool,
  isLoadingNext: bool,
  isLoadingPrevious: bool,
  refetch:
    (
      ~variables: 'variables,
      ~fetchPolicy: fetchPolicy=?,
      ~renderPolicy: renderPolicy=?,
      ~onComplete: option(Js.Exn.t) => unit=?,
      unit
    ) =>
    Disposable.t,
};

type paginationFragmentReturnRaw('fragmentData, 'variables) = {
  data: 'fragmentData,
  loadNext: (. int, paginationLoadMoreOptions) => Disposable.t,
  loadPrevious: (. int, paginationLoadMoreOptions) => Disposable.t,
  hasNext: bool,
  hasPrevious: bool,
  isLoadingNext: bool,
  isLoadingPrevious: bool,
  refetch: (. 'variables, refetchableFnOpts) => Disposable.t,
};

[@bs.module "react-relay/hooks"]
external internal_usePaginationFragment:
  (fragmentNode, 'fragmentRef) =>
  paginationFragmentReturnRaw('fragmentData, 'variables) =
  "usePaginationFragment";

[@bs.module "react-relay/hooks"]
external internal_useBlockingPaginationFragment:
  (fragmentNode, 'fragmentRef) =>
  paginationFragmentReturnRaw('fragmentData, 'variables) =
  "useBlockingPaginationFragment";

/**
 * MUTATION
 */

type updaterFn('response) = (RecordSourceSelectorProxy.t, 'response) => unit;
type optimisticUpdaterFn = RecordSourceSelectorProxy.t => unit;

type mutationError = {message: string};

type useMutationConfig('response, 'rawResponse, 'variables) = {
  onError: option(mutationError => unit),
  onCompleted: option(('response, option(array(mutationError))) => unit),
  onUnsubscribe: option(unit => unit),
  optimisticResponse: option('rawResponse),
  optimisticUpdater: option(optimisticUpdaterFn),
  updater: option(updaterFn('response)),
  variables: 'variables,
};

type useMutationConfigRaw('response, 'rawResponse, 'variables) = {
  onError: option(mutationError => unit),
  onCompleted:
    option(('response, Js.Nullable.t(array(mutationError))) => unit),
  onUnsubscribe: option(unit => unit),
  optimisticResponse: option('rawResponse),
  optimisticUpdater: option(optimisticUpdaterFn),
  updater: option(updaterFn('response)),
  variables: 'variables,
};

type commitMutationConfigRaw('variables, 'rawResponse, 'response) = {
  mutation: mutationNode,
  variables: 'variables,
  onCompleted:
    option(('response, Js.Nullable.t(array(mutationError))) => unit),
  onError: option(Js.Nullable.t(mutationError) => unit),
  optimisticResponse: option('rawResponse),
  optimisticUpdater: option(optimisticUpdaterFn),
  updater: option(updaterFn('response)),
};

exception Mutation_failed(array(mutationError));

[@bs.module "relay-runtime"]
external internal_commitMutation:
  (
    Environment.t,
    commitMutationConfigRaw('variables, 'rawResponse, 'response)
  ) =>
  Disposable.t =
  "commitMutation";

[@bs.module "react-relay/lib/relay-experimental"]
external internal_useMutation:
  mutationNode =>
  (
    useMutationConfigRaw('response, 'rawResponse, 'variables) => Disposable.t,
    bool,
  ) =
  "useMutation";

[@bs.module "relay-runtime"]
external commitLocalUpdate:
  (
    ~environment: Environment.t,
    ~updater: RecordSourceSelectorProxy.t => unit
  ) =>
  unit =
  "commitLocalUpdate";

[@bs.module "react-relay/hooks"]
external useSubscribeToInvalidationState:
  (array(dataId), unit => unit) => Disposable.t =
  "useSubscribeToInvalidationState";

[@bs.deriving abstract]
type subscriptionConfigRaw('response, 'variables) = {
  subscription: subscriptionNode,
  variables: 'variables,
  [@bs.optional]
  onCompleted: unit => unit,
  [@bs.optional]
  onError: Js.Exn.t => unit,
  [@bs.optional]
  onNext: 'response => unit,
  [@bs.optional]
  updater: updaterFn('response),
};

[@bs.module "relay-runtime"]
external internal_requestSubscription:
  (Environment.t, subscriptionConfigRaw('response, 'variables)) =>
  Disposable.t =
  "requestSubscription";
