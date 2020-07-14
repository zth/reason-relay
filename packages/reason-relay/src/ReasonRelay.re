type arguments;
type allFieldsMasked = {.};

type any;

type queryNode;
type fragmentNode;
type mutationNode;
type subscriptionNode;

type wrappedFragmentRef;

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
/**
 * Various helpers.
 */

// We occasionally have to remove undefined keys from objects, something I haven't figured out how to do with pure BuckleScript
let _cleanObjectFromUndefined = [%raw
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
let _cleanVariables = [%raw
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
external _convertObj:
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

  type unsetValueType =
    | Null
    | Undefined;

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
  external getValueArr:
    (t, ~name: string, ~arguments: option(arguments)) =>
    option(array('value)) =
    "getValue";

  [@bs.send] [@bs.return nullable]
  external getValueString:
    (t, ~name: string, ~arguments: arguments=?, unit) => option(string) =
    "getValue";

  let getValueStringArray =
      (t, ~name, ~arguments=?, ()): option(array(option(string))) =>
    getValueArr(~name, ~arguments, t);

  [@bs.send] [@bs.return nullable]
  external getValueInt:
    (t, ~name: string, ~arguments: arguments=?, unit) => option(int) =
    "getValue";

  let getValueIntArray =
      (t, ~name, ~arguments=?, ()): option(array(option(int))) =>
    getValueArr(~name, ~arguments, t);

  [@bs.send] [@bs.return nullable]
  external getValueFloat:
    (t, ~name: string, ~arguments: arguments=?, unit) => option(float) =
    "getValue";

  let getValueFloatArray =
      (t, ~name, ~arguments=?, ()): option(array(option(float))) =>
    getValueArr(~name, ~arguments, t);

  [@bs.send] [@bs.return nullable]
  external getValueBool:
    (t, ~name: string, ~arguments: arguments=?, unit) => option(bool) =
    "getValue";

  let getValueBoolArray =
      (t, ~name, ~arguments=?, ()): option(array(option(bool))) =>
    getValueArr(~name, ~arguments, t);

  [@bs.send]
  external setLinkedRecord:
    (t, ~record: t, ~name: string, ~arguments: arguments=?, unit) => t =
    "setLinkedRecord";

  [@bs.send]
  external unsetLinkedRecord: (t, 'nullable, string, option(arguments)) => t =
    "setLinkedRecord";
  let unsetLinkedRecord = (t, ~name, ~unsetValue, ~arguments=?, ()) =>
    switch (unsetValue) {
    | Null => unsetLinkedRecord(t, Js.null, name, arguments)
    | Undefined => unsetLinkedRecord(t, Js.undefined, name, arguments)
    };

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
  external unsetLinkedRecords: (t, 'nullable, string, option(arguments)) => t =
    "setLinkedRecords";
  let unsetLinkedRecords = (t, ~name, ~unsetValue, ~arguments=?, ()) =>
    switch (unsetValue) {
    | Null => unsetLinkedRecords(t, Js.null, name, arguments)
    | Undefined => unsetLinkedRecords(t, Js.undefined, name, arguments)
    };

  [@bs.send]
  external unsetValue_: (t, 'nullable, string, option(arguments)) => t =
    "setValue";
  let unsetValue = (t, ~name, ~unsetValue, ~arguments=?, ()) =>
    switch (unsetValue) {
    | Null => unsetValue_(t, Js.null, name, arguments)
    | Undefined => unsetValue_(t, Js.undefined, name, arguments)
    };

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

  type observer('response) = {
    start: option(subscription => unit),
    next: option('response => unit),
    error: option(Js.Exn.t => unit),
    complete: option(unit => unit),
    unsubscribe: option(subscription => unit),
  };

  let makeObserver =
      (~start=?, ~next=?, ~error=?, ~complete=?, ~unsubscribe=?, ()) => {
    start,
    next,
    error,
    complete,
    unsubscribe,
  };

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

  type environmentConfig('a) = {
    network: Network.t,
    store: Store.t,
    [@bs.as "UNSTABLE_DO_NOT_USE_getDataID"]
    getDataID: option((~nodeObj: 'a, ~typeName: string) => string),
    [@bs.as "UNSTABLE_defaultRenderPolicy"]
    defaultRenderPolicy: option(string),
    treatMissingFieldsAsNull: option(bool),
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
    make({
      network,
      store,
      getDataID,
      defaultRenderPolicy: defaultRenderPolicy->mapRenderPolicy,
      treatMissingFieldsAsNull,
      // This handler below enables automatic resolution of all cached items through the Node interface
      missingFieldHandlers: [%raw
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
    });

  [@bs.send] external getStore: t => Store.t = "getStore";
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

let useConvertedValue = (convert, v) =>
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
external useQuery: (queryNode, 'variables, useQueryConfig) => 'queryResponse =
  "useLazyLoadQuery";

[@bs.module "react-relay/hooks"] [@bs.scope "loadQuery"]
external loadQuery:
  (Environment.t, queryNode, 'variables, loadQueryConfig) => 'queryResponse =
  "loadQuery";

[@bs.module "react-relay/hooks"]
external usePreloadedQuery:
  (queryNode, 'token, option({. "UNSTABLE_renderPolicy": option(string)})) =>
  'queryResponse =
  "usePreloadedQuery";

module type MakeUseQueryConfig = {
  type responseRaw;
  type response;
  type variables;
  type queryRef;
  let query: queryNode;
  let convertResponse: responseRaw => response;
  let convertVariables: variables => variables;
};

module MakeUseQuery = (C: MakeUseQueryConfig) => {
  let use =
      (
        ~variables,
        ~fetchPolicy=?,
        ~renderPolicy=?,
        ~fetchKey=?,
        ~networkCacheConfig=?,
        (),
      )
      : C.response => {
    let data =
      useQuery(
        C.query,
        variables
        |> _cleanVariables
        |> C.convertVariables
        |> _cleanObjectFromUndefined,
        {
          fetchKey,
          fetchPolicy: fetchPolicy |> mapFetchPolicy,
          renderPolicy: renderPolicy |> mapRenderPolicy,
          networkCacheConfig,
        },
      );

    useConvertedValue(C.convertResponse, data);
  };

  let usePreloaded = (~queryRef: C.queryRef, ~renderPolicy=?, ()) => {
    let data =
      usePreloadedQuery(
        C.query,
        queryRef,
        switch (renderPolicy) {
        | Some(_) =>
          Some({"UNSTABLE_renderPolicy": renderPolicy |> mapRenderPolicy})
        | None => None
        },
      );
    data |> useConvertedValue(C.convertResponse);
  };

  let fetch =
      (
        ~environment: Environment.t,
        ~variables: C.variables,
        ~onResult: Belt.Result.t(C.response, Js.Exn.t) => unit,
        ~networkCacheConfig=?,
        ~fetchPolicy=?,
        (),
      )
      : unit => {
    let _ =
      fetchQuery(
        environment,
        C.query,
        variables |> C.convertVariables,
        Some({
          networkCacheConfig,
          fetchPolicy: fetchPolicy->mapFetchQueryFetchPolicy,
        }),
      )
      ->Observable.(
          subscribe(
            makeObserver(
              ~next=res => onResult(Ok(res->C.convertResponse)),
              ~error=err => onResult(Error(err)),
              (),
            ),
          )
        );
    ();
  };

  let fetchPromised =
      (
        ~environment: Environment.t,
        ~variables: C.variables,
        ~networkCacheConfig=?,
        ~fetchPolicy=?,
        (),
      )
      : Promise.t(Belt.Result.t(C.response, Js.Exn.t)) => {
    let (promise, resolve) = Promise.pending();

    let _ =
      fetchQuery(
        environment,
        C.query,
        variables |> C.convertVariables,
        Some({
          networkCacheConfig,
          fetchPolicy: fetchPolicy->mapFetchQueryFetchPolicy,
        }),
      )
      ->Observable.(
          subscribe(
            makeObserver(
              ~next=res => {resolve(Ok(res->C.convertResponse))},
              ~error=err => {resolve(Error(err))},
              (),
            ),
          )
        );

    promise;
  };
};

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
        variables |> C.convertVariables |> _cleanVariables,
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
external useFragment: (fragmentNode, 'fragmentRef) => 'fragmentData =
  "useFragment";

[@bs.module "react-relay/hooks"]
external useFragmentOpt:
  (fragmentNode, Js.Nullable.t('fragmentRef)) => Js.Nullable.t('fragmentData) =
  "useFragment";

module type MakeUseFragmentConfig = {
  type fragmentRaw;
  type fragment;
  type fragmentRef;
  let fragmentSpec: fragmentNode;
  let convertFragment: fragmentRaw => fragment;
};

module MakeUseFragment = (C: MakeUseFragmentConfig) => {
  let use = (fr: C.fragmentRef): C.fragment => {
    let data = useFragment(C.fragmentSpec, fr);
    useConvertedValue(C.convertFragment, data);
  };

  let useOpt = (fr: option(C.fragmentRef)): option(C.fragment) => {
    let nullableFragmentData: Js.Nullable.t(C.fragmentRaw) =
      useFragmentOpt(
        C.fragmentSpec,
        switch (fr) {
        | Some(fr) => Some(fr)->Js.Nullable.fromOption
        | None => Js.Nullable.null
        },
      );

    let data: option(C.fragmentRaw) =
      nullableFragmentData->Js.Nullable.toOption;

    useConvertedValue(
      rawFragment =>
        switch (rawFragment) {
        | Some(rawFragment) => Some(rawFragment->C.convertFragment)
        | None => None
        },
      data,
    );
  };
};

/** Refetchable */
type refetchFn('variables) =
  (
    ~variables: 'variables,
    ~fetchPolicy: fetchPolicy=?,
    ~renderPolicy: renderPolicy=?,
    ~onComplete: option(Js.Exn.t) => unit=?,
    unit
  ) =>
  unit;

type refetchFnRaw('variables) =
  (
    'variables,
    {
      .
      "fetchPolicy": option(string),
      "UNSTABLE_renderPolicy": option(string),
      "onComplete": option(Js.Nullable.t(Js.Exn.t) => unit),
    }
  ) =>
  unit;

let makeRefetchableFnOpts = (~fetchPolicy, ~renderPolicy, ~onComplete) => {
  "fetchPolicy": fetchPolicy |> mapFetchPolicy,
  "UNSTABLE_renderPolicy": renderPolicy |> mapRenderPolicy,
  "onComplete":
    Some(
      maybeExn =>
        switch (onComplete, maybeExn |> Js.Nullable.toOption) {
        | (Some(onComplete), maybeExn) => onComplete(maybeExn)
        | _ => ()
        },
    ),
};

[@bs.module "react-relay/hooks"]
external useRefetchableFragment:
  (fragmentNode, 'fragmentRef) => ('fragmentData, refetchFnRaw('variables)) =
  "useRefetchableFragment";

module type MakeUseRefetchableFragmentConfig = {
  type fragmentRaw;
  type fragment;
  type fragmentRef;
  type variables;
  let fragmentSpec: fragmentNode;
  let convertFragment: fragmentRaw => fragment;
  let convertVariables: variables => variables;
};

module MakeUseRefetchableFragment = (C: MakeUseRefetchableFragmentConfig) => {
  let useRefetchable = (fr: C.fragmentRef) => {
    let (fragmentData, refetchFn) =
      useRefetchableFragment(C.fragmentSpec, fr);

    let data = useConvertedValue(C.convertFragment, fragmentData);
    (
      data,
      (
        ~variables: C.variables,
        ~fetchPolicy=?,
        ~renderPolicy=?,
        ~onComplete=?,
        (),
      ) =>
        refetchFn(
          variables
          |> C.convertVariables
          |> _cleanVariables
          |> _cleanObjectFromUndefined,
          makeRefetchableFnOpts(~fetchPolicy, ~renderPolicy, ~onComplete),
        ),
    );
  };
};

/** Pagination */
module type MakeUsePaginationFragmentConfig = {
  type fragmentRaw;
  type fragment;
  type variables;
  type fragmentRef;
  let fragmentSpec: fragmentNode;
  let convertFragment: fragmentRaw => fragment;
  let convertVariables: variables => variables;
};

type paginationLoadMoreOptions = {
  onComplete: option(option(Js.Exn.t) => unit),
};

type paginationLoadMoreFn =
  (~count: int, ~onComplete: option(Js.Exn.t) => unit=?, unit) => Disposable.t;

type paginationBlockingFragmentReturn('fragmentData, 'variables) = {
  data: 'fragmentData,
  loadNext: paginationLoadMoreFn,
  loadPrevious: paginationLoadMoreFn,
  hasNext: bool,
  hasPrevious: bool,
  refetch: refetchFn('variables),
};

type paginationFragmentReturn('fragmentData, 'variables) = {
  data: 'fragmentData,
  loadNext: paginationLoadMoreFn,
  loadPrevious: paginationLoadMoreFn,
  hasNext: bool,
  hasPrevious: bool,
  isLoadingNext: bool,
  isLoadingPrevious: bool,
  refetch: refetchFn('variables),
};

[@bs.module "react-relay/hooks"]
external usePaginationFragment:
  (fragmentNode, 'fragmentRef) =>
  {
    .
    "data": 'fragmentData,
    "loadNext":
      [@bs.meth] ((int, option(paginationLoadMoreOptions)) => Disposable.t),
    "loadPrevious":
      [@bs.meth] ((int, option(paginationLoadMoreOptions)) => Disposable.t),
    "hasNext": bool,
    "hasPrevious": bool,
    "isLoadingNext": bool,
    "isLoadingPrevious": bool,
    "refetch":
      [@bs.meth] (
        (
          'variables,
          {
            .
            "fetchPolicy": option(string),
            "UNSTABLE_renderPolicy": option(string),
            "onComplete": option(Js.Nullable.t(Js.Exn.t) => unit),
          }
        ) =>
        unit
      ),
  } =
  "usePaginationFragment";

[@bs.module "react-relay/hooks"]
external useBlockingPaginationFragment:
  (fragmentNode, 'fragmentRef) =>
  {
    .
    "data": 'fragmentData,
    "loadNext":
      [@bs.meth] ((int, option(paginationLoadMoreOptions)) => Disposable.t),
    "loadPrevious":
      [@bs.meth] ((int, option(paginationLoadMoreOptions)) => Disposable.t),
    "hasNext": bool,
    "hasPrevious": bool,
    "isLoadingNext": bool,
    "isLoadingPrevious": bool,
    "refetch":
      [@bs.meth] (
        (
          'variables,
          {
            .
            "fetchPolicy": option(string),
            "UNSTABLE_renderPolicy": option(string),
            "onComplete": option(Js.Nullable.t(Js.Exn.t) => unit),
          }
        ) =>
        unit
      ),
  } =
  "useBlockingPaginationFragment";

module MakeUsePaginationFragment = (C: MakeUsePaginationFragmentConfig) => {
  let useBlockingPagination =
      (fr: C.fragmentRef)
      : paginationBlockingFragmentReturn(C.fragment, C.variables) => {
    let p = useBlockingPaginationFragment(C.fragmentSpec, fr);
    let data = useConvertedValue(C.convertFragment, p##data);

    {
      data,
      loadNext: (~count, ~onComplete=?, ()) =>
        p##loadNext(count, Some({onComplete: onComplete})),
      loadPrevious: (~count, ~onComplete=?, ()) =>
        p##loadPrevious(count, Some({onComplete: onComplete})),
      hasNext: p##hasNext,
      hasPrevious: p##hasPrevious,
      refetch:
        (
          ~variables: C.variables,
          ~fetchPolicy=?,
          ~renderPolicy=?,
          ~onComplete=?,
          (),
        ) =>
        p##refetch(
          variables |> C.convertVariables |> _cleanVariables,
          makeRefetchableFnOpts(~onComplete, ~fetchPolicy, ~renderPolicy),
        ),
    };
  };

  let usePagination =
      (fr: C.fragmentRef): paginationFragmentReturn(C.fragment, C.variables) => {
    let p = usePaginationFragment(C.fragmentSpec, fr);
    let data = useConvertedValue(C.convertFragment, p##data);

    {
      data,
      loadNext: (~count, ~onComplete=?, ()) =>
        p##loadNext(count, Some({onComplete: onComplete})),
      loadPrevious: (~count, ~onComplete=?, ()) =>
        p##loadPrevious(count, Some({onComplete: onComplete})),
      hasNext: p##hasNext,
      hasPrevious: p##hasPrevious,
      isLoadingNext: p##isLoadingNext,
      isLoadingPrevious: p##isLoadingPrevious,
      refetch:
        (
          ~variables: C.variables,
          ~fetchPolicy=?,
          ~renderPolicy=?,
          ~onComplete=?,
          (),
        ) =>
        p##refetch(
          variables |> C.convertVariables |> _cleanVariables,
          makeRefetchableFnOpts(~onComplete, ~fetchPolicy, ~renderPolicy),
        ),
    };
  };
};

/**
 * MUTATION
 */
module type MutationConfig = {
  type variables;
  type responseRaw;
  type response;
  let node: mutationNode;
  let convertResponse: responseRaw => response;
  let wrapResponse: response => responseRaw;
  let convertVariables: variables => variables;
};

type updaterFn('response) = (RecordSourceSelectorProxy.t, 'response) => unit;
type optimisticUpdaterFn = RecordSourceSelectorProxy.t => unit;

type mutationError = {message: string};

type useMutationConfig('response, 'variables) = {
  onError: option(mutationError => unit),
  onCompleted: option(('response, option(array(mutationError))) => unit),
  onUnsubscribe: option(unit => unit),
  optimisticResponse: option('response),
  optimisticUpdater: option(optimisticUpdaterFn),
  updater: option(updaterFn('response)),
  variables: 'variables,
};

type _useMutationConfig('response, 'variables) = {
  onError: option(mutationError => unit),
  onCompleted:
    option(('response, Js.Nullable.t(array(mutationError))) => unit),
  onUnsubscribe: option(unit => unit),
  optimisticResponse: option('response),
  optimisticUpdater: option(optimisticUpdaterFn),
  updater: option(updaterFn('response)),
  variables: 'variables,
};

type _commitMutationConfig('variables, 'response) = {
  mutation: mutationNode,
  variables: 'variables,
  onCompleted:
    option(('response, Js.Nullable.t(array(mutationError))) => unit),
  onError: option(Js.Nullable.t(mutationError) => unit),
  optimisticResponse: option('response),
  optimisticUpdater: option(optimisticUpdaterFn),
  updater: option(updaterFn('response)),
};

exception Mutation_failed(array(mutationError));

[@bs.module "relay-runtime"]
external commitMutation_:
  (Environment.t, _commitMutationConfig('variables, 'response)) =>
  Disposable.t =
  "commitMutation";

[@bs.module "react-relay/lib/relay-experimental"]
external useMutation:
  mutationNode =>
  (_useMutationConfig('response, 'variables) => Disposable.t, bool) =
  "useMutation";

module MakeUseMutation = (C: MutationConfig) => {
  let use = () => {
    let (mutate, mutating) = useMutation(C.node);
    (
      (
        ~onError=?,
        ~onCompleted=?,
        ~onUnsubscribe=?,
        ~optimisticResponse=?,
        ~optimisticUpdater=?,
        ~updater=?,
        ~variables,
        (),
      ) =>
        mutate({
          onError,
          onCompleted:
            switch (onCompleted) {
            | Some(fn) =>
              Some(
                (r, errors) =>
                  fn(r |> C.convertResponse, Js.Nullable.toOption(errors)),
              )

            | None => None
            },
          optimisticResponse:
            switch (optimisticResponse) {
            | None => None
            | Some(r) => Some(r |> C.wrapResponse)
            },
          onUnsubscribe,
          variables: variables |> C.convertVariables |> _cleanVariables,
          optimisticUpdater,
          updater:
            switch (updater) {
            | None => None
            | Some(updater) =>
              Some((store, r) => updater(store, r |> C.convertResponse))
            },
        }),
      mutating,
    );
  };
};

module MakeCommitMutation = (C: MutationConfig) => {
  let commitMutation =
      (
        ~environment: Environment.t,
        ~variables: C.variables,
        ~optimisticUpdater=?,
        ~optimisticResponse=?,
        ~updater=?,
        ~onCompleted=?,
        ~onError=?,
        (),
      )
      : Disposable.t =>
    commitMutation_(
      environment,
      {
        variables: variables |> C.convertVariables |> _cleanVariables,
        mutation: C.node,
        onCompleted:
          Some(
            (res, err) =>
              switch (onCompleted) {
              | Some(cb) =>
                cb(res->C.convertResponse, Js.Nullable.toOption(err))
              | None => ()
              },
          ),
        onError:
          Some(
            err =>
              switch (onError) {
              | Some(cb) => cb(Js.Nullable.toOption(err))
              | None => ()
              },
          ),
        optimisticResponse:
          switch (optimisticResponse) {
          | None => None
          | Some(r) => Some(r |> C.wrapResponse)
          },
        optimisticUpdater,
        updater:
          switch (updater) {
          | None => None
          | Some(updater) =>
            Some((store, r) => updater(store, r |> C.convertResponse))
          },
      },
    );

  let commitMutationPromised =
      (
        ~environment: Environment.t,
        ~variables: C.variables,
        ~optimisticUpdater=?,
        ~optimisticResponse=?,
        ~updater=?,
        (),
      )
      : Promise.t(
          Belt.Result.t(
            (C.response, option(array(mutationError))),
            option(mutationError),
          ),
        ) => {
    let (promise, resolve) = Promise.pending();

    let _: Disposable.t =
      commitMutation_(
        environment,
        {
          variables: variables |> C.convertVariables |> _cleanVariables,
          mutation: C.node,
          onCompleted:
            Some(
              (res, err) =>
                resolve(
                  Ok((res->C.convertResponse, Js.Nullable.toOption(err))),
                ),
            ),
          onError: Some(err => resolve(Error(Js.Nullable.toOption(err)))),
          optimisticResponse:
            switch (optimisticResponse) {
            | None => None
            | Some(r) => Some(r |> C.wrapResponse)
            },
          optimisticUpdater,
          updater:
            switch (updater) {
            | None => None
            | Some(updater) =>
              Some((store, r) => updater(store, r |> C.convertResponse))
            },
        },
      );

    promise;
  };
};

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

module type SubscriptionConfig = {
  type variables;
  type responseRaw;
  type response;
  let node: subscriptionNode;
  let convertResponse: responseRaw => response;
  let convertVariables: variables => variables;
};

type _subscriptionConfig('response, 'variables) = {
  .
  "subscription": subscriptionNode,
  "variables": 'variables,
  "onCompleted": option(unit => unit),
  "onError": option(Js.Exn.t => unit),
  "onNext": option('response => unit),
  "updater": option(updaterFn('response)),
};

[@bs.module "relay-runtime"]
external requestSubscription:
  (Environment.t, _subscriptionConfig('response, 'variables)) => Disposable.t =
  "requestSubscription";

module MakeUseSubscription = (C: SubscriptionConfig) => {
  let subscribe =
      (
        ~environment: Environment.t,
        ~variables: C.variables,
        ~onCompleted: option(unit => unit)=?,
        ~onError: option(Js.Exn.t => unit)=?,
        ~onNext: option(C.response => unit)=?,
        ~updater: option(updaterFn(C.response))=?,
        (),
      ) =>
    requestSubscription(
      environment,
      {
        "subscription": C.node,
        "variables": variables |> C.convertVariables |> _cleanVariables,
        "onCompleted": onCompleted,
        "onError": onError,
        "onNext":
          switch (onNext) {
          | None => None
          | Some(onNext) => Some(r => onNext(r |> C.convertResponse))
          },
        "updater":
          switch (updater) {
          | None => None
          | Some(updater) =>
            Some((store, r) => updater(store, C.convertResponse(r)))
          },
      },
    );
};
