let toOpt = Js.Nullable.toOption;
type jsObj('a) = Js.t({..} as 'a);

type any;

type queryNode;
type fragmentNode;
type mutationNode;
type subscriptionNode;

type dataId;

external _dataIdToString: dataId => string = "%identity";
let dataIdToString = dataId => _dataIdToString(dataId);

external _makeDataId: string => dataId = "%identity";
let makeDataId = string => _makeDataId(string);

module RecordProxy = {
  type t;
  type arguments('a) = jsObj('a);

  [@bs.send] external _copyFieldsFrom: (t, t) => unit = "copyFieldsFrom";
  let copyFieldsFrom = (t, ~sourceRecord: t) =>
    _copyFieldsFrom(t, sourceRecord);

  [@bs.send] external _getDataID: t => dataId = "getDataID";
  let getDataId = t => _getDataID(t);

  [@bs.send]
  external _getLinkedRecord:
    (t, string, option(arguments('a))) => Js.Nullable.t(t) =
    "getLinkedRecord";

  let getLinkedRecord = (t, ~name, ~arguments): option(t) =>
    _getLinkedRecord(t, name, arguments) |> toOpt;

  [@bs.send]
  external _getLinkedRecords:
    (t, string, option(arguments('a))) =>
    Js.Nullable.t(array(Js.Nullable.t(t))) =
    "getLinkedRecords";

  let getLinkedRecords = (t, ~name, ~arguments): option(array(option(t))) =>
    switch (_getLinkedRecords(t, name, arguments) |> toOpt) {
    | Some(records) => Some(records |> Array.map(v => v |> toOpt))
    | None => None
    };

  [@bs.send]
  external _getOrCreateLinkedRecord:
    (t, string, string, option(arguments('a))) => t =
    "getOrCreateLinkedRecord";

  let getOrCreateLinkedRecord = (t, ~name, ~typeName, ~arguments) =>
    _getOrCreateLinkedRecord(t, name, typeName, arguments);

  [@bs.send] external _getType: t => string = "getType";
  let getType = t => _getType(t);

  [@bs.send]
  external _getValue:
    (t, string, option(arguments('a))) => Js.Nullable.t('value) =
    "getValue";

  let _getValueArr = (t, ~name, ~arguments) =>
    switch (_getValue(t, name, arguments) |> toOpt) {
    | Some(arr) =>
      Some(arr |> Array.map(value => value |> Js.Nullable.toOption))
    | None => None
    };

  let getValueString = (t, ~name, ~arguments): option(string) =>
    _getValue(t, name, arguments) |> toOpt;

  let getValueStringArray =
      (t, ~name, ~arguments): option(array(option(string))) =>
    _getValueArr(~name, ~arguments, t);

  let getValueInt = (t, ~name, ~arguments): option(int) =>
    _getValue(t, name, arguments) |> toOpt;

  let getValueIntArray = (t, ~name, ~arguments): option(array(option(int))) =>
    _getValueArr(~name, ~arguments, t);

  let getValueFloat = (t, ~name, ~arguments): option(float) =>
    _getValue(t, name, arguments) |> toOpt;

  let getValueFloatArray =
      (t, ~name, ~arguments): option(array(option(float))) =>
    _getValueArr(~name, ~arguments, t);

  let getValueBool = (t, ~name, ~arguments): option(bool) =>
    _getValue(t, name, arguments) |> toOpt;

  let getValueBoolArray =
      (t, ~name, ~arguments): option(array(option(bool))) =>
    _getValueArr(~name, ~arguments, t);

  [@bs.send]
  external _setLinkedRecord: (t, t, string, option(arguments('a))) => t =
    "setLinkedRecord";
  let setLinkedRecord = (t, ~record, ~name, ~arguments) =>
    _setLinkedRecord(t, record, name, arguments);

  [@bs.send]
  external _setLinkedRecords:
    (t, array(option(t)), string, option(arguments('a))) => t =
    "setLinkedRecords";
  let setLinkedRecords = (t, ~records, ~name, ~arguments) =>
    _setLinkedRecords(t, records, name, arguments);

  [@bs.send]
  external _setValue: (t, 'value, string, option(arguments('a))) => t =
    "setValue";

  let setValueString = (t, ~value: string, ~name, ~arguments) =>
    _setValue(t, value, name, arguments);

  let setValueStringArray = (t, ~value: array(string), ~name, ~arguments) =>
    _setValue(t, value, name, arguments);

  let setValueInt = (t, ~value: int, ~name, ~arguments) =>
    _setValue(t, value, name, arguments);

  let setValueIntArray = (t, ~value: array(int), ~name, ~arguments) =>
    _setValue(t, value, name, arguments);

  let setValueFloat = (t, ~value: float, ~name, ~arguments) =>
    _setValue(t, value, name, arguments);

  let setValueFloatArray = (t, ~value: array(float), ~name, ~arguments) =>
    _setValue(t, value, name, arguments);

  let setValueBool = (t, ~value: bool, ~name, ~arguments) =>
    _setValue(t, value, name, arguments);

  let setValueBoolArray = (t, ~value: array(bool), ~name, ~arguments) =>
    _setValue(t, value, name, arguments);
};

module RecordSourceSelectorProxy = {
  type t;

  [@bs.send] external _create: (t, dataId, string) => RecordProxy.t = "create";
  let create = (t, ~dataId, ~typeName: string) =>
    _create(t, dataId, typeName);

  [@bs.send] external _delete: (t, dataId) => unit = "delete";
  let delete = (t, ~dataId) => _delete(t, dataId);

  [@bs.send]
  external _get: (t, dataId) => Js.Nullable.t(RecordProxy.t) = "get";
  let get = (t, ~dataId): option(RecordProxy.t) => _get(t, dataId) |> toOpt;

  [@bs.send] external getRoot: t => RecordProxy.t = "getRoot";

  [@bs.send]
  external _getRootField: (t, string) => Js.Nullable.t(RecordProxy.t) =
    "getRootField";
  let getRootField = (t, ~fieldName): option(RecordProxy.t) =>
    _getRootField(t, fieldName) |> toOpt;

  [@bs.send]
  external _getPluralRootField:
    (t, string) => Js.Nullable.t(array(Js.Nullable.t(RecordProxy.t))) =
    "getPluralRootField";

  let getPluralRootField =
      (t, ~fieldName): option(array(option(RecordProxy.t))) =>
    switch (_getPluralRootField(t, fieldName) |> toOpt) {
    | Some(arr) => Some(arr |> Array.map(v => v |> toOpt))
    | None => None
    };
};

module RecordSourceProxy = {
  type t;

  [@bs.send] external _create: (t, dataId, string) => RecordProxy.t = "create";
  let create = (t, ~dataId, ~typeName: string) =>
    _create(t, dataId, typeName);

  [@bs.send] external _delete: (t, dataId) => unit = "delete";
  let delete = (t, ~dataId) => _delete(t, dataId);

  [@bs.send]
  external _get: (t, dataId) => Js.Nullable.t(RecordProxy.t) = "get";
  let get = (t, ~dataId): option(RecordProxy.t) => _get(t, dataId) |> toOpt;

  [@bs.send] external getRoot: t => RecordProxy.t = "getRoot";
};

module ConnectionHandler = {
  type t;

  type filters('a) = jsObj('a);

  [@bs.module "relay-runtime"]
  external connectionHandler: t = "ConnectionHandler";

  [@bs.send]
  external _getConnection:
    (t, RecordProxy.t, string, option(filters('a))) =>
    Js.Nullable.t(RecordProxy.t) =
    "getConnection";

  let getConnection = (~record, ~key, ~filters) =>
    _getConnection(connectionHandler, record, key, filters)
    |> Js.Nullable.toOption;

  [@bs.send]
  external _createEdge:
    (t, RecordSourceProxy.t, RecordProxy.t, RecordProxy.t, string) =>
    RecordProxy.t =
    "createEdge";

  let createEdge = (~store, ~connection, ~node, ~edgeType) =>
    _createEdge(connectionHandler, store, connection, node, edgeType);

  [@bs.send]
  external _insertEdgeBefore:
    (t, RecordProxy.t, RecordProxy.t, option(string)) => unit =
    "insertEdgeBefore";

  let insertEdgeBefore = (~connection, ~newEdge, ~cursor) =>
    _insertEdgeBefore(connectionHandler, connection, newEdge, cursor);

  [@bs.send]
  external _insertEdgeAfter:
    (t, RecordProxy.t, RecordProxy.t, option(string)) => unit =
    "insertEdgeAfter";

  let insertEdgeAfter = (~connection, ~newEdge, ~cursor) =>
    _insertEdgeAfter(connectionHandler, connection, newEdge, cursor);

  [@bs.send]
  external _deleteNode: (t, RecordProxy.t, dataId) => unit = "deleteNode";

  let deleteNode = (~connection, ~nodeId) =>
    _deleteNode(connectionHandler, connection, nodeId);
};

/**
 * QUERY
 */

type queryResponse('data) =
  | Loading
  | Error(Js.Exn.t)
  | Data('data);

type dataFrom =
  | NetworkOnly
  | StoreThenNetwork
  | StoreOrNetwork
  | StoreOnly;

[@bs.module "relay-hooks"]
external _useQuery:
  {
    .
    "query": queryNode,
    "variables": 'variables,
    "dataFrom": option(string),
  } =>
  {
    .
    "props": Js.Nullable.t('props),
    "error": Js.Nullable.t(Js.Exn.t),
    "retry": Js.Nullable.t(unit => unit),
    "cached": bool,
  } =
  "useQuery";

module type MakeUseQueryConfig = {
  type response;
  type variables;
  let query: queryNode;
};

module MakeUseQuery = (C: MakeUseQueryConfig) => {
  type response = C.response;
  type variables = C.variables;

  let use:
    (~variables: variables, ~dataFrom: dataFrom=?, unit) =>
    queryResponse(response) =
    (~variables, ~dataFrom=?, ()) => {
      let q =
        _useQuery({
          "dataFrom":
            switch (dataFrom) {
            | Some(StoreThenNetwork) => Some("STORE_THEN_NETWORK")
            | Some(NetworkOnly) => Some("NETWORK_ONLY")
            | Some(StoreOrNetwork) => Some("STORE_OR_NETWORK")
            | Some(StoreOnly) => Some("STORE_ONLY")
            | None => None
            },
          "query": C.query,
          "variables": variables,
        });

      let res =
        switch (q##props |> toOpt, q##error |> toOpt) {
        | (None, None) => Loading
        | (Some(data), None) => Data(data)
        | (_, Some(err)) => Error(err)
        };

      res;
    };
};

/**
 * FRAGMENT
 */
[@bs.module "relay-hooks"]
external _useFragment: (fragmentNode, 'fragmentRef) => 'fragmentData =
  "useFragment";

module type MakeUseFragmentConfig = {
  type fragment;
  type fragmentRef;
  let fragmentSpec: fragmentNode;
};

module MakeUseFragment = (C: MakeUseFragmentConfig) => {
  let use = (fr: C.fragmentRef): C.fragment =>
    _useFragment(C.fragmentSpec, fr);
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

type mutationConfig('variables, 'response) = {
  .
  "variables": 'variables,
  "optimisticResponse": option('response),
  "updater": option(updaterFn),
  "optimisticUpdater": option(updaterFn),
};

type mutationError = {. "message": string};

type mutationStateRaw('response) = {
  .
  "loading": bool,
  "data": Js.Nullable.t('response),
  "error": Js.Nullable.t(mutationError),
};

type mutateFn('variables, 'response) =
  mutationConfig('variables, 'response) => Js.Promise.t('response);

[@bs.module "relay-hooks"]
external _useMutation:
  mutationNode =>
  (mutateFn('variables, 'response), mutationStateRaw('response)) =
  "useMutation";

type mutationState('response) =
  | Loading
  | Error(mutationError)
  | Success(option('response));

type mutationResult('response) =
  | Success('response)
  | Error(Js.Promise.error);

type useMutationConfigType('variables) = {variables: 'variables};

module MakeUseMutation = (C: MutationConfig) => {
  let use = () => {
    let (mutate, rawState) = _useMutation(C.node);
    let makeMutation =
        (
          ~variables: C.variables,
          ~optimisticResponse=?,
          ~optimisticUpdater=?,
          ~updater=?,
          (),
        )
        : Js.Promise.t(mutationResult(C.response)) =>
      mutate({
        "variables": variables,
        "optimisticResponse": optimisticResponse,
        "optimisticUpdater": optimisticUpdater,
        "updater": updater,
      })
      |> Js.Promise.then_(res => Js.Promise.resolve(Success(res)))
      |> Js.Promise.catch(err => Js.Promise.resolve(Error(err)));
    (
      makeMutation,
      switch (
        rawState##loading,
        rawState##data |> toOpt,
        rawState##error |> toOpt,
      ) {
      | (true, _, _) => Loading
      | (_, Some(data), _) => Success(Some(data))
      | (_, _, Some(error)) => Error(error)
      | (false, None, None) => Success(None)
      },
    );
  };
};

/**
 * Misc
 */
module Observable = {
  type t;

  type _sink('t) = {
    .
    "next": 't => unit,
    "error": Js.Exn.t => unit,
    "completed": unit => unit,
    "closed": bool,
  };

  type sink('t) = {
    next: 't => unit,
    error: Js.Exn.t => unit,
    completed: unit => unit,
    closed: bool,
  };

  [@bs.module "relay-runtime"] [@bs.scope "Observable"]
  external create: (_sink('t) => option('a)) => t = "create";

  let make = sinkFn =>
    create(s => {
      sinkFn({
        next: s##next,
        error: s##error,
        completed: s##completed,
        closed: s##closed,
      });
      None;
    });
};

module Network = {
  type t;

  type cacheConfig = {
    .
    "force": Js.Nullable.t(bool),
    "poll": Js.Nullable.t(int),
  };

  type operation = {
    .
    "text": string,
    "name": string,
    "operationKind": string,
  };

  type subscribeFn =
    {
      .
      "request": operation,
      "variables": Js.Json.t,
      "cacheConfig": cacheConfig,
    } =>
    Observable.t;

  type fetchFunctionPromise =
    (operation, Js.Json.t, cacheConfig) => Js.Promise.t(Js.Json.t);

  type fetchFunctionObservable =
    (operation, Js.Json.t, cacheConfig) => Observable.t;

  [@bs.module "relay-runtime"] [@bs.scope "Network"]
  external makeFromPromise: (fetchFunctionPromise, option(subscribeFn)) => t =
    "create";

  let makePromiseBased = (~fetchFunction, ~subscriptionFunction=?, ()) =>
    makeFromPromise(fetchFunction, subscriptionFunction);

  [@bs.module "relay-runtime"] [@bs.scope "Network"]
  external makeFromObservable:
    (fetchFunctionObservable, option(subscribeFn)) => t =
    "create";

  let makeObservableBased = (~observableFunction, ~subscriptionFunction=?, ()) =>
    makeFromObservable(observableFunction, subscriptionFunction);
};

module RecordSource = {
  type t;

  [@bs.module "relay-runtime"] [@bs.new]
  external make: unit => t = "RecordSource";
};

module Store = {
  type t;

  [@bs.module "relay-runtime"] [@bs.new]
  external make: RecordSource.t => t = "Store";
};

module Environment = {
  type t;

  [@bs.module "relay-runtime"] [@bs.new]
  external _make:
    {
      .
      "network": Network.t,
      "store": Store.t,
      "UNSTABLE_DO_NOT_USE_getDataID": option(('a, string) => string),
    } =>
    t =
    "Environment";

  let make = (~network, ~store, ~getDataID=?, ()) =>
    _make({
      "network": network,
      "store": store,
      "UNSTABLE_DO_NOT_USE_getDataID":
        switch (getDataID) {
        | Some(getDataID) =>
          Some((nodeObj, typeName) => getDataID(~nodeObj, ~typeName))
        | None => None
        },
    });
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

exception EnvironmentNotFoundInContext;

let useEnvironmentFromContext = () => {
  let context = React.useContext(Context.context);

  switch (context) {
  | Some(ctx) => ctx##environment
  | None => raise(EnvironmentNotFoundInContext)
  };
};

[@bs.module "relay-runtime"]
external fetchQuery:
  (Environment.t, queryNode, 'variables) => Js.Promise.t('response) =
  "fetchQuery";

type _commitMutationConfig('variables, 'response) = {
  .
  "mutation": mutationNode,
  "variables": 'variables,
  "onCompleted":
    option(
      (Js.Nullable.t('response), Js.Nullable.t(array(mutationError))) =>
      unit,
    ),
  "onError": option(Js.Nullable.t(mutationError) => unit),
  "optimisticResponse": option('response),
  "optimisticUpdater": option(RecordSourceSelectorProxy.t => unit),
  "updater": option(RecordSourceSelectorProxy.t => unit),
};

exception Mutation_failed(array(mutationError));

module MakeCommitMutation = (C: MutationConfig) => {
  [@bs.module "relay-runtime"]
  external _commitMutation:
    (Environment.t, _commitMutationConfig('variables, 'response)) => unit =
    "commitMutation";

  let commitMutation =
      (
        ~environment: Environment.t,
        ~variables: C.variables,
        ~optimisticUpdater=?,
        ~optimisticResponse=?,
        ~updater=?,
        (),
      )
      : Js.Promise.t(C.response) =>
    Js.Promise.make((~resolve, ~reject) =>
      _commitMutation(
        environment,
        {
          "variables": variables,
          "mutation": C.node,
          "onCompleted":
            Some(
              (res, errors) =>
                switch (res |> toOpt, errors |> toOpt) {
                | (_, Some(errors)) => reject(. Mutation_failed(errors))
                | (Some(res), None) => resolve(. res)
                | (None, None) => reject(. Mutation_failed([||]))
                },
            ),
          "onError":
            Some(
              error =>
                switch (error |> toOpt) {
                | Some(error) => reject(. Mutation_failed([|error|]))
                | None => reject(. Mutation_failed([||]))
                },
            ),
          "optimisticResponse": optimisticResponse,
          "optimisticUpdater": optimisticUpdater,
          "updater": updater,
        },
      )
    );
};

[@bs.module "relay-runtime"]
external _commitLocalUpdate:
  (Environment.t, RecordSourceSelectorProxy.t => unit) => unit =
  "commitLocalUpdate";

let commitLocalUpdate = (~environment, ~updater) =>
  _commitLocalUpdate(environment, updater);

module type SubscriptionConfig = {
  type variables;
  type response;
  let node: subscriptionNode;
};

module Disposable = {
  type t;

  [@bs.send] external dispose: t => unit = "dispose";
};

type _subscriptionConfig('response, 'variables) = {
  .
  "subscription": subscriptionNode,
  "variables": 'variables,
  "onCompleted": option(unit => unit),
  "onError": option(Js.Exn.t => unit),
  "onNext": option('response => unit),
  "updater": option(updaterFn),
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
        ~updater: option(updaterFn)=?,
        (),
      ) =>
    requestSubscription(
      environment,
      {
        "subscription": C.node,
        "variables": variables,
        "onCompleted": onCompleted,
        "onError": onError,
        "onNext": onNext,
        "updater": updater,
      },
    );
};
