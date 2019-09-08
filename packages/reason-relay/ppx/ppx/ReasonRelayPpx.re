open Ppxlib;

exception Could_not_extract_operation_name;
exception Could_not_extract_operation;

let extractGraphQLOperation = str =>
  switch (str |> Graphql_parser.parse) {
  | Ok(definitions) =>
    switch (definitions) {
    | [op, ..._] => op
    | _ => raise(Could_not_extract_operation)
    }
  | Error(_) => raise(Could_not_extract_operation)
  };

let extractTheQueryName = str =>
  switch (str |> extractGraphQLOperation) {
  | Operation({optype: Query, name: Some(name)}) => name
  | _ => raise(Could_not_extract_operation_name)
  };

let extractTheMutationName = str =>
  switch (str |> extractGraphQLOperation) {
  | Operation({optype: Mutation, name: Some(name)}) => name
  | _ => raise(Could_not_extract_operation_name)
  };

let extractTheFragmentName = str =>
  switch (str |> extractGraphQLOperation) {
  | Fragment({name}) => name
  | _ => raise(Could_not_extract_operation_name)
  };

let extractFragmentRefetchableQueryName = str =>
  switch (str |> extractGraphQLOperation) {
  | Fragment({
      name: _,
      directives: [
        {
          name: "refetchable",
          arguments: [("queryName", `String(queryName))],
        },
      ],
    }) =>
    Some(queryName)
  | _ => None
  };

let extractTheSubscriptionName = str =>
  switch (str |> extractGraphQLOperation) {
  | Operation({optype: Subscription, name: Some(name)}) => name
  | _ => raise(Could_not_extract_operation_name)
  };

let getGraphQLModuleName = opName => opName ++ "_graphql";

let extractOperationStr = (~loc, ~expr) =>
  switch (expr) {
  | PStr([
      {
        pstr_desc:
          [@implicit_arity]
          Pstr_eval(
            {
              pexp_loc: loc,
              pexp_desc:
                Pexp_constant(
                  [@implicit_arity] Pconst_string(operationStr, _),
                ),
              _,
            },
            _,
          ),
        _,
      },
    ]) => operationStr
  | _ =>
    raise(
      Location.Error(
        Obj.magic(),
        /*Location.Error.createf(
            ~loc,
            "All [%relay] operations must be provided a string, like [%relay.query {| { query SomeQuery { id } |}]",
          ),*/
      ),
    )
  };

let makeModuleNameAst = (~loc, ~moduleName) => {
  pmod_attributes: [],
  pmod_loc: loc,
  pmod_desc:
    Pmod_ident({loc, txt: Lident(getGraphQLModuleName(moduleName))}),
};

let makeFragment = (~loc, ~moduleName, ~refetchableQueryName) =>
  Ast_helper.Mod.mk(
    Pmod_structure([
      [%stri module Operation = [%m makeModuleNameAst(~loc, ~moduleName)]],
      switch (refetchableQueryName) {
      | Some(queryName) => [%stri
          module RefetchableOperation = [%m
            makeModuleNameAst(~loc, ~moduleName=queryName)
          ]
        ]
      | None =>
        %stri
        ()
      },
      [%stri include Operation.Unions],
      switch (refetchableQueryName) {
      | Some(queryName) => [%stri
          module UseRefetchableFragment =
            ReasonRelay.MakeUseRefetchableFragment({
              type fragment = Operation.fragment;
              type fragmentRef = Operation.fragmentRef;
              type variables = RefetchableOperation.variables;
              let fragmentSpec = Operation.node;
            })
        ]
      | None =>
        %stri
        ()
      },
      [%stri
        module UseFragment =
          ReasonRelay.MakeUseFragment({
            type fragment = Operation.fragment;
            type fragmentRef = Operation.fragmentRef;
            let fragmentSpec = Operation.node;
          })
      ],
      [%stri
        let use = fRef => UseFragment.use(fRef |> Operation.getFragmentRef)
      ],
      switch (refetchableQueryName) {
      | Some(_) => [%stri
          let useRefetchable = fRef =>
            UseRefetchableFragment.useRefetchable(fRef |> Operation.getFragmentRef)
        ]
      | None =>
        %stri
        ()
      },
    ]),
  );

let makeQuery = (~loc, ~moduleName) =>
  Ast_helper.Mod.mk(
    Pmod_structure([
      [%stri module Operation = [%m makeModuleNameAst(~loc, ~moduleName)]],
      [%stri include Operation.Unions],
      [%stri
        module UseQuery =
          ReasonRelay.MakeUseQuery({
            type response = Operation.response;
            type variables = Operation.variables;
            let query = Operation.node;
          })
      ],
      [%stri let use = UseQuery.use],
      [%stri
        let fetch =
            (
              ~environment: ReasonRelay.Environment.t,
              ~variables: Operation.variables,
            )
            : Js.Promise.t(Operation.response) =>
          ReasonRelay.fetchQuery(environment, Operation.node, variables)
      ],
    ]),
  );

let makeMutation = (~loc, ~moduleName) =>
  Ast_helper.Mod.mk(
    Pmod_structure([
      [%stri module Operation = [%m makeModuleNameAst(~loc, ~moduleName)]],
      [%stri include Operation.Unions],
      [%stri
        module Mutation =
          ReasonRelay.MakeCommitMutation({
            type variables = Operation.variables;
            type response = Operation.response;
            let node = Operation.node;
          })
      ],
      [%stri
        module UseMutation =
          ReasonRelay.MakeUseMutation({
            type variables = Operation.variables;
            type response = Operation.response;
            let node = Operation.node;
          })
      ],
      [%stri let use = UseMutation.use],
      [%stri let commitMutation = Mutation.commitMutation],
    ]),
  );

let makeSubscription = (~loc, ~moduleName) =>
  Ast_helper.Mod.mk(
    Pmod_structure([
      [%stri module Operation = [%m makeModuleNameAst(~loc, ~moduleName)]],
      [%stri include Operation.Unions],
      [%stri
        module Subscription =
          ReasonRelay.MakeUseSubscription({
            type variables = Operation.variables;
            type response = Operation.response;
            let node = Operation.node;
          })
      ],
      [%stri let subscribe = Subscription.subscribe],
    ]),
  );

let queryExtension =
  Extension.declare(
    "relay.query",
    Extension.Context.module_expr,
    Ast_pattern.__,
    (~loc, ~path as _, expr) =>
    makeQuery(
      ~moduleName=extractOperationStr(~loc, ~expr) |> extractTheQueryName,
      ~loc,
    )
  );

let fragmentExtension =
  Extension.declare(
    "relay.fragment",
    Extension.Context.module_expr,
    Ast_pattern.__,
    (~loc, ~path as _, expr) =>
    makeFragment(
      ~moduleName=extractOperationStr(~loc, ~expr) |> extractTheFragmentName,
      ~refetchableQueryName=
        extractOperationStr(~loc, ~expr)
        |> extractFragmentRefetchableQueryName,
      ~loc,
    )
  );

let mutationExtension =
  Extension.declare(
    "relay.mutation",
    Extension.Context.module_expr,
    Ast_pattern.__,
    (~loc, ~path as _, expr) =>
    makeMutation(
      ~moduleName=extractOperationStr(~loc, ~expr) |> extractTheMutationName,
      ~loc,
    )
  );

let subscriptionExtension =
  Extension.declare(
    "relay.subscription",
    Extension.Context.module_expr,
    Ast_pattern.__,
    (~loc, ~path as _, expr) =>
    makeSubscription(
      ~moduleName=
        extractOperationStr(~loc, ~expr) |> extractTheSubscriptionName,
      ~loc,
    )
  );

let () =
  Driver.register_transformation(
    ~extensions=[
      queryExtension,
      fragmentExtension,
      mutationExtension,
      subscriptionExtension,
    ],
    "reason-relay",
  );