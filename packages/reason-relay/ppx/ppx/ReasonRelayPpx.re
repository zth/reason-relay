/**
 * This PPX defines the [%relay.<operation> {| ... |}] extension points.
 */

// Ppxlib provides a number of helpers for writing and registering PPXs that life is very difficult without.
open Ppxlib;

exception Could_not_extract_operation_name;
exception Could_not_extract_operation;

/**
 * This function takes a GraphQL document as a string (typically extracted from the [%relay.<operation>] nodes),
 * uses Graphql_parser to parse the string into a list of GraphQL definitions, and then extracts the _first_ operation
 * of the document only. This is because Relay disallows multiple operations in the same definition.
 */
let extractGraphQLOperation = str =>
  switch (str |> Graphql_parser.parse) {
  | Ok(definitions) =>
    switch (definitions) {
    | [op, ..._] => op
    | _ => raise(Could_not_extract_operation)
    }
  | Error(_) => raise(Could_not_extract_operation)
  };

/**
 * Takes a raw GraphQL document as a string and extracts the query name. Raises an error if it's not a query
 * or the query has no name.
 */
let extractTheQueryName = str =>
  switch (str |> extractGraphQLOperation) {
  | Operation({optype: Query, name: Some(name)}) => name
  | _ => raise(Could_not_extract_operation_name)
  };

/**
 * Takes a raw GraphQL document as a string and extracts the mutation name. Raises an error if it's not a mutation
 * or the mutation has no name.
 */
let extractTheMutationName = str =>
  switch (str |> extractGraphQLOperation) {
  | Operation({optype: Mutation, name: Some(name)}) => name
  | _ => raise(Could_not_extract_operation_name)
  };

/**
 * Takes a raw GraphQL document as a string and extracts the fragment name. Raises an error if it's not a fragment
 * or the fragment has no name.
 */
let extractTheFragmentName = str =>
  switch (str |> extractGraphQLOperation) {
  | Fragment({name}) => name
  | _ => raise(Could_not_extract_operation_name)
  };

/**
 * Takes a raw GraphQL document as a string and extracts the subscription name. Raises an error if it's not a subscription
 * or the subscription has no name.
 */
let extractTheSubscriptionName = str =>
  switch (str |> extractGraphQLOperation) {
  | Operation({optype: Subscription, name: Some(name)}) => name
  | _ => raise(Could_not_extract_operation_name)
  };

/**
 * Takes a raw GraphQL document as a string and attempts to extract the refetchable query name if there's one defined.
 * Relay wants you to define refetchable fragments roughly like this:
 * 
 * fragment SomeFragment_someName on SomeType @refetchable(queryName: "SomeFragmentRefetchQuery") {
 *   ...
 * }
 * 
 * So, this functions makes sure that @refetchable is defined and the queryName arg exists, and if so, extracts and
 * returns "SomeFragmentRefetchQuery" as an option string.
 * 
 * Right now, only one directive is allowed per fragment, which might need to change in the future.
 */
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

/**
 * Neither of these (strHasConnection, selectionSetHasConnection) seem to work,
 * or something else is up. TODO fix, but broken currently.
 */
let strHasConnection = str =>
  switch (Str.search_forward(Str.regexp("/@connection/g"), str, 0)) {
  | exception Not_found => false
  | _ => true
  };

let rec selectionSetHasConnection = selections =>
  switch (
    selections
    |> List.find_opt(sel =>
         switch (sel) {
         | Graphql_parser.Field({directives, selection_set}) =>
           switch (
             directives
             |> List.find_opt((dir: Graphql_parser.directive) =>
                  switch (dir) {
                  | {name: "connection"} => true
                  | _ => false
                  }
                )
           ) {
           | Some(_) => true
           | None => selectionSetHasConnection(selection_set)
           }
         | _ => false
         }
       )
  ) {
  | Some(_) => true
  | None => false
  };

let fragmentHasConnectionNotation = str =>
  switch (str |> extractGraphQLOperation) {
  | Fragment({name: _, selection_set}) =>
    selectionSetHasConnection(selection_set)
  | _ => false
  };


let getGraphQLModuleName = opName => opName ++ "_graphql";

/**
 * This is some AST voodoo to extract the provided string from [%relay.<operation> {| ...string here... |}].
 * It basically just matches on the correct AST structure for having an extension node with a string, and 
 * returns that string.
 */
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

/**
 * This returns an AST record representing a module name definition.
 */
let makeModuleNameAst = (~loc, ~moduleName) => {
  pmod_attributes: [],
  pmod_loc: loc,
  pmod_desc:
    Pmod_ident({loc, txt: Lident(getGraphQLModuleName(moduleName))}),
};

/**
 * This constructs a module definition AST, in this case for fragments. Note it's only the definition structure,
 * not the full definition.
 */
let makeFragment = (~loc, ~moduleName, ~refetchableQueryName, ~hasConnection) =>
  Ast_helper.Mod.mk(
    Pmod_structure([
      // The %stri PPX comes from Ppxlib and means "make a structure item AST out of this raw string"
      [%stri module Operation = [%m makeModuleNameAst(~loc, ~moduleName)]], // %m also comes from Ppxlib and means "make a module definition"
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
      hasConnection
        ? [%stri
          module UsePaginationFragment =
            ReasonRelay.MakeUsePaginationFragment({
              type fragment = Operation.fragment;
              type fragmentRef = Operation.fragmentRef;
              type variables = RefetchableOperation.variables;
              let fragmentSpec = Operation.node;
            })
        ]
        : [%stri ()],
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
      hasConnection
        ? [%stri
          let useLegacyPagination = fRef =>
            UsePaginationFragment.useLegacyPagination(
              fRef |> Operation.getFragmentRef,
            )
        ]
        : [%stri ()],
      hasConnection
        ? [%stri
          let useBlockingPagination = fRef =>
            UsePaginationFragment.useBlockingPagination(
              fRef |> Operation.getFragmentRef,
            )
        ]
        : [%stri ()],
      switch (refetchableQueryName) {
      | Some(_) => [%stri
          let useRefetchable = fRef =>
            UseRefetchableFragment.useRefetchable(
              fRef |> Operation.getFragmentRef,
            )
        ]
      | None =>
        %stri
        ()
      },
    ]),
  );

/**
 * Check out the comments for makeFragment, this does the same thing but for queries.
 */
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

/**
 * Check out the comments for makeFragment, this does the same thing but for mutations.
 */
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

/**
 * Check out the comments for makeFragment, this does the same thing but for subscriptions.
 */
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

/**
 * This is what defines [%relay.query] as an extension point and provides the PPX
 * with how to transform the extension point when it finds one.
 */
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

// Same as queryExtension but for [%relay.fragment]
let fragmentExtension =
  Extension.declare(
    "relay.fragment",
    Extension.Context.module_expr,
    Ast_pattern.__,
    (~loc, ~path as _, expr) => {
      let operationStr = extractOperationStr(~loc, ~expr);
      let refetchableQueryName =
        operationStr |> extractFragmentRefetchableQueryName;

      makeFragment(
        ~moduleName=operationStr |> extractTheFragmentName,
        ~refetchableQueryName,
        ~hasConnection=
          switch (
            refetchableQueryName,
            operationStr |> strHasConnection,
          ) {
          | (Some(_), true) => true
          | _ => false
          },
        ~loc,
      );
    },
  );

// Same as queryExtension but for [%relay.mutation]
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

// Same as queryExtension but for [%relay.subscription]
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

// This registers all defined extension points to the "reason-relay" ppx.
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