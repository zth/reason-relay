/**
 * This PPX defines the [%relay.<operation> {| ... |}] extension points.
 */
// Ppxlib provides a number of helpers for writing and registering PPXs that life is very difficult without.
open Ppxlib;

/**
 * This function takes a GraphQL document as a string (typically extracted from the [%relay.<operation>] nodes),
 * uses Graphql_parser to parse the string into a list of GraphQL definitions, and then extracts the _first_ operation
 * of the document only. This is because Relay disallows multiple operations in the same definition.
 */
let extractGraphQLOperation = (~loc, str) =>
  switch (str |> Graphql_parser.parse) {
  | Ok(definitions) =>
    switch (definitions) {
    | [op] => op
    | _ =>
      Location.raise_errorf(
        ~loc,
        "Only one GraphQL operation per [%%relay]-node is allowed.",
      )
    }
  | Error(err) =>
    Location.raise_errorf(
      ~loc,
      "[%%relay]-nodes must define a single, valid GraphQL operation. GraphQL error message: %s",
      err,
    )
  };

/**
 * Takes a raw GraphQL document as a string and extracts the query name. Raises an error if it's not a query
 * or the query has no name.
 */
let extractTheQueryName = (~loc, str) =>
  switch (extractGraphQLOperation(~loc, str)) {
  | Operation({optype: Query, name: Some(name)}) => name
  | Operation({optype: Query, name: None}) =>
    Location.raise_errorf(~loc, "GraphQL query must be named.")
  | _ =>
    Location.raise_errorf(
      ~loc,
      "[%%relay.query] must contain a query definition, and nothing else.",
    )
  };

/**
 * Takes a raw GraphQL document as a string and extracts the mutation name. Raises an error if it's not a mutation
 * or the mutation has no name.
 */
let extractTheMutationName = (~loc, str) =>
  switch (extractGraphQLOperation(~loc, str)) {
  | Operation({optype: Mutation, name: Some(name)}) => name
  | Operation({optype: Mutation, name: None}) =>
    Location.raise_errorf(~loc, "GraphQL mutation must be named.")
  | _ =>
    Location.raise_errorf(
      ~loc,
      "[%%relay.mutation] must contain a mutation definition, and nothing else.",
    )
  };

/**
 * Takes a raw GraphQL document as a string and extracts the fragment name. Raises an error if it's not a fragment
 * or the fragment has no name.
 */
let extractTheFragmentName = (~loc, str) =>
  switch (extractGraphQLOperation(~loc, str)) {
  | Fragment({name}) => name
  | _ =>
    Location.raise_errorf(
      ~loc,
      "[%%relay.fragment] must contain a fragment definition with a name, and nothing else.",
    )
  };

/**
 * Takes a raw GraphQL document as a string and extracts the subscription name. Raises an error if it's not a subscription
 * or the subscription has no name.
 */
let extractTheSubscriptionName = (~loc, str) =>
  switch (extractGraphQLOperation(~loc, str)) {
  | Operation({optype: Subscription, name: Some(name)}) => name
  | Operation({optype: Subscription, name: None}) =>
    Location.raise_errorf(~loc, "GraphQL subscription must be named.")

  | _ =>
    Location.raise_errorf(
      ~loc,
      "[%%relay.subscription] must contain a subscription definition, and nothing else.",
    )
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
 */
let extractFragmentRefetchableQueryName = (~loc, str) =>
  switch (extractGraphQLOperation(~loc, str)) {
  | Fragment({name: _, directives}) =>
    let refetchableQueryName = ref(None);

    directives
    |> List.iter((dir: Graphql_parser.directive) =>
         switch (dir) {
         | {
             name: "refetchable",
             arguments: [("queryName", `String(queryName))],
           } =>
           refetchableQueryName := Some(queryName)
         | _ => ()
         }
       );

    refetchableQueryName^;
  | _ => None
  };

/**
 * This recursively traverses all selection sets and looks for a @connection directive.
 */
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

// Returns whether a fragment has a @connection annotation or not
let fragmentHasConnectionNotation = (~loc, str) =>
  switch (extractGraphQLOperation(~loc, str)) {
  | Fragment({name: _, selection_set}) =>
    selectionSetHasConnection(selection_set)
  | _ => false
  };

let getGraphQLModuleName = opName =>
  String.capitalize_ascii(opName) ++ "_graphql";

/**
 * This is some AST voodoo to extract the provided string from [%relay.<operation> {| ...string here... |}].
 * It basically just matches on the correct AST structure for having an extension node with a string, and
 * returns that string.
 *
 * It also returns loc, which keeps track of *where* in the code the string is located, so editors can highlight
 * the actual operation string as a whole when it errors, rather than just the module keyword.
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
    ]) => (
      operationStr,
      loc,
    )
  | _ =>
    Location.raise_errorf(
      ~loc,
      "All [%%relay] operations must be provided a string, like [%%relay.query {| { query SomeQuery { id } |}]",
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
      [%stri include Operation.Utils],
      [%stri module Types = Operation.Types],
      switch (refetchableQueryName) {
      | Some(queryName) => [%stri
          module UseRefetchableFragment =
            ReasonRelay.MakeUseRefetchableFragment({
              type fragmentRaw = Operation.Internal.fragmentRaw;
              type fragment = Types.fragment;
              type fragmentRef = Operation.fragmentRef;
              type variables = RefetchableOperation.Types.refetchVariables;
              let fragmentSpec = Operation.node;
              let convertFragment = Operation.Internal.convertFragment;
              let convertVariables = RefetchableOperation.Internal.convertVariables;
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
              type fragmentRaw = Operation.Internal.fragmentRaw;
              type fragment = Types.fragment;
              type fragmentRef = Operation.fragmentRef;
              type variables = RefetchableOperation.Types.refetchVariables;
              let fragmentSpec = Operation.node;
              let convertFragment = Operation.Internal.convertFragment;
              let convertVariables = RefetchableOperation.Internal.convertVariables;
            })
        ]
        : [%stri ()],
      [%stri
        module UseFragment =
          ReasonRelay.MakeUseFragment({
            type fragmentRaw = Operation.Internal.fragmentRaw;
            type fragment = Types.fragment;
            type fragmentRef = Operation.fragmentRef;
            let fragmentSpec = Operation.node;
            let convertFragment = Operation.Internal.convertFragment;
          })
      ],
      [%stri let use = UseFragment.use],
      [%stri let useOpt = UseFragment.useOpt],
      hasConnection
        ? [%stri let usePagination = UsePaginationFragment.usePagination]
        : [%stri ()],
      hasConnection
        ? [%stri
          let useBlockingPagination = UsePaginationFragment.useBlockingPagination
        ]
        : [%stri ()],
      switch (refetchableQueryName) {
      | Some(_) => [%stri
          let useRefetchable = UseRefetchableFragment.useRefetchable
        ]
      | None =>
        %stri
        ()
      },
      switch (refetchableQueryName, hasConnection) {
      | (_, true)
      | (Some(_), _) => [%stri
          let makeRefetchVariables = RefetchableOperation.Types.makeRefetchVariables
        ]
      | _ =>
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
      [%stri include Operation.Utils],
      [%stri module Types = Operation.Types],
      [%stri
        module UseQuery =
          ReasonRelay.MakeUseQuery({
            type responseRaw = Operation.Internal.responseRaw;
            type response = Types.response;
            type variables = Types.variables;
            type queryRef = Operation.queryRef;
            let query = Operation.node;
            let convertResponse = Operation.Internal.convertResponse;
            let convertVariables = Operation.Internal.convertVariables;
          })
      ],
      [%stri let use = UseQuery.use],
      [%stri let fetch = UseQuery.fetch],
      [%stri let fetchPromised = UseQuery.fetchPromised],
      [%stri let usePreloaded = UseQuery.usePreloaded],
    ]),
  );

/**
 * Check out the comments for makeFragment, this does the same thing but for mutations.
 */
let makeMutation = (~loc, ~moduleName) =>
  Ast_helper.Mod.mk(
    Pmod_structure([
      [%stri module Operation = [%m makeModuleNameAst(~loc, ~moduleName)]],
      [%stri include Operation.Utils],
      [%stri module Types = Operation.Types],
      [%stri
        module Mutation =
          ReasonRelay.MakeCommitMutation({
            type variables = Types.variables;
            type responseRaw = Operation.Internal.responseRaw;
            type response = Types.response;
            let node = Operation.node;
            let convertResponse = Operation.Internal.convertResponse;
            let wrapResponse = Operation.Internal.convertWrapResponse;
            let convertVariables = Operation.Internal.convertVariables;
          })
      ],
      [%stri
        module UseMutation =
          ReasonRelay.MakeUseMutation({
            type variables = Types.variables;
            type responseRaw = Operation.Internal.responseRaw;
            type response = Types.response;
            let node = Operation.node;
            let convertResponse = Operation.Internal.convertResponse;
            let wrapResponse = Operation.Internal.convertWrapResponse;
            let convertVariables = Operation.Internal.convertVariables;
          })
      ],
      [%stri let commitMutation = Mutation.commitMutation],
      [%stri let commitMutationPromised = Mutation.commitMutationPromised],
      [%stri let use = UseMutation.use],
    ]),
  );

/**
 * Check out the comments for makeFragment, this does the same thing but for subscriptions.
 */
let makeSubscription = (~loc, ~moduleName) =>
  Ast_helper.Mod.mk(
    Pmod_structure([
      [%stri module Operation = [%m makeModuleNameAst(~loc, ~moduleName)]],
      [%stri include Operation.Utils],
      [%stri module Types = Operation.Types],
      [%stri
        module Subscription =
          ReasonRelay.MakeUseSubscription({
            type variables = Types.variables;
            type responseRaw = Operation.Internal.responseRaw;
            type response = Types.response;
            let node = Operation.node;
            let convertResponse = Operation.Internal.convertResponse;
            let convertVariables = Operation.Internal.convertResponse;
          })
      ],
      [%stri let subscribe = Subscription.subscribe],
    ]),
  );
