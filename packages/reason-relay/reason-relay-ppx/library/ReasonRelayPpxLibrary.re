open Ppxlib;
open Util;

module Util = Util;

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
            operationStr |> fragmentHasConnectionNotation,
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