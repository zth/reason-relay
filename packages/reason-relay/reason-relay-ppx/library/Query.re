open Ppxlib;
open Util;

/**
 * Check out the comments for makeFragment, this does the same thing but for queries.
 */
let make = (~loc, ~moduleName, ~hasRawResponseType) => {
  let typeFromGeneratedModule = makeTypeAccessor(~loc, ~moduleName);
  let valFromGeneratedModule = makeExprAccessor(~loc, ~moduleName);
  let moduleIdentFromGeneratedModule = makeModuleIdent(~loc, ~moduleName);

  Ast_helper.Mod.mk(
    Pmod_structure([
      [%stri include [%m moduleIdentFromGeneratedModule(["Utils"])]],
      [%stri module Types = [%m moduleIdentFromGeneratedModule(["Types"])]],
      [%stri
        module Internal = {
          [@bs.module "relay-runtime"]
          external internal_createOperationDescriptor:
            (
              ReasonRelay.queryNode(
                [%t typeFromGeneratedModule(["relayOperationNode"])],
              ),
              [%t typeFromGeneratedModule(["Types", "variables"])]
            ) =>
            ReasonRelay.operationDescriptor =
            "createOperationDescriptor";

          type useQueryConfig = {
            fetchKey: option(string),
            fetchPolicy: option(string),
            [@bs.as "UNSTABLE_renderPolicy"]
            renderPolicy: option(string),
            networkCacheConfig: option(ReasonRelay.cacheConfig),
          };

          [@bs.module "react-relay/hooks"]
          external internal_useQuery:
            (
              ReasonRelay.queryNode(
                [%t typeFromGeneratedModule(["relayOperationNode"])],
              ),
              [%t typeFromGeneratedModule(["Types", "variables"])],
              useQueryConfig
            ) =>
            [%t typeFromGeneratedModule(["Types", "response"])] =
            "useLazyLoadQuery";

          [@bs.module "react-relay/hooks"]
          external internal_usePreloadedQuery:
            (
              ReasonRelay.queryNode(
                [%t typeFromGeneratedModule(["relayOperationNode"])],
              ),
              [%t typeFromGeneratedModule(["queryRef"])],
              option({. "UNSTABLE_renderPolicy": option(string)})
            ) =>
            [%t typeFromGeneratedModule(["Types", "response"])] =
            "usePreloadedQuery";

          type useQueryLoaderOptions = {
            fetchPolicy: option(ReasonRelay.fetchPolicy),
            networkCacheConfig: option(ReasonRelay.cacheConfig),
          };

          [@bs.module "react-relay/hooks"]
          external internal_useQueryLoader:
            ReasonRelay.queryNode(
              [%t typeFromGeneratedModule(["relayOperationNode"])],
            ) =>
            (
              Js.nullable([%t typeFromGeneratedModule(["queryRef"])]),
              (
                [%t typeFromGeneratedModule(["Types", "variables"])],
                useQueryLoaderOptions
              ) =>
              unit,
              unit => unit,
            ) =
            "useQueryLoader";

          [@bs.module "react-relay/hooks"]
          external internal_fetchQuery:
            (
              ReasonRelay.Environment.t,
              ReasonRelay.queryNode(
                [%t typeFromGeneratedModule(["relayOperationNode"])],
              ),
              [%t typeFromGeneratedModule(["Types", "variables"])],
              option(ReasonRelay.fetchQueryOptions)
            ) =>
            ReasonRelay.Observable.t(
              [%t typeFromGeneratedModule(["Types", "response"])],
            ) =
            "fetchQuery";
        }
      ],
      [%stri
        let use =
            (
              ~variables: [%t typeFromGeneratedModule(["Types", "variables"])],
              ~fetchPolicy: option(ReasonRelay.fetchPolicy)=?,
              ~renderPolicy: option(ReasonRelay.renderPolicy)=?,
              ~fetchKey: option(string)=?,
              ~networkCacheConfig: option(ReasonRelay.cacheConfig)=?,
              (),
            )
            : [%t typeFromGeneratedModule(["Types", "response"])] => {
          let data: [%t typeFromGeneratedModule(["Types", "response"])] =
            Internal.internal_useQuery(
              [%e valFromGeneratedModule(["node"])],
              variables
              ->ReasonRelay_Internal.internal_cleanVariablesRaw
              ->[%e valFromGeneratedModule(["Internal", "convertVariables"])]
              ->ReasonRelay_Internal.internal_cleanObjectFromUndefinedRaw,
              {
                fetchKey,
                fetchPolicy: fetchPolicy->ReasonRelay.mapFetchPolicy,
                renderPolicy: renderPolicy->ReasonRelay.mapRenderPolicy,
                networkCacheConfig,
              },
            );

          ReasonRelay_Internal.internal_useConvertedValue(
            [%e valFromGeneratedModule(["Internal", "convertResponse"])],
            data,
          );
        }
      ],
      [%stri
        let useLoader =
            ()
            : (
                option([%t typeFromGeneratedModule(["queryRef"])]),
                (
                  ~variables: [%t
                                typeFromGeneratedModule([
                                  "Types",
                                  "variables",
                                ])
                              ],
                  ~fetchPolicy: ReasonRelay.fetchPolicy=?,
                  ~networkCacheConfig: ReasonRelay.cacheConfig=?,
                  unit
                ) =>
                unit,
                unit => unit,
              ) => {
          let (nullableQueryRef, loadQueryFn, disposableFn) =
            Internal.internal_useQueryLoader(
              [%e valFromGeneratedModule(["node"])],
            );

          // TODO: Fix stability of this reference. Can't seem to use React.useCallback with labelled arguments for some reason.
          let loadQuery =
              (
                ~variables: [%t
                   typeFromGeneratedModule(["Types", "variables"])
                 ],
                ~fetchPolicy=?,
                ~networkCacheConfig=?,
                (),
              ) =>
            loadQueryFn(
              variables->[%e
                           valFromGeneratedModule([
                             "Internal",
                             "convertVariables",
                           ])
                         ],
              {fetchPolicy, networkCacheConfig},
            );

          (Js.Nullable.toOption(nullableQueryRef), loadQuery, disposableFn);
        }
      ],
      [%stri
        let fetch =
            (
              ~environment: ReasonRelay.Environment.t,
              ~variables: [%t
                 typeFromGeneratedModule(["Types", "variables"])
               ],
              ~onResult:
                 Belt.Result.t(
                   [%t typeFromGeneratedModule(["Types", "response"])],
                   Js.Exn.t,
                 ) =>
                 unit,
              ~networkCacheConfig: option(ReasonRelay.cacheConfig)=?,
              ~fetchPolicy: option(ReasonRelay.fetchQueryFetchPolicy)=?,
              (),
            )
            : unit => {
          let _ =
            Internal.internal_fetchQuery(
              environment,
              [%e valFromGeneratedModule(["node"])],
              variables->[%e
                           valFromGeneratedModule([
                             "Internal",
                             "convertVariables",
                           ])
                         ],
              Some({
                networkCacheConfig,
                fetchPolicy: fetchPolicy->ReasonRelay.mapFetchQueryFetchPolicy,
              }),
            )
            ->ReasonRelay.Observable.(
                subscribe(
                  makeObserver(
                    ~next=
                      res =>
                        onResult(
                          Ok(
                            res->[%e
                                   valFromGeneratedModule([
                                     "Internal",
                                     "convertResponse",
                                   ])
                                 ],
                          ),
                        ),
                    ~error=err => onResult(Error(err)),
                    (),
                  ),
                )
              );
          ();
        }
      ],
      [%stri
        let fetchPromised =
            (
              ~environment: ReasonRelay.Environment.t,
              ~variables: [%t
                 typeFromGeneratedModule(["Types", "variables"])
               ],
              ~networkCacheConfig: option(ReasonRelay.cacheConfig)=?,
              ~fetchPolicy: option(ReasonRelay.fetchQueryFetchPolicy)=?,
              (),
            )
            : Promise.t(
                Belt.Result.t(
                  [%t typeFromGeneratedModule(["Types", "response"])],
                  Js.Exn.t,
                ),
              ) => {
          let (promise, resolve) = Promise.pending();

          let _ =
            Internal.internal_fetchQuery(
              environment,
              [%e valFromGeneratedModule(["node"])],
              variables->[%e
                           valFromGeneratedModule([
                             "Internal",
                             "convertVariables",
                           ])
                         ],
              Some({
                networkCacheConfig,
                fetchPolicy: fetchPolicy->ReasonRelay.mapFetchQueryFetchPolicy,
              }),
            )
            ->ReasonRelay.Observable.(
                subscribe(
                  makeObserver(
                    ~next=
                      res => {
                        resolve(
                          Ok(
                            res->[%e
                                   valFromGeneratedModule([
                                     "Internal",
                                     "convertResponse",
                                   ])
                                 ],
                          ),
                        )
                      },
                    ~error=err => {resolve(Error(err))},
                    (),
                  ),
                )
              );

          promise;
        }
      ],
      [%stri
        let usePreloaded =
            (
              ~queryRef: [%t typeFromGeneratedModule(["queryRef"])],
              ~renderPolicy: option(ReasonRelay.renderPolicy)=?,
              (),
            )
            : [%t typeFromGeneratedModule(["Types", "response"])] => {
          let data: [%t typeFromGeneratedModule(["Types", "response"])] =
            Internal.internal_usePreloadedQuery(
              [%e valFromGeneratedModule(["node"])],
              queryRef,
              switch (renderPolicy) {
              | Some(_) =>
                Some({
                  "UNSTABLE_renderPolicy":
                    renderPolicy->ReasonRelay.mapRenderPolicy,
                })
              | None => None
              },
            );
          ReasonRelay_Internal.internal_useConvertedValue(
            [%e valFromGeneratedModule(["Internal", "convertResponse"])],
            data,
          );
        }
      ],
      hasRawResponseType
        ? [%stri
          let commitLocalPayload =
              (
                ~environment: ReasonRelay.Environment.t,
                ~variables: [%t
                   typeFromGeneratedModule(["Types", "variables"])
                 ],
                ~payload: [%t
                   typeFromGeneratedModule(["Types", "rawResponse"])
                 ],
              ) => {
            let operationDescriptor =
              Internal.internal_createOperationDescriptor(
                [%e valFromGeneratedModule(["node"])],
                variables->[%e
                             valFromGeneratedModule([
                               "Internal",
                               "convertVariables",
                             ])
                           ],
              );

            environment->ReasonRelay.Environment.commitPayload(
              operationDescriptor,
              payload->[%e
                         valFromGeneratedModule([
                           "Internal",
                           "convertWrapRawResponse",
                         ])
                       ],
            );
          }
        ]
        : [%stri ()],
    ]),
  );
};
