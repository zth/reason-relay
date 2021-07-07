open Ppxlib;
open Util;

/**
 * This constructs a module definition AST, in this case for fragments. Note it's only the definition structure,
 * not the full definition.
 */
let make =
    (
      ~loc,
      ~moduleName,
      ~refetchableQueryName,
      ~extractedConnectionInfo,
      ~hasInlineDirective,
      ~isOnQuery,
    ) => {
  let typeFromGeneratedModule = makeTypeAccessor(~loc, ~moduleName);
  let valFromGeneratedModule = makeExprAccessor(~loc, ~moduleName);
  let moduleIdentFromGeneratedModule = makeModuleIdent(~loc, ~moduleName);

  let hasConnection =
    switch (extractedConnectionInfo) {
    | Some(_) => true
    | None => false
    };

  Ast_helper.Mod.mk(
    Pmod_structure(
      List.concat([
        FragmentUtils.makeGeneratedModuleImports(
          ~loc,
          ~moduleIdentFromGeneratedModule,
        ),
        FragmentUtils.makeInternalExternals(~loc, ~typeFromGeneratedModule),
        FragmentUtils.makeRefetchableAssets(
          ~loc,
          ~refetchableQueryName,
          ~typeFromGeneratedModule,
          ~makeTypeAccessor,
          ~makeExprAccessor,
          ~valFromGeneratedModule,
          ~fragmentIsOnQuery=isOnQuery,
        ),
        [
          [%stri
            /**React hook for getting the data of this fragment. Pass the `fragmentRefs` of any object where you've spread your fragment into this and get the fragment data back.

### Fragment data outside of React's render
If you're looking for a way to use fragments _outside_ of render (for regular function calls for instance, like for logging etc), look in to adding `@inline` to your fragment definition (like `fragment SomeFragment_user on User @inline {...}`) and then use `Fragment.readInline`. This will allow you to get the fragment data, but outside of React's render.*/
            let use =
                (fRef): [%t typeFromGeneratedModule(["Types", "fragment"])] => {
              let data =
                internal_useFragment(
                  [%e valFromGeneratedModule(["node"])],
                  fRef->[%e valFromGeneratedModule(["getFragmentRef"])],
                );

              RescriptRelay_Internal.internal_useConvertedValue(
                [%e valFromGeneratedModule(["Internal", "convertFragment"])],
                data,
              );
            }
          ],
          [%stri
            /**A version of `Fragment.use` that'll allow you to pass `option<fragmentRefs>` and get `option<'fragmentData>` back. Useful for scenarios where you don't have the fragmentRefs yet.*/
            let useOpt =
                (opt_fRef)
                : option([%t typeFromGeneratedModule(["Types", "fragment"])]) => {
              let fr =
                switch (opt_fRef) {
                | Some(fRef) =>
                  Some(fRef->[%e valFromGeneratedModule(["getFragmentRef"])])
                | None => None
                };

              let nullableFragmentData:
                Js.Nullable.t(
                  [%t typeFromGeneratedModule(["Types", "fragment"])],
                ) =
                internal_useFragmentOpt(
                  [%e valFromGeneratedModule(["node"])],
                  switch (fr) {
                  | Some(fr) => Some(fr)->Js.Nullable.fromOption
                  | None => Js.Nullable.null
                  },
                );

              let data = nullableFragmentData->Js.Nullable.toOption;

              RescriptRelay_Internal.internal_useConvertedValue(
                rawFragment =>
                  switch (rawFragment) {
                  | Some(rawFragment) =>
                    Some(
                      rawFragment->[%e
                                     valFromGeneratedModule([
                                       "Internal",
                                       "convertFragment",
                                     ])
                                   ],
                    )
                  | None => None
                  },
                data,
              );
            }
          ],
          hasInlineDirective
            ? [%stri
              /**This lets you get the data for this fragment _outside of React's render_. Useful for letting functions with with fragments too, for things like logging etc.*/
              let readInline =
                  (fRef): [%t typeFromGeneratedModule(["Types", "fragment"])] => {
                internal_readInlineData(
                  [%e valFromGeneratedModule(["node"])],
                  fRef->[%e valFromGeneratedModule(["getFragmentRef"])],
                )
                ->[%e
                    valFromGeneratedModule(["Internal", "convertFragment"])
                  ];
              }
            ]
            : [%stri ()],
          switch (hasConnection, refetchableQueryName) {
          | (true, Some(queryName)) => [%stri
              /**React hook for paginating a fragment. Paginating with this hook will _not_ cause your component to suspend. If you want pagination to trigger suspense, look into using `Fragment.useBlockingPagination`.*/
              let usePagination = (fr): paginationFragmentReturn => {
                let p =
                  internal_usePaginationFragment(
                    [%e valFromGeneratedModule(["node"])],
                    [%e valFromGeneratedModule(["getFragmentRef"])](fr),
                  );
                let data =
                  RescriptRelay_Internal.internal_useConvertedValue(
                    [%e
                      valFromGeneratedModule(["Internal", "convertFragment"])
                    ],
                    p.data,
                  );

                {
                  data,
                  loadNext:
                    React.useMemo1(
                      ((), ~count, ~onComplete=?, ()) =>
                        p.loadNext(.
                          count,
                          {
                            onComplete:
                              onComplete->RescriptRelay_Internal.internal_nullableToOptionalExnHandler,
                          },
                        ),
                      [|p.loadNext|],
                    ),
                  loadPrevious:
                    React.useMemo1(
                      ((), ~count, ~onComplete=?, ()) =>
                        p.loadPrevious(.
                          count,
                          {
                            onComplete:
                              onComplete->RescriptRelay_Internal.internal_nullableToOptionalExnHandler,
                          },
                        ),
                      [|p.loadPrevious|],
                    ),
                  hasNext: p.hasNext,
                  hasPrevious: p.hasPrevious,
                  isLoadingNext: p.isLoadingNext,
                  isLoadingPrevious: p.isLoadingPrevious,
                  refetch:
                    React.useMemo1(
                      (
                        (),
                        ~variables: [%t
                           makeTypeAccessor(
                             ~loc,
                             ~moduleName=queryName,
                             ["Types", "refetchVariables"],
                           )
                         ],
                        ~fetchPolicy=?,
                        ~onComplete=?,
                        (),
                      ) =>
                        p.refetch(.
                          variables
                          ->[%e
                              makeExprAccessor(
                                ~loc,
                                ~moduleName=queryName,
                                ["Internal", "convertVariables"],
                              )
                            ]
                          ->RescriptRelay_Internal.internal_cleanObjectFromUndefinedRaw,
                          internal_makeRefetchableFnOpts(
                            ~onComplete?,
                            ~fetchPolicy?,
                            (),
                          ),
                        ),
                      [|p.refetch|],
                    ),
                };
              }
            ]
          | _ =>
            %stri
            ()
          },
          switch (hasConnection, refetchableQueryName) {
          | (true, Some(queryName)) => [%stri
              /**Like `Fragment.usePagination`, but calling the pagination function will trigger suspense. Useful for all-at-once pagination.*/
              let useBlockingPagination =
                  (fRef): paginationBlockingFragmentReturn => {
                let p =
                  internal_useBlockingPaginationFragment(
                    [%e valFromGeneratedModule(["node"])],
                    fRef->[%e valFromGeneratedModule(["getFragmentRef"])],
                  );
                let data =
                  RescriptRelay_Internal.internal_useConvertedValue(
                    [%e
                      valFromGeneratedModule(["Internal", "convertFragment"])
                    ],
                    p.data,
                  );

                {
                  data,
                  loadNext:
                    React.useMemo1(
                      ((), ~count, ~onComplete=?, ()) =>
                        p.loadNext(.
                          count,
                          {
                            onComplete:
                              onComplete->RescriptRelay_Internal.internal_nullableToOptionalExnHandler,
                          },
                        ),
                      [|p.loadNext|],
                    ),
                  loadPrevious:
                    React.useMemo1(
                      ((), ~count, ~onComplete=?, ()) =>
                        p.loadPrevious(.
                          count,
                          {
                            onComplete:
                              onComplete->RescriptRelay_Internal.internal_nullableToOptionalExnHandler,
                          },
                        ),
                      [|p.loadPrevious|],
                    ),
                  hasNext: p.hasNext,
                  hasPrevious: p.hasPrevious,
                  refetch:
                    React.useMemo1(
                      (
                        (),
                        ~variables: [%t
                           makeTypeAccessor(
                             ~loc,
                             ~moduleName=queryName,
                             ["Types", "refetchVariables"],
                           )
                         ],
                        ~fetchPolicy=?,
                        ~onComplete=?,
                        (),
                      ) =>
                        p.refetch(.
                          variables
                          ->[%e
                              makeExprAccessor(
                                ~loc,
                                ~moduleName=queryName,
                                ["Internal", "convertVariables"],
                              )
                            ]
                          ->RescriptRelay_Internal.internal_cleanObjectFromUndefinedRaw,
                          internal_makeRefetchableFnOpts(
                            ~onComplete?,
                            ~fetchPolicy?,
                            (),
                          ),
                        ),
                      [|p.refetch|],
                    ),
                };
              }
            ]
          | _ =>
            %stri
            ()
          },
          switch (refetchableQueryName, hasConnection) {
          | (Some(queryName), _) => [%stri
              /**A helper to make refetch variables. */
              let makeRefetchVariables = [%e
                makeExprAccessor(
                  ~loc,
                  ~moduleName=queryName,
                  ["Types", "makeRefetchVariables"],
                )
              ]
            ]
          | _ =>
            %stri
            ()
          },
        ],
      ]),
    ),
  );
};
