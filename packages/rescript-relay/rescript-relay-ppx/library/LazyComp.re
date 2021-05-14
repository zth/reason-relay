open Ppxlib;
open Util;

let lazyExtension =
  Extension.declare(
    "relay.lazyComponent",
    Extension.Context.module_expr,
    /**
     * This matches both SomeModule and SomeModule.make by mapping SomeModule to
       SomeModule.make.
     */
    Ast_pattern.(
      single_expr_payload(
        pexp_ident(__)
        ||| map(pexp_construct(__, none), ~f=(f, ident) =>
              f(Ldot(ident, "make"))
            ),
      )
    ),
    (~loc, ~path as _, ident) => {
    switch (ident) {
    | Ldot(Lident(moduleName), "make") =>
      let moduleIdent =
        Ppxlib.Ast_helper.Mod.ident(~loc, {txt: Lident(moduleName), loc});

      Ast_helper.Mod.mk(
        Pmod_structure([
          [%stri module type T = (module type of [%m moduleIdent])],
          [%stri
            [@val]
            external import_:
              (
                [@as [%e makeStringExpr(~loc, "./" ++ moduleName ++ ".bs.js")]] _,
                unit
              ) =>
              Js.Promise.t(module T) =
              "import"
          ],
          [%stri type component('props) = 'props => React.element],
          [%stri type withDefault('t) = {default: 't}],
          [%stri
            [@module "react"]
            external lazy_:
              (unit => Js.Promise.t(withDefault(component('props)))) =>
              component('props) =
              "lazy"
          ],
          [%stri
            let loadReactComponent = () =>
              import_()
              ->Js.Promise.then_(
                  m => {
                    let (module M): (module T) = m;
                    Js.Promise.resolve({default: M.make});
                  },
                  _,
                )
          ],
          [%stri
            let preload = () => {
              let _ = import_();
              ();
            }
          ],
          [%stri let%private unsafePlaceholder: module T = [%raw {|{}|}]],
          [%stri module UnsafePlaceholder = (val unsafePlaceholder)],
          [%stri let makeProps = UnsafePlaceholder.makeProps],
          [%stri let make = lazy_(loadReactComponent)],
        ]),
      );
    | _ =>
      Location.raise_errorf(
        ~loc,
        "Please provide a reference either to the make function of a top level React component module you want to import (MyComponent.make), or the React component module itself (MyComponent).",
      )
    }
  });
