open GabnorBsFlowParser;

exception Missing_typename_field_on_union;

let parse_options: option(Parser_env.parse_options) =
  Some({
    esproposal_class_instance_fields: true,
    esproposal_class_static_fields: true,
    esproposal_decorators: true,
    esproposal_export_star_as: true,
    esproposal_optional_chaining: true,
    esproposal_nullish_coalescing: true,
    types: true,
    use_strict: false,
  });

type propList('a, 'b) = list(Flow_ast.Type.Object.property('a, 'b));

/**
 * Maps an object prop (represented by a Flow type) to a prop value
 * in our own model of the selections.
 */
let rec mapObjProp =
        (
          ~optional,
          ~state: Types.intermediateState,
          ~path: list(string),
          (_, prop): Flow_ast.Type.t('a, 'b),
        )
        : Types.propValue =>
  switch (prop) {
  | String => {nullable: optional, propType: Scalar(String)}

  // Reason does not have string literals, so we map literals to normal strings
  | StringLiteral(_) => {nullable: optional, propType: Scalar(String)}

  | Nullable((_, String)) => {nullable: true, propType: Scalar(String)}
  | Nullable((_, StringLiteral(_))) => {
      nullable: true,
      propType: Scalar(String),
    }

  // Our compiler fork already emits int/float as generic Flow types instead of number, so these are probably not needed, but leaving them in there anyway just in case.
  | Number
  | NumberLiteral(_) => {nullable: optional, propType: Scalar(Float)}
  | Nullable((_, Number))
  | Nullable((_, NumberLiteral(_))) => {
      nullable: true,
      propType: Scalar(Float),
    }

  // Booleans
  | Boolean => {nullable: optional, propType: Scalar(Boolean)}
  | BooleanLiteral(_) => {nullable: optional, propType: Scalar(Boolean)}
  | Nullable((_, Boolean)) => {nullable: true, propType: Scalar(Boolean)}
  | Nullable((_, BooleanLiteral(_))) => {
      nullable: true,
      propType: Scalar(Boolean),
    }

  // Arrays
  | Array(typ) => {
      nullable: optional,
      propType: Array(typ |> mapObjProp(~path, ~state, ~optional=false)),
    }
  | Nullable((_, Array(typ))) => {
      nullable: true,
      propType: Array(typ |> mapObjProp(~path, ~state, ~optional=false)),
    }
  | Generic({
      id: Unqualified((_, "$ReadOnlyArray")),
      targs: Some((_, [typ])),
    }) => {
      nullable: optional,
      propType: Array(typ |> mapObjProp(~path, ~state, ~optional=false)),
    }
  | Nullable((
      _,
      Generic({
        id: Unqualified((_, "$ReadOnlyArray")),
        targs: Some((_, [typ])),
      }),
    )) => {
      nullable: true,
      propType: Array(typ |> mapObjProp(~path, ~state, ~optional=false)),
    }

  // Objects
  | Object({properties}) => {
      nullable: optional,
      propType: Object(makeObjShape(~path, ~state, properties)),
    }
  | Nullable((_, Object({properties}))) => {
      nullable: true,
      propType: Object(makeObjShape(~path, ~state, properties)),
    }

  // Unions
  | Nullable((
      _,
      Union(
        (_, Object({properties: firstProps})),
        (_, Object({properties: secondProps})),
        maybeMoreMembers,
      ),
    )) =>
    // Node interface special treatment
    // --
    // To simplify using the Node interface, we'll convert any usage of the
    // top level node field with just 1 subselection type into its own type.

    switch (path, firstProps, secondProps) {
    | (
        // Match the `node` field name one level down (from `response` or `fragment`)
        ["node", _],
        [
          Property((
            _,
            {
              value: Init((_, StringLiteral({value: nodeFieldTypeName}))),
              key: Identifier((_, "__typename")),
            },
          )),
          ..._rest,
        ],
        // This matches what Relay outputs as the last matching case for any union/interface
        [
          Property((
            _,
            {
              value: Init((_, StringLiteral({value: {|%other|}}))),
              key: Identifier((_, "__typename")),
            },
          )),
        ],
      ) => {
        nullable: true,
        propType:
          TopLevelNodeField(
            nodeFieldTypeName,
            firstProps |> makeObjShape(~state, ~path),
          ),
      }
    | _ =>
      makeUnion(
        ~firstProps,
        ~secondProps,
        ~maybeMoreMembers,
        ~state,
        ~path,
        ~optional=true,
      )
    }

  | Union(
      (_, Object({properties: firstProps})),
      (_, Object({properties: secondProps})),
      maybeMoreMembers,
    ) =>
    makeUnion(
      ~firstProps,
      ~state,
      ~secondProps,
      ~maybeMoreMembers,
      ~path,
      ~optional,
    )

  // This handles generic type references, extracting it if it's an enum
  | Generic({id: Unqualified((_, typeName))}) => {
      nullable: optional,
      propType:
        switch (
          state.enums
          |> Tablecloth.List.find(~f=(enum: Types.fullEnum) =>
               enum.name == typeName
             )
        ) {
        | Some(name) => Enum(name)
        | None => TypeReference(typeName |> Utils.unmaskDots)
        },
    }
  | Nullable((_, Generic({id: Unqualified((_, typeName))}))) => {
      nullable: true,
      propType:
        switch (
          state.enums
          |> Tablecloth.List.find(~f=(enum: Types.fullEnum) =>
               enum.name == typeName
             )
        ) {
        | Some(name) => Enum(name)
        | None => TypeReference(typeName |> Utils.unmaskDots)
        },
    }

  // Handle anything we haven't matched above, including any which needs no explicit match as it's handled here
  | _ => {nullable: optional, propType: Scalar(Any)}
  }

/**
 * This turns a list of props from the Flow AST into our own object definition.
 */
and makeObjShape =
    (
      ~path: list(string),
      ~state,
      props: list(Flow_ast.Type.Object.property('a, 'b)),
    )
    : Types.object_ => {
  let values: ref(list(Types.propValues)) = ref([]);
  let addValue = value => values := [value, ...values^];
  let addFragmentRef = rawFragmentRef =>
    addValue(
      Types.(
        FragmentRef(
          Tablecloth.String.dropRight(
            ~count=String.length("$ref"),
            rawFragmentRef,
          ),
        )
      ),
    );

  props
  |> List.iter((prop: Flow_ast.Type.Object.property('a, 'b)) =>
       switch (prop) {
       // Map __id to dataId
       | Property((_, {value: Init(_), key: Identifier((_, "__id" as id))})) =>
         addValue(Types.(Prop(id, {nullable: false, propType: DataId})))
       // Map all $fragmentRefs. These are either a single type (+$fragmentRefs: SomeFragment_someType$ref), or an intersection of types (+$fragmentRefs: SomeFragment_someType$ref & SomeOtherFragment_someType$ref)
       | Property((
           _,
           {value: Init((_, typ)), key: Identifier((_, "$fragmentRefs"))},
         )) =>
         switch (typ) {
         | Generic({id: Unqualified((_, firstId))}) =>
           addFragmentRef(firstId)
         | Intersection(
             (_, Generic({id: Unqualified((_, firstId))})),
             (_, Generic({id: Unqualified((_, secondId))})),
             maybeMore,
           ) =>
           addFragmentRef(firstId);
           addFragmentRef(secondId);
           maybeMore
           |> List.iter((intersectionMember: Flow_ast.Type.t('a, 'b)) =>
                switch (intersectionMember) {
                | (_, Generic({id: Unqualified((_, fragmentName))})) =>
                  addFragmentRef(fragmentName)
                | _ => ()
                }
              );
         | _ => ()
         }
       // Do not add any props that start with "$". These are usually internal Flow types that we don't need/use in Reason
       | Property((_, {key: Identifier((_, id))}))
           when Tablecloth.String.startsWith(~prefix="$", id) =>
         ()
       | Property((
           _,
           {optional, value: Init(typ), key: Identifier((_, id))},
         )) =>
         addValue(
           Types.(
             Prop(
               id,
               typ |> mapObjProp(~state, ~path=[id, ...path], ~optional),
             )
           ),
         )
       | _ => ()
       }
     );

  {
    atPath: path,
    values: values^ |> Tablecloth.List.reverse |> Tablecloth.Array.fromList,
  };
}
and makeUnionMember =
    (~state, ~path, props: list(Flow_ast.Type.Object.property('a, 'b)))
    : Types.unionMember => {
  let name = ref(None);
  let filteredProps = ref([]);

  props
  |> Tablecloth.List.iter(~f=(prop: Flow_ast.Type.Object.property('a, 'b)) =>
       switch (prop) {
       | Property((
           _,
           {
             value: Init((_, StringLiteral({value: typeName}))),
             key: Identifier((_, "__typename")),
           },
         )) =>
         name := Some(typeName)
       | _ => filteredProps := [prop, ...filteredProps^]
       }
     );

  switch (name^) {
  | Some(name) => {
      name,
      shape: filteredProps^ |> makeObjShape(~state, ~path=[name, ...path]),
    }
  | None => raise(Missing_typename_field_on_union)
  };
}
and makeUnionDefinition =
    (~firstProps, ~secondProps, ~maybeMoreMembers, ~path, ~state): Types.union => {
  let unionMembers =
    ref([
      firstProps |> makeUnionMember(~state, ~path),
      secondProps |> makeUnionMember(~state, ~path),
    ]);

  maybeMoreMembers
  |> Tablecloth.List.iter(~f=(prop: Flow_ast.Type.t('a, 'b)) =>
       switch (prop) {
       | (_, Object({properties})) =>
         unionMembers :=
           [properties |> makeUnionMember(~state, ~path), ...unionMembers^]
       | _ => ()
       }
     );

  {
    members:
      unionMembers^
      |> Tablecloth.List.filter(~f=(member: Types.unionMember) =>
           member.name != {|%other|}
         ),
    atPath: path,
  };
}
and makeUnion =
    (~firstProps, ~secondProps, ~maybeMoreMembers, ~path, ~optional, ~state) => {
  {
    nullable: optional,
    propType:
      Union(
        makeUnionDefinition(
          ~firstProps,
          ~secondProps,
          ~maybeMoreMembers,
          ~path,
          ~state,
        ),
      ),
  };
};

let flowTypesToFullState = (~content, ~operationType): Types.fullState => {
  let initialState: Types.intermediateState = {
    enums: [],
    objects: [],
    variables: None,
    response: None,
    rawResponse: None,
    fragment: None,
  };

  let state = ref(initialState);

  let setState = updater => state := updater(state^);

  // This iterates through all top level type declarations from the Flow types generated
  // by the Relay compiler, and extracts the things we're interested in.
  switch (
    operationType,
    Parser_flow.program(~fail=true, ~parse_options, content),
  ) {
  | (
      Types.Mutation(name) | Query(name) | Subscription(name),
      ((_, statements, _), []),
    ) =>
    statements
    |> List.iter(((_, statement): Flow_ast.Statement.t(Loc.t, Loc.t)) =>
         switch (statement) {
         /*** Avoid full mutation object */
         | ExportNamedDeclaration({
             declaration:
               Some((
                 _,
                 TypeAlias({right: (_, Object(_)), id: (_, typeName)}),
               )),
           })
             when typeName == name =>
           ()
         /***
          * Objects
          */
         | ExportNamedDeclaration({
             declaration:
               Some((
                 _,
                 TypeAlias({
                   right: (_, Object({properties})),
                   id: (_, typeName),
                 }),
               )),
           }) =>
           switch (typeName) {
           | _ when typeName == name ++ "Variables" =>
             setState(state =>
               {
                 ...state,
                 variables:
                   Some({
                     originalFlowTypeName: None,
                     foundInUnion: false,
                     definition:
                       properties
                       |> makeObjShape(~state, ~path=["variables"]),
                   }),
               }
             )
           | _ when typeName == name ++ "Response" =>
             setState(state =>
               {
                 ...state,
                 response:
                   Some({
                     originalFlowTypeName: None,
                     foundInUnion: false,
                     definition:
                       properties |> makeObjShape(~state, ~path=["response"]),
                   }),
               }
             )
           | _ when typeName == name ++ "RawResponse" =>
             setState(state =>
               {
                 ...state,
                 rawResponse:
                   Some({
                     originalFlowTypeName: None,
                     foundInUnion: false,
                     definition:
                       properties
                       |> makeObjShape(~state, ~path=["rawResponse"]),
                   }),
               }
             )
           | typeName =>
             setState(state =>
               {
                 ...state,
                 objects: [
                   {
                     originalFlowTypeName: Some(typeName),
                     foundInUnion: false,
                     definition:
                       properties |> makeObjShape(~state, ~path=["objects"]),
                   },
                   ...state.objects,
                 ],
               }
             )
           }
         /***
          * Enums
          */
         | ExportNamedDeclaration({
             declaration:
               Some((
                 _,
                 TypeAlias({
                   right: (
                     _,
                     Union(
                       (_, StringLiteral({value: firstMember})),
                       maybeSecondMember,
                       maybeMore,
                     ),
                   ),
                   id: (_, typeName),
                 }),
               )),
           }) =>
           let enum: Types.fullEnum = {
             name: typeName,
             values: [|firstMember|],
           };

           let addValue = v => enum.values |> Js.Array.push(v) |> ignore;

           [maybeSecondMember, ...maybeMore]
           ->Belt.List.forEach(
               fun
               | (_, StringLiteral({value: v}))
                   when v != "%future added value" =>
                 addValue(v)
               | _ => (),
             );

           setState(state => {...state, enums: [enum, ...state.enums]});
         | _ => ()
         }
       )
  | (Fragment(name, plural), ((_, statements, _), [])) =>
    statements
    |> List.iter(((_, statement): Flow_ast.Statement.t(Loc.t, Loc.t)) =>
         switch (statement) {
         /***
          * Objects
          */

         /***
          * Match fragments
          */
         // Union
         | ExportNamedDeclaration({
             declaration:
               Some((
                 _,
                 TypeAlias({
                   right: (
                     _,
                     // Match unions
                     Union(
                       (_, Object({properties: firstProps})),
                       (_, Object({properties: secondProps})),
                       maybeMoreMembers,
                     ) |
                     Generic({
                       id: Unqualified((_, "$ReadOnlyArray")),
                       targs:
                         Some((
                           _,
                           [
                             (
                               _,
                               Union(
                                 (_, Object({properties: firstProps})),
                                 (_, Object({properties: secondProps})),
                                 maybeMoreMembers,
                               ),
                             ),
                           ],
                         )),
                     }),
                   ),
                   id: (_, typeName),
                 }),
               )),
           }) =>
           switch (typeName) {
           | _ when typeName == name =>
             setState(state =>
               {
                 ...state,
                 fragment:
                   Some({
                     plural,
                     definition:
                       Union(
                         makeUnionDefinition(
                           ~firstProps,
                           ~state,
                           ~secondProps,
                           ~maybeMoreMembers,
                           ~path=["fragment"],
                         ),
                       ),
                     name,
                   }),
               }
             )
           | _ => ()
           }
         // Regular objects
         | ExportNamedDeclaration({
             declaration:
               Some((
                 _,
                 TypeAlias({
                   right: (
                     _,
                     // Match regular object, or a $ReadOnlyArray (plural fragments)
                     Object({properties}) |
                     Generic({
                       id: Unqualified((_, "$ReadOnlyArray")),
                       targs: Some((_, [(_, Object({properties}))])),
                     }),
                   ),
                   id: (_, typeName),
                 }),
               )),
           }) =>
           switch (typeName) {
           | _ when typeName == name =>
             setState(state =>
               {
                 ...state,
                 fragment:
                   Some({
                     plural,
                     definition:
                       Object(
                         properties
                         |> makeObjShape(~state, ~path=["fragment"]),
                       ),
                     name,
                   }),
               }
             )
           | _ when !Tablecloth.String.contains(~substring="$", typeName) =>
             setState(state =>
               {
                 ...state,
                 objects: [
                   {
                     foundInUnion: false,
                     originalFlowTypeName: Some(typeName),
                     definition:
                       properties |> makeObjShape(~state, ~path=["objects"]),
                   },
                   ...state.objects,
                 ],
               }
             )
           | _ => ()
           }
         /***
          * Enums
          */
         | ExportNamedDeclaration({
             declaration:
               Some((
                 _,
                 TypeAlias({
                   right: (
                     _,
                     Union(
                       (_, StringLiteral({value: firstMember})),
                       maybeSecondMember,
                       maybeMore,
                     ),
                   ),
                   id: (_, typeName),
                 }),
               )),
           }) =>
           let enum: Types.fullEnum = {
             name: typeName,
             values: [|firstMember|],
           };

           let addValue = v => enum.values |> Js.Array.push(v) |> ignore;

           [maybeSecondMember, ...maybeMore]
           ->Belt.List.forEach(
               fun
               | (_, StringLiteral({value: v}))
                   when v != "%future added value" =>
                 addValue(v)
               | _ => (),
             );

           setState(state => {...state, enums: [enum, ...state.enums]});
         | _ => ()
         }
       )
  | (_, _errors) => Js.log("Parse error.")
  };

  state^ |> TransformState.intermediateToFull;
};
