open BsFlowParser;

exception Missing_typename_field_on_union;
exception Could_not_map_number;
exception No_extractable_operations_found;

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

type obj('a, 'b) = {
  name: string,
  atPath: list(string),
  properties: propList('a, 'b),
};

type fragment('a, 'b) = {
  name: string,
  plural: bool,
  properties: propList('a, 'b),
};

type input('a, 'b) = {
  name: string,
  properties: propList('a, 'b),
};

/**
 * The state keeps track of what's defined in the
 * Flow types. It's then used to print the Reason
 * types.
 */
type state('a, 'b) = {
  enums: list(string), // A list of the enum names. Enums are referenced by the global generated SchemaAssets.re file.
  objects: list(obj('a, 'b)),
  variables: option(propList('a, 'b)),
  response: option(propList('a, 'b)),
  fragment: option(fragment('a, 'b)),
  input: option(input('a, 'b)),
};

type controls = {addUnion: Types.union => unit};
/**
 * Maps an object prop (represented by a Flow type) to a prop value
 * in our own model of the selections.
 */
let rec mapObjProp =
        (
          ~state: state('a, 'a),
          ~optional,
          ~controls,
          ~path: list(string),
          (_, prop): Flow_ast.Type.t('a, 'b),
        )
        : Types.propValue =>
  switch (prop) {
  | String => {nullable: optional, propType: Scalar(String)}
  | StringLiteral(_) => {nullable: optional, propType: Scalar(String)} // Reason does not have string literals, so we map literals to normal strings
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
      propType:
        Array(typ |> mapObjProp(~controls, ~state, ~path, ~optional=false)),
    }
  | Nullable((_, Array(typ))) => {
      nullable: true,
      propType:
        Array(typ |> mapObjProp(~controls, ~state, ~path, ~optional=false)),
    }
  | Generic({
      id: Unqualified((_, "$ReadOnlyArray")),
      targs: Some((_, [typ])),
    }) => {
      nullable: true,
      propType:
        Array(typ |> mapObjProp(~controls, ~state, ~path, ~optional=false)),
    }

  | Nullable((
      _,
      Generic({
        id: Unqualified((_, "$ReadOnlyArray")),
        targs: Some((_, [typ])),
      }),
    )) => {
      nullable: true,
      propType:
        Array(typ |> mapObjProp(~controls, ~state, ~path, ~optional=false)),
    }

  // Objects
  | Object({properties}) => {
      nullable: optional,
      propType: Object(makeObjShape(~controls, ~state, ~path, properties)),
    }
  | Nullable((_, Object({properties}))) => {
      nullable: true,
      propType: Object(makeObjShape(~controls, ~state, ~path, properties)),
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
    makeUnion(
      ~controls,
      ~firstProps,
      ~secondProps,
      ~maybeMoreMembers,
      ~state,
      ~path,
      ~optional=true,
    )

  | Union(
      (_, Object({properties: firstProps})),
      (_, Object({properties: secondProps})),
      maybeMoreMembers,
    ) =>
    makeUnion(
      ~controls,
      ~firstProps,
      ~secondProps,
      ~maybeMoreMembers,
      ~state,
      ~path,
      ~optional,
    )

  // This handles generic type references.
  // We check whether it's an enum or object, and if not, we just print it as a type reference.
  | Generic({id: Unqualified((_, typeName))}) =>
    makeGenericTypeReference(~typeName, ~state, ~optional, ~controls, ~path)

  | Nullable((_, Generic({id: Unqualified((_, typeName))}))) =>
    makeGenericTypeReference(
      ~typeName,
      ~state,
      ~optional=true,
      ~controls,
      ~path,
    )

  // Handle anything we haven't matched above, including any which needs no explicit match as it's handled here
  | _ => {nullable: optional, propType: Scalar(Any)}
  }
and makeObjShape =
    (
      ~controls,
      ~state: state('a, 'b),
      ~path: list(string),
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
               typ
               |> mapObjProp(
                    ~controls,
                    ~state,
                    ~path=[id, ...path],
                    ~optional,
                  ),
             )
           ),
         )
       | _ => ()
       }
     );

  {values: values^ |> Belt.List.toArray};
}
and makeUnionMember =
    (
      ~controls,
      ~state,
      ~path,
      props: list(Flow_ast.Type.Object.property('a, 'b)),
    )
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
      shape: filteredProps^ |> makeObjShape(~controls, ~state, ~path),
    }
  | None => raise(Missing_typename_field_on_union)
  };
}
and makeGenericTypeReference =
    (~optional, ~state, ~typeName, ~controls, ~path): Types.propValue => {
  nullable: optional,
  propType:
    switch (
      state.enums |> Tablecloth.List.find(~f=name => name == typeName),
      state.objects
      |> Tablecloth.List.find(~f=({name}: obj('a, 'b)) => name == typeName),
    ) {
    | (Some(_), _) => Enum(typeName)
    | (_, Some(obj)) =>
      Object(obj.properties |> makeObjShape(~controls, ~state, ~path)) // We inline all object definitions. I feel it gives better DX than printing them as separate objects.
    | (_, _) => TypeReference(typeName |> Utils.unmaskDots)
    },
}
and makeUnion =
    (
      ~controls: controls,
      ~firstProps,
      ~secondProps,
      ~maybeMoreMembers,
      ~state,
      ~path,
      ~optional,
    ) => {
  let unionMembers =
    ref([
      firstProps |> makeUnionMember(~controls, ~state, ~path),
      secondProps |> makeUnionMember(~controls, ~state, ~path),
    ]);

  maybeMoreMembers
  |> Tablecloth.List.iter(~f=(prop: Flow_ast.Type.t('a, 'b)) =>
       switch (prop) {
       | (_, Object({properties})) =>
         unionMembers :=
           [
             properties |> makeUnionMember(~controls, ~state, ~path),
             ...unionMembers^,
           ]
       | _ => ()
       }
     );

  let union: Types.union = {
    members:
      unionMembers^
      |> Tablecloth.List.filter(~f=(member: Types.unionMember) =>
           member.name != {|%other|}
         ),
    atPath: path,
  };

  controls.addUnion(union);
  {nullable: optional, propType: Union(Printer.makeUnionName(path))};
};

[@gentype]
let printFromFlowTypes = (~content, ~operationType) => {
  let state =
    ref({
      enums: [],
      objects: [],
      variables: None,
      response: None,
      fragment: None,
      input: None,
    });

  let setState = updater => state := updater(state^);
  let unions: ref(list(Types.union)) = ref([]);
  let addUnion = union => {
    unions := [union, ...unions^];
  };
  let controls = {addUnion: addUnion};

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
             setState(state => {...state, variables: Some(properties)})
           | _ when typeName == name ++ "Response" =>
             setState(state => {...state, response: Some(properties)})
           | _ when typeName == name ++ "Input" =>
             setState(state => {...state, response: Some(properties)})
           | _ =>
             setState(state =>
               {
                 ...state,
                 objects: [
                   {name: typeName, atPath: [typeName], properties},
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
                   right: (_, Union((_, StringLiteral(_)), _, _)),
                   id: (_, typeName),
                 }),
               )),
           }) =>
           setState(state => {...state, enums: [typeName, ...state.enums]})
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
               {...state, fragment: Some({plural, properties, name})}
             )
           | _ when !Tablecloth.String.contains(~substring="$", typeName) =>
             setState(state =>
               {
                 ...state,
                 objects: [
                   {name: typeName, atPath: [typeName], properties},
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
                   right: (_, Union((_, StringLiteral(_)), _, _)),
                   id: (_, typeName),
                 }),
               )),
           }) =>
           setState(state => {...state, enums: [typeName, ...state.enums]})
         | _ => ()
         }
       )
  | (_, _errors) => Js.log("Parse error.")
  };

  let finalStr = ref("");
  let addToStr = Utils.makeAddToStr(finalStr);

  let definitions: ref(list(Types.rootType)) = ref([]);
  let addDefinition = Utils.makeAddToList(definitions);

  // We check and add all definitions we've found to a list that'll later be printed as types.
  switch (state^.variables) {
  | Some(variables) =>
    addDefinition(
      Types.(
        Variables(
          variables
          |> makeObjShape(~controls, ~state=state^, ~path=["variables"]),
        )
      ),
    )
  | None => ()
  };

  switch (state^.response) {
  | Some(response) =>
    addDefinition(
      Types.(
        Operation(
          response
          |> makeObjShape(~controls, ~state=state^, ~path=["response"]),
        )
      ),
    )
  | None => ()
  };

  switch (state^.fragment) {
  | Some({properties, plural, name}) =>
    let shape =
      properties
      |> makeObjShape(~controls, ~state=state^, ~path=["response"]);
    addDefinition(
      plural ? Types.PluralFragment(shape) : Types.Fragment(shape),
    );
    addToStr(Printer.fragmentRefAssets(name));
  | None => ()
  };

  switch (state^.input) {
  | Some(input) =>
    addDefinition(
      Types.(
        InputObject(
          input.name |> Printer.getInputObjName,
          input.properties
          |> makeObjShape(~controls, ~state=state^, ~path=["input"]),
        )
      ),
    )
  | None => ()
  };

  // This prints the opaque union types. We need to do it after everything above as makeObjShape will add the unions.
  addToStr(Printer.opaqueUnionType(unions^) ++ "\n");

  // Print all definitions we've found
  Printer.(definitions^ |> List.iter(def => def |> printRootType |> addToStr));

  /**
   * This adds operationType, which is refernced in the raw output of the Relay
   * runtime representation.
   */
  addToStr(Printer.operationType(operationType));

  // We always output a Unions module, so we can include it through our PPX
  addToStr("\n\n" ++ Printer.unionModule(unions^));

  switch (state^) {
  | {fragment: None, response: None, variables: None} =>
    raise(No_extractable_operations_found)
  | _ => ()
  };

  finalStr^;
};