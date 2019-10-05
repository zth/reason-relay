open BsFlowParser;

[@bs.module "./testStrings"] external testQuery: string = "testQuery";

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

let refName = "$ref";

[@gentype]
type operationType =
  | Fragment(string, bool)
  | Mutation(string)
  | Subscription(string)
  | Query(string);

type enumName = string;

type propList('a, 'b) = list(Flow_ast.Type.Object.property('a, 'b));

type obj('a, 'b) = {
  name: string,
  properties: propList('a, 'b),
};

type fragment('a, 'b) = {
  plural: bool,
  properties: propList('a, 'b),
};

type input('a, 'b) = {
  name: string,
  properties: propList('a, 'b),
};

type state('a, 'b) = {
  enums: list(enumName),
  objects: list(obj('a, 'b)),
  variables: option(propList('a, 'b)),
  response: option(propList('a, 'b)),
  fragment: option(fragment('a, 'b)),
  input: option(input('a, 'b)),
};

type controls = {addUnion: Types.union => unit};

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
  | StringLiteral(_) => {nullable: optional, propType: Scalar(String)}
  | Nullable((_, String)) => {nullable: true, propType: Scalar(String)}
  | Nullable((_, StringLiteral(_))) => {
      nullable: true,
      propType: Scalar(String),
    }

  | Number => {nullable: optional, propType: Scalar(Int)} // Must lookup
  | NumberLiteral(_) => {nullable: optional, propType: Scalar(Int)} // Must lookup
  | Nullable((_, Number)) => {nullable: true, propType: Scalar(Int)} // Must lookup
  | Nullable((_, NumberLiteral(_))) => {
      nullable: true,
      propType: Scalar(Int),
    } // Must lookup

  | Boolean => {nullable: optional, propType: Scalar(Boolean)}
  | BooleanLiteral(_) => {nullable: optional, propType: Scalar(Boolean)}
  | Nullable((_, Boolean)) => {nullable: true, propType: Scalar(Boolean)}
  | Nullable((_, BooleanLiteral(_))) => {
      nullable: true,
      propType: Scalar(Boolean),
    }

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

  | Object({properties}) => {
      nullable: optional,
      propType: Object(makeObjShape(~controls, ~state, ~path, properties)),
    }
  | Nullable((_, Object({properties}))) => {
      nullable: true,
      propType: Object(makeObjShape(~controls, ~state, ~path, properties)),
    }

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
      ~optional,
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

  // This handles generic type references
  | Generic({id: Unqualified((_, typeName))}) => {
      nullable: optional,
      propType:
        switch (
          state.enums |> Tablecloth.List.find(~f=name => name == typeName),
          state.objects
          |> Tablecloth.List.find(~f=({name}: obj('a, 'b)) =>
               name == typeName
             ),
        ) {
        | (Some(_), _) => Enum(typeName)
        | (_, Some(_)) => TypeReference(typeName) // TODO: Obj type reference
        | (_, _) => TypeReference(typeName)
        },
    }

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
            ~count=String.length(refName),
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

  controls.addUnion(
    Types.{
      members:
        unionMembers^
        |> Tablecloth.List.filter(~f=(member: Types.unionMember) =>
             member.name != {|%other|}
           ),
      atPath: path,
    },
  );
  {nullable: optional, propType: Union(Printer.makeUnionName(path))};
};

[@gentype]
type atPath =
  | Int
  | Float
  | Unmapped;

[@gentype]
let printFromFlowTypes =
    (~content, ~operationType, ~lookupAtPath: array(string) => atPath) => {
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
  let addUnion = union => unions := [union, ...unions^];
  let controls = {addUnion: addUnion};

  switch (
    operationType,
    Parser_flow.program(~fail=true, ~parse_options, content),
  ) {
  | (
      Mutation(name) | Query(name) | Subscription(name),
      ((_, statements, _), []),
    ) =>
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
                 objects: [{name: typeName, properties}, ...state.objects],
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
  | (_, errors) => Js.log2("error...", errors)
  };

  let finalStr = ref("");
  let addToStr = str => finalStr := finalStr^ ++ str;

  let definitions: ref(list(Types.rootType)) = ref([]);
  let addDefinition = def => definitions := [def, ...definitions^];

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

  switch (state^.input) {
  | Some(input) =>
    addDefinition(
      Types.(
        InputObject(
          input.name,
          input.properties
          |> makeObjShape(~controls, ~state=state^, ~path=["input"]),
        )
      ),
    )
  | None => ()
  };

  unions^
  |> List.iter((union: Types.union) =>
       addToStr(
         "type "
         ++ Printer.(union.atPath |> makeUnionName |> printWrappedUnionName)
         ++ ";\n",
       )
     );

  addToStr("\n");

  Printer.(
    unions^
    |> List.iteri((index, union) =>
         union
         |> printUnion(~chainedDeclaration=false)
         |> printCode
         |> addToStr
       )
  );

  Printer.(
    definitions^
    |> List.iter(def => def |> printRootType |> printCode |> addToStr)
  );

  finalStr^;
};

/*printFromFlowTypes(~content=testQuery, ~operationType=Query("appQuery"))
  |> Js.log;*/