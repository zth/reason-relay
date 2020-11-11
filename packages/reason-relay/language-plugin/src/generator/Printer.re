open Types;

exception Could_not_find_matching_record_definition(string);
exception Invalid_top_level_shape;

let printQuoted = propName => "\"" ++ propName ++ "\"";
let makeUnionName = path =>
  path |> Tablecloth.List.reverse |> Tablecloth.String.join(~sep="_");

let makeEnumName = enumName => "enum_" ++ enumName;
let makeUnwrapEnumFnName = enumName => "unwrap_" ++ makeEnumName(enumName);
let makeWrapEnumFnName = enumName => "wrap_" ++ makeEnumName(enumName);

let printRecordPropName = propName =>
  switch (
    ReservedKeywords.reservedKeywords->Tablecloth.Array.find(~f=w =>
      w == propName
    )
  ) {
  | Some(_) => "[@bs.as \"" ++ propName ++ "\"] " ++ propName ++ "_"
  | None => propName
  };

let printSafeName = propName =>
  switch (
    ReservedKeywords.reservedKeywords->Tablecloth.Array.find(~f=w =>
      w == propName
    )
  ) {
  | Some(_) => propName ++ "_"
  | None => propName
  };
let printEnumName = name => "enum_" ++ name;
let getObjName = name => "obj_" ++ name;
let printEnumTypeName = name => makeEnumName(name);
let printEnumUnwrapFnReference = name => makeUnwrapEnumFnName(name);
let printEnumWrapFnReference = name => makeWrapEnumFnName(name);

let printLocalUnionName = (union: union) =>
  union.atPath->Utils.makeRecordName;
let printUnionUnwrapFnReference = (union: union) =>
  "unwrap_" ++ union.atPath->Utils.makeRecordName;
let printUnionWrapFnReference = (union: union) =>
  "wrap_" ++ union.atPath->Utils.makeRecordName;
let printFragmentRef = name =>
  Tablecloth.String.capitalize(name) ++ "_graphql.t";
let printAnyType = () => "ReasonRelay.any";

let printScalar = scalarValue =>
  switch (scalarValue) {
  | String => "string"
  | Int => "int"
  | Float => "float"
  | Boolean => "bool"
  | CustomScalar(str) => str
  | Any => printAnyType()
  };

let printStringLiteral = literal => "[ | `" ++ literal ++ "]";

let printDataIdType = () => "ReasonRelay.dataId";

let getEnumFutureAddedValueName = (enum: fullEnum) =>
  switch (enum.values->Tablecloth.Array.find(~f=e => e === "FutureAddedValue")) {
  | Some(_) => "FutureAddedValue_"
  | None => "FutureAddedValue"
  };

let printEnumDefinition = (enum: fullEnum): string => {
  let enumName = makeEnumName(enum.name);

  let str = ref("type " ++ enumName ++ " = pri [>");

  let addToStr = s => str := str^ ++ s;

  enum.values
  ->Belt.Array.forEach(v => addToStr(" | `" ++ printSafeName(v) ++ " "));

  addToStr("];\n\n");

  str^;
};

let objHasPrintableContents = (obj: object_) =>
  obj.values
  ->Belt.Array.some(
      fun
      | Prop(_) => true
      | _ => false,
    );

let rec printTypeReference = (~state: option(fullState), typeName: string) =>
  switch (state) {
  | Some(state) =>
    switch (
      state.enums
      |> Tablecloth.List.find(~f=(enum: fullEnum) => enum.name == typeName),
      state.objects
      |> Tablecloth.List.find(~f=(obj: finalizedObj) =>
           obj.originalFlowTypeName == Some(typeName)
         ),
    ) {
    | (Some(enum), _) => printEnumName(enum.name)
    | (_, Some(_)) => Tablecloth.String.uncapitalize(typeName)
    | _ =>
      // If this doesn't match any existing types in the state, and if it's a module name
      // (decided by being uppercased), we'll go ahead and assume this is a custom field.

      typeName->Utils.isModuleName ? typeName ++ ".t" : typeName
    }
  | None => typeName->Utils.isModuleName ? typeName ++ ".t" : typeName
  }
and printPropType = (~propType, ~state: Types.fullState) =>
  switch (propType) {
  | DataId => printDataIdType()
  | Scalar(scalar) => printScalar(scalar)
  | StringLiteral(literal) => printStringLiteral(literal)
  | Object(obj) => printRecordReference(~obj, ~state)
  | TopLevelNodeField(_, obj) => printRecordReference(~obj, ~state)
  | Array(propValue) => printArray(~propValue, ~state)
  | Enum(enum) => printEnumName(enum.name)
  | Union(union) =>
    union->printUnionTypeDefinition(
      ~includeSemi=false,
      ~prefixWithTypesModule=false,
    )
  | FragmentRefValue(name) => printFragmentRef(name)
  | TypeReference(name) => printTypeReference(~state=Some(state), name)
  }
and printPropValue = (~propValue, ~state) => {
  let str = ref("");
  let addToStr = s => str := str^ ++ s;

  if (propValue.nullable) {
    addToStr("option(");
  };

  printPropType(~propType=propValue.propType, ~state) |> addToStr;

  if (propValue.nullable) {
    addToStr(")");
  };

  str^;
}
and printObject = (~obj: object_, ~state, ~ignoreFragmentRefs=false, ()) => {
  switch (obj.values |> Array.length) {
  | 0 => "unit"
  | _ =>
    let str = ref("");
    let addToStr = s => str := str^ ++ s;

    let hasFragments =
      obj.values
      ->Belt.Array.some(
          fun
          | FragmentRef(_) => true
          | Prop(_) => false,
        );

    let hasProps =
      obj.values
      ->Belt.Array.some(
          fun
          | FragmentRef(_) => false
          | Prop(_) => true,
        );

    // Return an empty JS object if we are ignoring fragment refs and have no props
    switch (ignoreFragmentRefs, hasProps) {
    | (true, false) => addToStr("ReasonRelay.allFieldsMasked")
    | _ =>
      addToStr("{");

      obj.values
      |> Tablecloth.Array.filter(
           ~f=
             fun
             | FragmentRef(_) => false
             | Prop(_) => true,
         )
      |> Array.iter(p => {
           addToStr(
             switch (p) {
             | Prop(name, propValue) =>
               printRecordPropName(name)
               ++ ": "
               ++ printPropValue(~propValue, ~state)
               ++ ","
             | FragmentRef(_) => ""
             },
           )
         });

      if (hasFragments && !ignoreFragmentRefs) {
        addToStr(
          printRecordPropName("fragmentRefs")
          ++ ": "
          ++ obj->printFragmentRefs,
        );
      };

      addToStr("}");
    };

    str^;
  };
}
and printFragmentRefs = (obj: object_) => {
  let str = ref("ReasonRelay.fragmentRefs(");
  let addToStr = s => str := str^ ++ s;

  obj.values
  |> Tablecloth.Array.filter(~f=v =>
       switch (v) {
       | FragmentRef(_) => true
       | Prop(_) => false
       }
     )
  |> Array.iteri((index, p) => {
       index == 0 ? addToStr("[") : ();

       addToStr(
         switch (p) {
         | FragmentRef(name) => " | `" ++ name
         | Prop(_) => ""
         },
       );
     });

  addToStr("])");

  str^;
}
and printArray = (~propValue, ~state) =>
  "array(" ++ printPropValue(~propValue, ~state) ++ ")"
and printRecordReference = (~state: fullState, ~obj: object_) => {
  switch (
    state.objects |> Tablecloth.List.find(~f=o => {o.atPath == obj.atPath})
  ) {
  | Some({recordName: Some(recordName)}) =>
    Tablecloth.String.uncapitalize(recordName)
  | Some(_)
  | None =>
    raise(
      Could_not_find_matching_record_definition(
        obj.atPath
        |> Tablecloth.List.reverse
        |> Tablecloth.String.join(~sep="_"),
      ),
    )
  };
}
and printObjectMaker = (obj: object_, ~targetType, ~name) => {
  let hasContents = obj->objHasPrintableContents;

  let str = ref("");
  let addToStr = s => str := str^ ++ s;

  addToStr("let " ++ name ++ " = (");

  if (hasContents) {
    obj.values
    |> Array.iteri((index, p) => {
         addToStr(
           switch (p) {
           | Prop(name, {nullable}) =>
             (index > 0 ? "," : "")
             ++ "~"
             ++ printSafeName(name)
             ++ (nullable ? "=?" : "")
           | FragmentRef(_) => ""
           },
         )
       });

    let shouldAddUnit =
      obj.values
      ->Belt.Array.some(
          fun
          | Prop(_, {nullable}) => nullable
          | _ => false,
        );

    addToStr((shouldAddUnit ? ", ()" : "") ++ "): " ++ targetType ++ " => {");

    obj.values
    |> Array.iteri((index, p) => {
         addToStr(
           switch (p) {
           | Prop(name, _) =>
             (index > 0 ? "," : "")
             ++ printSafeName(name)
             ++ ": "
             ++ printSafeName(name)
           | FragmentRef(_) => ""
           },
         )
       });

    addToStr("};");
  } else {
    addToStr(") => [@ocaml.warning \"-27\"] {};");
  };
  str^;
}
and printUnionTypeDefinition =
    (union, ~includeSemi, ~prefixWithTypesModule): string => {
  let futureAddedValueName =
    switch (
      union.members
      ->Tablecloth.List.find(~f=m => m.name === "UnselectedUnionMember")
    ) {
    | Some(_) => "UnselectedUnionMember_"
    | None => "UnselectedUnionMember"
    };

  "["
  ++ (
    union.members
    ->Belt.List.map(({name, shape: {atPath}}) =>
        " | `"
        ++ name
        ++ "("
        ++ (prefixWithTypesModule ? "Types." : "")
        ++ Utils.makeRecordName(atPath)
        ++ ")"
      )
    |> Tablecloth.String.join(~sep="\n")
  )
  ++ " | `"
  ++ futureAddedValueName
  ++ "(string) "
  ++ "]"
  ++ (includeSemi ? ";" : "");
};

let printInputObjectMaker = (obj: object_, ~state, ~targetType, ~name) => {
  let hasContents = obj->objHasPrintableContents;

  let str = ref("");
  let addToStr = s => str := str^ ++ s;

  if (hasContents) {
    addToStr("[@bs.obj] external " ++ name ++ ": (");
    obj.values
    |> Array.iteri((index, p) => {
         addToStr(
           switch (p) {
           | Prop(name, {nullable, propType}) =>
             (index > 0 ? "," : "")
             ++ "~"
             ++ printSafeName(name)
             ++ ": "
             ++ printPropType(~propType, ~state)
             ++ (nullable ? "=?" : "")
           | FragmentRef(_) => ""
           },
         )
       });

    let shouldAddUnit =
      obj.values
      ->Belt.Array.some(
          fun
          | Prop(_, {nullable}) => nullable
          | _ => false,
        );

    addToStr(
      (shouldAddUnit ? ", unit" : "") ++ ") => " ++ targetType ++ " = \"\"",
    );
  } else {
    addToStr(
      "[@bs.obj] external " ++ name ++ ": unit => " ++ targetType ++ " = \"\"",
    );
  };
  str^;
};

let printRefetchVariablesMaker = (obj: object_, ~state) => {
  let str = ref("");
  let addToStr = s => str := str^ ++ s;

  let optionalObj = {
    atPath: [],
    values:
      obj.values
      |> Array.map(value =>
           switch (value) {
           | Prop(name, {nullable: false} as propValue) =>
             Prop(name, {...propValue, nullable: true})
           | a => a
           }
         ),
  };

  addToStr("type refetchVariables;");

  addToStr(
    optionalObj->printInputObjectMaker(
      ~state,
      ~targetType="refetchVariables",
      ~name="makeRefetchVariables",
    ),
  );

  str^;
};

let printRootType =
    (~recursiveMode=None, ~state: fullState, ~ignoreFragmentRefs, rootType) => {
  switch (rootType) {
  | Operation(Object(obj)) =>
    "type response = "
    ++ printObject(~obj, ~state, ~ignoreFragmentRefs, ())
    ++ ";"
  | RawResponse(Some(Object(obj))) =>
    "type rawResponse = "
    ++ printObject(~obj, ~state, ~ignoreFragmentRefs, ())
    ++ ";"
  | RawResponse(None) => "type rawResponse = response;"
  | RawResponse(Some(Union(_)))
  | Operation(Union(_)) => raise(Invalid_top_level_shape)
  | Variables(Object(obj)) =>
    switch (obj.values |> Array.length) {
    | 0 => "type variables = unit;"
    | _ => "type variables;"
    }
  | Variables(Union(_)) => raise(Invalid_top_level_shape)
  | RefetchVariables(obj) =>
    switch (obj.values |> Array.length) {
    | 0 => ""
    | _ => printRefetchVariablesMaker(obj, ~state) ++ ";"
    }

  | Fragment(Object(obj)) =>
    "type fragment = "
    ++ printObject(~obj, ~state, ~ignoreFragmentRefs, ())
    ++ ";"
  | Fragment(Union(union)) =>
    "type fragment = "
    ++ union->printUnionTypeDefinition(
         ~includeSemi=false,
         ~prefixWithTypesModule=false,
       )
    ++ ";"
  | ObjectTypeDeclaration({name, definition}) =>
    let typeDef =
      Tablecloth.String.uncapitalize(name)
      ++ (
        switch (definition.values |> Tablecloth.Array.length) {
        | 0 => ""
        | _ =>
          " = "
          ++ printObject(~obj=definition, ~state, ~ignoreFragmentRefs, ())
        }
      );

    let (prefix, suffix) =
      switch (recursiveMode) {
      | None => (" type ", "; ")
      | Some(`Head) => (" type ", " ")
      | Some(`Member) => (" and ", " ")
      | Some(`Tail) => (" and ", "; ")
      };

    prefix ++ typeDef ++ suffix;
  | PluralFragment(Object(obj)) =>
    "type fragment_t = "
    ++ printObject(~obj, ~state, ~ignoreFragmentRefs, ())
    ++ ";\n"
    ++ "type fragment = array(fragment_t);"
  | PluralFragment(Union(union)) =>
    "type fragment_t = "
    ++ union->printUnionTypeDefinition(
         ~includeSemi=false,
         ~prefixWithTypesModule=false,
       )
    ++ ";\n"
    ++ "type fragment = array(fragment_t);"
  };
};

let printUnionConverters = (union: union) => {
  let str = ref("");
  let addToStr = Utils.makeAddToStr(str);

  let futureAddedValueName =
    switch (
      union.members
      ->Tablecloth.List.find(~f=m => m.name === "UnselectedUnionMember")
    ) {
    | Some(_) => "UnselectedUnionMember_"
    | None => "UnselectedUnionMember"
    };

  let unionName = union.atPath->Utils.makeRecordName;

  // Unwrap
  addToStr(
    "let unwrap_"
    ++ unionName
    ++ ": {. \"__typename\": string } => "
    ++ union->printUnionTypeDefinition(
         ~includeSemi=false,
         ~prefixWithTypesModule=true,
       )
    ++ " = u => switch(u##__typename) {",
  );
  union.members
  ->Belt.List.forEach(member => {
      addToStr(
        "\n | \""
        ++ member.name
        ++ "\" => `"
        ++ member.name
        ++ "(u->Obj.magic) ",
      )
    });
  addToStr("\n | v => `" ++ futureAddedValueName ++ "(v)");
  addToStr("};\n\n");

  // Wrap
  addToStr(
    "let wrap_"
    ++ unionName
    ++ ": "
    ++ union->printUnionTypeDefinition(
         ~includeSemi=false,
         ~prefixWithTypesModule=true,
       )
    ++ " => {. \"__typename\": string } = fun",
  );
  union.members
  ->Belt.List.forEach(member => {
      addToStr("\n | `" ++ member.name ++ "(v) => v->Obj.magic ")
    });
  addToStr("\n | `" ++ futureAddedValueName ++ "(v) => {\"__typename\": v} ");
  addToStr(";\n\n");
  str^;
};

let printUnionTypes = (~state, ~printName, union: union) => {
  let typeDefs = ref("");
  let addToTypeDefs = Utils.makeAddToStr(typeDefs);

  union.members
  |> List.iter(({shape}: Types.unionMember) => {
       let allObjects: list(Types.finalizedObj) =
         Tablecloth.List.append(shape |> Utils.extractNestedObjects, [shape])
         |> List.map((definition: Types.object_) =>
              {
                originalFlowTypeName: None,
                recordName: {
                  Some(definition.atPath->Utils.makeRecordName);
                },
                atPath: definition.atPath,
                foundInUnion: true,
                definition,
              }
            );

       let definitions =
         allObjects
         |> List.rev
         |> Tablecloth.List.map(~f=(definition: Types.finalizedObj) =>
              ObjectTypeDeclaration({
                name:
                  Tablecloth.Option.withDefault(
                    ~default="",
                    definition.recordName,
                  ),
                atPath: definition.atPath,
                definition: definition.definition,
              })
            );

       let stateWithUnionDefinitions = {...state, objects: allObjects};

       definitions
       |> List.rev
       |> Tablecloth.List.iter(~f=definition => {
            definition
            |> printRootType(
                 ~state=stateWithUnionDefinitions,
                 ~ignoreFragmentRefs=false,
               )
            |> addToTypeDefs
          });
     });

  let typeT =
    "type "
    ++ union.atPath->Utils.makeRecordName
    ++ " = "
    ++ union->printUnionTypeDefinition(
         ~includeSemi=true,
         ~prefixWithTypesModule=false,
       );

  typeDefs^ ++ "\n" ++ (printName ? typeT : "");
};

let printEnumToStringFn = (enum: fullEnum): string =>
  "external "
  ++ Tablecloth.String.uncapitalize(enum.name)
  ++ "_toString: Types."
  ++ printEnumName(enum.name)
  ++ " => string = \"%identity\";";

let printEnum = (enum: fullEnum): string => enum->printEnumDefinition;

let fragmentRefAssets = (~plural=false, fragmentName) => {
  let str = ref("");
  let addToStr = s => str := str^ ++ s;

  addToStr("type t;");
  addToStr("type fragmentRef;");

  addToStr(
    "external getFragmentRef: "
    ++ (plural ? "array(" : "")
    ++ " ReasonRelay.fragmentRefs([> | `"
    ++ fragmentName
    ++ "])"
    ++ (plural ? ")" : "")
    ++ " => fragmentRef = \"%identity\";",
  );

  str^;
};

let operationType = (operationType: Types.operationType) => {
  let opType =
    switch (operationType) {
    | Fragment(_) => "fragment"
    | Query(_) => "query"
    | Mutation(_) => "mutation"
    | Subscription(_) => "subscription"
    };

  "type relayOperationNode; \n\ntype operationType = ReasonRelay."
  ++ opType
  ++ "Node(relayOperationNode);";
};

let printType = typeText => {j|type $typeText;|j};

[@genType]
let printCode = str => str |> Reason.parseRE |> Reason.printRE;
