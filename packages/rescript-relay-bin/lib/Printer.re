open Types;

exception Could_not_find_matching_record_definition(string);
exception Invalid_top_level_shape;

type printingContext =
  | Variables
  | InputObject
  | Other;

let printQuoted = propName => "\"" ++ propName ++ "\"";
let makeUnionName = path =>
  path |> List.rev |> Tablecloth.String.join(~sep="_");

let printComment = (comment: option(string)) =>
  switch (comment) {
  | Some(comment) => "@ocaml.doc(\"" ++ comment ++ "\")\n"
  | None => ""
  };

let printRecordPropComment = (propValue: Types.propValue) =>
  printComment(propValue.comment);

let printRecordComment = (obj: Types.object_) => printComment(obj.comment);

let printRecordPropName = propName =>
  switch (
    ReservedKeywords.reservedKeywords
    |> Tablecloth.Array.find(~f=w => w == propName)
  ) {
  | Some(_) => "@as(\"" ++ propName ++ "\") " ++ propName ++ "_"
  | None => propName
  };

let printSafeName = propName =>
  switch (
    ReservedKeywords.reservedKeywords
    |> Tablecloth.Array.find(~f=w => w == propName)
  ) {
  | Some(_) => propName ++ "_"
  | None => propName
  };
let printEnumName = (~printingContext, ~prefix=true, name) =>
  (prefix ? "enum_" ++ name : Tablecloth.String.uncapitalize(name))
  ++ (
    switch (printingContext) {
    | Variables
    | InputObject => "_input"
    | Other => ""
    }
  );
let getObjName = name => "obj_" ++ name;

let printLocalUnionName = (union: union) =>
  union.atPath |> Utils.makeRecordName;
let printUnionUnwrapFnReference = (union: union) =>
  "unwrap_" ++ (union.atPath |> Utils.makeRecordName);
let printUnionWrapFnReference = (union: union) =>
  "wrap_" ++ (union.atPath |> Utils.makeRecordName);
let printFragmentRef = name =>
  Tablecloth.String.capitalize(name) ++ "_graphql.t";
let printAnyType = () => "RescriptRelay.any";

let printScalar = scalarValue =>
  switch (scalarValue) {
  | String => "string"
  | Int => "int"
  | Float => "float"
  | Boolean => "bool"
  | CustomScalar(str) => str
  | Any => printAnyType()
  };

let printStringLiteral = (~literal, ~needsEscaping) =>
  "[ | #"
  ++ (needsEscaping ? Format.sprintf("\"%s\"", literal) : literal)
  ++ "]";

let printDataIdType = () => "RescriptRelay.dataId";

let getEnumFutureAddedValueName = (enum: fullEnum) =>
  switch (enum.values |> List.find_opt(e => e === "FutureAddedValue")) {
  | Some(_) => "FutureAddedValue_"
  | None => "FutureAddedValue"
  };

let printEnumBody = (~printingContext, enum: fullEnum) => {
  let str =
    ref(
      switch (printingContext) {
      | Variables
      | InputObject => "["
      | Other => "[>"
      },
    );

  let addToStr = Utils.makeAddToStr(str);

  enum.values |> List.iter(v => addToStr("\n  | #" ++ printSafeName(v)));

  addToStr("\n  ]");

  str^;
};

let printEnumDefinition = (~printingContext, enum: fullEnum): string => {
  let enumName = printEnumName(~printingContext, enum.name);
  "type "
  ++ enumName
  ++ " = "
  ++ (
    switch (printingContext) {
    | Variables
    | InputObject => ""
    | Other => "private "
    }
  )
  ++ printEnumBody(~printingContext, enum)
  ++ "\n";
};

// This is mighty ugly, but a simple way to figure out whether a definition is
// an input object or not is to check if its path is just of length 1. All other
// objects are prefixed with "response", "fragment" or similar.
let isInputObject = (obj: Types.object_) => List.length(obj.atPath) == 1;

let objHasPrintableContents = (obj: object_) =>
  obj.values
  |> List.exists(
       fun
       | Prop(_) => true
       | _ => false,
     );

let rec printTypeReference =
        (~printingContext, ~state: option(fullState), typeName: string) =>
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
    | (Some(enum), _) => printEnumName(~printingContext, enum.name)
    | (_, Some(_)) => Tablecloth.String.uncapitalize(typeName)
    | _ =>
      // If this doesn't match any existing types in the state, and if it's a module name
      // (decided by being uppercased), we'll go ahead and assume this is a custom field.

      typeName |> Utils.isModuleName ? typeName ++ ".t" : typeName
    }
  | None => typeName |> Utils.isModuleName ? typeName ++ ".t" : typeName
  }
and printPropType = (~propType, ~printingContext, ~state: Types.fullState) =>
  switch (propType) {
  | DataId => printDataIdType()
  | Scalar(scalar) => printScalar(scalar)
  | StringLiteral(literal) =>
    printStringLiteral(~literal, ~needsEscaping=false)
  | StringLiteralNeedsEscaping(literal) =>
    printStringLiteral(~literal, ~needsEscaping=true)
  | Object(obj) => printRecordReference(~obj, ~state)
  | TopLevelNodeField(_, obj) => printRecordReference(~obj, ~state)
  | Array(propValue) => printArray(~printingContext, ~propValue, ~state)
  | Enum(enum) =>
    switch (printingContext) {
    | Variables
    | InputObject => printEnumBody(~printingContext, enum)
    | Other => printEnumName(~printingContext, enum.name)
    }
  | Union(union) =>
    printUnionTypeDefinition(
      union,
      ~prefixWithTypesModule=false,
      ~extraIndent=true,
    )
  | FragmentRefValue(name) => printFragmentRef(name)
  | TypeReference(name) =>
    printTypeReference(~printingContext, ~state=Some(state), name)
  }
and printPropValue = (~printingContext, ~propValue, ~state) => {
  let str = ref("");
  let addToStr = s => str := str^ ++ s;

  if (propValue.nullable) {
    addToStr("option<");
  };

  printPropType(~printingContext, ~propType=propValue.propType, ~state)
  |> addToStr;

  if (propValue.nullable) {
    addToStr(">");
  };

  str^;
}
and printObject = (~printingContext, ~obj: object_, ~state, ()) => {
  switch (obj.values |> List.length) {
  | 0 => "unit"
  | _ =>
    let str = ref("");
    let addToStr = s => str := str^ ++ s;

    let hasFragments =
      obj.values
      |> List.exists(
           fun
           | FragmentRef(_) => true
           | Prop(_) => false,
         );

    addToStr("{");

    obj.values
    |> List.filter(
         fun
         | FragmentRef(_) => false
         | Prop(_) => true,
       )
    |> List.iter(p => {
         addToStr(
           switch (p) {
           | Prop(name, propValue) =>
             "\n  "
             ++ printRecordPropComment(propValue)
             ++ printRecordPropName(name)
             ++ ": "
             ++ printPropValue(~printingContext, ~propValue, ~state)
             ++ ","
           | FragmentRef(_) => ""
           },
         )
       });

    if (hasFragments) {
      addToStr(
        printRecordPropName("\n  fragmentRefs")
        ++ ": "
        ++ (obj |> printFragmentRefs),
      );
    };

    addToStr("\n}");

    str^;
  };
}
and printFragmentRefs = (obj: object_) => {
  let str = ref("RescriptRelay.fragmentRefs<");
  let addToStr = s => str := str^ ++ s;

  obj.values
  |> List.filter(v =>
       switch (v) {
       | FragmentRef(_) => true
       | Prop(_) => false
       }
     )
  |> List.iteri((index, p) => {
       index == 0 ? addToStr("[") : ();

       addToStr(
         switch (p) {
         | FragmentRef(name) => " | #" ++ name
         | Prop(_) => ""
         },
       );
     });

  addToStr("]>");

  str^;
}
and printArray = (~printingContext, ~propValue, ~state) =>
  "array<" ++ printPropValue(~printingContext, ~propValue, ~state) ++ ">"
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
  let hasContents = obj |> objHasPrintableContents;

  let str = ref("");
  let addToStr = s => str := str^ ++ s;

  addToStr("let " ++ name ++ " = (");

  if (hasContents) {
    let hasPrintedFragmentRefs = ref(false);

    obj.values
    |> List.iteri((index, p) => {
         addToStr(
           switch (hasPrintedFragmentRefs^, p) {
           | (_, Prop(name, {nullable})) =>
             (index > 0 ? "," : "")
             ++ "\n  ~"
             ++ printSafeName(name)
             ++ (nullable ? "=?" : "")
           | (false, FragmentRef(_)) =>
             hasPrintedFragmentRefs := true;
             (index > 0 ? "," : "") ++ "\n  ~" ++ "fragmentRefs";
           | (true, FragmentRef(_)) => ""
           },
         )
       });

    let shouldAddUnit =
      obj.values
      |> List.exists(
           fun
           | Prop(_, {nullable}) => nullable
           | _ => false,
         );

    addToStr(
      (shouldAddUnit ? ",\n  ()" : "") ++ "\n): " ++ targetType ++ " => {",
    );

    let hasPrintedFragmentRefs = ref(false);

    obj.values
    |> List.iteri((index, p) => {
         addToStr(
           switch (hasPrintedFragmentRefs^, p) {
           | (_, Prop(name, _)) =>
             (index > 0 ? "," : "")
             ++ "\n  "
             ++ printSafeName(name)
             ++ ": "
             ++ printSafeName(name)
           | (false, FragmentRef(_)) =>
             hasPrintedFragmentRefs := true;
             (index > 0 ? "," : "")
             ++ "\n  "
             ++ "fragmentRefs"
             ++ ": "
             ++ "fragmentRefs";
           | (true, FragmentRef(_)) => ""
           },
         )
       });

    addToStr("\n}");
  } else {
    addToStr("\n) => ()");
  };

  addToStr("\n");
  str^;
}
and printRefetchVariablesMaker = (obj: object_, ~state) => {
  let str = ref("");
  let addToStr = s => str := str^ ++ s;

  let optionalObj = {
    comment: None,
    atPath: [],
    values:
      obj.values
      |> List.map(value =>
           switch (value) {
           | Prop(name, {nullable: false} as propValue) =>
             Prop(name, {...propValue, nullable: true})
           | a => a
           }
         ),
  };

  addToStr("type refetchVariables = ");
  addToStr(
    printObject(~printingContext=Variables, ~state, ~obj=optionalObj, ()),
  );
  addToStr("\n");

  addToStr(
    printObjectMaker(
      optionalObj,
      ~targetType="refetchVariables",
      ~name="makeRefetchVariables",
    ),
  );

  str^;
}
and printRootType = (~recursiveMode=None, ~state: fullState, rootType) => {
  switch (rootType) {
  | Operation(Object(obj)) =>
    printRecordComment(obj)
    ++ "type response = "
    ++ printObject(~printingContext=Other, ~obj, ~state, ())
    ++ "\n"
  | RawResponse(Some(Object(obj))) =>
    printRecordComment(obj)
    ++ "type rawResponse = "
    ++ printObject(~printingContext=Other, ~obj, ~state, ())
    ++ "\n"
  | RawResponse(None) => "type rawResponse = response\n"
  | RawResponse(Some(Union(_)))
  | Operation(Union(_)) => raise(Invalid_top_level_shape)
  | Variables(Object(obj)) =>
    printRecordComment(obj)
    ++ "type variables = "
    ++ printObject(~printingContext=Variables, ~obj, ~state, ())
    ++ "\n"
  | Variables(Union(_)) => raise(Invalid_top_level_shape)
  | RefetchVariables(obj) => printRefetchVariablesMaker(~state, obj) ++ "\n"
  | Fragment(Object(obj)) =>
    printRecordComment(obj)
    ++ "type fragment = "
    ++ printObject(~printingContext=Other, ~obj, ~state, ())
    ++ "\n"
  | Fragment(Union(union)) =>
    printComment(union.comment)
    ++ "type fragment = "
    ++ printUnionTypeDefinition(
         union,
         ~prefixWithTypesModule=false,
         ~extraIndent=false,
       )
    ++ "\n"
  | ObjectTypeDeclaration({name, definition}) =>
    let typeDef =
      Tablecloth.String.uncapitalize(name)
      ++ (
        switch (definition.values |> List.length) {
        | 0 => ""
        | _ =>
          " = "
          ++ printObject(
               ~printingContext=
                 isInputObject(definition) ? InputObject : Other,
               ~obj=definition,
               ~state,
               (),
             )
          ++ "\n"
        }
      );

    let (prefix, suffix) =
      switch (recursiveMode) {
      | None => ("type ", "\n")
      | Some(`Head) => ("type rec ", "")
      | Some(`Member) => (" and ", "")
      | Some(`Tail) => (" and ", "\n\n")
      };

    printRecordComment(definition) ++ prefix ++ typeDef ++ suffix;
  | PluralFragment(Object(obj)) =>
    "type fragment_t = "
    ++ printObject(~printingContext=Other, ~obj, ~state, ())
    ++ "\n"
    ++ printRecordComment(obj)
    ++ "type fragment = array<fragment_t>\n"
  | PluralFragment(Union(union)) =>
    "type fragment_t = "
    ++ printUnionTypeDefinition(
         union,
         ~prefixWithTypesModule=false,
         ~extraIndent=false,
       )
    ++ "\n"
    ++ printComment(union.comment)
    ++ "type fragment = array<fragment_t>\n"
  };
}
and printUnionTypeDefinition =
    (union, ~prefixWithTypesModule, ~extraIndent): string => {
  let futureAddedValueName =
    switch (
      union.members
      |> Tablecloth.List.find(~f=(m: Types.unionMember) =>
           m.name === "UnselectedUnionMember"
         )
    ) {
    | Some(_) => "UnselectedUnionMember_"
    | None => "UnselectedUnionMember"
    };

  "["
  ++ (
    union.members
    |> List.map(({name, shape: {atPath}}) =>
         "\n  "
         ++ (extraIndent ? "  " : "")
         ++ "| #"
         ++ name
         ++ "("
         ++ (prefixWithTypesModule ? "Types." : "")
         ++ Utils.makeRecordName(atPath)
         ++ ")"
       )
    |> Tablecloth.String.join(~sep="\n")
  )
  ++ "\n  "
  ++ (extraIndent ? "  " : "")
  ++ "| #"
  ++ futureAddedValueName
  ++ "(string)"
  ++ (extraIndent ? "\n  " : "\n")
  ++ "]";
};

let printUnionConverters = (union: union) => {
  let str = ref("");
  let addToStr = Utils.makeAddToStr(str);

  let futureAddedValueName =
    switch (
      union.members
      |> Tablecloth.List.find(~f=(m: Types.unionMember) =>
           m.name === "UnselectedUnionMember"
         )
    ) {
    | Some(_) => "UnselectedUnionMember_"
    | None => "UnselectedUnionMember"
    };

  let unionName = union.atPath |> Utils.makeRecordName;

  // Unwrap
  addToStr(
    "let unwrap_"
    ++ unionName
    ++ ": {. \"__typename\": string } => "
    ++ printUnionTypeDefinition(
         union,
         ~prefixWithTypesModule=true,
         ~extraIndent=false,
       )
    ++ " = u => switch u[\"__typename\"] {",
  );
  union.members
  |> List.iter((member: Types.unionMember) => {
       addToStr(
         "\n | \""
         ++ member.name
         ++ "\" => #"
         ++ member.name
         ++ "(u->Obj.magic) ",
       )
     });
  addToStr("\n | v => #" ++ futureAddedValueName ++ "(v)");
  addToStr("\n}\n\n");

  // Wrap
  addToStr(
    "let wrap_"
    ++ unionName
    ++ ": "
    ++ printUnionTypeDefinition(
         union,
         ~prefixWithTypesModule=true,
         ~extraIndent=false,
       )
    ++ " => {. \"__typename\": string } = v => switch v {",
  );
  union.members
  |> List.iter((member: Types.unionMember) => {
       addToStr("\n | #" ++ member.name ++ "(v) => v->Obj.magic ")
     });
  addToStr("\n | #" ++ futureAddedValueName ++ "(v) => {\"__typename\": v} ");
  addToStr("\n}\n\n");
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
                  Some(definition.atPath |> Utils.makeRecordName);
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
            |> printRootType(~state=stateWithUnionDefinitions)
            |> addToTypeDefs
          });
     });

  let typeT =
    "type "
    ++ Utils.makeRecordName(union.atPath)
    ++ " = "
    ++ printUnionTypeDefinition(
         union,
         ~prefixWithTypesModule=false,
         ~extraIndent=false,
       )
    ++ "\n";

  typeDefs^ ++ "\n" ++ (printName ? typeT : "");
};

let printEnumToStringFn = (~printingContext, enum: fullEnum): string =>
  "external "
  ++ printEnumName(~printingContext, ~prefix=false, enum.name)
  ++ "_toString:\n"
  ++ printEnumName(~printingContext, enum.name)
  ++ " => string = \"%identity\"";

let printEnum = (~printingContext, enum: fullEnum): string =>
  printEnumDefinition(~printingContext, enum);

let fragmentRefAssets = (~plural=false, fragmentName) => {
  let str = ref("");
  let addToStr = s => str := str^ ++ s;

  addToStr("type t\n");
  addToStr("type fragmentRef\n");

  addToStr(
    "external getFragmentRef:\n  "
    ++ (plural ? "array<" : "")
    ++ "RescriptRelay.fragmentRefs<[> | #"
    ++ fragmentName
    ++ "]>"
    ++ (plural ? ">" : "")
    ++ " => fragmentRef = \"%identity\"",
  );

  str^;
};

let printOperationType = (operationType: Types.operationType) => {
  let opType =
    switch (operationType) {
    | Fragment(_) => "fragment"
    | Query(_) => "query"
    | Mutation(_) => "mutation"
    | Subscription(_) => "subscription"
    };

  "type relayOperationNode\ntype operationType = RescriptRelay."
  ++ opType
  ++ "Node<relayOperationNode>\n";
};

let printType = typeText => {j|type $typeText;|j};

let printCode: string => string = str => str;
