open Types;

let printQuoted = propName => "\"" ++ propName ++ "\"";
let printPropName = propName => propName |> printQuoted;
let printEnumName = name => "enum_" ++ name;
let getInputObjName = name => "input_" ++ name;
let getObjName = name => "obj_" ++ name;
let printWrappedEnumName = name => "SchemaAssets.Enum_" ++ name ++ ".wrapped";
let printUnionName = name => "Union_" ++ name;
let printWrappedUnionName = name => "union_" ++ name ++ "_wrapped";
let printFragmentRef = name => name ++ "_graphql.t";
let getFragmentRefName = name => "__$fragment_ref__" ++ name;
let getInputTypeName = name => "input_" ++ name;
let printAnyType = () => "ReasonRelay.any";
let printTypeReference = (typeName: string) => typeName;

type objectOptionalType =
  | JsNullable
  | Option;

let printScalar = scalarValue =>
  switch (scalarValue) {
  | String => "string"
  | Int => "int"
  | Float => "float"
  | Boolean => "bool"
  | CustomScalar(str) => str
  | Any => printAnyType()
  };

let rec printPropType = (~propType, ~optType) =>
  switch (propType) {
  | Scalar(scalar) => printScalar(scalar)
  | Object(obj) => printObject(~obj, ~optType)
  | ObjectReference(objName) => objName |> getObjName |> printTypeReference
  | Array(propValue) => printArray(~propValue, ~optType)
  | Enum(name) => printWrappedEnumName(name)
  | Union(name) => printWrappedUnionName(name)
  | FragmentRefValue(name) => printFragmentRef(name)
  | TypeReference(name) => printTypeReference(name)
  }
and printPropValue = (~propValue, ~optType) => {
  let str = ref("");
  let addToStr = s => str := str^ ++ s;

  if (propValue.nullable) {
    switch (optType) {
    | JsNullable => addToStr("Js.Nullable.t(")
    | Option => addToStr("option(")
    };
  };

  printPropType(~propType=propValue.propType, ~optType) |> addToStr;

  if (propValue.nullable) {
    addToStr(")");
  };

  str^;
}
and printObject = (~obj, ~optType: objectOptionalType) => {
  switch (obj.values |> Array.length) {
  | 0 => "unit"
  | _ =>
    let str = ref("{.");
    let addToStr = s => str := str^ ++ s;

    obj.values
    |> Array.iteri((index, p) => {
         if (index > 0) {
           addToStr(",");
         };

         addToStr(
           switch (p) {
           | Prop(name, propValue) =>
             printPropName(name)
             ++ ": "
             ++ printPropValue(~propValue, ~optType)
           | FragmentRef(name) =>
             (name |> getFragmentRefName |> printQuoted)
             ++ ": "
             ++ printFragmentRef(name)
           },
         );
       });

    addToStr("}");
    str^;
  };
}
and printArray = (~propValue, ~optType) =>
  "array(" ++ printPropValue(~propValue, ~optType) ++ ")";

let printRootType = rootType =>
  switch (rootType) {
  | Operation(obj) =>
    "type response = " ++ printObject(~obj, ~optType=JsNullable) ++ ";"
  | Variables(obj) =>
    "type variables = " ++ printObject(~obj, ~optType=Option) ++ ";"
  | InputObject(name, obj) =>
    "type " ++ name ++ " = " ++ printObject(~obj, ~optType=Option) ++ ";"
  | Fragment(obj) =>
    "type fragment = " ++ printObject(~obj, ~optType=JsNullable) ++ ";"
  | PluralFragment(obj) =>
    "type fragment = array(" ++ printObject(~obj, ~optType=JsNullable) ++ ");"
  };

[@genType]
let printCode = str => str |> Reason.parseRE |> Reason.printRE;

let makeRootType = rootType => rootType |> printRootType |> printCode;

let makeEnum = fullEnum => {
  let valuesStr = ref("");

  fullEnum.values
  |> Array.iter(value => valuesStr := valuesStr^ ++ "| `" ++ value);

  "type " ++ printEnumName(fullEnum.name) ++ " = [ " ++ valuesStr^ ++ " ];";
};

let makeUnionName = path =>
  path |> Tablecloth.List.reverse |> Tablecloth.String.join(~sep="_");

let printUnion = (union: union) => {
  let prefix = "module ";
  let unionName = union.atPath |> makeUnionName |> printUnionName;
  let unionWrappedName =
    union.atPath |> makeUnionName |> printWrappedUnionName;

  let unwrapUnion =
    "external __unwrap_union: "
    ++ unionWrappedName
    ++ " => {. \"__typename\": string } = \"%identity\";";

  let typeDefs = ref("");
  let addToTypeDefs = Utils.makeAddToStr(typeDefs);

  let unwrappers = ref("");
  let addToUnwrappers = Utils.makeAddToStr(unwrappers);

  let typeT = ref("type t = [");
  let addToTypeT = Utils.makeAddToStr(typeT);

  union.members
  |> List.iter(({name, shape}: Types.unionMember) => {
       addToTypeDefs(
         "type type_"
         ++ name
         ++ " = "
         ++ printObject(~obj=shape, ~optType=JsNullable)
         ++ ";",
       );

       addToUnwrappers(
         "external __unwrap_"
         ++ name
         ++ ": "
         ++ unionWrappedName
         ++ " => type_"
         ++ name
         ++ " = \"%identity\";",
       );
     });

  union.members
  |> List.iter(({name}: Types.unionMember) =>
       addToTypeT(" | `" ++ name ++ "(type_" ++ name ++ ")")
     );

  addToTypeT(" | `UnmappedUnionMember];");

  let unwrapFnImpl =
    ref(
      {|
  let unwrap = wrapped => {
    let unwrappedUnion = wrapped |> __unwrap_union;
    switch (unwrappedUnion##__typename) {
  |},
    );

  let addToUnwrapFnImpl = Utils.makeAddToStr(unwrapFnImpl);

  union.members
  |> List.iter(({name}: Types.unionMember) =>
       addToUnwrapFnImpl(
         "| \""
         ++ name
         ++ "\" => `"
         ++ name
         ++ "(wrapped |> __unwrap_"
         ++ name
         ++ ")",
       )
     );

  addToUnwrapFnImpl({|
      | _ => `UnmappedUnionMember
    };
  };
  |});

  prefix
  ++ unionName
  ++ ": {"
  ++ typeDefs^
  ++ typeT^
  ++ "let unwrap: "
  ++ unionWrappedName
  ++ " => t;"
  ++ "} = { "
  ++ unwrapUnion
  ++ typeDefs^
  ++ typeT^
  ++ unwrappers^
  ++ unwrapFnImpl^
  ++ "}";
};

let fragmentRefAssets = fragmentName => {
  let fref = fragmentName |> getFragmentRefName;
  {j|
type t;
type fragmentRef;
type fragmentRefSelector('a) = {.. "$fref": t} as 'a;
external getFragmentRef: fragmentRefSelector('a) => fragmentRef = "%identity";
|j};
};

let unionModule = unions => {
  let unionsText =
    Tablecloth.(unions |> List.map(~f=printUnion) |> String.join(~sep="\n"));
  {j|
    module Unions {
      $unionsText
    };
    |j};
};

let operationType = (operationType: Types.operationType) => {
  let opType =
    switch (operationType) {
    | Fragment(_) => "fragment"
    | Query(_) => "query"
    | Mutation(_) => "mutation"
    | Subscription(_) => "subscription"
    };

  "type operationType = ReasonRelay." ++ opType ++ "Node;";
};

let printType = typeText => {j|type $typeText;|j};

let opaqueUnionType = unions =>
  Tablecloth.(
    unions
    |> List.map(~f=(union: Types.union) =>
         union.atPath |> makeUnionName |> printWrappedUnionName |> printType
       )
    |> String.join(~sep="\n")
  );