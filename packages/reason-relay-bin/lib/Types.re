type operationType =
  | Fragment(string, bool) // Name, isPlural
  | Mutation(string)
  | Subscription(string)
  | Query(string);

type connectionInfo = {
  key: string,
  atObjectPath: array(string),
  fieldName: string,
};

type unionMember = {
  name: string,
  shape: object_,
}
and union = {
  members: list(unionMember),
  atPath: list(string),
}
and scalarValues =
  | Int
  | String
  | Float
  | Boolean
  | CustomScalar(string)
  | Any
and propType =
  | DataId
  | Scalar(scalarValues)
  | StringLiteral(string)
  | Enum(fullEnum)
  | Object(object_)
  | Array(propValue)
  | FragmentRefValue(string)
  | TypeReference(string)
  | Union(union)
  | TopLevelNodeField(string, object_)
and propValue = {
  nullable: bool,
  propType,
}
and propValues =
  | FragmentRef(string)
  | Prop(string, propValue)
and object_ = {
  values: list(propValues),
  atPath: list(string),
}
and rootType =
  | Operation(rootStructure)
  | RawResponse(option(rootStructure))
  | Fragment(rootStructure)
  | Variables(rootStructure)
  | ObjectTypeDeclaration({
      definition: object_,
      name: string,
      atPath: list(string),
    })
  | RefetchVariables(object_)
  | PluralFragment(rootStructure)
and fullEnum = {
  name: string,
  values: list(string),
}
and rootStructure =
  | Union(union)
  | Object(object_)
and fragment = {
  name: string,
  plural: bool,
  definition: rootStructure,
};

type obj = {
  originalFlowTypeName: option(string),
  definition: object_,
  foundInUnion: bool,
};

type unionInState = {
  printName: bool,
  union,
};

type finalizedObj = {
  originalFlowTypeName: option(string),
  recordName: option(string),
  atPath: list(string),
  definition: object_,
  foundInUnion: bool,
};

type intermediateState = {
  enums: list(fullEnum),
  objects: list(obj),
  variables: option(obj),
  response: option(obj),
  rawResponse: option(obj),
  fragment: option(fragment),
};

type fullState = {
  enums: list(fullEnum),
  unions: list(unionInState),
  objects: list(finalizedObj),
  variables: option(object_),
  response: option(object_),
  rawResponse: option(object_),
  fragment: option(fragment),
};

type printConfig = {
  operation_node: string,
  operation_hash: string,
  raw_js: string,
};
