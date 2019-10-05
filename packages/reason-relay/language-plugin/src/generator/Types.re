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
  | Scalar(scalarValues)
  | Enum(string)
  | Object(object_)
  | Array(propValue)
  | FragmentRefValue(string)
  | TypeReference(string)
  | Union(string)
and propValue = {
  nullable: bool,
  propType,
}
and propValues =
  | FragmentRef(string)
  | Prop(string, propValue)
and object_ = {values: array(propValues)}
and rootType =
  | Operation(object_)
  | Fragment(object_)
  | Variables(object_)
  | InputObject(string, object_)
  | PluralFragment(object_);

type fullEnum = {
  name: string,
  values: array(string),
};