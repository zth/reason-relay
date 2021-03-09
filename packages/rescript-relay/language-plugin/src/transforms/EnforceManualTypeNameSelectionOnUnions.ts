const IRTransformer = require("relay-compiler/lib/core/IRTransformer");
const { createUserError } = require("relay-compiler/lib/core/CompilerError");

const {
  hasUnaliasedSelection
} = require("relay-compiler/lib/transforms/TransformUtils");

const TYPENAME_KEY = "__typename";

export function transform(context: any) {
  return IRTransformer.transform(context, {
    LinkedField: visitLinkedField
  });
}

function visitLinkedField(field: any) {
  // @ts-ignore
  const schema = this.getContext().getSchema();
  // @ts-ignore
  let transformedNode = this.traverse(field);
  if (
    schema.isAbstractType(schema.getRawType(transformedNode.type)) &&
    !hasUnaliasedSelection(transformedNode, TYPENAME_KEY)
  ) {
    throw createUserError(
      'Unions and interfaces must have the field __typename explicitly selected. Please add __typename to the fields selected by "' +
        field.alias +
        '" in your operation.',
      [field.loc]
    );
  }
  return transformedNode;
}
