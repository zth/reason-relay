// @ts-ignore
import * as RelayFlowGenerator from "relay-compiler/lib/language/javascript/RelayFlowGenerator";

import { Fragment, Node } from "relay-compiler";
import { TypeGeneratorOptions } from "relay-compiler/lib/language/RelayLanguagePluginInterface";
import { printFromFlowTypes } from "./transformer/TypesTransformer.gen";
import { makeOperationDescriptor } from "./transformer/transformerUtils";
import { atPath } from "./transformer/TypesTransformer.gen";
import { GraphQLScalarType, GraphQLNonNull } from "graphql";

function generate(
  node: Node | Fragment,
  options: TypeGeneratorOptions
): string {
  return printFromFlowTypes({
    content: RelayFlowGenerator.generate(node, options),
    operationType: makeOperationDescriptor(node)
  });
}

module.exports = {
  generate,
  transforms: RelayFlowGenerator.transforms
};
