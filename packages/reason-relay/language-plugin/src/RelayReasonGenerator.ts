// @ts-ignore
import * as RelayFlowGenerator from "relay-compiler/lib/language/javascript/RelayFlowGenerator";

import { Fragment, Node } from "relay-compiler";
import { TypeGeneratorOptions } from "relay-compiler/lib/language/RelayLanguagePluginInterface";
import { printFromFlowTypes } from "./transformer/TypesTransformer.gen";
import {
  makeOperationDescriptor,
  lookupPropAtPath
} from "./transformer/transformerUtils";
import { atPath } from "./transformer/TypesTransformer.gen";
import { GraphQLScalarType, GraphQLNonNull } from "graphql";

function generate(
  node: Node | Fragment,
  options: TypeGeneratorOptions
): string {
  return printFromFlowTypes({
    content: RelayFlowGenerator.generate(node, options),
    operationType: makeOperationDescriptor(node),
    lookupAtPath: (path: string[]): atPath => {
      let atPath = lookupPropAtPath(node, path);

      if (atPath != null && atPath.kind === "ScalarField") {
        let targetType =
          atPath.type instanceof GraphQLNonNull
            ? atPath.type.ofType
            : atPath.type;

        if (targetType instanceof GraphQLNonNull) {
          throw new Error("Cannot have nested nullable types.");
        }

        if (targetType instanceof GraphQLScalarType) {
          switch (targetType.name) {
            case "Int":
            case "Float":
              return atPath.type.name;
          }
        }
      }

      return "Unmapped";
    }
  });
}

module.exports = {
  generate,
  transforms: RelayFlowGenerator.transforms
};
