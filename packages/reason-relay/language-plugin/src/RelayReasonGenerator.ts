// @ts-ignore
import * as RelayFlowGenerator from "relay-compiler/lib/language/javascript/RelayFlowGenerator";

import { Fragment, Node } from "relay-compiler";
import { TypeGeneratorOptions } from "relay-compiler/lib/language/RelayLanguagePluginInterface";
import { printFromFlowTypes } from "./transformer/TypesTransformer.gen";
import { makeOperationDescriptor } from "./transformer/transformerUtils";
import { ScalarTypeMapping } from "relay-compiler/lib/language/javascript/RelayFlowTypeTransformers";
import { maskDots } from "./generator/Utils.gen";

function mapCustomScalars(customScalars: ScalarTypeMapping): ScalarTypeMapping {
  const newCustomScalars = { ...customScalars };
  Object.keys(newCustomScalars).forEach(key => {
    newCustomScalars[key] = maskDots(newCustomScalars[key]);
  });

  return newCustomScalars;
}

function generate(
  node: Node | Fragment,
  options: TypeGeneratorOptions
): string {
  let flowTypes = RelayFlowGenerator.generate(node, {
    ...options,
    customScalars: mapCustomScalars(options.customScalars)
  });

  return printFromFlowTypes({
    content: flowTypes,
    operationType: makeOperationDescriptor(node)
  });
}

module.exports = {
  generate,
  transforms: RelayFlowGenerator.transforms
};
