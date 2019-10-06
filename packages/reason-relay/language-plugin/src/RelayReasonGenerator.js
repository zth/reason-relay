"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// @ts-ignore
const RelayFlowGenerator = require("relay-compiler/lib/language/javascript/RelayFlowGenerator");
const TypesTransformer_gen_1 = require("./transformer/TypesTransformer.gen");
const transformerUtils_1 = require("./transformer/transformerUtils");
const Utils_gen_1 = require("./generator/Utils.gen");
function mapCustomScalars(customScalars) {
    const newCustomScalars = { ...customScalars };
    Object.keys(newCustomScalars).forEach(key => {
        newCustomScalars[key] = Utils_gen_1.maskDots(newCustomScalars[key]);
    });
    return newCustomScalars;
}
function generate(node, options) {
    let flowTypes = RelayFlowGenerator.generate(node, {
        ...options,
        customScalars: mapCustomScalars(options.customScalars)
    });
    return TypesTransformer_gen_1.printFromFlowTypes({
        content: flowTypes,
        operationType: transformerUtils_1.makeOperationDescriptor(node)
    });
}
module.exports = {
    generate,
    transforms: RelayFlowGenerator.transforms
};
