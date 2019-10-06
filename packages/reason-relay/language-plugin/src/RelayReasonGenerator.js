"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// @ts-ignore
const RelayFlowGenerator = require("relay-compiler/lib/language/javascript/RelayFlowGenerator");
const TypesTransformer_gen_1 = require("./transformer/TypesTransformer.gen");
const transformerUtils_1 = require("./transformer/transformerUtils");
function generate(node, options) {
    return TypesTransformer_gen_1.printFromFlowTypes({
        content: RelayFlowGenerator.generate(node, options),
        operationType: transformerUtils_1.makeOperationDescriptor(node)
    });
}
module.exports = {
    generate,
    transforms: RelayFlowGenerator.transforms
};
