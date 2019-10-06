"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// @ts-ignore
const RelayFlowGenerator = require("relay-compiler/lib/language/javascript/RelayFlowGenerator");
const TypesTransformer_gen_1 = require("./transformer/TypesTransformer.gen");
const transformerUtils_1 = require("./transformer/transformerUtils");
const graphql_1 = require("graphql");
function generate(node, options) {
    return TypesTransformer_gen_1.printFromFlowTypes({
        content: RelayFlowGenerator.generate(node, options),
        operationType: transformerUtils_1.makeOperationDescriptor(node),
        lookupAtPath: (path) => {
            let thePath = path.slice().reverse();
            let atPath = transformerUtils_1.lookupPropAtPath(node, thePath);
            if (atPath != null &&
                (atPath.kind === "ScalarField" ||
                    atPath.kind === "LocalArgumentDefinition")) {
                let targetType = atPath.type instanceof graphql_1.GraphQLNonNull
                    ? atPath.type.ofType
                    : atPath.type;
                if (targetType instanceof graphql_1.GraphQLNonNull) {
                    throw new Error("Cannot have nested nullable types.");
                }
                if (targetType instanceof graphql_1.GraphQLScalarType) {
                    switch (targetType.name) {
                        case "Int":
                        case "Float":
                            return targetType.name;
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
