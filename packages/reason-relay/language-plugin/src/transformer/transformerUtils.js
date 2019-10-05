"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
// @ts-ignore
var lookupPropAtPath_1 = require("./lookupPropAtPath");
exports.lookupPropAtPath = lookupPropAtPath_1.lookupPropAtPath;
function makeOperationDescriptor(node) {
    if (node.kind === "Root") {
        switch (node.operation) {
            case "mutation":
                return {
                    tag: "Mutation",
                    value: node.name
                };
            case "query":
                return {
                    tag: "Query",
                    value: node.name
                };
            case "subscription":
                return {
                    tag: "Subscription",
                    value: node.name
                };
        }
    }
    else if (node.kind == "Fragment") {
        return {
            tag: "Fragment",
            value: [node.name, Boolean(node.metadata && node.metadata.plural)]
        };
    }
    throw new Error("Could not map root node. This should not happen.");
}
exports.makeOperationDescriptor = makeOperationDescriptor;
