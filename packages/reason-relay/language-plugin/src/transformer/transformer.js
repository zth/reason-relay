"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const parser_1 = require("@babel/parser");
// import RelayFlowGenerator from "relay-compiler/lib/language/javascript/RelayFlowGenerator";
let testQuery = `
import type { ConcreteRequest } from 'relay-runtime';
import type { TodoApp_user$ref } from "./TodoApp_user.graphql";
export type appQueryVariables = {|
  userId?: ?string
|};
export type appQueryResponse = {|
  +user: ?{|
    +$fragmentRefs: TodoApp_user$ref
  |}
|};
export type appQuery = {|
  variables: appQueryVariables,
  response: appQueryResponse,
|};`;
let ast = parser_1.parse(testQuery, {
    plugins: ["flow"]
});
let variables = null;
let response = null;
function getNextSelections(selection) {
    switch (selection.kind) {
        case "FragmentSpread":
        case "ScalarField":
            return null;
        default:
            return selection.selections;
    }
}
function mapObjectTypeProperty(otp) {
    return {
        name: otp.key.type === "Identifier" ? otp.key.name : otp.key.value,
        optional: !!otp.optional
    };
}
function mapObjectType(ot) { }
ast.program.body.forEach(node => {
    if (node.type === "ExportNamedDeclaration" &&
        node.declaration &&
        node.declaration.type === "TypeAlias") {
        let { id: { name }, right } = node.declaration;
        if (right.type === "ObjectTypeAnnotation") {
            if (name.endsWith("Variables")) {
                variables = right;
            }
            else if (name.endsWith("Response")) {
                response = right;
            }
        }
    }
});
console.log(variables, response);
