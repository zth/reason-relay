/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @emails oncall+relay
 */

"use strict";

const { buildSchema } = require("graphql");
const fs = require("fs");
const path = require("path");
const RelayReasonGenerator = require("../RelayReasonGenerator");
const { printCode } = require("../generator/Printer.gen");

const GraphQLCompilerContext = require("relay-compiler/lib/core/GraphQLCompilerContext");
const RelayFlowGenerator = require("relay-compiler/lib/language/javascript/RelayFlowGenerator");
const RelayIRTransforms = require("relay-compiler/lib/core/RelayIRTransforms");

const { transformASTSchema } = require("relay-compiler/lib/core/ASTConvert");
const { parseGraphQLText } = require("relay-test-utils-internal");

const testSchema = buildSchema(
  fs.readFileSync(
    path.resolve(path.join(__dirname, "testSchema.graphql")),
    "utf8"
  )
);

import type { TypeGeneratorOptions } from "relay-compiler/lib/RelayLanguagePluginInterface";

function generate(text, options?: TypeGeneratorOptions, extraDefs = "") {
  const schema = transformASTSchema(testSchema, [
    ...RelayIRTransforms.schemaExtensions,
    extraDefs
  ]);
  const { definitions } = parseGraphQLText(schema, text);
  return new GraphQLCompilerContext(testSchema, schema)
    .addAll(definitions)
    .applyTransforms(RelayFlowGenerator.transforms)
    .documents()
    .map(
      doc =>
        `// ${doc.name}.graphql\n${printCode(
          RelayReasonGenerator.generate(doc, {
            customScalars: {},
            optionalInputFields: [],
            existingFragmentNames: new Set([]),
            ...options
          })
        )}`
    )
    .join("\n\n");
}

describe("Language plugin tests", () => {
  describe("Query", () => {
    it("prints simple responses and variables", () => {
      let generated = generate(
        `query appQuery($userId: ID!) {
            user(id: $userId) {
              id
              firstName
            }
          }`
      );

      expect(generated).toMatchSnapshot();
    });

    it("prints nested objects", () => {
      let generated = generate(
        `query appQuery($location: LocationBounds!) {
            userByLocation(location: $location) {
              id
              firstName
            }
          }`
      );

      expect(generated).toMatchSnapshot();
    });
  });
});
