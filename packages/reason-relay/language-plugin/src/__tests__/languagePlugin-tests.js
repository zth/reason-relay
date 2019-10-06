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
    it("prints the correct operationType type", () => {
      expect(
        generate(
          `query appQuery($userId: ID!) {
            user(id: $userId) {
              id
              firstName
            }
          }`
        ).includes("type operationType = ReasonRelay.queryNode;")
      ).toBe(true);
    });

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

    it("prints variables as unit if not variables are supplied", () => {
      let generated = generate(
        `query appQuery {
            me {
              id
              firstName
            }
          }`
      );

      expect(generated.includes("type variables = unit;")).toBe(true);
    });

    it("prints nested objects inlined in types", () => {
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

  describe("Mutation", () => {
    it("prints the correct operationType type", () => {
      expect(
        generate(
          `mutation SetUserLocationMutation($input: SetUserLocationInput!) {
            setUserLocation(input: $input) {
              changedUser {
                id
                firstName
              }
            }
          }`
        ).includes("type operationType = ReasonRelay.mutationNode;")
      ).toBe(true);
    });
  });

  describe("Subscription", () => {
    it("prints the correct operationType type", () => {
      expect(
        generate(
          `subscription SomeSubscription($input: UserChangedInput!) {
            userChanged(input: $input) {
              user {
                id
                firstName
              }
            }
          }`
        ).includes("type operationType = ReasonRelay.subscriptionNode;")
      ).toBe(true);
    });
  });

  describe("Fragment", () => {
    it("prints the correct operationType type", () => {
      expect(
        generate(
          `fragment SomeComponent_user on User {
            id
            firstName
          }`
        ).includes("type operationType = ReasonRelay.fragmentNode;")
      ).toBe(true);
    });
  });

  describe("Enums", () => {
    it("references any enums by global, generated schema assets file", () => {
      let generated = generate(
        `query appQuery {
            me {
              role
            }
          }`
      );

      expect(
        generated.includes(`"role": SchemaAssets.Enum_UserRole.wrapped`)
      ).toBe(true);
    });
  });

  describe("Unions", () => {
    it("generates code to unwrap unions", () => {
      let generated = generate(
        `query appQuery {
            participantById(id: "123") {
              __typename
              ... on User {
                id
                firstName
                lastName
              }

              ... on Observer {
                id
                name
              }
            }
          }`
      );

      expect(generated).toMatchSnapshot();
    });

    it("generates opaque wrapped union types referenced by path in types", () => {
      let generated = generate(
        `query appQuery {
            participantById(id: "123") {
              __typename
              ... on User {
                id
                firstName
                lastName
              }

              ... on Observer {
                id
                name
              }
            }
          }`
      );

      expect(
        generated.includes("type union_response_participantById_wrapped;")
      ).toBe(true);
    });
  });
});
