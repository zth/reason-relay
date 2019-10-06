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

function collapseString(str) {
  return str.replace(/\r?\n|\r|\t/g, "").replace(/\s+/g, " ");
}

function generate(text, options, extraDefs = "") {
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

    it("prints single fragment references", () => {
      let generated = generate(
        `
        fragment SomeComponent_user on User {
          id
        }

        query appQuery {
            me {
              id
              firstName
              ...SomeComponent_user
            }
          }`
      );

      expect(
        generated.includes(
          '"__$fragment_ref__SomeComponent_user": SomeComponent_user_graphql.t'
        )
      ).toBe(true);
    });

    it("prints two fragment references", () => {
      let generated = generate(
        `
        fragment SomeComponent_user on User {
          id
        }

        fragment OtherComponent_user on User {
          id
        }

        query appQuery {
            me {
              id
              firstName
              ...SomeComponent_user
              ...OtherComponent_user
            }
          }`
      );

      expect(
        generated.includes(
          '"__$fragment_ref__SomeComponent_user": SomeComponent_user_graphql.t'
        )
      ).toBe(true);

      expect(
        generated.includes(
          '"__$fragment_ref__OtherComponent_user": OtherComponent_user_graphql.t'
        )
      ).toBe(true);
    });

    it("prints many fragment references", () => {
      let generated = generate(
        `
        fragment SomeComponent_user on User {
          id
        }

        fragment OtherComponent_user on User {
          id
        }

        fragment AnotherComponent_user on User {
          id
        }

        fragment LastComponent_user on User {
          id
        }

        query appQuery {
            me {
              id
              firstName
              ...SomeComponent_user
              ...OtherComponent_user
              ...AnotherComponent_user
              ...LastComponent_user
            }
          }`
      );

      expect(
        generated.includes(
          '"__$fragment_ref__SomeComponent_user": SomeComponent_user_graphql.t'
        )
      ).toBe(true);

      expect(
        generated.includes(
          '"__$fragment_ref__OtherComponent_user": OtherComponent_user_graphql.t'
        )
      ).toBe(true);

      expect(
        generated.includes(
          '"__$fragment_ref__AnotherComponent_user": AnotherComponent_user_graphql.t'
        )
      ).toBe(true);

      expect(
        generated.includes(
          '"__$fragment_ref__LastComponent_user": LastComponent_user_graphql.t'
        )
      ).toBe(true);
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

    it("prints the correct basic structure for mutations", () => {
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
        )
      ).toMatchSnapshot();
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

    it("prints the correct basic structure for subscriptions", () => {
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
        )
      ).toMatchSnapshot();
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

    it("handles plural fragments", () => {
      let generated = generate(
        `fragment SomeComponent_user on User @relay(plural: true) {
          id
          firstName
        }`
      );

      expect(
        collapseString(generated).includes(
          `type fragment = array({ . "firstName": string, "id": string, });`
        )
      ).toBe(true);
    });

    it("prints the correct fragment ref extractor helpers and base types", () => {
      expect(
        generate(
          `fragment SomeComponent_user on User {
            id
          }`
        )
      ).toMatchSnapshot();
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

  describe("Custom scalars", () => {
    it("outputs any unmapped custom scalars as any", () => {
      let generated = generate(
        `query appQuery {
            me {
              favoriteColor
            }
          }`
      );

      expect(generated.includes(`"favoriteColor": ReasonRelay.any`)).toBe(true);
    });

    it("handles provided custom scalars", () => {
      let generated = generate(
        `query appQuery {
            me {
              favoriteColor
            }
          }`,
        {
          customScalars: {
            Color: "Color.t"
          }
        }
      );

      expect(generated.includes(`"favoriteColor": Color.t`)).toBe(true);
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
