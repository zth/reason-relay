module.exports = {
  client: {
    tagName: "graphql",
    includes: ["./src/**/*.re"],
    excludes: ["**/__generated__/**"],
    service: {
      name: "graphql-schema",
      localSchemaFile: "./example/schema.graphql"
    }
  }
};
