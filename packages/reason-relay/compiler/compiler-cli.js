#!/usr/bin/env node
const path = require("path");
const fs = require("fs");
const { parseRE, printRE } = require("reason");
const mkdir = require("mkdirp-sync");
const { spawn } = require("child_process");
const { buildSchema, introspectionQuery, graphql } = require("graphql");
const { generateSchemaAssets } = require("./generateSchemaAssets");

if (process.argv.length > 1) {
  console.warn(
    "Warning: ReasonRelay compiler takes no CLI arguments. All configuration must be done through relay-config, preferably by creating a relay.config.js file at the project root."
  );
}

let relayConfig = require("relay-config").loadConfig();

if (!relayConfig) {
  console.error(
    "Could not find relay.config.js. You must configure Relay through relay.config.js for ReasonRelay to work."
  );

  process.exit(1);
}

if (!relayConfig.artifactDirectory) {
  console.error(
    "ReasonRelay requires you to define 'artifactDirectory' (for outputing generated files in a single directory) in your relay.config.js. Please define it and re-run this command."
  );
  process.exit(1);
}

function runRelayCompiler(args) {
  spawn(
    path.resolve(
      __dirname + "/../src/vendor/relay-compiler/bin/relay-compiler"
    ),
    args,
    {
      stdio: "inherit"
    }
  );
}

async function parseSchema(rawSchemaContent, pathToSchema) {
  const schemaType = pathToSchema.split(".").pop();
  let schema;

  switch (schemaType.toLowerCase()) {
    default:
    case "json":
      schema = JSON.parse(rawSchemaContent);
      break;
    case "graphql":
      const graphqlSchema = buildSchema(rawSchemaContent);
      const jsonIntrospectionSchema = await graphql(
        graphqlSchema,
        introspectionQuery,
        {}
      );
      schema = jsonIntrospectionSchema;
      break;
  }

  return schema;
}

function findArg(name) {
  return relayConfig[name];
}

async function runCompiler() {
  const schemaPath = findArg("schema");

  if (schemaPath) {
    console.log("Generating ReasonRelay assets...");

    parseSchema(fs.readFileSync(schemaPath, "utf8"), schemaPath).then(
      schemaJson => {
        let targetPath = relayConfig.artifactDirectory;
        mkdir(targetPath);
        generateSchemaAssets(schemaJson, targetPath);
      }
    );

    runRelayCompiler([
      "--language",
      path.resolve(__dirname + "/../language-plugin/index.js")
    ]);
  } else {
    runRelayCompiler(["--help"]);
  }
}

runCompiler();
