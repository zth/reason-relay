#!/usr/bin/env node
const path = require("path");
const { spawn } = require("child_process");

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
  spawn("relay-compiler", args, {
    stdio: "inherit",
  })
    // Propagate the relay compiler's exit code.
    .on("close", process.exit.bind(process));
}

function findArg(name) {
  return relayConfig[name];
}

async function runCompiler() {
  const schemaPath = findArg("schema");

  if (schemaPath) {
    runRelayCompiler(
      [
        "--language",
        path.resolve(__dirname + "/../language-plugin/dist/index.js"),
        process.argv.find((a) => a === "--watch"),
      ].filter(Boolean)
    );
  } else {
    runRelayCompiler(["--help"]);
  }
}

runCompiler();
