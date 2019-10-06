const RelayReasonGenerator = require("./RelayReasonGenerator");

const formatGeneratedModule = require("./formatGeneratedModule");

const { find } = require("./FindGraphQLTags");
const path = require("path");
const fs = require("fs");

function getFileFilter(baseDir: string) {
  return (file: { relPath: string }) => {
    const filePath = path.join(baseDir, file.relPath);
    let text = "";
    try {
      text = fs.readFileSync(filePath, "utf8");
    } catch {
      // eslint-disable no-console
      console.warn(
        `RelaySourceModuleParser: Unable to read the file "${filePath}". Looks like it was removed.`
      );
      return false;
    }
    return text.indexOf("[%relay.") >= 0;
  };
}

module.exports = () => ({
  inputExtensions: ["re"],
  outputExtension: "re",
  typeGenerator: RelayReasonGenerator,
  formatModule: formatGeneratedModule,
  findGraphQLTags: find,
  isGeneratedFile: (fileName: string) =>
    fileName.endsWith("_graphql.re") || fileName.endsWith("_graphql.bs.js"),
  getFileFilter,
  getRefetchOperationModuleImportStatement: (operationName: string) =>
    `${operationName}_graphql.bs`,
  getModuleName: (operationName: string) => `${operationName}_graphql`
});
