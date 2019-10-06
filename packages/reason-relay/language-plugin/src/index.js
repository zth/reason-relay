"use strict";
const RelayReasonGenerator = require("./RelayReasonGenerator");
const formatGeneratedModule = require("./formatGeneratedModule");
const { find } = require("./FindGraphQLTags");
const path = require("path");
const fs = require("fs");
function getFileFilter(baseDir) {
    return (file) => {
        const filePath = path.join(baseDir, file.relPath);
        let text = "";
        try {
            text = fs.readFileSync(filePath, "utf8");
        }
        catch {
            // eslint-disable no-console
            console.warn(`RelaySourceModuleParser: Unable to read the file "${filePath}". Looks like it was removed.`);
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
    isGeneratedFile: (fileName) => fileName.endsWith("_graphql.re") || fileName.endsWith("_graphql.bs.js"),
    getFileFilter,
    getRefetchOperationModuleImportStatement: (operationName) => `${operationName}_graphql.bs`,
    getModuleName: (operationName) => `${operationName}_graphql`
});
