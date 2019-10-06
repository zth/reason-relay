"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const invariant = require("invariant");
function parseFile(text, file) {
    if (!text.includes("[%relay.")) {
        return [];
    }
    invariant(text.indexOf("[%relay.") >= 0, "RelayFileIRParser: Files should be filtered before passed to the " +
        "parser, got unfiltered file `%s`.", file);
    /**
     * This should eventually be done in a native Reason program and not through a (horrible)
     * regexp, but this will do just to get things working.
     */
    const matched = text.match(/(?<=\[%relay\.(query|fragment|mutation|subscription))([\s\S]*?)(?=];)/g);
    if (matched) {
        // Removes {||} used in multiline Reason strings
        return matched.map(text => ({
            template: text.replace(/({\||\|})/g, ""),
            keyName: "",
            sourceLocationOffset: { line: 0, column: 0 }
        }));
    }
    return [];
}
function find(text, filePath) {
    return parseFile(text, filePath);
}
exports.find = find;
