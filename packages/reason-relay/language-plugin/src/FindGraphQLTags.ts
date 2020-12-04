import { GraphQLTag } from "relay-compiler/lib/language/RelayLanguagePluginInterface";
import invariant = require("invariant");

function parseFile(text: string, file: string) {
  if (!text.includes("%relay")) {
    return [];
  }

  invariant(
    text.indexOf("%relay") >= 0,
    "RelayFileIRParser: Files should be filtered before passed to the " +
      "parser, got unfiltered file `%s`.",
    file
  );

  /**
   * This should eventually be done in a native Reason program and not through a (horrible)
   * regexp, but this will do just to get things working.
   */

  const matchedReason = text.match(
    /(?<=\[%relay\.(query|fragment|mutation|subscription))([\s\S]*?)(?=];)/g
  );

  if (matchedReason) {
    // Removes {||} used in multiline Reason strings
    return matchedReason.map((text) => ({
      template: text.replace(/({\||\|})/g, ""),
      keyName: null,
      sourceLocationOffset: { line: 1, column: 1 },
    }));
  }

  const matchedReScript = text.match(
    /(?<=%relay\.(query|fragment|mutation|subscription)\()([\s\S]*?)(?=(\`\s*)\))(\`)/g
  );

  if (matchedReScript) {
    // Removes `` used in multiline ReScript strings
    return matchedReScript.map((text) => ({
      template: text.replace(/`/g, ""),
      keyName: null,
      sourceLocationOffset: { line: 1, column: 1 },
    }));
  }

  return [];
}

export function find(text: string, filePath: string): Array<GraphQLTag> {
  return parseFile(text, filePath);
}
