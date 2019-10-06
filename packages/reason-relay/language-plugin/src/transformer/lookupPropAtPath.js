/**
 * Paths can only move through LinkedFields.
 */
function lookupPropAtPath(root, path) {
  let node = null;
  let currentSelections = root.selections;

  let thePath = path.slice(1);
  let operationType = path[0];
  console.log(operationType);
  

  /**
   * Variables are handled independently.
   */

  if (operationType === "variables") {
    let variableName = thePath[0];
    let variable = root.argumentDefinitions.find(a => a.name === variableName);

    if (!variable) {
      throw new Error("Could not resolve variable '" + variableName + "'.");
    }

    return variable;
  }

  for (let i = 0; i <= thePath.length - 1; i += 1) {
    let isEnd = i === thePath.length - 1;
    let currentPath = thePath[i];

    let newNode = isEnd
      ? currentSelections.find(s => s.alias && s.alias === currentPath)
      : currentSelections.find(
          s => s.kind === "LinkedField" && s.alias === currentPath
        );

    if (!newNode || (!isEnd && newNode && newNode.kind !== "LinkedField")) {
      throw new Error("Could not resolve path '" + path.join(" -> ") + ".");
    }

    if (!isEnd) {
      currentSelections = newNode.selections;
    }

    node = newNode;
  }

  return node;
}

module.exports = {
  lookupPropAtPath
};
