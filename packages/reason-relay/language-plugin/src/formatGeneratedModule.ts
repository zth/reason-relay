import { FormatModule } from "relay-compiler";
import { processConcreteText } from "./utils/processConcreteText";

const formatGeneratedModule: FormatModule = ({
  moduleName,
  documentType,
  docText,
  concreteText,
  typeText,
  kind,
  hash,
  sourceHash,
}) => {
  const preloadText =
    // @ts-ignore The type definitions are actually wrong from DefinitivelyTyped
    documentType === "ConcreteRequest" &&
    moduleName.toLowerCase().endsWith("query_graphql")
      ? `include ReasonRelay.MakeLoadQuery({
    type variables = Types.variables;
    type loadedQueryRef = queryRef;
    type response = Types.response;
    type node = relayOperationNode;
    let query = node;
    let convertVariables = Internal.convertVariables;
  });`
      : "";

  return `
${typeText || ""}

let node: operationType = [%raw {json| ${processConcreteText(
    concreteText
  )} |json}];

${preloadText}
`;
};

module.exports = formatGeneratedModule;
