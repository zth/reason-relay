import { printCode } from "./generator/Printer.gen";
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
  sourceHash
}) => {
  return printCode(`
${typeText || ""}

let node: operationType = [%bs.raw {| ${processConcreteText(concreteText)} |}];
`);
};

module.exports = formatGeneratedModule;
