"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const Printer_gen_1 = require("./generator/Printer.gen");
const processConcreteText_1 = require("./utils/processConcreteText");
const formatGeneratedModule = ({ moduleName, documentType, docText, concreteText, typeText, kind, hash, sourceHash }) => {
    return Printer_gen_1.printCode(`
${typeText || ""}

let node: operationType = [%bs.raw {| ${processConcreteText_1.processConcreteText(concreteText)} |}];
`);
};
module.exports = formatGeneratedModule;
