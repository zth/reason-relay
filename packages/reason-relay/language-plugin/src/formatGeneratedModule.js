/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

"use strict";

// $FlowFixMe
import type { FormatModule } from "../RelayLanguagePluginInterface";
import { printCode } from "./generator/Printer.gen";
import processConcreteText from "./utils/processConcreteText";

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
