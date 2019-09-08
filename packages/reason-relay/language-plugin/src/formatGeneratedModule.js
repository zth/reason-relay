/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

'use strict';

// $FlowFixMe
import type { FormatModule } from '../RelayLanguagePluginInterface';
import { printCode } from './generator/Printer.gen';

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
  const modName = moduleName.split('_graphql')[0];

  const opKind: null | 'fragment' | 'query' | 'mutation' | 'subscription' =
    kind === 'Fragment'
      ? 'fragment'
      : modName.endsWith('Query')
      ? 'query'
      : modName.endsWith('Mutation')
      ? 'mutation'
      : modName.endsWith('Subscription')
      ? 'subscription'
      : null;

  if (!opKind) {
    throw new Error(
      'Something went wrong, uninterpreted module type: "' + moduleName + '"'
    );
  }

  return printCode(`
${typeText || ''}

let node: ReasonRelay.${opKind}Node = [%bs.raw {| ${concreteText} |}];
`);
};

module.exports = formatGeneratedModule;
