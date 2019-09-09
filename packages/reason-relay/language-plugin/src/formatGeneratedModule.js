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

/**
 * Right now, we need to post-process the generated JS some. We do the following here:
 * - Alter all require()-operations to point at our generated files. This is because
 *   Relay by default outputs haste-style require calls for @refetchable operations,
 *   and that we're not currently allowed to alter the generated module names, before
 *   a PR for that lands.
 *
 *   We do this here via a hacky regexp because it's likely a good long while before
 *   any of those PRs lands in Relay.
 */

function processConcreteText(concreteText: string): string {
  let requireRegexp = /(require\(')([A-Za-z_.0-9]+)('\))/g;
  let str = concreteText;

  let result;

  while ((result = requireRegexp.exec(concreteText)) !== null) {
    let [fullStr, _, moduleName] = result;
    str = str.replace(fullStr, `require('./${moduleName}.bs.js')`);
  }

  return str;
}

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

let node: ReasonRelay.${opKind}Node = [%bs.raw {| ${processConcreteText(concreteText)} |}];
`);
};

module.exports = formatGeneratedModule;
