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
import type { GraphQLTag } from 'relay-compiler/language/RelayLanguagePluginInterface';

const invariant = require('invariant');

function parseFile(text, file) {
  if (!text.includes('[%relay.')) {
    return [];
  }

  invariant(
    text.indexOf('[%relay.') >= 0,
    'RelayFileIRParser: Files should be filtered before passed to the ' +
      'parser, got unfiltered file `%s`.',
    file
  );

  /**
   * This should eventually be done in a native Reason program and not through a (horrible)
   * regexp, but this will do just to get things working.
   */

  const matched = text.match(
    /(?<=\[%relay\.(query|fragment|mutation|subscription))([\s\S]*?)(?=];)/g
  );

  if (matched) {
    // Removes {||} used in multiline Reason strings
    return matched.map(text => ({ template: text.replace(/({\||\|})/g, '') }));
  }

  return [];
}

function find(text: string, filePath: string): Array<GraphQLTag> {
  return parseFile(text, filePath);
}

module.exports = {
  find
};
