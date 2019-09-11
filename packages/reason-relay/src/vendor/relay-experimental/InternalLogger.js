/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @emails oncall+relay
 * 
 * @format
 */
'use strict';

var loggerImpl = function loggerImpl(eventData) {};

module.exports = {
  setLoggerImplementation: function setLoggerImplementation(loggerFn) {
    loggerImpl = loggerFn;
  },
  logEvent: function logEvent(eventData) {
    return loggerImpl(eventData);
  }
};