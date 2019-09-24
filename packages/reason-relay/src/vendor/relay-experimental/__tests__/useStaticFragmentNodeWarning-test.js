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

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault");

var _objectSpread2 = _interopRequireDefault(require("@babel/runtime/helpers/objectSpread"));

var mockWarning = jest.fn();
jest.mock("fbjs/lib/warning", function () {
  return mockWarning;
});

var React = require('react'); // $FlowFixMe


var TestRenderer = require('react-test-renderer');

var useStaticFragmentNodeWarning = require('../useStaticFragmentNodeWarning');

var warningMessage = 'Relay: The %s has to remain the same over the lifetime of a component. Changing ' + 'it is not supported and will result in unexpected behavior.';
var notWarned = [true, warningMessage, 'fragment input'];
var warned = [false, warningMessage, 'fragment input'];

function Example(props) {
  // $FlowFixMe
  useStaticFragmentNodeWarning(props.foo, 'fragment input');
  return null;
}

test('warn when a static prop changes', function () {
  var fragmentNode = {
    name: 'Fragment_foo'
  }; // initial render doesn't warn

  var testRenderer = TestRenderer.create(React.createElement(Example, {
    foo: fragmentNode,
    bar: "bar1"
  }));
  expect(mockWarning.mock.calls.length).toBe(1);
  expect(mockWarning.mock.calls[0]).toEqual(notWarned); // not updating props doesnt warn

  testRenderer.update(React.createElement(Example, {
    foo: fragmentNode,
    bar: "bar1"
  }));
  expect(mockWarning.mock.calls.length).toBe(2);
  expect(mockWarning.mock.calls[1]).toEqual(notWarned); // updating a non-checked prop doesn't warn

  testRenderer.update(React.createElement(Example, {
    foo: fragmentNode,
    bar: "bar2"
  }));
  expect(mockWarning.mock.calls.length).toBe(3);
  expect(mockWarning.mock.calls[2]).toEqual(notWarned); // different fragment node with same name doesn't warn

  testRenderer.update(React.createElement(Example, {
    foo: (0, _objectSpread2["default"])({}, fragmentNode),
    bar: "bar1"
  }));
  expect(mockWarning.mock.calls.length).toBe(4);
  expect(mockWarning.mock.calls[3]).toEqual(notWarned); // updating a expected static prop warns

  testRenderer.update(React.createElement(Example, {
    foo: {
      name: 'OtherFragment_foo'
    },
    bar: "bar1"
  }));
  expect(mockWarning.mock.calls.length).toBe(5);
  expect(mockWarning.mock.calls[4]).toEqual(warned);
});