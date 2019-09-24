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

var _extends2 = _interopRequireDefault(require("@babel/runtime/helpers/extends"));

var _objectWithoutPropertiesLoose2 = _interopRequireDefault(require("@babel/runtime/helpers/objectWithoutPropertiesLoose"));

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

var React = require('react');

var useMemo = React.useMemo;

var TestRenderer = require('react-test-renderer');

var invariant = require("fbjs/lib/invariant");

var useRefetchableFragmentOriginal = require('../useRefetchableFragment');

var ReactRelayContext = require('react-relay/ReactRelayContext');

var _require = require('relay-runtime'),
    FRAGMENT_OWNER_KEY = _require.FRAGMENT_OWNER_KEY,
    FRAGMENTS_KEY = _require.FRAGMENTS_KEY,
    ID_KEY = _require.ID_KEY,
    createOperationDescriptor = _require.createOperationDescriptor;

describe('useRefetchableFragment', function () {
  var environment;
  var gqlQuery;
  var gqlRefetchQuery;
  var gqlFragment;
  var createMockEnvironment;
  var generateAndCompile;
  var query;
  var variables;
  var renderFragment;
  var renderSpy;
  var Renderer;

  function useRefetchableFragment(fragmentNode, fragmentRef) {
    var _useRefetchableFragme = useRefetchableFragmentOriginal(fragmentNode, fragmentRef),
        data = _useRefetchableFragme[0],
        refetch = _useRefetchableFragme[1];

    renderSpy(data, refetch);
    return [data, refetch];
  }

  function assertCall(expected, idx) {
    var actualData = renderSpy.mock.calls[idx][0];
    expect(actualData).toEqual(expected.data);
  }

  function assertFragmentResults(expectedCalls) {
    // This ensures that useEffect runs
    TestRenderer.act(function () {
      return jest.runAllImmediates();
    });
    expect(renderSpy).toBeCalledTimes(expectedCalls.length);
    expectedCalls.forEach(function (expected, idx) {
      return assertCall(expected, idx);
    });
    renderSpy.mockClear();
  }

  function expectFragmentResults(expectedCalls) {
    assertFragmentResults(expectedCalls);
  }

  function createFragmentRef(id, owner) {
    var _ref;

    return _ref = {}, (0, _defineProperty2["default"])(_ref, ID_KEY, id), (0, _defineProperty2["default"])(_ref, FRAGMENTS_KEY, {
      NestedUserFragment: {}
    }), (0, _defineProperty2["default"])(_ref, FRAGMENT_OWNER_KEY, owner.request), _ref;
  }

  beforeEach(function () {
    var _gqlFragment$metadata, _gqlFragment$metadata2;

    // Set up mocks
    jest.resetModules();
    jest.spyOn(console, 'warn').mockImplementationOnce(function () {});
    jest.mock("fbjs/lib/warning");
    renderSpy = jest.fn();

    var _require2 = require('relay-test-utils-internal'),
        createMockEnvironment = _require2.createMockEnvironment,
        generateAndCompile = _require2.generateAndCompile; // Set up environment and base data


    environment = createMockEnvironment();
    var generated = generateAndCompile("\n        fragment NestedUserFragment on User {\n          username\n        }\n\n        fragment UserFragment on User\n        @refetchable(queryName: \"UserFragmentRefetchQuery\") {\n          id\n          name\n          profile_picture(scale: $scale) {\n            uri\n          }\n          ...NestedUserFragment\n        }\n\n        query UserQuery($id: ID!, $scale: Int!) {\n          node(id: $id) {\n            ...UserFragment\n          }\n        }\n    ");
    variables = {
      id: '1',
      scale: 16
    };
    gqlQuery = generated.UserQuery;
    gqlRefetchQuery = generated.UserFragmentRefetchQuery;
    gqlFragment = generated.UserFragment;
    !(((_gqlFragment$metadata = gqlFragment.metadata) === null || _gqlFragment$metadata === void 0 ? void 0 : (_gqlFragment$metadata2 = _gqlFragment$metadata.refetch) === null || _gqlFragment$metadata2 === void 0 ? void 0 : _gqlFragment$metadata2.operation) === '@@MODULE_START@@UserFragmentRefetchQuery.graphql@@MODULE_END@@') ? process.env.NODE_ENV !== "production" ? invariant(false, 'useRefetchableFragment-test: Expected refetchable fragment metadata to contain operation.') : invariant(false) : void 0; // Manually set the refetchable operation for the test.

    gqlFragment.metadata.refetch.operation = gqlRefetchQuery;
    query = createOperationDescriptor(gqlQuery, variables);
    environment.commitPayload(query, {
      node: {
        __typename: 'User',
        id: '1',
        name: 'Alice',
        username: 'useralice',
        profile_picture: null
      }
    }); // Set up renderers

    Renderer = function Renderer(props) {
      return null;
    };

    var Container = function Container(props) {
      // We need a render a component to run a Hook
      var artificialUserRef = useMemo(function () {
        var _query$request$variab, _ref2;

        return _ref2 = {}, (0, _defineProperty2["default"])(_ref2, ID_KEY, (_query$request$variab = query.request.variables.id) !== null && _query$request$variab !== void 0 ? _query$request$variab : query.request.variables.nodeID), (0, _defineProperty2["default"])(_ref2, FRAGMENTS_KEY, (0, _defineProperty2["default"])({}, gqlFragment.name, {})), (0, _defineProperty2["default"])(_ref2, FRAGMENT_OWNER_KEY, query.request), _ref2;
      }, []);

      var _useRefetchableFragme2 = useRefetchableFragment(gqlFragment, artificialUserRef),
          userData = _useRefetchableFragme2[0];

      return React.createElement(Renderer, {
        user: userData
      });
    };

    var ContextProvider = function ContextProvider(_ref3) {
      var children = _ref3.children;
      // TODO(T39494051) - We set empty variables in relay context to make
      // Flow happy, but useRefetchableFragment does not use them, instead it uses
      // the variables from the fragment owner.
      var relayContext = useMemo(function () {
        return {
          environment: environment,
          variables: {}
        };
      }, []);
      return React.createElement(ReactRelayContext.Provider, {
        value: relayContext
      }, children);
    };

    renderFragment = function renderFragment(args) {
      var _args;

      var _ref4 = (_args = args) !== null && _args !== void 0 ? _args : {},
          _ref4$isConcurrent = _ref4.isConcurrent,
          isConcurrent = _ref4$isConcurrent === void 0 ? false : _ref4$isConcurrent,
          props = (0, _objectWithoutPropertiesLoose2["default"])(_ref4, ["isConcurrent"]);

      return TestRenderer.create(React.createElement(React.Suspense, {
        fallback: "Fallback"
      }, React.createElement(ContextProvider, null, React.createElement(Container, (0, _extends2["default"])({
        owner: query
      }, props)))), {
        unstable_isConcurrent: isConcurrent
      });
    };
  });
  afterEach(function () {
    environment.mockClear();
    renderSpy.mockClear();
  }); // This test is only a sanity check for useRefetchableFragment as a wrapper
  // around useRefetchableFragmentNode.
  // See full test behavior in useRefetchableFragmentNode-test.

  it('should render fragment without error when data is available', function () {
    renderFragment();
    expectFragmentResults([{
      data: (0, _objectSpread2["default"])({
        id: '1',
        name: 'Alice',
        profile_picture: null
      }, createFragmentRef('1', query))
    }]);
  });
});