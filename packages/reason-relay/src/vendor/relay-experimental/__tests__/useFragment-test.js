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

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

var React = require('react');

var ReactRelayContext = require('react-relay/ReactRelayContext');

var TestRenderer = require('react-test-renderer');

var useFragmentOriginal = require('../useFragment');

var _require = require('relay-runtime'),
    FRAGMENT_OWNER_KEY = _require.FRAGMENT_OWNER_KEY,
    FRAGMENTS_KEY = _require.FRAGMENTS_KEY,
    ID_KEY = _require.ID_KEY,
    createOperationDescriptor = _require.createOperationDescriptor;

describe('useFragment', function () {
  var environment;
  var createMockEnvironment;
  var generateAndCompile;
  var gqlSingularQuery;
  var gqlSingularFragment;
  var gqlPluralQuery;
  var gqlPluralFragment;
  var singularQuery;
  var pluralQuery;
  var singularVariables;
  var pluralVariables;
  var renderSingularFragment;
  var renderPluralFragment;
  var renderSpy;
  var SingularRenderer;
  var PluralRenderer;
  var ContextProvider;

  function useFragment(fragmentNode, fragmentRef) {
    var data = useFragmentOriginal(fragmentNode, fragmentRef);
    renderSpy(data);
    return data;
  }

  function assertFragmentResults(expected) {
    // This ensures that useEffect runs
    jest.runAllImmediates();
    expect(renderSpy).toBeCalledTimes(1);
    var actualData = renderSpy.mock.calls[0][0];
    expect(actualData).toEqual(expected);
    renderSpy.mockClear();
  }

  function createFragmentRef(id, owner) {
    var _ref;

    return _ref = {}, (0, _defineProperty2["default"])(_ref, ID_KEY, id), (0, _defineProperty2["default"])(_ref, FRAGMENTS_KEY, {
      NestedUserFragment: {}
    }), (0, _defineProperty2["default"])(_ref, FRAGMENT_OWNER_KEY, owner.request), _ref;
  }

  beforeEach(function () {
    // Set up mocks
    jest.resetModules();
    renderSpy = jest.fn();

    var _require2 = require('relay-test-utils-internal');

    createMockEnvironment = _require2.createMockEnvironment;
    generateAndCompile = _require2.generateAndCompile;
    // Set up environment and base data
    environment = createMockEnvironment();
    var generated = generateAndCompile("\n        fragment NestedUserFragment on User {\n          username\n        }\n\n        fragment UserFragment on User  {\n          id\n          name\n          ...NestedUserFragment\n        }\n\n        query UserQuery($id: ID!) {\n          node(id: $id) {\n            ...UserFragment\n          }\n        }\n\n        fragment UsersFragment on User @relay(plural: true) {\n          id\n          name\n          ...NestedUserFragment\n        }\n\n        query UsersQuery($ids: [ID!]!, $scale: Int!) {\n          nodes(ids: $ids) {\n            ...UsersFragment\n          }\n        }\n    ");
    singularVariables = {
      id: '1'
    };
    pluralVariables = {
      ids: ['1', '2']
    };
    gqlSingularQuery = generated.UserQuery;
    gqlSingularFragment = generated.UserFragment;
    gqlPluralQuery = generated.UsersQuery;
    gqlPluralFragment = generated.UsersFragment;
    singularQuery = createOperationDescriptor(gqlSingularQuery, singularVariables);
    pluralQuery = createOperationDescriptor(gqlPluralQuery, pluralVariables);
    environment.commitPayload(singularQuery, {
      node: {
        __typename: 'User',
        id: '1',
        name: 'Alice',
        username: 'useralice'
      }
    });
    environment.commitPayload(pluralQuery, {
      nodes: [{
        __typename: 'User',
        id: '1',
        name: 'Alice',
        username: 'useralice',
        profile_picture: null
      }, {
        __typename: 'User',
        id: '2',
        name: 'Bob',
        username: 'userbob',
        profile_picture: null
      }]
    }); // Set up renderers

    SingularRenderer = function SingularRenderer(props) {
      return null;
    };

    PluralRenderer = function PluralRenderer(props) {
      return null;
    };

    var SingularContainer = function SingularContainer(props) {
      var _ref2;

      // We need a render a component to run a Hook
      var owner = props.owner;
      var userRef = props.hasOwnProperty('userRef') ? props.userRef : (_ref2 = {}, (0, _defineProperty2["default"])(_ref2, ID_KEY, owner.request.variables.id), (0, _defineProperty2["default"])(_ref2, FRAGMENTS_KEY, {
        UserFragment: {}
      }), (0, _defineProperty2["default"])(_ref2, FRAGMENT_OWNER_KEY, owner.request), _ref2);
      var userData = useFragment(gqlSingularFragment, userRef);
      return React.createElement(SingularRenderer, {
        user: userData
      });
    };

    var PluralContainer = function PluralContainer(props) {
      // We need a render a component to run a Hook
      var owner = props.owner;
      var usersRef = props.hasOwnProperty('usersRef') ? props.usersRef : owner.request.variables.ids.map(function (id) {
        var _ref3;

        return _ref3 = {}, (0, _defineProperty2["default"])(_ref3, ID_KEY, id), (0, _defineProperty2["default"])(_ref3, FRAGMENTS_KEY, {
          UsersFragment: {}
        }), (0, _defineProperty2["default"])(_ref3, FRAGMENT_OWNER_KEY, owner.request), _ref3;
      });
      var usersData = useFragment(gqlPluralFragment, usersRef);
      return React.createElement(PluralRenderer, {
        users: usersData
      });
    };

    var relayContext = {
      environment: environment,
      // TODO(T39494051) - We set empty variables in relay context to make
      // Flow happy, but useFragmentNodes does not use them, instead it uses
      // the variables from the fragment owner.
      variables: {}
    };

    ContextProvider = function ContextProvider(_ref4) {
      var children = _ref4.children;
      return React.createElement(ReactRelayContext.Provider, {
        value: relayContext
      }, children);
    };

    renderSingularFragment = function renderSingularFragment(props) {
      return TestRenderer.create(React.createElement(React.Suspense, {
        fallback: "Singular Fallback"
      }, React.createElement(ContextProvider, null, React.createElement(SingularContainer, (0, _extends2["default"])({
        owner: singularQuery
      }, props)))));
    };

    renderPluralFragment = function renderPluralFragment(props) {
      return TestRenderer.create(React.createElement(React.Suspense, {
        fallback: "Plural Fallback"
      }, React.createElement(ContextProvider, null, React.createElement(PluralContainer, (0, _extends2["default"])({
        owner: pluralQuery
      }, props)))));
    };
  });
  afterEach(function () {
    environment.mockClear();
    renderSpy.mockClear();
  }); // These tests are only a sanity check for useFragment as a wrapper
  // around useFragmentNodes
  // See full test behavior in useFragmentNodes-test.

  it('should render singular fragment without error when data is available', function () {
    renderSingularFragment();
    assertFragmentResults((0, _objectSpread2["default"])({
      id: '1',
      name: 'Alice'
    }, createFragmentRef('1', singularQuery)));
  });
  it('should render plural fragment without error when data is available', function () {
    renderPluralFragment();
    assertFragmentResults([(0, _objectSpread2["default"])({
      id: '1',
      name: 'Alice'
    }, createFragmentRef('1', pluralQuery)), (0, _objectSpread2["default"])({
      id: '2',
      name: 'Bob'
    }, createFragmentRef('2', pluralQuery))]);
  });
});