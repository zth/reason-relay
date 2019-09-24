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

var React = require('react');

var RelayEnvironmentProvider = require('../RelayEnvironmentProvider'); // $FlowFixMe


var TestRenderer = require('react-test-renderer');

var useIsParentQueryInFlight = require('../useIsParentQueryInFlight');

var _require = require('relay-runtime'),
    Environment = _require.Environment,
    Network = _require.Network,
    Observable = _require.Observable,
    RecordSource = _require.RecordSource,
    Store = _require.Store,
    fetchQuery = _require.__internal.fetchQuery,
    createOperationDescriptor = _require.createOperationDescriptor;

var dataSource;
var environment;
var fetch;
var fragment;
var fragmentRef;
var operation;
var query;
beforeEach(function () {
  var _snapshot$data;

  jest.spyOn(console, 'warn').mockImplementation(function () {});
  jest.spyOn(console, 'error').mockImplementation(function () {});

  var _require2 = require('relay-test-utils-internal'),
      generateAndCompile = _require2.generateAndCompile;

  var source = new RecordSource();
  var store = new Store(source);
  fetch = jest.fn(function (_query, _variables, _cacheConfig) {
    return Observable.create(function (sink) {
      dataSource = sink;
    });
  });
  environment = new Environment({
    network: Network.create(fetch),
    store: store
  });

  var _generateAndCompile = generateAndCompile("\n    query UserQuery($id: ID!) {\n      node(id: $id) {\n        ...UserFragment\n      }\n    }\n\n    fragment UserFragment on User {\n      id\n      name\n    }\n  "),
      UserQuery = _generateAndCompile.UserQuery,
      UserFragment = _generateAndCompile.UserFragment;

  query = UserQuery;
  fragment = UserFragment;
  operation = createOperationDescriptor(UserQuery, {
    id: '4'
  });
  environment.commitPayload(operation, {
    node: {
      __typename: 'User',
      id: '4',
      name: 'Zuck'
    }
  });
  var snapshot = environment.lookup(operation.fragment);
  fragmentRef = (_snapshot$data = snapshot.data) === null || _snapshot$data === void 0 ? void 0 : _snapshot$data.node;
});
it('returns false when owner is not pending', function () {
  var pending = null;

  function Component() {
    pending = useIsParentQueryInFlight(fragment, fragmentRef);
    return null;
  }

  TestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, null)));
  expect(fetch).toBeCalledTimes(0);
  expect(pending).toBe(false);
});
it('returns false when an unrelated owner is pending', function () {
  // fetch a different id
  fetchQuery(environment, createOperationDescriptor(query, {
    id: '842472'
  })).subscribe({});
  expect(fetch).toBeCalledTimes(1);
  var pending = null;

  function Component() {
    pending = useIsParentQueryInFlight(fragment, fragmentRef);
    return null;
  }

  TestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, null)));
  expect(pending).toBe(false);
});
it('returns true when owner is started but has not returned payloads', function () {
  fetchQuery(environment, operation).subscribe({});
  expect(fetch).toBeCalledTimes(1);
  var pending = null;

  function Component() {
    pending = useIsParentQueryInFlight(fragment, fragmentRef);
    return null;
  }

  TestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, null)));
  expect(pending).toBe(true);
});
it('returns true when owner fetch has returned payloads but not completed', function () {
  fetchQuery(environment, operation).subscribe({});
  expect(fetch).toBeCalledTimes(1);
  TestRenderer.act(function () {
    dataSource.next({
      data: {
        node: {
          __typename: 'User',
          id: '4',
          name: 'Mark'
        }
      }
    });
  });
  var pending = null;

  function Component() {
    pending = useIsParentQueryInFlight(fragment, fragmentRef);
    return null;
  }

  TestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, null)));
  expect(pending).toBe(true);
});
it('returns false when owner fetch completed', function () {
  fetchQuery(environment, operation).subscribe({});
  expect(fetch).toBeCalledTimes(1);
  TestRenderer.act(function () {
    dataSource.next({
      data: {
        node: {
          __typename: 'User',
          id: '4',
          name: 'Mark'
        }
      }
    });
    dataSource.complete();
  });
  var pending = null;

  function Component() {
    pending = useIsParentQueryInFlight(fragment, fragmentRef);
    return null;
  }

  TestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, null)));
  expect(pending).toBe(false);
});
it('returns false when owner fetch errored', function () {
  var onError = jest.fn();
  fetchQuery(environment, operation).subscribe({
    error: onError
  });
  expect(fetch).toBeCalledTimes(1);
  dataSource.next({
    data: {
      node: {
        __typename: 'User',
        id: '4',
        name: 'Mark'
      }
    }
  });
  dataSource.error(new Error('wtf'));
  var pending = null;

  function Component() {
    pending = useIsParentQueryInFlight(fragment, fragmentRef);
    return null;
  }

  TestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, null)));
  expect(onError).toBeCalledTimes(1);
  expect(pending).toBe(false);
});
it('does not update the component when the owner is fetched', function () {
  var states = [];

  function Component() {
    states.push(useIsParentQueryInFlight(fragment, fragmentRef));
    return null;
  }

  TestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, null))); // Ensure that useEffect runs

  TestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(fetch).toBeCalledTimes(0);
  expect(states).toEqual([false]);
  fetchQuery(environment, operation).subscribe({});
  expect(fetch).toBeCalledTimes(1);
  expect(states).toEqual([false]);
});
it('does not update the component when a pending owner fetch returns a payload', function () {
  fetchQuery(environment, operation).subscribe({});
  expect(fetch).toBeCalledTimes(1);
  var states = [];

  function Component() {
    states.push(useIsParentQueryInFlight(fragment, fragmentRef));
    return null;
  }

  TestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, null))); // Ensure that useEffect runs

  TestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(states).toEqual([true]);
  TestRenderer.act(function () {
    dataSource.next({
      data: {
        node: {
          __typename: 'User',
          id: '4',
          name: 'Mark'
        }
      }
    });
    jest.runAllImmediates();
  });
  expect(states).toEqual([true]);
});
it('updates the component when a pending owner fetch completes', function () {
  fetchQuery(environment, operation).subscribe({});
  expect(fetch).toBeCalledTimes(1);
  var states = [];

  function Component() {
    states.push(useIsParentQueryInFlight(fragment, fragmentRef));
    return null;
  }

  TestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, null))); // Ensure that useEffect runs

  TestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(states).toEqual([true]);
  TestRenderer.act(function () {
    dataSource.complete();
    jest.runAllImmediates();
  });
  expect(states).toEqual([true, false]);
});
it('updates the component when a pending owner fetch errors', function () {
  var onError = jest.fn();
  fetchQuery(environment, operation).subscribe({
    error: onError
  });
  expect(fetch).toBeCalledTimes(1);
  var states = [];

  function Component() {
    states.push(useIsParentQueryInFlight(fragment, fragmentRef));
    return null;
  }

  TestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, null))); // Ensure that useEffect runs

  TestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(states).toEqual([true]);
  TestRenderer.act(function () {
    dataSource.error(new Error('wtf'));
    jest.runAllImmediates();
  });
  expect(onError).toBeCalledTimes(1);
  expect(states).toEqual([true, false]);
});
it('updates the component when a pending owner fetch with multiple payloads completes ', function () {
  fetchQuery(environment, operation).subscribe({});
  expect(fetch).toBeCalledTimes(1);
  var states = [];

  function Component() {
    states.push(useIsParentQueryInFlight(fragment, fragmentRef));
    return null;
  }

  TestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, null))); // Ensure that useEffect runs

  TestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(states).toEqual([true]);
  TestRenderer.act(function () {
    dataSource.next({
      data: {
        node: {
          id: '1',
          __typename: 'User'
        }
      }
    });
    jest.runAllImmediates();
  });
  TestRenderer.act(function () {
    dataSource.next({
      data: {
        id: '1',
        __typename: 'User',
        name: 'Mark'
      },
      label: 'UserQuery$defer$UserFragment',
      path: ['node']
    });
    dataSource.complete();
    jest.runAllImmediates();
  });
  expect(states).toEqual([true, false]);
});
it('should only update if the latest owner completes the query', function () {
  var _snapshot$data2;

  fetchQuery(environment, operation).subscribe({});
  var oldDataSource = dataSource;
  expect(fetch).toBeCalledTimes(1);

  var setRef = function setRef(ref) {};

  var mockFn = jest.fn(function () {});

  var Renderer = function Renderer(props) {
    mockFn(props.pending);
    return props.pending;
  };

  function Component() {
    var _React$useState = React.useState(fragmentRef),
        ref = _React$useState[0],
        setRefFn = _React$useState[1];

    setRef = setRefFn;
    var pending = useIsParentQueryInFlight(fragment, ref);
    return React.createElement(Renderer, {
      pending: pending
    });
  }

  TestRenderer.create(React.createElement(RelayEnvironmentProvider, {
    environment: environment
  }, React.createElement(Component, null)));
  TestRenderer.act(function () {
    return jest.runAllImmediates();
  });
  expect(mockFn.mock.calls[0]).toEqual([true]);
  var newOperation = createOperationDescriptor(query, {
    id: '5'
  });
  environment.commitPayload(newOperation, {
    node: {
      __typename: 'User',
      id: '5',
      name: 'Mark'
    }
  });
  var snapshot = environment.lookup(newOperation.fragment);
  var newFragmentRef = (_snapshot$data2 = snapshot.data) === null || _snapshot$data2 === void 0 ? void 0 : _snapshot$data2.node;
  expect(mockFn.mock.calls[0]).toEqual([true]);
  TestRenderer.act(function () {
    fetchQuery(environment, newOperation).subscribe({});
    setRef(newFragmentRef);
  });
  expect(mockFn.mock.calls).toEqual([[true], [true]]);
  TestRenderer.act(function () {
    return oldDataSource.complete();
  });
  expect(mockFn.mock.calls).toEqual([[true], [true]]);
  TestRenderer.act(function () {
    return dataSource.complete();
  });
  expect(mockFn.mock.calls).toEqual([[true], [true], [false]]);
  TestRenderer.act(function () {
    setRef(fragmentRef);
  });
  expect(mockFn.mock.calls).toEqual([[true], [true], [false], [false]]);
});