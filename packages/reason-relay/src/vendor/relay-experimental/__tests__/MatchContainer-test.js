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

var _defineProperty2 = _interopRequireDefault(require("@babel/runtime/helpers/defineProperty"));

var MatchContainer = require('../MatchContainer');

var React = require('react');

var TestRenderer = require('react-test-renderer');

var _require = require('relay-runtime'),
    FRAGMENT_OWNER_KEY = _require.FRAGMENT_OWNER_KEY,
    FRAGMENTS_KEY = _require.FRAGMENTS_KEY,
    ID_KEY = _require.ID_KEY;

function createMatchPointer(_ref) {
  var _pointer;

  var id = _ref.id,
      fragment = _ref.fragment,
      variables = _ref.variables,
      propName = _ref.propName,
      module = _ref.module;
  var pointer = (_pointer = {
    $fragmentRefs: {}
  }, (0, _defineProperty2["default"])(_pointer, ID_KEY, id), (0, _defineProperty2["default"])(_pointer, FRAGMENTS_KEY, {}), (0, _defineProperty2["default"])(_pointer, FRAGMENT_OWNER_KEY, null), (0, _defineProperty2["default"])(_pointer, "__fragmentPropName", propName), (0, _defineProperty2["default"])(_pointer, "__module_component", module), _pointer);

  if (fragment != null && variables != null) {
    pointer[FRAGMENTS_KEY][fragment.name] = variables;
  }

  return pointer;
}

describe('MatchContainer', function () {
  var ActorComponent;
  var UserComponent;
  var loader;
  beforeEach(function () {
    jest.resetModules();
    loader = jest.fn();
    UserComponent = jest.fn(function (props) {
      return React.createElement("div", null, React.createElement("h1", null, "User"), React.createElement("pre", null, JSON.stringify(props, null, 2)));
    });
    ActorComponent = jest.fn(function (props) {
      return React.createElement("div", null, React.createElement("h1", null, "Actor"), React.createElement("pre", null, JSON.stringify(props, null, 2)));
    });
  });
  it('throws when match prop is null', function () {
    // This prevents console.error output in the test, which is expected
    jest.spyOn(console, 'error').mockImplementationOnce(function () {});
    expect(function () {
      TestRenderer.create(React.createElement(MatchContainer, {
        loader: loader,
        match: 42
      }));
    }).toThrow('MatchContainer: Expected `match` value to be an object or null/undefined.');
  });
  it('loads and renders dynamic components', function () {
    loader.mockReturnValue(React.memo(UserComponent));
    var match = createMatchPointer({
      id: '4',
      fragment: {
        name: 'UserFragment'
      },
      variables: {},
      propName: 'user',
      module: 'UserContainer.react'
    });
    var renderer = TestRenderer.create(React.createElement(MatchContainer, {
      loader: loader,
      match: match,
      props: {
        otherProp: 'hello!'
      }
    }));
    expect(renderer.toJSON()).toMatchSnapshot();
    expect(loader).toBeCalledTimes(1);
    expect(UserComponent).toBeCalledTimes(1);
  });
  it('reloads if new props have a different component', function () {
    loader.mockReturnValue(React.memo(UserComponent));
    var match = createMatchPointer({
      id: '4',
      fragment: {
        name: 'UserFragment'
      },
      variables: {},
      propName: 'user',
      module: 'UserContainer.react'
    });
    var renderer = TestRenderer.create(React.createElement(MatchContainer, {
      loader: loader,
      match: match,
      props: {
        otherProp: 'hello!'
      }
    }));
    loader.mockReturnValue(React.memo(ActorComponent));
    var match2 = createMatchPointer({
      id: '4',
      fragment: {
        name: 'ActorFragment'
      },
      variables: {},
      propName: 'actor',
      module: 'ActorContainer.react'
    });
    renderer.update(React.createElement(MatchContainer, {
      loader: loader,
      match: match2,
      props: {
        otherProp: 'hello!'
      }
    }));
    expect(renderer.toJSON()).toMatchSnapshot();
    expect(loader).toBeCalledTimes(2);
    expect(UserComponent).toBeCalledTimes(1);
    expect(ActorComponent).toBeCalledTimes(1);
  });
  it('calls load again when re-rendered, even with the same component', function () {
    loader.mockReturnValue(React.memo(UserComponent));
    var match = createMatchPointer({
      id: '4',
      fragment: {
        name: 'UserFragment'
      },
      variables: {},
      propName: 'user',
      module: 'UserContainer.react'
    });
    var renderer = TestRenderer.create(React.createElement(MatchContainer, {
      loader: loader,
      match: match,
      props: {
        otherProp: 'hello!'
      }
    }));
    var match2 = (0, _objectSpread2["default"])({}, match, {
      __id: '0'
    });
    renderer.update(React.createElement(MatchContainer, {
      loader: loader,
      match: match2,
      props: {
        otherProp: 'hello!'
      }
    }));
    expect(renderer.toJSON()).toMatchSnapshot(); // We expect loader to already be caching module results

    expect(loader).toBeCalledTimes(2);
    expect(UserComponent).toBeCalledTimes(2);
    expect(ActorComponent).toBeCalledTimes(0);
  });
  it('passes the same child props when the match values does not change', function () {
    loader.mockReturnValue(React.memo(UserComponent));
    var match = createMatchPointer({
      id: '4',
      fragment: {
        name: 'UserFragment'
      },
      variables: {},
      propName: 'user',
      module: 'UserContainer.react'
    });
    var otherProps = {
      otherProp: 'hello!'
    };
    var renderer = TestRenderer.create(React.createElement(MatchContainer, {
      loader: loader,
      match: match,
      props: otherProps
    }));
    var match2 = (0, _objectSpread2["default"])({}, match);
    renderer.update(React.createElement(MatchContainer, {
      loader: loader,
      match: match2,
      props: otherProps
    }));
    expect(renderer.toJSON()).toMatchSnapshot();
    expect(loader).toBeCalledTimes(2);
    expect(UserComponent).toBeCalledTimes(1);
  });
  it('renders the fallback if the match object is empty', function () {
    loader.mockReturnValue(React.memo(UserComponent));
    var otherProps = {
      otherProp: 'hello!'
    };
    var Fallback = jest.fn(function () {
      return React.createElement("div", null, "fallback");
    });
    var renderer = TestRenderer.create(React.createElement(MatchContainer, {
      loader: loader,
      match: {} // intentionally empty
      ,
      props: otherProps,
      fallback: React.createElement(Fallback, null)
    }));
    expect(renderer.toJSON()).toMatchSnapshot();
    expect(loader).toBeCalledTimes(0);
    expect(UserComponent).toBeCalledTimes(0);
    expect(ActorComponent).toBeCalledTimes(0);
    expect(Fallback).toBeCalledTimes(1);
  });
  it('renders the fallback if the match object is missing expected fields', function () {
    loader.mockReturnValue(React.memo(UserComponent));
    var otherProps = {
      otherProp: 'hello!'
    };
    var Fallback = jest.fn(function () {
      return React.createElement("div", null, "fallback");
    });
    var renderer = TestRenderer.create(React.createElement(MatchContainer, {
      loader: loader,
      match: {
        __id: null,
        __fragments: null,
        __fragmentPropName: null,
        __fragmentOwner: null,
        __module_component: null
      } // intentionally all null
      ,
      props: otherProps,
      fallback: React.createElement(Fallback, null)
    }));
    expect(renderer.toJSON()).toMatchSnapshot();
    expect(loader).toBeCalledTimes(0);
    expect(UserComponent).toBeCalledTimes(0);
    expect(ActorComponent).toBeCalledTimes(0);
    expect(Fallback).toBeCalledTimes(1);
  });
  it('throws if the match object is invalid (__id)', function () {
    jest.spyOn(console, 'error').mockImplementationOnce(function () {});
    loader.mockReturnValue(React.memo(UserComponent));
    var otherProps = {
      otherProp: 'hello!'
    };
    var Fallback = jest.fn(function () {
      return React.createElement("div", null, "fallback");
    });
    expect(function () {
      TestRenderer.create(React.createElement(MatchContainer, {
        loader: loader,
        match: {
          __id: 42,
          // not a string
          __fragments: null,
          __fragmentPropName: null,
          __fragmentOwner: null,
          __module_component: null
        } // intentionally all null
        ,
        props: otherProps,
        fallback: React.createElement(Fallback, null)
      }));
    }).toThrow("MatchContainer: Invalid 'match' value, expected an object that has a '...SomeFragment' spread.");
  });
  it('throws if the match object is invalid (__fragments)', function () {
    jest.spyOn(console, 'error').mockImplementationOnce(function () {});
    loader.mockReturnValue(React.memo(UserComponent));
    var otherProps = {
      otherProp: 'hello!'
    };
    var Fallback = jest.fn(function () {
      return React.createElement("div", null, "fallback");
    });
    expect(function () {
      TestRenderer.create(React.createElement(MatchContainer, {
        loader: loader,
        match: {
          __id: null,
          __fragments: 42,
          // not an object
          __fragmentPropName: null,
          __fragmentOwner: null,
          __module_component: null
        } // intentionally all null
        ,
        props: otherProps,
        fallback: React.createElement(Fallback, null)
      }));
    }).toThrow("MatchContainer: Invalid 'match' value, expected an object that has a '...SomeFragment' spread.");
  });
  it('throws if the match object is invalid (__fragmentOwner)', function () {
    jest.spyOn(console, 'error').mockImplementationOnce(function () {});
    loader.mockReturnValue(React.memo(UserComponent));
    var otherProps = {
      otherProp: 'hello!'
    };
    var Fallback = jest.fn(function () {
      return React.createElement("div", null, "fallback");
    });
    expect(function () {
      TestRenderer.create(React.createElement(MatchContainer, {
        loader: loader,
        match: {
          __id: null,
          __fragments: null,
          __fragmentPropName: null,
          __fragmentOwner: 42,
          // not an object
          __module_component: null
        } // intentionally all null
        ,
        props: otherProps,
        fallback: React.createElement(Fallback, null)
      }));
    }).toThrow("MatchContainer: Invalid 'match' value, expected an object that has a '...SomeFragment' spread.");
  });
  it('throws if the match object is invalid (__fragmentPropName)', function () {
    jest.spyOn(console, 'error').mockImplementationOnce(function () {});
    loader.mockReturnValue(React.memo(UserComponent));
    var otherProps = {
      otherProp: 'hello!'
    };
    var Fallback = jest.fn(function () {
      return React.createElement("div", null, "fallback");
    });
    expect(function () {
      TestRenderer.create(React.createElement(MatchContainer, {
        loader: loader,
        match: {
          __id: null,
          __fragments: null,
          __fragmentPropName: 42,
          // not a string
          __fragmentOwner: null,
          __module_component: null
        } // intentionally all null
        ,
        props: otherProps,
        fallback: React.createElement(Fallback, null)
      }));
    }).toThrow("MatchContainer: Invalid 'match' value, expected an object that has a '...SomeFragment' spread.");
  });
  it('renders the fallback if the match value is null', function () {
    loader.mockReturnValue(React.memo(UserComponent));
    var otherProps = {
      otherProp: 'hello!'
    };
    var Fallback = jest.fn(function () {
      return React.createElement("div", null, "fallback");
    });
    var renderer = TestRenderer.create(React.createElement(MatchContainer, {
      loader: loader,
      match: null,
      props: otherProps,
      fallback: React.createElement(Fallback, null)
    }));
    expect(renderer.toJSON()).toMatchSnapshot();
    expect(loader).toBeCalledTimes(0);
    expect(UserComponent).toBeCalledTimes(0);
    expect(ActorComponent).toBeCalledTimes(0);
    expect(Fallback).toBeCalledTimes(1);
  });
  it('renders null if the match value is null and no fallback is provided', function () {
    loader.mockReturnValue(React.memo(UserComponent));
    var otherProps = {
      otherProp: 'hello!'
    };
    var renderer = TestRenderer.create(React.createElement(MatchContainer, {
      loader: loader,
      match: null,
      props: otherProps
    }));
    expect(renderer.toJSON()).toMatchSnapshot();
    expect(loader).toBeCalledTimes(0);
    expect(UserComponent).toBeCalledTimes(0);
    expect(ActorComponent).toBeCalledTimes(0);
  });
  it('renders the fallback if the match value is undefined', function () {
    loader.mockReturnValue(React.memo(UserComponent));
    var otherProps = {
      otherProp: 'hello!'
    };
    var Fallback = jest.fn(function () {
      return React.createElement("div", null, "fallback");
    });
    var renderer = TestRenderer.create(React.createElement(MatchContainer, {
      loader: loader,
      match: undefined,
      props: otherProps,
      fallback: React.createElement(Fallback, null)
    }));
    expect(renderer.toJSON()).toMatchSnapshot();
    expect(loader).toBeCalledTimes(0);
    expect(UserComponent).toBeCalledTimes(0);
    expect(ActorComponent).toBeCalledTimes(0);
    expect(Fallback).toBeCalledTimes(1);
  });
  it('transitions from fallback when new props have a component', function () {
    loader.mockReturnValue(React.memo(UserComponent));
    var Fallback = jest.fn(function () {
      return React.createElement("div", null, "fallback");
    });
    var renderer = TestRenderer.create(React.createElement(MatchContainer, {
      loader: loader,
      match: {} // intentionally empty
      ,
      props: {
        otherProp: 'hello!'
      },
      fallback: React.createElement(Fallback, null)
    }));
    expect(Fallback).toBeCalledTimes(1);
    loader.mockReturnValue(React.memo(ActorComponent));
    var match2 = createMatchPointer({
      id: '4',
      fragment: {
        name: 'ActorFragment'
      },
      variables: {},
      propName: 'actor',
      module: 'ActorContainer.react'
    });
    renderer.update(React.createElement(MatchContainer, {
      loader: loader,
      match: match2,
      props: {
        otherProp: 'hello!'
      },
      fallback: React.createElement(Fallback, null)
    }));
    expect(renderer.toJSON()).toMatchSnapshot();
    expect(loader).toBeCalledTimes(1);
    expect(UserComponent).toBeCalledTimes(0);
    expect(ActorComponent).toBeCalledTimes(1);
  });
  it('transitions to fallback when new props have a null component', function () {
    loader.mockReturnValue(React.memo(ActorComponent));
    var match = createMatchPointer({
      id: '4',
      fragment: {
        name: 'ActorFragment'
      },
      variables: {},
      propName: 'actor',
      module: 'ActorContainer.react'
    });
    var Fallback = jest.fn(function () {
      return React.createElement("div", null, "fallback");
    });
    var renderer = TestRenderer.create(React.createElement(MatchContainer, {
      loader: loader,
      match: match,
      props: {
        otherProp: 'hello!'
      },
      fallback: React.createElement(Fallback, null)
    }));
    expect(ActorComponent).toBeCalledTimes(1);
    renderer.update(React.createElement(MatchContainer, {
      loader: loader,
      match: {} // intentionally empty
      ,
      props: {
        otherProp: 'hello!'
      },
      fallback: React.createElement(Fallback, null)
    }));
    expect(renderer.toJSON()).toMatchSnapshot();
    expect(loader).toBeCalledTimes(1);
    expect(Fallback).toBeCalledTimes(1);
    expect(UserComponent).toBeCalledTimes(0);
  });
});