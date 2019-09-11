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

var _extends2 = _interopRequireDefault(require("@babel/runtime/helpers/extends"));

var React = require('react');

var useMemo = React.useMemo;
/**
 * Renders the results of a data-driven dependency fetched with the `@match`
 * directive. The `@match` directive can be used to specify a mapping of
 * result types to the containers used to render those types. The result
 * value is an opaque object that described which component was selected
 * and a reference to its data. Use <MatchContainer/> to render these
 * values.
 *
 * ## Example
 *
 * For example, consider a piece of media content that might be text or
 * an image, where for clients that don't support images the application
 * should fall back to rendering the image caption as text. @match can be
 * used to dynamically select whether to render a given media item as
 * an image or text (on the server) and then fetch the corresponding
 * React component and its data dependencies (information about the
 * image or about the text).
 *
 * ```
 * // Media.react.js
 *
 * // Define a React component that uses <MatchContainer> to render the
 * // results of a @module selection
 * function Media(props) {
 *   const {media, ...restPropsj} = props;
 *
 *   const loader = moduleReference => {
 *      // given the data returned by your server for the @module directive,
 *      // return the React component (or throw a Suspense promise if
 *      // it is loading asynchronously).
 *      todo_returnModuleOrThrowPromise(moduleReference);
 *   };
 *   return <MatchContainer match={media.mediaAttachment} props={restProps} />;
 * }
 *
 * module.exports = createSuspenseFragmentContainer(
 *   Media,
 *   {
 *     media: graphql`
 *       fragment Media_media on Media {
 *         # ...
 *         mediaAttachment @match {
 *            ...ImageContainer_image @module(name: "ImageContainer.react")
 *            ...TextContainer_text @module(name: "TextContainer.react")
 *         }
 *       }
 *     `
 *   },
 * );
 * ```
 *
 * ## API
 *
 * MatchContainer accepts the following props:
 * - `match`: The results (an opaque object) of a `@match` field.
 * - `props`: Props that should be passed through to the dynamically
 *   selected component. Note that any of the components listed in
 *   `@module()` could be selected, so all components should accept
 *   the value passed here.
 * - `loader`: A function to load a module given a reference (whatever
 *   your server returns for the `js(moduleName: String)` field).
 *
 */
// Note: this type is intentionally non-exact, it is expected that the
// object may contain sibling fields.

function MatchContainer(_ref2) {
  var _ref;

  var fallback = _ref2.fallback,
      loader = _ref2.loader,
      match = _ref2.match,
      props = _ref2.props;

  if (match != null && typeof match !== 'object') {
    throw new Error('MatchContainer: Expected `match` value to be an object or null/undefined.');
  } // NOTE: the MatchPointer type has a $fragmentRefs field to ensure that only
  // an object that contains a FragmentSpread can be passed. If the fragment
  // spread matches, then the metadata fields below (__id, __fragments, etc)
  // will be present. But they can be missing if all the fragment spreads use
  // @module and none of the types matched. The cast here is necessary because
  // fragment Flow types don't describe metadata fields, only the actual schema
  // fields the developer selected.


  var _ref3 = (_ref = match) !== null && _ref !== void 0 ? _ref : {},
      __id = _ref3.__id,
      __fragments = _ref3.__fragments,
      __fragmentOwner = _ref3.__fragmentOwner,
      __fragmentPropName = _ref3.__fragmentPropName,
      __module_component = _ref3.__module_component;

  if (__fragmentOwner != null && typeof __fragmentOwner !== 'object' || __fragmentPropName != null && typeof __fragmentPropName !== 'string' || __fragments != null && typeof __fragments !== 'object' || __id != null && typeof __id !== 'string') {
    throw new Error("MatchContainer: Invalid 'match' value, expected an object that has a " + "'...SomeFragment' spread.");
  }

  var LoadedContainer = __module_component != null ? loader(__module_component) : null;
  var fragmentProps = useMemo(function () {
    // TODO: Perform this transformation in RelayReader so that unchanged
    // output of subscriptions already has a stable identity.
    if (__fragmentPropName != null && __id != null && __fragments != null) {
      var fragProps = {};
      fragProps[__fragmentPropName] = {
        __id: __id,
        __fragments: __fragments,
        __fragmentOwner: __fragmentOwner
      };
      return fragProps;
    }

    return null;
  }, [__id, __fragments, __fragmentOwner, __fragmentPropName]);

  if (LoadedContainer != null && fragmentProps != null) {
    return React.createElement(LoadedContainer, (0, _extends2["default"])({}, props, fragmentProps));
  } else {
    var _fallback;

    return (_fallback = fallback) !== null && _fallback !== void 0 ? _fallback : null;
  }
}

module.exports = MatchContainer;