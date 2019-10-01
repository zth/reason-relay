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

import { GraphQLUnionType } from 'graphql';

const {
  transformInputType,
  transformScalarType
} = require('./RelayReasonTypeTransformers');

import type { GraphQLEnumType } from 'graphql';
// $FlowFixMe
import { TypeGeneratorOptions } from 'relay-compiler';
// $FlowFixMe
import type { Root } from 'relay-compiler/lib/core/GraphQLIR';
// $FlowFixMe
import type { IRTransform } from 'relay-compiler/core/GraphQLIRTransforms';
import * as FlattenTransform from 'relay-compiler/lib/transforms/FlattenTransform';
import * as IRVisitor from 'relay-compiler/lib/core/GraphQLIRVisitor';
import * as RelayMaskTransform from 'relay-compiler/lib/transforms/RelayMaskTransform';
import * as RelayMatchTransform from 'relay-compiler/lib/transforms/RelayMatchTransform';
import * as RelayRefetchableFragmentTransform from 'relay-compiler/lib/transforms/RelayRefetchableFragmentTransform';
import * as RelayRelayDirectiveTransform from 'relay-compiler/lib/transforms/RelayRelayDirectiveTransform';
import {
  makeFragment,
  makeFragmentRefProp,
  makeInputObject,
  makeObj,
  makeObjProp,
  makeOperation,
  makePluralFragment,
  makeStandaloneObjectType,
  makePropUnion,
  makePropValue,
  makeVariables
} from './generator/Generators.gen';
import {
  getFragmentRefName,
  getInputTypeName,
  makeRootType
} from './generator/Printer.gen';
import type { propValues } from './generator/Types.gen';
import type { SelectionMap, Selection } from './RelayTypeUtils';
const { GraphQLString, isAbstractType } = require('graphql');
const invariant = require('invariant');
const nullthrows = require('nullthrows');
const {
  mergeSelection,
  mergeSelections,
  isPlural,
  flattenArray,
  isTypenameSelection,
  hasTypenameSelection,
  onlySelectsTypename
} = require('./RelayTypeUtils');

export type State = {|
  ...TypeGeneratorOptions,
  +generatedFragments: Set<string>,
  +generatedInputObjectTypes: {
    [name: string]: Array<propValues> | 'pending'
  },
  +usedEnums: { [name: string]: GraphQLEnumType },
  +usedFragments: Set<string>,
  +usedUnions: { [identifier: string]: GraphQLUnionType }
|};

function generate(node: Root, options: TypeGeneratorOptions): string {
  return IRVisitor.visit(node, createVisitor(options));
}

function makeProp(
  selection: Selection,
  state: State,
  unmasked: boolean,
  concreteType?: string
): propValues {
  const {
    key,
    schemaName,
    value,
    conditional,
    nodeType,
    nodeSelections
  } = selection;
  if (key === '$fragmentRefs' && value) {
    return makeFragmentRefProp(value);
  }

  if (nodeType) {
    return makeObjProp({
      name: key,
      propValue: transformScalarType(
        nodeType,
        state,
        selectionsToReason(
          [Array.from(nullthrows(nodeSelections).values())],
          state,
          unmasked,
          key
        ),
        conditional,
        key
      )
    });
  }

  if (value) {
    return makeObjProp({
      name: key,
      propValue: value
    });
  }

  throw new Error('Something went horribly wrong!');
}

function selectionsToReason(
  selections,
  state: State,
  unmasked: boolean,
  parentKey?: string
): Array<propValues> {
  const baseFields = new Map();
  const byConcreteType = {};

  flattenArray(
    // $FlowFixMe
    selections
  ).forEach(selection => {
    const { concreteType } = selection;
    if (concreteType) {
      byConcreteType[concreteType] = byConcreteType[concreteType] ?? [];
      byConcreteType[concreteType].push(selection);
    } else {
      const previousSel = baseFields.get(selection.key);

      baseFields.set(
        selection.key,
        previousSel ? mergeSelection(selection, previousSel) : selection
      );
    }
  });

  let types: Array<Array<propValues>> = [];

  if (
    Object.keys(byConcreteType).length &&
    onlySelectsTypename(Array.from(baseFields.values())) &&
    (hasTypenameSelection(Array.from(baseFields.values())) ||
      Object.keys(byConcreteType).every(type =>
        hasTypenameSelection(byConcreteType[type])
      ))
  ) {
    const typenameAliases = new Set();
    for (const concreteType in byConcreteType) {
      types.push(
        groupRefs([
          ...Array.from(baseFields.values()),
          ...byConcreteType[concreteType]
        ]).map(selection => {
          if (selection.schemaName === '__typename') {
            typenameAliases.add(selection.key);
          }
          return makeProp(selection, state, unmasked, concreteType);
        })
      );
    }
  } else {
    let selectionMap = selectionsToMap(Array.from(baseFields.values()));
    for (const concreteType in byConcreteType) {
      selectionMap = mergeSelections(
        selectionMap,
        selectionsToMap(
          byConcreteType[concreteType].map(sel => ({
            ...sel,
            conditional: true
          }))
        )
      );
    }
    const selectionMapValues = groupRefs(Array.from(selectionMap.values())).map(
      sel =>
        isTypenameSelection(sel) && sel.concreteType
          ? makeProp(
              { ...sel, conditional: false },
              state,
              unmasked,
              sel.concreteType
            )
          : makeProp(sel, state, unmasked)
    );
    types.push(selectionMapValues);
  }

  /**
   * If there's more than 1 concrete type this is a union,
   * and unions need to be treated differently since Reason
   * does not support multiple types/shapes on a
   * single prop like JS/Flow/TS does.
   *
   * So, we turn unions into an abstract "wrapped" type, and
   * generate utils for unwrapping that to a more idiomatic
   * Reason type (polymorphic variants in this case).
   */
  if (parentKey && Object.keys(byConcreteType).length > 1) {
    invariant(
      selections[0].find(({ schemaName }) => schemaName === '__typename'),
      `You must include the __typename field for every union you use. Failed for union on field: "${parentKey}".`
    );

    let unionKeyName = null;

    /**
     * Pretty unlikely, but if there's more than one field in
     * the given GraphQL string that returns the same union
     * we'll need to add something to the name to make them
     * not clash. This should hopefully happen extremely
     * rarely.
     */
    if (state.usedUnions[parentKey]) {
      for (let i = 1; i <= 100; i += 1) {
        let theName = parentKey + '_' + i.toString();

        if (!state.usedUnions[theName]) {
          state.usedUnions[theName] = byConcreteType;
          unionKeyName = theName;
          break;
        }
      }
    } else {
      state.usedUnions[parentKey] = byConcreteType;
      unionKeyName = parentKey;
    }

    if (!unionKeyName) {
      throw new Error('Could not deconstruct union "' + parentKey + '".');
    }

    return [
      makeObjProp({
        name: parentKey,
        propValue: makePropValue({
          nullable: true,
          propType: makePropUnion(unionKeyName)
        })
      })
    ];
  }

  return flattenArray(types);
}

function generateUnions(state: State) {
  return `module Unions { 
  ${Object.keys(state.usedUnions)
    .map(unionName => {
      const union = state.usedUnions[unionName];
      return `module Union_${unionName} = {
      type wrapped;
      
      external __unwrap_union: wrapped => {. "__typename": string } = "%identity";      
      ${Object.keys(union)
        .map(
          typeName =>
            `type type_${typeName} = ${makeStandaloneObjectType(
              makeObj(selectionsToReason([union[typeName]], state, false))
            )};`
        )
        .join('\n')}
      ${Object.keys(union)
        .map(
          typeName =>
            `external __unwrap_${typeName}: wrapped => type_${typeName} = "%identity";`
        )
        .join('\n')}
      
      type t = [ ${Object.keys(union)
        .map(typeName => ` | \`${typeName}(type_${typeName}) `)
        .join('\n')} | \`UnmappedUnionMember];
      
      let unwrap = wrapped => {
        let unwrappedUnion = wrapped |> __unwrap_union;
        switch (unwrappedUnion##__typename) {
          ${Object.keys(union)
            .map(
              typeName =>
                `| "${typeName}" => \`${typeName}(wrapped |> __unwrap_${typeName})`
            )
            .join('\n')}
          | _ => \`UnmappedUnionMember
        };
      };
    };`;
    })
    .join('\n')}
    };
    ${Object.keys(state.usedUnions).length > 0 ? 'open Unions;' : ''}`;
}

function createVisitor(options: TypeGeneratorOptions) {
  const state: State = {
    customScalars: options.customScalars,
    enumsHasteModule: options.enumsHasteModule,
    existingFragmentNames: options.existingFragmentNames,
    generatedFragments: new Set(),
    generatedInputObjectTypes: {},
    optionalInputFields: options.optionalInputFields,
    usedEnums: {},
    usedUnions: {},
    usedFragments: new Set(),
    useHaste: options.useHaste,
    useSingleArtifactDirectory: options.useSingleArtifactDirectory,
    noFutureProofEnums: options.noFutureProofEnums
  };
  return {
    leave: {
      Root(node): string {
        const inputVariablesType = generateInputVariablesType(node, state);
        const inputObjectTypes = generateInputObjectTypes(state);

        const responseTypeSelections = selectionsToReason(
          node.selections,
          state,
          false
        );

        /**
         * Ugly, should be replaced with something smarter.
         * But it's about making the type definitions recursive by chaining
         * them with `and`.
         */

        const inp = [
          inputObjectTypes[0],
          ...inputObjectTypes.slice(1).map(type => type.replace('type ', ''))
        ]
          .filter(Boolean)
          .map(type => type.replace(';', ''))
          .join(' and ');

        let returnStr = '';

        returnStr += generateUnions(state);
        returnStr += inp;
        returnStr += inp ? ';' : ''; // Closing ; if there's input objects
        returnStr += inputVariablesType;
        returnStr += makeRootType(
          makeOperation(makeObj(responseTypeSelections))
        );

        return returnStr;
      },
      /**
       * @return {string}
       */
      Fragment(node) {
        let selections = flattenArray(
          // $FlowFixMe
          node.selections
        );
        const numConecreteSelections = selections.filter(s => s.concreteType)
          .length;
        selections = selections.map(selection => {
          if (
            numConecreteSelections <= 1 &&
            isTypenameSelection(selection) &&
            !isAbstractType(node.type)
          ) {
            return [
              {
                ...selection,
                concreteType: node.type.toString()
              }
            ];
          }
          return [selection];
        });
        state.generatedFragments.add(node.name);

        const unmasked = node.metadata && node.metadata.mask === false;
        const baseType = selectionsToReason(
          selections,
          state,
          // $FlowFixMe
          unmasked
        );
        const type = makeObj(baseType);

        let returnStr = '';
        returnStr += generateUnions(state);
        returnStr += makeRootType(
          isPlural(node) ? makePluralFragment(type) : makeFragment(type)
        );

        let fragmentRefSelectorType = `{.. "${getFragmentRefName(
          node.name
        )}": t } as 'a`;

        returnStr += `
        type t;
        type fragmentRef;
        type fragmentRefSelector('a) = ${
          isPlural(node)
            ? `array(${fragmentRefSelectorType})`
            : fragmentRefSelectorType
        };
        external getFragmentRef: fragmentRefSelector('a) => fragmentRef = "%identity";
        `;

        return returnStr;
      },
      InlineFragment(node) {
        const typeCondition = node.typeCondition;
        return flattenArray(
          // $FlowFixMe
          node.selections
        ).map(typeSelection => {
          return isAbstractType(typeCondition)
            ? {
                ...typeSelection,
                conditional: true
              }
            : {
                ...typeSelection,
                concreteType: typeCondition.toString()
              };
        });
      },
      Condition(node) {
        return flattenArray(
          // $FlowFixMe
          node.selections
        ).map(selection => {
          return {
            ...selection,
            conditional: true
          };
        });
      },
      ScalarField(node) {
        return [
          {
            key: node.alias ?? node.name,
            schemaName: node.name,
            value: transformScalarType(node.type, state)
          }
        ];
      },
      LinkedField(node) {
        return [
          {
            key: node.alias ?? node.name,
            schemaName: node.name,
            nodeType: node.type,
            nodeSelections: selectionsToMap(
              flattenArray(
                // $FlowFixMe
                node.selections
              ),
              /*
               * append concreteType to key so overlapping fields with different
               * concreteTypes don't get overwritten by each other
               */
              true
            )
          }
        ];
      },
      ModuleImport(node) {
        return [
          {
            key: '__fragmentPropName',
            conditional: true,
            value: transformScalarType(GraphQLString, state)
          },
          {
            key: '__module_component',
            conditional: true,
            value: transformScalarType(GraphQLString, state)
          },
          {
            key: '__fragments_' + node.name,
            ref: node.name
          }
        ];
      },
      FragmentSpread(node) {
        state.usedFragments.add(node.name);
        return [
          {
            key: '__fragments_' + node.name,
            ref: node.name
          }
        ];
      }
    }
  };
}

function selectionsToMap(
  selections: $ReadOnlyArray<Selection>,
  appendType?: boolean
): SelectionMap {
  const map = new Map();
  selections.forEach(selection => {
    const key =
      appendType && selection.concreteType
        ? `${selection.key}::${selection.concreteType}`
        : selection.key;
    const previousSel = map.get(key);
    map.set(
      key,
      previousSel ? mergeSelection(previousSel, selection) : selection
    );
  });
  return map;
}

function generateInputObjectTypes(state: State) {
  return Object.keys(state.generatedInputObjectTypes).map(typeIdentifier => {
    const inputObjectType = state.generatedInputObjectTypes[typeIdentifier];
    invariant(
      typeof inputObjectType !== 'string',
      'RelayCompilerFlowGenerator: Expected input object type to have been' +
        ' defined before calling `generateInputObjectTypes`'
    );

    return makeRootType(
      makeInputObject({
        name: getInputTypeName(typeIdentifier),
        definition: makeObj(inputObjectType)
      })
    );
  });
}

function generateInputVariablesType(node: Root, state: State) {
  node.argumentDefinitions.forEach(arg => {
    transformInputType(arg.type, state);
  });

  return makeRootType(
    makeVariables(
      makeObj(
        node.argumentDefinitions.map(arg =>
          makeObjProp({
            name: arg.name,
            propValue: transformInputType(arg.type, state)
          })
        )
      )
    )
  );
}

function groupRefs(props): Array<Selection> {
  const result = [];
  const refs = [];
  props.forEach(prop => {
    if (prop.ref) {
      refs.push(prop.ref);
    } else {
      result.push(prop);
    }
  });
  if (refs.length > 0) {
    refs.forEach(ref => {
      result.push({
        key: '$fragmentRefs',
        conditional: false,
        value: ref
      });
    });
  }
  return result;
}

const FLOW_TRANSFORMS: Array<IRTransform> = [
  RelayRelayDirectiveTransform.transform,
  RelayMaskTransform.transform,
  RelayMatchTransform.transform,
  FlattenTransform.transformWithOptions({}),
  RelayRefetchableFragmentTransform.transform
];

module.exports = {
  generate,
  transforms: FLOW_TRANSFORMS
};
