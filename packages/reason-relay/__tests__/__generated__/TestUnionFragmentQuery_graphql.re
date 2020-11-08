/* @generated */

module Types = {
  [@ocaml.warning "-30"];
  type response_member = {
    __typename: string,
    fragmentRefs:
      ReasonRelay.fragmentRefs(
        [ | `TestUnionFragment_member | `TestUnionFragment_plural_member],
      ),
  };

  type response = {member: option(response_member)};
  type rawResponse = response;
  type variables = unit;
};

module Internal = {
  type wrapResponseRaw;
  let wrapResponseConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"member":{"n":"","f":""}}} |json}
  ];
  let wrapResponseConverterMap = ();
  let convertWrapResponse = v =>
    v
    ->ReasonRelay.convertObj(
        wrapResponseConverter,
        wrapResponseConverterMap,
        Js.null,
      );

  type responseRaw;
  let responseConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {"__root":{"member":{"n":"","f":""}}} |json}
  ];
  let responseConverterMap = ();
  let convertResponse = v =>
    v
    ->ReasonRelay.convertObj(
        responseConverter,
        responseConverterMap,
        Js.undefined,
      );

  type wrapRawResponseRaw = wrapResponseRaw;
  let convertWrapRawResponse = convertWrapResponse;

  type rawResponseRaw = responseRaw;
  let convertRawResponse = convertResponse;

  let variablesConverter: Js.Dict.t(Js.Dict.t(Js.Dict.t(string))) = [%raw
    {json| {} |json}
  ];
  let variablesConverterMap = ();
  let convertVariables = v =>
    v
    ->ReasonRelay.convertObj(
        variablesConverter,
        variablesConverterMap,
        Js.undefined,
      );
};

type preloadedQuery;
type queryRef = ReasonRelay.queryRef(preloadedQuery);

module Utils = {};

type relayOperationNode;

type operationType = ReasonRelay.queryNode(relayOperationNode);

let node: operationType = [%raw
  {json| (function(){
var v0 = [
  {
    "kind": "Literal",
    "name": "id",
    "value": "123"
  }
],
v1 = {
  "alias": null,
  "args": null,
  "kind": "ScalarField",
  "name": "__typename",
  "storageKey": null
};
return {
  "fragment": {
    "argumentDefinitions": [],
    "kind": "Fragment",
    "metadata": null,
    "name": "TestUnionFragmentQuery",
    "selections": [
      {
        "alias": null,
        "args": (v0/*: any*/),
        "concreteType": null,
        "kind": "LinkedField",
        "name": "member",
        "plural": false,
        "selections": [
          (v1/*: any*/),
          {
            "args": null,
            "kind": "FragmentSpread",
            "name": "TestUnionFragment_member"
          },
          {
            "args": null,
            "kind": "FragmentSpread",
            "name": "TestUnionFragment_plural_member"
          }
        ],
        "storageKey": "member(id:\"123\")"
      }
    ],
    "type": "Query",
    "abstractKey": null
  },
  "kind": "Request",
  "operation": {
    "argumentDefinitions": [],
    "kind": "Operation",
    "name": "TestUnionFragmentQuery",
    "selections": [
      {
        "alias": null,
        "args": (v0/*: any*/),
        "concreteType": null,
        "kind": "LinkedField",
        "name": "member",
        "plural": false,
        "selections": [
          (v1/*: any*/),
          {
            "kind": "TypeDiscriminator",
            "abstractKey": "__isMember"
          },
          {
            "kind": "InlineFragment",
            "selections": [
              {
                "alias": null,
                "args": null,
                "kind": "ScalarField",
                "name": "firstName",
                "storageKey": null
              },
              {
                "alias": null,
                "args": null,
                "kind": "ScalarField",
                "name": "onlineStatus",
                "storageKey": null
              }
            ],
            "type": "User",
            "abstractKey": null
          },
          {
            "kind": "InlineFragment",
            "selections": [
              {
                "alias": null,
                "args": null,
                "kind": "ScalarField",
                "name": "name",
                "storageKey": null
              }
            ],
            "type": "Group",
            "abstractKey": null
          },
          {
            "kind": "InlineFragment",
            "selections": [
              {
                "alias": null,
                "args": null,
                "kind": "ScalarField",
                "name": "id",
                "storageKey": null
              }
            ],
            "type": "Node",
            "abstractKey": "__isNode"
          }
        ],
        "storageKey": "member(id:\"123\")"
      }
    ]
  },
  "params": {
    "cacheID": "0d7eadaa771e8f878dc640c17cfc6714",
    "id": null,
    "metadata": {},
    "name": "TestUnionFragmentQuery",
    "operationKind": "query",
    "text": "query TestUnionFragmentQuery {\n  member(id: \"123\") {\n    __typename\n    ...TestUnionFragment_member\n    ...TestUnionFragment_plural_member\n    ... on Node {\n      __isNode: __typename\n      id\n    }\n  }\n}\n\nfragment TestUnionFragment_member on Member {\n  __isMember: __typename\n  __typename\n  ... on User {\n    firstName\n    onlineStatus\n  }\n  ... on Group {\n    name\n  }\n}\n\nfragment TestUnionFragment_plural_member on Member {\n  __isMember: __typename\n  __typename\n  ... on User {\n    firstName\n    onlineStatus\n  }\n  ... on Group {\n    name\n  }\n}\n"
  }
};
})() |json}
];

include ReasonRelay.MakeLoadQuery({
  type variables = Types.variables;
  type loadedQueryRef = queryRef;
  type response = Types.response;
  type node = relayOperationNode;
  let query = node;
  let convertVariables = Internal.convertVariables;
});

type entryPoint;

module EntryPoint: {
  type t;
  let make: (~variables: Types.varaibles) => t;
} = {
  type t = ReasonRelay.entryPoint(operationType, Types.variables);
  let make = (~variables) => {parameters: node, variables};
};
