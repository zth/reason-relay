/* @generated */
%%raw("/* @generated */")
module Types = {
  @@ocaml.warning("-30")
  
  type rec fragment_membersConnection = {
    edges: option<array<option<fragment_membersConnection_edges>>>,
  }
   and fragment_membersConnection_edges = {
    node: option<fragment_membersConnection_edges_node>,
  }
   and fragment_membersConnection_edges_node = {
    id: string,
    fullName: string,
    fragmentRefs: RescriptRelay.fragmentRefs<[ | #Avatar_user]>
  }
  
  
  type fragment = {
    name: string,
    membersConnection: option<fragment_membersConnection>,
    id: string,
  }
}

module Internal = {
  type fragmentRaw
  let fragmentConverter: 
    Js.Dict.t<Js.Dict.t<Js.Dict.t<string>>> = 
    %raw(
      json`{"__root":{"membersConnection_edges":{"n":"","na":""},"membersConnection":{"n":""},"membersConnection_edges_node":{"f":"","n":""}}}`
    )
  
  let fragmentConverterMap = ()
  let convertFragment = v => v->RescriptRelay.convertObj(
    fragmentConverter, 
    fragmentConverterMap, 
    Js.undefined
  )
}
type t
type fragmentRef
external getFragmentRef:
  RescriptRelay.fragmentRefs<[> | #SingleTicketWorkingGroup_workingGroup]> => fragmentRef = "%identity"


module Utils = {

}
type relayOperationNode
type operationType = RescriptRelay.fragmentNode<relayOperationNode>


%%private(let makeNode = (node_SingleTicketWorkingGroupRefetchQuery): operationType => {
  ignore(node_SingleTicketWorkingGroupRefetchQuery)
  %raw(json` (function(){
var v0 = {
  "alias": null,
  "args": null,
  "kind": "ScalarField",
  "name": "id",
  "storageKey": null
};
return {
  "argumentDefinitions": [
    {
      "defaultValue": false,
      "kind": "LocalArgument",
      "name": "includeMembers"
    }
  ],
  "kind": "Fragment",
  "metadata": {
    "refetch": {
      "connection": null,
      "fragmentPathInResult": [
        "node"
      ],
      "operation": node_SingleTicketWorkingGroupRefetchQuery,
      "identifierField": "id"
    }
  },
  "name": "SingleTicketWorkingGroup_workingGroup",
  "selections": [
    {
      "alias": null,
      "args": null,
      "kind": "ScalarField",
      "name": "name",
      "storageKey": null
    },
    (v0/*: any*/),
    {
      "condition": "includeMembers",
      "kind": "Condition",
      "passingValue": true,
      "selections": [
        {
          "alias": null,
          "args": null,
          "concreteType": "UserConnection",
          "kind": "LinkedField",
          "name": "membersConnection",
          "plural": false,
          "selections": [
            {
              "alias": null,
              "args": null,
              "concreteType": "UserEdge",
              "kind": "LinkedField",
              "name": "edges",
              "plural": true,
              "selections": [
                {
                  "alias": null,
                  "args": null,
                  "concreteType": "User",
                  "kind": "LinkedField",
                  "name": "node",
                  "plural": false,
                  "selections": [
                    (v0/*: any*/),
                    {
                      "alias": null,
                      "args": null,
                      "kind": "ScalarField",
                      "name": "fullName",
                      "storageKey": null
                    },
                    {
                      "args": null,
                      "kind": "FragmentSpread",
                      "name": "Avatar_user"
                    }
                  ],
                  "storageKey": null
                }
              ],
              "storageKey": null
            }
          ],
          "storageKey": null
        }
      ]
    }
  ],
  "type": "WorkingGroup",
  "abstractKey": null
};
})() `)
})
let node: operationType = makeNode(SingleTicketWorkingGroupRefetchQuery_graphql.node)


