let testQuery = `import type { ConcreteRequest } from 'relay-runtime';
import type { TodoApp_user$ref } from "./TodoApp_user.graphql";
export type appQueryVariables = {|
  userId?: ?string
|};
export type TestEnums = "mark" | "zuck" | "%future added value";
export type appQueryResponse = {|
  +user: ?{|
    +$fragmentRefs: TodoApp_user$ref & SomOtherComp_user$ref & TodoApp_user2$ref & TodoApp_user3$ref
  |},
  +optionalUser?: {|
    +firstName?: string,
  |},
  +nonOptUser: {|
    +firstName: string
  |},
  +someOtherUser: ?{|
    +firstName: string,
    +lastNames: $ReadOnlyArray<?{|
      +something: boolean
    |}>,
    +$fragmentRefs: SomOtherComp_user$ref
  |},
  +actor: ?({|
    +__typename: "Page",
    +id: string,
    +anotherUnion: {| 
      +__typename: "Page", 
      +id: "string" 
    |} | {| 
      +__typename: "User", 
      +id: "string" 
    |},
    +$fragmentRefs: PageFragment$ref,
  |} | {|
    +__typename: "User",
    +name: ?string,
  |} | {|
    // This will never be '%other', but we need some
    // value in case none of the concrete values match.
    +__typename: "%other"
  |})
|};

export type TodoApp_user = {|
  +firstName: string
|};

export type SomOtherComp_user = $ReadOnlyArray<{|
  +lastName: string
|}>;

export type appQuery = {|
  variables: appQueryVariables,
  response: appQueryResponse,
|};`;

module.exports = {
  testQuery
};