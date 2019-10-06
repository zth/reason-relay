let testQuery = `import type { ConcreteRequest } from 'relay-runtime';
import type { TodoApp_user$ref } from "./TodoApp_user.graphql";
export type appQueryVariables = {|
  userId?: ?string
|};
export type appQueryResponse = {|
  +user: ?{|
    +$fragmentRefs: TodoApp_user$ref
  |}
|};
export type appQuery = {|
  variables: appQueryVariables,
  response: appQueryResponse,
|};`;

let testFragment = `import type { ReaderFragment } from 'relay-runtime';
import type { TodoListFooter_user$ref } from "./TodoListFooter_user.graphql";
import type { TodoList_user$ref } from "./TodoList_user.graphql";
import type { FragmentReference } from "relay-runtime";
declare export opaque type TodoApp_user$ref: FragmentReference;
declare export opaque type TodoApp_user$fragmentType: TodoApp_user$ref;
export type TodoApp_user = {|
  +id: string,
  +userId: string,
  +totalCount: number,
  +$fragmentRefs: TodoListFooter_user$ref & TodoList_user$ref,
  +$refType: TodoApp_user$ref,
|};
export type TodoApp_user$data = TodoApp_user;
export type TodoApp_user$key = {
  +$data?: TodoApp_user$data,
  +$fragmentRefs: TodoApp_user$ref,
};`;

let testMutation = `import type { ConcreteRequest } from 'relay-runtime';
export type AddTodoInput = {|
  text: string,
  userId: string,
  clientMutationId?: ?string,
|};
export type AddTodoMutationVariables = {|
  input: AddTodoInput
|};
export type AddTodoMutationResponse = {|
  +addTodo: ?{|
    +todoEdge: {|
      +__typename: string,
      +cursor: string,
      +node: ?{|
        +complete: boolean,
        +id: string,
        +text: string,
      |},
    |},
    +user: {|
      +id: string,
      +totalCount: number,
    |},
  |}
|};
export type AddTodoMutation = {|
  variables: AddTodoMutationVariables,
  response: AddTodoMutationResponse,
|};`;

module.exports = {
  testQuery,
  testFragment,
  testMutation
};
