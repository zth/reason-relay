module TodoFragment = %relay(`
  fragment SingleTodo_todoItem on TodoItem {
    id
    text
    completed
  }
`)

module DeleteMutation = %relay(`
  mutation SingleTodoDeleteMutation(
    $input: DeleteTodoItemInput!
    $connections: [ID!]!
  ) @raw_response_type {
    deleteTodoItem(input: $input) {
      deletedTodoItemId @deleteEdge(connections: $connections)
    }
  }
`)

module UpdateMutation = %relay(`
  mutation SingleTodoUpdateMutation($input: UpdateTodoItemInput!) {
    updateTodoItem(input: $input) {
      updatedTodoItem {
        id
        text
        completed
      }
    }
  }
`)

@react.component
let make = (~checked, ~todoItem as todoItemRef, ~todosConnectionId) => {
  let todoItem = TodoFragment.use(todoItemRef)

  <li
    className={switch todoItem.completed {
    | Some(true) => "completed"
    | Some(false)
    | None => ""
    }}>
    <div className="form-check">
      <label className="form-check-label">
        <input
          className="checkbox"
          type_="checkbox"
          checked
          onChange={_ => {
            let completed = !Belt.Option.getWithDefault(todoItem.completed, false)
            UpdateMutation.commitMutation(
              ~environment=RelayEnv.environment,
              ~variables={
                input: {
                  clientMutationId: None,
                  id: todoItem.id,
                  completed: completed,
                  text: todoItem.text,
                },
              },
              ~optimisticResponse={
                updateTodoItem: Some({
                  updatedTodoItem: Some({
                    id: todoItem.id,
                    completed: Some(completed),
                    text: todoItem.text,
                  }),
                }),
              },
              (),
            ) |> ignore
          }}
        />
        {React.string(todoItem.text)}
      </label>
    </div>
    <i
      onClick={_ =>
        DeleteMutation.commitMutation(
          ~environment=RelayEnv.environment,
          ~variables={
            input: {
              clientMutationId: None,
              id: todoItem.id,
            },
            connections: [todosConnectionId],
          },
          ~optimisticResponse={
            deleteTodoItem: Some({deletedTodoItemId: Some(todoItem.id)}),
          },
          (),
        ) |> ignore}
      role="button"
      className="remove mdi mdi-close-circle-outline"
    />
  </li>
}
