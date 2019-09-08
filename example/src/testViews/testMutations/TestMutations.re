/**
 * This file demonstrates mutations, both using the standard
 * commitMutation and using hooks.
 *
 * It also demonstrates optimistic updates (the "provide-your-response"
 * kind) and a semi-complex update of the store after a mutation (adding
 * the added book to an existing lists of books).
 */
module AddMutation = [%relay.mutation
  {|
  mutation TestMutationsAddBookMutation($input: AddBookInput!) {
    addBook(input: $input) {
      book {
        id
        title
        author
        status
      }
    }
  }
|}
];

module UpdateMutation = [%relay.mutation
  {|
  mutation TestMutationsUpdateBookMutation($input: UpdateBookInput!) {
    updateBook(input: $input) {
      book {
        id
        title
        author
        status
      }
    }
  }
|}
];

module DeleteMutation = [%relay.mutation
  {|
  mutation TestMutationsDeleteBookMutation($input: AddBookInput!) {
    addBook(input: $input) {
      book {
        id
        title
        author
        status
      }
    }
  }
|}
];

module Query = [%relay.query
  {|
  query TestMutationsQuery {
    books {
      id
      title
      author
      status
    }
  }
|}
];

[@react.component]
let make = () => {
  let environment = ReasonRelay.useEnvironmentFromContext();
  let query = Query.use(~variables=(), ());
  let (updateBook, updateBookStatus) = UpdateMutation.use();

  <>
    {query##books
     |> Array.map(book =>
          <div key=book##id>
            <p> {React.string(book##title)} </p>
            <p> {React.string(book##author)} </p>
            <button
              type_="button"
              onClick={_ =>
                updateBook(
                  ~variables={
                    "input": {
                      "clientMutationId": None,
                      "id": book##id,
                      "author": "New author",
                      "title": book##title,
                      "status": book##status |> Js.Nullable.toOption,
                    },
                  },
                  ~optimisticResponse={
                    "updateBook": {
                      "book":
                        Some({
                          "id": book##id,
                          "title": book##title,
                          "author": "New author",
                          "status": book##status,
                        })
                        |> Js.Nullable.fromOption,
                    },
                  },
                  (),
                )
                |> ignore
              }>
              {React.string("Update " ++ book##title ++ " optimistic")}
            </button>
          </div>
        )
     |> React.array}
    {switch (updateBookStatus) {
     | Loading => <p> {React.string("Doing mutation...")} </p>
     | _ => React.null
     }}
    <button
      onClick={_ =>
        AddMutation.commitMutation(
          ~variables={
            "input": {
              "clientMutationId": None,
              "title": "New book",
              "author": "Some author",
            },
          },
          ~environment,
          ~updater=
            store => {
              open ReasonRelay;

              let mutationRes =
                store->RecordSourceSelectorProxy.getRootField(
                  ~fieldName="addBook",
                );

              let rootNode = store->RecordSourceSelectorProxy.getRoot;
              let rootBooks =
                rootNode->RecordProxy.getLinkedRecords(
                  ~name="books",
                  ~arguments=None,
                );

              let addedBook =
                switch (mutationRes) {
                | Some(res) =>
                  res->RecordProxy.getLinkedRecord(
                    ~name="book",
                    ~arguments=None,
                  )
                | None => None
                };

              switch (rootBooks, addedBook) {
              | (Some(rootBooks), Some(addedBook)) =>
                rootNode
                ->RecordProxy.setLinkedRecords(
                    ~name="books",
                    ~records=Array.append(rootBooks, [|Some(addedBook)|]),
                    ~arguments=None,
                  )
                ->ignore
              | (None, Some(addedBook)) =>
                rootNode
                ->RecordProxy.setLinkedRecords(
                    ~name="books",
                    ~records=[|Some(addedBook)|],
                    ~arguments=None,
                  )
                ->ignore
              | _ => ()
              };

              ();
            },
          (),
        )
        |> ignore
      }>
      {React.string("Add book by Commit mutation")}
    </button>
  </>;
};