module CreateMutation = [%relay.mutation
  {|
  mutation CreateBookViewMutation($input: AddBookInput!) {
    addBook(input: $input) {
      book {
        title
        author
        shelf {
          name
        }
      }
    }
  }
|}
];

module Query = [%relay.query
  {|
  query CreateBookViewQuery {
    books {
      ...CreateBookViewExistingBookDisplayer_book
    }
  }
|}
];

type state = {
  title: string,
  author: string,
};

type action =
  | Reset
  | SetTitle(string)
  | SetAuthor(string);

let initialState = {title: "", author: ""};

let reducer = (state, action) =>
  switch (action) {
  | Reset => initialState
  | SetTitle(title) => {...state, title}
  | SetAuthor(author) => {...state, author}
  };

[@react.component]
let make = () => {
  let (addBook, addBookStatus) = CreateMutation.use();
  let query = Query.use(~variables=(), ~fetchPolicy=StoreAndNetwork, ());
  let (state, dispatch) = React.useReducer(reducer, initialState);

  <div>
    <h1> {React.string("Create book")} </h1>
    <p>
      <strong> {React.string("Title")} </strong>
      <p>
        <input
          type_="text"
          value={state.title}
          onChange={e =>
            dispatch(SetTitle(ReactEvent.Form.currentTarget(e)##value))
          }
        />
      </p>
    </p>
    <p>
      <strong> {React.string("Author")} </strong>
      <p>
        <input
          type_="text"
          value={state.author}
          onChange={e =>
            dispatch(SetAuthor(ReactEvent.Form.currentTarget(e)##value))
          }
        />
      </p>
    </p>
    <p>
      <button
        onClick={_ =>
          addBook(
            ~variables={
              "input": {
                "clientMutationId": None,
                "title": state.title,
                "author": state.author,
              },
            },
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
          |> Js.Promise.then_((res: ReasonRelay.mutationResult('a)) =>
               switch (res) {
               | Success(_) => Js.log("Yay!") |> Js.Promise.resolve
               | Error(err) => Js.log(err) |> Js.Promise.resolve
               }
             )
          |> ignore
        }>
        {React.string("Save")}
      </button>
      <p>
        {switch (addBookStatus) {
         | Loading => React.string("Saving...")
         | _ => React.null
         }}
      </p>
    </p>
    <p> <h3> {React.string("Existing books")} </h3> </p>
    <p>
      {query##books
       |> Array.mapi((index, book) =>
            <CreateBookViewExistingBookDisplayer
              book
              key={string_of_int(index)}
            />
          )
       |> React.array}
    </p>
  </div>;
};