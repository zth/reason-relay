/**
 * This file shows:
   1. Using queries as hooks
   2. Using fragments
   3. Making local updates to the store
 */
module Query = [%relay.query
  {|
  query TestQueryFragmentQuery {
    books {
      id
      ...TestQueryFragment_book
    }
  }
|}
];

module Fragment = [%relay.fragment
  {|
  fragment TestQueryFragment_book on Book {
    id
    title
    author
  }
|}
];

module BookViewer = {
  [@react.component]
  let make = (~book as bookRef) => {
    let book = Fragment.use(bookRef);
    let environment = ReasonRelay.useEnvironmentFromContext();
    let (title, setTitle) = React.useState(() => "New Title");

    <div>
      <h1> {book##title |> React.string} </h1>
      <p> {book##author |> React.string} </p>
      <p>
        <input
          type_="text"
          value=title
          onChange={e =>
            setTitle(_ => ReactEvent.Form.currentTarget(e)##value)
          }
          placeholder={book##title ++ " title"}
        />
        /***
         * This demonstrates updating the store locally only, without doing an actual mutation.
         *
         */
        <button
          onClick={_ =>
            ReasonRelay.commitLocalUpdate(
              ~environment,
              ~updater=store => {
                open ReasonRelay; // We open ReasonRelay to simplify working with RecordSourceSelectorProxy etc

                let bookNode =
                  store->RecordSourceSelectorProxy.get(
                    ~dataId=makeDataId(book##id),
                  );

                switch (bookNode) {
                | Some(bookNode) =>
                  let currentTitle =
                    bookNode->RecordProxy.getValueString(
                      ~name="title",
                      ~arguments=None,
                    );

                  switch (currentTitle) {
                  | Some(currentTitle) =>
                    bookNode
                    ->RecordProxy.setValueString(
                        ~name="title",
                        ~value=currentTitle ++ " " ++ title,
                        ~arguments=None,
                      )
                    ->ignore
                  | None => ()
                  };
                | None => ()
                };
              },
            )
          }>
          {React.string("Update " ++ book##title ++ " locally")}
        </button>
      </p>
    </div>;
  };
};

[@react.component]
let make = () => {
  let query = Query.use(~variables=(), ());

  query##books
  |> Array.map(book => <BookViewer key=book##id book />)
  |> React.array;
};