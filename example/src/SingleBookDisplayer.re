module Query = [%relay.query
  {|
    query SingleBookDisplayerQuery($bookId: ID!) {
        book(id: $bookId) {
            ...BookDisplayer_book
            ...BookEditor_book
        }
    }
|}
];

[@react.component]
let make = (~bookId) => {
  let query =
    Query.use(
      ~variables={"bookId": bookId},
      ~fetchPolicy=StoreOrNetwork,
      (),
    );

  switch (query##book |> Js.Nullable.toOption) {
  | Some(book) => <BookDisplayer book allowEditMode=true />
  | None => <div> {React.string("404")} </div>
  };
};