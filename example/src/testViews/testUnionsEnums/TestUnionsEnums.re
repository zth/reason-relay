module Query = [%relay.query
  {|
  query TestUnionsEnumsQuery($bookStatus: BookStatus!, $shelfId: ID!) {
    books(status: $bookStatus) {
      id
      title
      status
    }

    fromShelf(shelfId: $shelfId) {
      __typename
      ... on Book {
        id
        title
        status
      }

      ... on BookCollection {
        id
        books {
          title
          status
        }
      }
    }
  }
|}
];

[@react.component]
let make = () => {
  let query =
    Query.use(
      ~variables={
        "shelfId": "123",
        "bookStatus": SchemaAssets.Enum_BookStatus.wrap(`Discontinued),
      },
      (),
    );

  <div>
    <h2> {React.string("Books")} </h2>
    {query##books
     |> Array.mapi((index, book) =>
          <div key={string_of_int(index)}>
            {React.string(book##title)}
            <div>
              {React.string(
                 switch (book##status |> Js.Nullable.toOption) {
                 | Some(status) =>
                   switch (status |> SchemaAssets.Enum_BookStatus.unwrap) {
                   | `Discontinued => "Not used anymore"
                   | `Draft => "Undergoing work"
                   | `Published => "Here!"
                   | `FUTURE_ADDED_VALUE__ => ""
                   }
                 | None => ""
                 },
               )}
            </div>
          </div>
        )
     |> React.array}
    <h2> {React.string("From shelf")} </h2>
    {switch (query##fromShelf |> Js.Nullable.toOption) {
     | Some(fromShelf) =>
       fromShelf
       |> Array.mapi((index, item) =>
            <div key={string_of_int(index)}>
              {switch (item |> Query.Union_fromShelf.unwrap) {
               | `Book(book) =>
                 <p>
                   <strong> {React.string("Book: " ++ book##title)} </strong>
                 </p>
               | `BookCollection(bookCollection) =>
                 <p>
                   <strong>
                     {React.string(
                        "Collection size: "
                        ++ string_of_int(
                             Array.length(
                               switch (
                                 bookCollection##books |> Js.Nullable.toOption
                               ) {
                               | Some(books) => books
                               | None => [||]
                               },
                             ),
                           ),
                      )}
                   </strong>
                 </p>
               | `UnmappedUnionMember => <p> {React.string("Unknown...")} </p>
               }}
            </div>
          )
       |> React.array

     | None => React.null
     }}
  </div>;
};