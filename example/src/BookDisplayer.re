module BookFragment = [%relay.fragment
  {|
    fragment BookDisplayer_book on Book @refetchable(queryName: "BookDisplayerRefetchQuery") {
      ...BookEditor_book
      title
      author
      shelf {
        ...ShelfDisplayer_shelf
      }
    }
  |}
];

[@react.component]
let make = (~book as bookRef, ~allowEditMode: bool=false) => {
  let book = BookFragment.use(bookRef);
  let (editMode, setEditMode) = React.useState(() => false);

  <div>
    <h1> {React.string(book##title ++ " " ++ book##author)} </h1>
    <ShelfDisplayer shelf=book##shelf />
    {allowEditMode
       ? <p>
           <button onClick={_ => setEditMode(current => !current)}>
             {React.string(editMode ? "Stop editing" : "Start editing")}
           </button>
         </p>
       : React.null}
    <p> {editMode && allowEditMode ? <BookEditor book /> : React.null} </p>
  </div>;
};