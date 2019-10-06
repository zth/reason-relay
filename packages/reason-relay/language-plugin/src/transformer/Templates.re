open Printer;

let fragmentRefTemplate = fragmentName => {
  let fref = fragmentName |> getFragmentRefName;
  {j|
type t;
type fragmentRef;
type fragmentRefSelector('a) = {.. "$fref": t} as 'a;
external getFragmentRef: fragmentRefSelector('a) => fragmentRef = "%identity";
|j};
};