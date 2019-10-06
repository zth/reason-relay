[@bs.module "./testStrings"] external testQuery: string = "testQuery";
[@bs.module "./testStrings"] external testFragment: string = "testFragment";
[@bs.module "./testStrings"] external testMutation: string = "testMutation";

TypesTransformer.printFromFlowTypes(
  ~content=testQuery, ~operationType=Query("appQuery"), ~lookupAtPath=path =>
  Int
)
|> Js.log2("### QUERY ###");

TypesTransformer.printFromFlowTypes(
  ~content=testFragment,
  ~operationType=Fragment("TodoApp_user", false),
  ~lookupAtPath=path =>
  Float
)
|> Js.log2("### FRAGMENT ###");

TypesTransformer.printFromFlowTypes(
  ~content=testMutation,
  ~operationType=Mutation("AddTodoMutation"),
  ~lookupAtPath=path =>
  Float
)
|> Js.log2("### MUTATION ###");