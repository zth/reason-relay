module WorkingGroupFragment = %relay(
  `
  fragment SingleTicketWorkingGroup_workingGroup on WorkingGroup
    @argumentDefinitions(
      includeMembers: { type: "Boolean!", defaultValue: false }
    )
    @refetchable(queryName: "SingleTicketWorkingGroupRefetchQuery") {
    name
    membersConnection @include(if: $includeMembers) {
      edges {
        node {
          id
          fullName
          ...Avatar_user
        }
      }
    }
  }
`
)

@react.component
let make = (~workingGroup as wgRef) => {
  let (workingGroup, refetch) = WorkingGroupFragment.useRefetchable(wgRef)
  let (startTransition, isPending) = ReactExperimental.unstable_useTransition()

  <div>
    <strong> {React.string(workingGroup.name)} </strong>
    <div>
      {switch workingGroup.membersConnection {
      | Some(membersConnection) =>
        switch membersConnection {
        | {edges: Some(edges)} => edges->Belt.Array.keepMap(edge =>
            switch edge {
            | Some({node: Some(node)}) => Some(node)
            | _ => None
            }
          )->Belt.Array.map(member =>
            <div key=member.id> <em> {React.string(member.fullName)} </em> </div>
          )->React.array
        | _ => React.null
        }
      | None => <button type_="button" onClick={_ => startTransition(() => {
              let _ = refetch(
                ~variables=WorkingGroupFragment.makeRefetchVariables(~includeMembers=true, ()),
                (),
              )
            })}> {React.string(isPending ? "Loading..." : "See members")} </button>
      }}
    </div>
  </div>
}
