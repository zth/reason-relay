module TicketFragment = %relay(
  `
  fragment SingleTicket_ticket on Ticket {
    assignee {
      __typename

      ... on User {
        ...Avatar_user
      }

      ... on WorkingGroup {
        ...SingleTicketWorkingGroup_workingGroup
      }
    }
    id
    subject
    lastUpdated
    trackingId
    ...TicketStatusBadge_ticket
  }
`
)

@react.component
let make = (~ticket as ticketRef) => {
  let ticket = TicketFragment.use(ticketRef)

  <tr>
    <td>
      {switch ticket.assignee {
      | Some(assignee) =>
        switch assignee {
        | #User(user) => <Avatar user=user.fragmentRefs />
        | #WorkingGroup(workingGroup) =>
          <React.Suspense fallback={<Loading />}>
            <SingleTicketWorkingGroup workingGroup=workingGroup.fragmentRefs />
          </React.Suspense>
        | #UnselectedUnionMember(_) => <span> {React.string("-")} </span>
        }
      | None => <em> {React.string("Unassigned")} </em>
      }}
    </td>
    <td> {React.string(ticket.subject)} </td>
    <td> <TicketStatusBadge ticket=ticket.fragmentRefs /> </td>
    <td> {React.string(Belt.Option.getWithDefault(ticket.lastUpdated, "-"))} </td>
    <td> {React.string(ticket.trackingId)} </td>
  </tr>
}
