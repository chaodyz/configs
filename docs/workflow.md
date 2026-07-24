## How to work (WIP)

Use breif and concise wording.

### Convention

- **Record technical decisions with plan doc**. Small feature (small effort, straightforward no much tech design), can directly implement without plan.
  med to large feature, use plan mode, write a plan to record technical decisions.
- Maintain a Hand-Off doc, create a handoff doc from get go, and update it when new ticket is created, keep track the implemenation and testing progress of each ticket. TODO items, action items. Update it after each implementation and review is done, and after each ticket creation, and after each plan doc created.
- Plan and Hand-off docs store in /docs/agent/{EPICNUMBER_EPIC_NAME}/
- never commit /agent docs, keep it local, back it up with ONE stash record with Epic name then apply back

### 1. create ticket

when create a ticket, i will try to gather the spec for you. keep in mind the ticket should be Business focus, AC should be goal focus. Impl detail can be vary for a goal.

interview with me if you think detail is missing that the Business goal isnt clear or the AC goal is not clear.

### 2.Plan with Brainstorm

We will then brainstorm the technical implementation, where we figure out the possible technical solutions, trade offs.

### 3.Implement

Start with writing test cases, keep them simple and true to AC.
Keep in mind, we can go back edit AC when edge cases needs to be handled, or certain AC needs to be satisfied to achieve the goal.

### 4.Review

Run build ,test, verify passes, then handover me to test with description of whats done, and how to test

### Bonus: bugfix

when work with bugfix, do not include unesesary context as comment, unless it's important to be there for handover/future phases, or maintaince,limitation with workaround reason.
