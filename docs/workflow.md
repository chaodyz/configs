# Engineering Workflow

How it looks in short:

1. **Is there a `HANDOFF.md`?**
   - No → create one.

2. **Is there a ticket?**
   - No → gather design and specs.
     - PRD missing → brainstorm the business idea first.
     - Then → create the ticket
   - Yes → continue.

3. **Is it a small task?**
   - Yes → implement directly.
   - No → brainstorm implementation, enter plan mode, and write the plan.
     - Review and refine the plan.
     - Execute the plan.

## Working Style
* Use brief, concise communication.
* Keep changes scoped to the ticket.
* Prefer simple solutions over unnecessary abstraction.
* Record important decisions outside production code.

## Agent Documents

Store all working documents under:

`/docs/agent/{EPIC_NUMBER}_{EPIC_NAME}/`

Recommended files:

* `PLAN-{TICKET_NUMBER}.md`
* `HANDOFF.md`

These files must remain local and must never be auto committed.

Keep one Git stash named after the Epic as the backup for all agent documents. Re-apply it when resuming work and refresh the same stash when documents change.

---

## Planning Threshold

### Small Change

Implement directly when the work is:

* Low effort
* Straightforward
* Low risk
* Limited to a small area
* Not dependent on significant technical decisions

A plan document is optional.

### Medium or Large Change

Use plan mode when the work includes:

* Multiple components or services
* Data-model or API changes
* Architectural decisions
* Migration or compatibility concerns
* Significant edge cases
* Higher regression risk
* Multiple implementation options

Create `PLAN.md` before implementation.

---

## Hand-Off Document

Create `HANDOFF.md` as soon as work on the Epic begins.

Update it after:

* Each ticket is created
* Each plan document is created or changed
* Each implementation is completed
* Each review is completed
* Testing results change
* A blocker, limitation, or follow-up is discovered

Track:

* Ticket list
* Ticket status
* Implementation progress
* Test progress
* Completed work
* TODO items
* Action items
* Blockers
* Known limitations
* Manual testing instructions

Statuses:

* Not started
* Planning
* In progress
* In review
* Ready for testing
* Blocked
* Done

---

# 1. Create the Ticket

Tickets should focus on the business problem and expected outcome.

## Ticket Content

Include:

* Business problem
* Business value
* Expected user or system outcome
* Acceptance criteria
* Relevant constraints
* Explicitly excluded scope

## Acceptance Criteria

Acceptance criteria should:

* Describe observable outcomes
* Be goal-focused
* Be testable
* Avoid prescribing implementation details
* Allow different technical solutions

Ask questions when:

* The business goal is unclear
* The expected outcome is unclear
* Acceptance criteria are ambiguous
* Acceptance criteria cannot be tested
* Important constraints are missing
* Scope boundaries are unclear

Create or update the Hand-Off document after the ticket is created.

---

# 2. Plan and Brainstorm

For medium or large work, explore technical options before selecting an implementation.

Consider:

* Possible solutions
* Trade-offs
* Complexity
* Maintainability
* Performance
* Security
* Compatibility
* Migration requirements
* Failure scenarios
* Testing strategy
* Rollback strategy

The plan should contain:

1. Business goal
2. Current behavior
3. Desired behavior
4. Constraints and assumptions
5. Considered solutions
6. Selected solution
7. Decision rationale
8. Implementation steps
9. Testing strategy
10. Risks and limitations
11. Follow-up work

Record meaningful rejected approaches and why they were rejected.

Update the Hand-Off document after the plan is created or changed.

---

# 3. Implement

## Start With Tests

Write simple tests that directly represent the acceptance criteria.

Tests should:

* Focus on behavior
* Avoid unnecessary implementation coupling
* Cover the main success path
* Cover important failure paths
* Cover confirmed edge cases

Do not add speculative tests for unsupported requirements.

## Implementation Rules

* Implement the smallest change that satisfies the acceptance criteria.
* Follow existing project patterns unless there is a documented reason not to.
* Avoid unrelated refactoring.
* Avoid premature abstraction.
* Keep commits and diffs easy to review.
* Do not hide new scope inside implementation details.

## Discovered Requirements

When implementation reveals a missing requirement or edge case:

1. Confirm the intended business behavior.
2. Update the ticket and acceptance criteria.
3. Update the plan when the technical decision changes.
4. Update the Hand-Off document.
5. Add or update tests.
6. Implement the agreed behavior.

Acceptance criteria should be updated before treating the new behavior as required.

---

# 4. Review and Verify

Before handoff:

* Review the final diff
* Remove debugging code
* Remove unrelated changes
* Run formatting
* Run linting or static analysis
* Run the build
* Run relevant unit tests
* Run relevant integration tests
* Run relevant E2E tests
* Verify each acceptance criterion
* Check affected existing behavior

When a check cannot be run, document:

* Which check was skipped
* Why it was skipped
* What risk remains
* How it should be verified later

Update the Hand-Off document after implementation and review.

---

# 5. Handoff for Testing

Provide a concise handoff containing:

## What Changed

* Main behavior implemented
* Components or services affected
* Important technical decisions

## Verification Completed

* Build result
* Test result
* Lint or static-analysis result
* Acceptance criteria verified

## How to Test

Provide clear manual steps:

1. Required setup
2. Action to perform
3. Expected result
4. Important edge cases to verify

## Remaining Work

Include:

* Known limitations
* Deferred scope
* Follow-up tickets
* Risks
* Blockers

Do not mark the ticket done until the agreed verification is complete.

---

# Bug-Fix Workflow

For bug fixes:

1. Reproduce the issue.
2. Identify the root cause.
3. Add a regression test when practical.
4. Implement the smallest safe fix.
5. Verify the original issue.
6. Verify surrounding behavior.
7. Record limitations or follow-up work in the Hand-Off document.

Avoid unnecessary comments in production code.

Add comments only when they explain:

* A non-obvious constraint
* A necessary workaround
* A compatibility limitation
* A future maintenance risk
* Why a simpler-looking solution is incorrect

Keep investigation history, discarded theories, and temporary context in the ticket, plan, or Hand-Off document.
