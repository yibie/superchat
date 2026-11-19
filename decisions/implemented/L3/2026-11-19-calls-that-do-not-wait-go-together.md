# Decision: Calls that do not wait on each other go together

Status: implemented

## Problem

In the person's own use (timeline `20261118-165024-f2af`) 609 of 739 model
responses held one call and 99 held more. Each response is a request: a
model that reads three files one by one spends three requests, three waits
and three copies of the conversation where one would do. The base
instructions said nothing about it.

## Decision

The base instructions say: "Calls that do not wait on each other go in one
response: read the files you need together, and look at another file while
a check runs." The run already runs every call of a response.

## Alternatives considered

- Say nothing and rely on the model. Not taken: the session above shows how
  rarely it does it unasked.
- Run the calls of a response at the same time. Not part of this: they run
  in order now; this is about how many requests a task takes, not how long
  each call waits.

## Consequences

- Fewer requests for the same work, when the model follows it; a live run
  after the change read a directory and two files in one response.
- A model may group calls that do depend on each other, such as an edit and
  the read of its result; the second then sees the first, since calls run in
  order.
