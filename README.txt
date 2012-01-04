----------------------------------------------------------------------
Note

This was a first stab and at the present time I've determined
that--for my application--I'm better served by a simpler approach,
which lives here: https://github.com/austinhaas/mailbox-plus

----------------------------------------------------------------------
Description

This code is an extension to SBCL's concurrent mailbox implementation
[1] which allows us to treat the mailbox like a pseudo-sequence. With
it we can do things like wait for a particular message (e.g., using
STREAM-FIND) while keeping any preceding messages intact and in order
for later operations.

One very important caveat is that, while any thread can send messages
to the mailbox used by a message-stream, only one thread can safely
use the message-stream functions, since the added buffer and the
functions on the buffer are not thread-safe.

The motivation for this code was to create an analog to Erlang's
receiver function, which can selectively match incoming messages
against a pattern, and transparently maintain the original order of
the incoming messages (including those that didn't match) after the
receive function exits. One use-case is to send an asynchronous
request to another process and then block on an incoming message
stream until the reply is received. Any other incoming messages are
ignored in the meantime, but they will be intact for subsequent
operations.

----------------------------------------------------------------------
Implementation Notes

This stream implementation is adapted from chapter 3.5 of Structure
and Interpretation of Computer Programs. It affords us a clean and
simple way to develop the base primitives, on top of which the
sequence operations are easily implementable. Certainly, this is not
the most efficient solution.

Implementing a sequence on the mailbox system has two important
implications: 1. The sequence is infinite and 2. The next item in the
sequence is governed by an outside process, i.e., whatever adds a new
message to the mailbox.

To address #2, we've modified the stream implementation to support
lazy evaluation of the CAR, in addition to the CDR. This is necessary
because we don't want to start trying to retrieve the first message as
soon as we create the stream, and similarly, we may not want to
immediately retrieve the next message every time we move up the
list. Consider that, at any time, the next message might not be
available; so we wouldn't want to wait for it before we even need it.

To address both #1 and #2, we've added an optional timeout parameter
to most interface methods. When an operation times out, a
timeout-condition is raised.

----------------------------------------------------------------------

[1] From the SBCL manual: sb-concurrency:mailbox is a lock-free
message queue where one or multiple ends can send messages to one or
multiple receivers. http://www.sbcl.org/manual/#sb_002dconcurrency
