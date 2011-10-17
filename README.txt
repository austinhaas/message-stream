This stream implementation is adapted from chapter 3.5 of Structure
and Interpretation of Computer Programs.

From the SBCL manual: sb-concurrency:mailbox is a lock-free message
queue where one or multiple ends can send messages to one or multiple
receivers. http://www.sbcl.org/manual/#sb_002dconcurrency

The intent of this code is to treat SBCL's mailbox implementation as
an infinite sequence, so that we may create and use analogs to
familiar sequence operations, such as FIND. The stream algorithm from
SICP is a clean and simple way to develop the base primitives, on top
of which the sequence operations are easily implemented. Certainly,
this is not the most efficient solution.

One very important caveat is that, while any thread can send messages
to the mailbox, only one thread can safely use this stream
implementation.

Implementing a sequence on the mailbox system has two important
implications: 1. The sequence is infinite. 2. The next item in the
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
