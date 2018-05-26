
# `cue`

`cue` is a minimal, esoteric, assembly-like language based on
manipulating various queue structures.


## building `cue`

`cue` is implemented in Haskell and can be built easily using
stack or cabal. Simply cloning this repo and running `stack install`
or `cabal install` in the root of the repository should be sufficient.

This creates the executable `cue` which is the interface to the
interpreter for the language.

## running `cue` programs

The `cue` executable expects to be supplied
- cli options if present
- the name of the script file to execute
- potentially, input to the script

In this order.

Options recognized are
- `-s` Implies the input should be treated as a string, 
  and each character transformed into integers using ASCII,
  rather than as a sequence of space-separated integers.
- `-e` Implies the input should be read from `stdin` rather than as
  further command line arguments

Example programs are present in the `examples/` folder of this repo, and
should be seen as demonstrations and starting points to test ideas.

## I/O

Input and output are achieved using queue zero (`%0`). Initial program input
is written to the queue, and whatever is in this queue at the end of
execution is printed out in order. There is currently no facility to
interact with the user during execution, or to output in a form other
than a sequence of integers, though this is a consideration.

## regarding computational class

The computational class of the language is unknown, but it is suspected
that a bitwise cyclic tag implementation could potentially
be created to prove turing-completeness, while not suffering particularly
from the queue-based nature of the language.



# Syntax of `cue`

A program in `cue` is made up of any number of procedure declarations,
which consist of an identifier followed by any number of statements
within curly braces. For example:
```
main {
    # line comments begin with `#`
    # this is an empty `main` procedure
}

# other procedures
foo { ... }
bar { ... }
...
```

Failing to declare a procedure is equivalent
to declaring it as an empty procedure with no statements in its body.

The declaration order of procedures is not usually significant, but if two
identically named procedures are declared, the first definition will be
used.

# Statements

Each statement performs some task, usually involving the _accumulator_,
an implicit variable which is used to store intermediate results of
computations, among other things. They are executed in the order they appear.

The accumulator, and all other values in the program, are integer-valued,
and unbounded in range. Within each function body, the accumulator
is initially zero-valued, and its state is not preserved across calls.

Generally statements require one or more _queue identifiers_ as arguments,
specifying which queues are operated upon. There are an infinite number of
these queues, each addressable by a unique integer, and each initially empty
of values. An example identifier is `%0`, meaning "the queue associated
with the integer value zero". Abstractly, these are somewhat like the registers
of a real cpu, but they hold multiple values retrievable in FIFO order.

Statements are generally terminated by a semicolon, but there is no semicolon
following the block of a `tst` statement (specified later).

# Accumulator-modifying commands

These simplest commands do nothing but modify the accumulator directly,
and do not involve queues. As such they have little power but are
also necessary for various tasks.

## `inc`

This command increments the value of the accumulator by one.
Since the accumulator always begins at zero, we can use this
to form any number we like, in principle.
```
inc; # the accumulator is now 1.
inc; # etc.
```

## `dec`

Conversely, this decrements the accumulator by one.
```
dec; # accumulator becomes -1.
inc; # back to 0.
```


# Queue-manipulation commands

Purely the accumulator won't get us very far, we need to add and remove
values from queues to be able to store information for later. These
commands can allow us to do so.

## `get`

The `get` command removes a value from the specified queue
and loads it into the _accumulator_. It takes one argument, which is
the queue identifier we wish to retrieve a value from.
For example,
```
get %0; # load the value at the front of queue 0 into the accumulator
get %1; # likewise for queue 1
```

This is a destructive operation in that the value at the front
of the queue is _moved_ into the accumulator, and so the state of the
queue being accessed has changed. In general, operations which view elements
of a queue can only do so in this manner.

If the queue being accessed is empty, the value 0 will be produced.

## `pop`

The `pop` command, like `get`, removes a value from a queue, but does
nothing to the state of the accumulator, simply throwing the value away:
```
pop %0; # clear a value from queue 0
```

If the queue being popped from is already empty, this will have no effect.

## `put`

The `put` command, as its name may suggests, works as the opposite of
`get`, and enqueues the value of the accumulator at the end of the
queue given as an argument. For example:
```
inc;
inc;    # set the accumulator to 2
put %0; # enqueue the value 2 onto queue 0. 
get %0; # load a new value from queue 0.
```


# Arithmetic commands

In order to do many calculations of interest, we have several arithmetic
commands which modify the accumulator using the value from a queue.

The available commands are
- `add` (addition)
- `sub` (subtraction)
- `mul` (multiplication)
- `div` (division)
- `mod` (modulus)

Each command takes a queue identifier as its argument, takes the
top value of this queue, and combines it with the accumulator
using the appropriate operator. For example,
```
inc;
inc;    # acc initially 2

put %1; # store current accumulator value
put %1; # store another copy

add %1; # doubles our accumulator to 4.
div %1; # divides by 2, accumulator is now 2.
mul %1; # sets acc to zero since queue is empty and zero-valued.
```

Although it may be technically possible to implement some form of these
operations using only `inc`, `dec`, and control flow, it would be
nightmarish and so they have been added for convenience. Despite this,
nevertheless `inc` and `dec` seem to be more common as they are necessary
to initialize known constant values.


# Control flow commands

Without conditional execution and procedures,
we could hardly implement anything of interest.
As such, we have commands which can work
together to provide interesting control flow.

## `tst`

The `tst` command provides a way to condition the execution of
a block of code on a comparison between two values.

It is written by following `tst` by two queue identifiers in a comparison
expression, using one of the operators supported:
- `=`  (equality)
- `>`  (greater-than)
- `<`  (less-than)
- `>=` (greater-than or equal)
- `<=` (less-than or equal)
- `!`  (inequality)

The values will be removed from the queues to perform the comparison,
with the left operand being popped before the right. It is important to
note that just like a `get` operation, the retrieval of values to be used
in this comparison is a mutation of the queues involved.

After the comparison, a block of statements in curly braces follows
to describe what should be done if the comparison is _true_.

For example:
```
inc;
inc;
put %0;
put %1; # put the value 2 onto queues 0 and 1

tst %0 = %1 {
    # If the values from the two queues are equal,
    # then this code will execute. Otherwise, it will
    # simply be skipped.
    put %2;
    inc;    # any number of commands can be in this block.
}

tst %2 > %0 {
    # will occur if the previous block occurred
    get %3;
}
```

The usefulness of this is limited however when we cannot involve
the accumulator in our comparisons. As such, if the left operand
of the comparison is omitted, the value of the accumulator will
be used for the comparison here:
```

inc;

tst < %0 {
    # if the value from %0 is greater than 1, this will execute.
    put %0;
    put %1;
    ...
}

# all the comparisons can still be used.
tst ! %3 {
    ...
}

```


## `die`

The `die` command can be used in combination with the `tst` command
to prevent other code from executing in case a condition is true.

When the `die` command is reached, the currently executing procedure
immediately ends and control flow proceeds to the next procedure in
the call queue.

```
inc;
tst > %0 { die; }
put %2; # will not execute if %0 is greater than or equal to 1
```


## `cue`

Finally, the `cue` command adds a procedure by name to the call queue,
to be executed later (e.g. once the current procedure has ended,
and any preceeding calls in the queue have finished as well).

The state of the call queue initially contains only the procedure
`main`, so this is the entry point of the program. A procedure
can cue itself.

```
# a procedure which loops forever, enqueueing
# every positive integer to queue 0 in order.
count {
    get %1;    # get the last value of this queue, perhaps zero
    inc;       # increment to count upwards
    put %1;    # save value for next call
    put %0;    # output to queue zero as well
    cue count; # the recursive cue.
}
```

# More on Queue Identifiers

To this point, queues have been identified by percent-sign-prefixed
integers only. This is the most common way to address a queue,
but a more subtle addressing mode is allowed to permit indirection.

To clarify the basic notation, a queue identifier `%r` where `r` is
some integer refers directly to the queue with this number. Any
operations using this identifier act directly and solely upon
this queue.

As such, operations like
```
put %0;
get %1;
mul %2;
```
are simple to understand, but cannot access different queues
in different scenarios, the values are constant. To allow more
complex data flow, queue identifiers can be referential.

A queue identifier such as `%%r` is legal, and can be seen as `%(%r)`,
or, the queue whose number is the value retrieved from queue `r`. This,
like all queue value retrieval, is a destructive operation to queue `r`.

This extends to arbitrarily many layers of indirection, e.g. `%%%0` is also
a valid queue identifier. To find the queue we want to access, we load a value from queue `%0`, use this value to find another queue (e.g. `%2`), load a value from this, and its value tells us the final queue to perform some action upon. Nested as before, it can be seen as `%(%(%0))` although
this notation is not supported within the language.

Keep in mind that any empty queue will return a zero value, and so
the queue `%0` is likely to be interacted with heavily in an especially
large indirection chain.

# Further features?

That's it so far. `cue` is a very simple language in concept.

It may be interesting to expand `cue` to support arguments to procedures
usable as queue references, or to allow the accumulator to be used
as a constant to access some queue, among other ideas.

Whether these will be added depends on whether it is
- turing complete
- humanly possible to write algorithms
- remotely intriguing to think about

in their absence. Minimalism is a good property of a language with such a strange
core idea as this at first, but some extensions could
be enabled with command line arguments if we wish to retain the restricted
modality as well.