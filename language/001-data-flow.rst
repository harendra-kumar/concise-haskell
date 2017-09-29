Flow Based Programming
======================

Logic & Data Flow
-----------------

`Logic` and `data flow` are the two fundamental dimensions of any program. For
scaling a program, logic must follow data flow or vice versa. If these two are
orthogonal to each other we will not be able to scale the program.

Example
-------

We have to write the input data to a file and compute its length. There are
multiple ways of doing that:

1) accumulate all the data into a memory, write it to file, compute its length
2) Create an explicit loop, which writes data block by block in each iteration
   and keeps accumulating the length.
3) Use a streaming abstraction like pipe, let the data flow thorugh the pipes
and each pipe does different things to data like writing to a file or computing
its length.

Program Structure
-----------------

There are two ways to structure your program.

Logic driven data
~~~~~~~~~~~~~~~~~

LazyIO says, I the program logic, will decide in what order the data will be
accessed, give it to me when I demand it. Which means the data can be accessed
in any random order, over any span of time, placing no limits on what needs to
remain in-memory at a given time. This is a data pull model and the root of all
evils due to lazyIO. Random access of data is always expensive.

Also, in this model the whole data has to be available before we can process it
since we may demand any part of the data at any time. This will be problematic
when the data is being produced dynamically and we do not even know how much it
is.

Data driven logic
~~~~~~~~~~~~~~~~~

In contrast the pipe model says, you the program, will receive the data in a
predefined order and you will have to structure your logic based on that.  This
is a data push model. The pipes are not allowed to buffer random amounts of
data, they will process a chunk and let it go. Then they will move on to the
next chunk. Thus there is bound at how much data is bufferred in the system.

Data access is serial in this case which is cheap. Pipes observe the data
flowing through them and make observations or perform transformations.

The `logic drives data` model is more like the whole village dipping in the
pool while `data drives logic` is like a pipeline based supply system and
everyone drinks from the tap.

Composing Programs
------------------

Functions are low level pipes: Pipes are very similar to functions but at a
higher level of abstraction. Like functions they also take an input and produce
an output. They encapsulate some closely related logic that operates on the
data flowing through them.

Pipes are like fatter functions isolating a related piece of logic within a
boundary. They reduce the footprint or hold-up boundaries of data.

Lazy evaluation in general allows you unbounded data footprint, resulting in
space leaks. There is no guarantee upto what point data will be held. Pipes
chop your program into pieces which introduces fences beyond which data will
not be held up, thus avoiding space leaks.

The way functions allow us to compose the logic elegantly, the same way pipes
allow us to compose bundles of closely related data flow logic elegantly. These
pieces are structured according to the data flow. Or in other words they allow
us to compose smaller special purpose programs together to create bigger
programs which are structured according to the data flow.

Higher order pipes: The way we have higher order functions we can have higher
order pipes. Basically each pipe is a tee, it observes the data flowing
through, accumulates its observations and passes it onto another pipe which is
a higher order pipe. All higher order pipes are connected together to create a
higher order logic and process all those observations further.

Lazy evaluation as fine grained data flow programming
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Functions are low level pipes.

Infinite data structures are like the indefinite data producers. Lazy
evaluation composes multiple functions together and a data element passes
through those functions like data passes through pipes. The usual design model
is to let the data pass through all stages quickly and then get garbage
collected and not hold up the data. If you hold up it turns into a space leak.

Each function is a pipe connected to another function, another pipe. Lazy
evaluation drives the data through this big chain of pipes. The pipes get
activated when we want to draw something from the remote end. This is literally
data driven programming.

We need a picture here with functions/closures connected to each other and how
data flows through them. How the whole evaluation machinery is cranked to
generate output from input.

Haskell lazy evaluation is fundamentally data driven or flow based programming,
or in other words stream based programming.

Stream fusion basically fuses multiple stages of lazy evaluation into one
big imperative like loop instead of chain invocation of each stage. Same way we
represent loops by pipes.

Within a high level pipe we can use strict evaluation rather than lazy
evaluation and fuse that whole logic together to make it more efficient.

Can we automatically make everything strict rather than lazy depending on
feasibility?

Comparison with Imperative Design Patterns
------------------------------------------

Programmer drives everything
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In imperative languages the programmer is free to interleave logic and data the
way he wants. This is totally non-composable obviously. You depend on the
programmer discipline.

"for" loops translate into composed pipes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

From the imperative land you can think about pipes as a translation of your
"for" loops. For example the snippet I gave you can be implemented as a for
loop in imperative languages. The for loop will read a chunk of data, write the
chunk to a file, add the length of the chunk to a length variable. When the
loop is done you will have written it out to the output file and you will have
the length.

In haskell you will use two pipes instead. The first one will write the data to
the output file and the second one will compute the length.
