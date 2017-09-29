  * Arrows (category composition) (binary composition)
      They are equivalents of binary functions that can be composed, they are
      profunctors.

We can think of arrows as computations, too. The Arrow class we have de- fined
is clearly analogous to the usual Monad class — we have a way of creating a
pure computation without effects (arr/return), and a way of sequencing
computations ((>>>)/(>>=)). ``But whereas monadic computations are parameterised
over the type of their output, but not their input, arrow computations are
parameterised over both.`` The way monadic programs take input cannot be varied
by varying the monad, but arrow programs, in contrast, can take their input in
many different ways depending on the particular arrow used. The stream function
example above illustrates an arrow which takes its input in a different way, as
a stream of values rather than a single value, so this is an example of a kind
of computation which cannot be represented as a monad.

-----
My notes:

Notice that a monad type is ``m a`` where a is the output type and an arrow
type is ``c a b`` where ``a`` is the input type and ``b`` is the output type.
Another way to put it is that a monad puts a value in a context whereas an
arrow puts a function in a context.
----

Arrows thus offer a competing way to represent computations in Haskell.  But
their purpose is not to replace monads, it is to bring the benefits of a shared
interface, discussed above, to a wider class of computations than monads can
accomodate. And in practice, this often means computations that represent
processes.

In the case of monads, the second argument of (>>=) is a Haskell function,
which permits the user of this interface to use all of Haskell to map the
result of the first computation to the computation to be performed next. Every
time we sequence two monadic computations, we have an opportunity to run
arbitrary Haskell code in between them. But in the case of arrows, in contrast,
the second argument of (>>>) is just an arrow, an element of an abstract
datatype, and the only things we can do in that arrow are things that the
abstract data type interface provides.  Certainly, the arr combinator enables
us to have the output of the first arrow passed to a Haskell function — but
this function is a pure function, with the type b -> c, which thus has no
opportunity to perform further effects. If we want the effects of the second
arrow to depend on the output of the first, then we must construct it using
operations other than arr and (>>>).

Idioms are oblivious, arrows are meticulous, monads are promiscuous
-------------------------------------------------------------------

We revisit the connection between three notions of computation: Moggi’s monads,
Hughes’s arrows and McBride and Paterson’s idioms (also called applicative
functors). We show that idioms are equivalent to arrows that satisfy the type
isomorphism A ❀ B ' 1 ❀ (A → B) and that monads are equivalent to arrows that
satisfy the type isomorphism A ❀ B ' A → (1 ❀ B). Further, idioms embed into
arrows and arrows embed into monads

Arrows vs Profunctors
---------------------

An Arrow is just a Strong Category. Purescript deprecated the arrow package in
favor of profunctors package.
* https://github.com/purescript-deprecated/purescript-arrows

* https://stackoverflow.com/questions/38169453/whats-the-relationship-between-profunctors-and-arrows
* https://www.reddit.com/r/haskell/comments/4fkkzo/when_does_one_consider_using_arrows/?st=j7a7kiim&sh=0fb14070
* https://cdsmith.wordpress.com/2011/07/30/arrow-category-applicative-part-i/
* https://cdsmith.wordpress.com/2011/08/13/arrow-category-applicative-part-iia/
* https://stackoverflow.com/questions/24668313/arrows-are-exactly-equivalent-to-applicative-functors
* https://elvishjerricco.github.io/2017/03/10/profunctors-arrows-and-static-analysis.html
* http://blog.sigfpe.com/2011/07/profunctors-in-haskell.html
* http://www-kb.is.s.u-tokyo.ac.jp/~asada/papers/fromComptoComp.pdf
  Categorifying Computations into Components via Arrows as Profunctors

-------
* https://stackoverflow.com/questions/17668452/can-someone-explain-to-me-why-the-app-function-of-arrowapply-makes-them-as-power/17673690#17673690

You can use Applicative to get the generality, and you can use Monads to get
the interface-functionality, so you could argue that theoretically we don't
need them, but some people like the notation for arrows because it's like do
notation, and you can indeed use Arrow to implement parsers that have a static
component, thus apply compile-time optimisations. I believe you can do that
with Applicative, but it was done with Arrow first.

A note about Applicative being "less powerful":
The paper points out that Applicative is more general than Monad, but you could
make Applicative functors have the same abilities by providing a function run
:: Applicative f => f (f b) -> f b that lets you run a produced computation, or
use :: Applicative f => f (a -> f b) -> f a -> f b that allows you to promote a
produced computation to a computation. If we define join = run and unit = (<$>)
we get the two functions that make one theoretical basis for Monads, and if we
define (>>=) = flip (use.pure) and return = unit we get the other one that's
used in Haskell. There isn't an ApplicativeRun class, simply because if you can
make that, you can make a monad, and the type signatures are almost identical.
The only reason we have ArrowApply instead of reusing Monad is that the types
aren't identical; ~> is abstracted (generalised) into the interface in
ArrowApply but function application -> is used directly in Monad. This
distinction is what makes programming with Arrows feel different in many ways
to programming in monads, despite the equivalence of ArrowApply and Monad.

-------

* http://haskell.cs.yale.edu/wp-content/uploads/2012/06/FromJFP.pdf Yampa
  arrows
* http://homepages.inf.ed.ac.uk/wadler/papers/arrows-and-idioms/arrows-and-idioms.pdf
Idioms are oblivious, arrows are meticulous, monads are promiscuous

Use Cases
----------

A good use case of arrows is stream processing, signal processing, circuit
diagrams.

References
----------

* https://softwareengineering.stackexchange.com/questions/114681/what-is-the-purpose-of-arrows
* http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf
* https://en.wikibooks.org/wiki/Haskell/Understanding_arrows
* https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial
