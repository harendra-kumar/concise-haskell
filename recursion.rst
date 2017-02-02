Recursion
=========

Recursion and Corecursion
-------------------------

Recursion expresses a well defined pattern. We just specify a rule to govern
the repetitive pattern. When we use a rule to consume a recursively defined
data structure , and reduce it to a non-recursive data structure, it is called
recursion. When we use a rule to generate a recursive data structure from a
non-recursive seed structure, it is called corecursion.

Iteration and Fix Point
-----------------------

Iteration of a function is defined as applying the function repeatedly to its
previous result.  As an example, we can apply `sqrt` to a number iteratively::

  takeWhile (/= 1) $ iterate sqrt 2

`sqrt` converges to 1 if we keep iterating it starting with any number. 1 is
called the fix-point of sqrt. In general when `c = f c`, `c` is called the fix
point of a function.

Recursion versus iteration
--------------------------

Recursion and iteration are equally expressive: recursion can be replaced by
iteration with an explicit stack, while iteration can be replaced with tail
recursion. Which approach is preferable depends on the problem under
consideration and the language used. In imperative programming, iteration is
preferred, particularly for simple recursion, as it avoids the overhead of
function calls and call stack management, but recursion is generally used for
multiple recursion. By contrast, in functional languages recursion is
preferred, with tail recursion optimization leading to little overhead, and
sometimes explicit iteration is not available.

Recursion Terminology
---------------------

::

  Recursive data     -----> recursive function   ----> non-recursive data
  finite codata      -----> catamorphism         ----> data
                            tear "down"

  Non-recursive data -----> corecursive function ----> recursive data
  data               -----> anamorphism          ----> possibly infinite codata
                            build "up"

  recursive data     ------> cata + ana  = meta  ----> recursive data
  non-recursive data ------> ana  + cata = hylo  ----> non-recursive data

  recursion   -----> algebra   + recursion scheme
  corecursion -----> coalgebra + recursion scheme

* Recursion consumes recursive data structures in a pattern
* Corecursion produces a pattern of recursive data structures
* algebra: a specification to deconstruct or fold data structures - generates data
* coalgebra: a specification to unfold or construct a data structure - generates codata
* data: the `necessarily` finite product of an initial algebra
* codata: the `possibly` infinite product of a final coalgebra

+-----------+-------------+--------------+
|           | Recursion   | Corecursion  |
+===========+=============+==============+
| Nature    | Consumption | Production   |
+-----------+-------------+--------------+
| Nature    | Reduce      | Produce      |
+-----------+-------------+--------------+
|           | fold        | unfold       |
+-----------+-------------+--------------+
| Driven by | Functions   | Constructors |
+-----------+-------------+--------------+
|           | Finite      | Infinite     |
+-----------+-------------+--------------+
|           | data        | codata       |
+-----------+-------------+--------------+
|           | algebra     | coalgebra    |
+-----------+-------------+--------------+

Recursive Expressions
---------------------

An expression can be defined recursively by referring to the value being
defined within the definition.  Any recursive definition can be reduced to the
following normalized version::

  x = f x

We can see `x` unfold clearly by repeatedly substituting the term `x` in the
expression for its own definition::

  f x
  f (f x)           -- after substituting x by (f x)
  f (f (f x))       -- after substituting x by (f x)
  ...

This is in fact how we defined iteration earlier i.e. applying a function
repeatedly on the previous result. Though we do not have much control over it.

Recursive Concrete Values
-------------------------

Note that in general `x` could be any type of value, a concrete value or a
function. But in this section we are talking about only concrete values. Later,
in the following sections, we will generalize this discussion to functions as
well. For concrete values we can easily deduce the following results:

+----------------------+------------------------------------------------------+
| When                 | What happens?                                        |
+======================+======================================================+
| `f` does not use `x` | `x` reduces to a non-recursive definition            |
+----------------------+------------------------------------------------------+
| `f` scrutinizes `x`  | evaluation of `x` results in an infinite loop.       |
| (recursion)          | Any side effects before the scrutiny are produced    |
|                      | in the loop.                                         |
+----------------------+------------------------------------------------------+
| `f` uses `x` in      | the data type of `x` has to be recursive. `x`        |
| output construction  | represents a lazy recursive infinite data            |
| (corecursion)        | construction. Pattern match on the structure         |
|                      | retrieves a component which is exactly the same      |
|                      | as original `x` and can be pattern matched again.    |
+----------------------+------------------------------------------------------+

Let us now see examples of each case described above:

* `f` discards its argument::

    -- non-recursive definition
    x = f x where f = const 10 -- x = 10

* `f` scrutinizes `x`::

    -- infinite loop
    x = x     -- x = _|_
    x = x + 1 -- x = _|_
    x = id x  -- x = _|_

* `f` produces side effects before it scrutinizes `x`::

    -- prints "yes" in infinite loop
    x = putStrLn "yes" >> x >> putStrLn "no"

* `f` does not scrutinize `x` but uses it in result construction::

    -- infinite lazy data construction
    let x = 1 : x in take 10 x
    let x = 1 : 2 : 3 : x in take 10 x
    let fibs = 1 : 1 : zipWith (+) fibs (tail fibs) in take 10 fibs

    data X = Cons X Int
    let x = Cons x 1
    in let Cons y 1 = x
           Cons z 1 = y
           ...

Recursive Functions
-------------------

In the previous section we talked about recursion on concrete values.  However,
recursion with functions is more common and therefore intuitive to most
programmers.  Let us write a simple recursive function that finds the fixed
point of `sqrt`::

  fixSqrt x =
      case (sqrt x == x) of
        True -> x
        False -> fixSqrt (sqrt x)

  >> fixSqrt 256
  1.0

When we evaluate `fixSqrt 256`, for example, it results in a call to `fixSqrt
16` in the first step and then `fixSqrt 4` in the next step, and so on. Finally
when the argument `x` passed to the next call becomes very close to 1 then we
hit the `True` case and the value gets evaluated to `x` i.e. 1.0.

Functions as values
-------------------

A function is a layer of indirection. Recursion at the function level does not
translate to recursion at concrete level because, unlike in the case of
concrete-value recursion where a value is defined in terms of itself, each
function call is a different concrete value.  When we unfold the recursion at
the function level it results in a flat concrete-value level space::

  fixSqrt 256 => fixSqrt 16 => fixSqrt 4 ... 1.0

When thinking in terms of concrete values, each function call can be treated as
a new anonymous concrete value (just like anonymous functions) the value of
which is uniquely determined by the combination of the function and the
argument values. We can imagine a new name for each function call i.e.
dynamically generated value instances::

  y1 => y2 => y3 ... yn (1.0)

In case of concrete values, when they are defined recursively they result in
infinite loop or infinite data structures because you cannot define a concrete
value in terms of itself, that means you will have to know the value to
determine itself, resulting in a paradox.

In function level recursion the termination problem too gets abstracted to the
function level. When recursing at the function level i.e. repeatedly `applying
a function` to previously generated concrete values. If the function i.e. the
abstract level value is not defined to converge we will continue applying it
forever. In that case the function application can be thought of as resulting
in a `bottom` value.

Notice `applying a function` which is different from evaluating a concrete
value.

A function represents a set of concrete values. A recursive function represents
a series (iterative) of concrete values.

Recursion as Iteration
----------------------

We will study how to iterate using recursion and lazy evaluation. We will also
understand how we can lazily build and evaluate a function on the fly
representing the logic for each step of the iteration.

As we saw in the previous section, iterating a function forever over an
eternally undefined value is not much interesting. But if that undefined value
is a function and we iterate a higher order function on top we can build a
function out of nothing on the fly.

What if the `x` in `x = f x` is a function instead? Let's call it `g`, and give
it the type `g :: a -> b`. Then f must be of type `f :: (a -> b) -> (a -> b)`;
that makes `f` a higher order function, and our iteration now becomes::

  g = f g
  g x = f g x

The value of this expression is a lazy infinite function `g` that represents an
infinite series of iterations of `f`.  `g` is created lazily by iterations of
`f` on the fly as it is evaluated.

Modifying fixSqrt
~~~~~~~~~~~~~~~~~

If we look carefully the body of `fixSqrt` is a function of `fixSqrt` and `x`
the argument::

  g = f g
    where
      f g x =
          case (sqrt x == x) of
            True -> x
            False -> g (sqrt x)

From this definition, it is obvious that `f` not only uses `g` but also the
argument of the output function `f g`, which is `x`, in its definition. When we
pass `g` to `f` it will return us a function which takes one argument `x` and
is defined in terms of that argument.

Termination
~~~~~~~~~~~

The iterations over the higher order function `f` are non-terminating and
infinite but we do not need them all if the evaluation of the function that
they are creating terminates.  Consider the following definition of `f`::

  f g = h
    where
      h x -> case (sqrt x == x) of
        True -> x
        False -> g (sqrt x)

In each iteration, `f` accepts a function `g`, and builds the function `h` which
is part of `g`. `h` terminates if a condition is met otherwise performs the
next iteration by calling `g` again.

`f` is a wrapper function which wraps `g` to take `sqrt` of its argument and
terminate if it is the same as the argument else pass it on to the next
iteration.

Now we can try evaluating `g`::

  >> g 10
  1.0

Note that we could determine the value of expression `g = f g` here because it
not only depends on `g` but also on the argument that is passed to the
resulting function, `f g`, produced by the iteration. As we can see, `f` is
defined in terms of the function `g` as well as the argument of `f g`. In other
words, `f` is conditionally strict on `g`, when the condition is met the self
dependency is broken and the iteration stops.

Each lazy evaluation step of `g` either generates a new application of `f` or
terminates.


There is no difference between a concrete value and a function when the
function is agnostic of its parameters that is it is not defined in terms of
its parameters.

If we consider g as just a value, it is easier to understand this. f is
modifying the value g by iterating on it. The value g is a collection of all
those iterations. When g happens to be a function we evaluate the value by
passing it parameters and each iteration built by f gets evaluated. Notice
there is no recursion in g. f is the one which is controlling the recursion
here. f is manually building the recursion. f knows whether to call g again or
not. We are iterating f building an infinite value g. That is a different way
of understanding higher order functions, we can think of lower order functions
just as plain values in the context of higher order functions.

Note that depending on how `f` is defined in terms of `g` and `x`, the
stop condition may never be met and the iteration may never stop.

Iteration Equivalent
~~~~~~~~~~~~~~~~~~~~

* We can think of this as an imperative for loop
* Stop condition `sqrt x == x`
* Step `sqrt x`

Fix
---

If we generalize the type of `f` so that we can include functions having any number
of parameters then we get::

  f :: (a -> a)

We can write a utility function to iterate on a higher order function `f`, we
will call it `fix`::

  fix :: (a -> a) -> a
  fix f = let x = f x in x

Note that this is generally useful only when `a` is a function type as we
discussed previously. Therefore, `f` is usually a higher order function and
`fix f` returns a function.

This really generates an iterative version of a recursive function using lazy
evaluation. Each iteration generates a closure on the heap rather than a stack
frame on the stack as in the case of recursion. We can, in fact, write a
recursive version of the function we wrote in the previous section::

  sqrtFix x = if sqrt x == x then x else sqrtFix (sqrt x)

TBD: Compare stack and heap for recursion and iteration cases graphically.

The Y-Combinator
~~~~~~~~~~~~~~~~

fix is also called the fixed-point combinator or the Y combinator in lambda
calculus discovered by Haskell B. Curry::

  fix f = f (fix f)                -- Lambda lifted
  fix f = let x = f x in x         -- Lambda dropped

A Generic Fixer
~~~~~~~~~~~~~~~

We can write a generic wrapper function to find the fixed point of any single
argument function::

  fixer g h x =
      case (g x == x) of
        True -> x
        False -> h (g x)

  >> fix (fixer sqrt) 2
  1.0
  >> fix (fixer cos) 2
  0.7390851332151607

The function `fix` is a misnomer, it does not really find a fixed point of a
function it really only iterates, and you could have any condition to stop the
iteration not just the fixed point of a function. A more apt name for it will
perhaps be iterate. The function `fixer` is the one which finds the fixed point
of a function by iterating.

Using direct recursion::

  fixit g x =
      case (g x == x) of
        True -> x
        False -> fixit g (g x)

  >> fixit sqrt 2
  1.0
  >> fixit cos 2
  0.7390851332151607

Recursion Schemes
-----------------

Recursion schemes are higher level constructs to abstract recursion. They
provide a higher level language (algebras and coalgebras) to express common
patterns of recursion in a convenient manner hiding boilerplate under the hood.

Given a recursive data structure (or nested boxes of constructors) we want to
fold the structure in some way. The structure is represented as a functor, for
example from type `a` to `f a` (e.g. from Int to [Int]). An algebra provides
rules to reverse map from `f a` to `a`. Given the algebra and the structure we
can fold the structure back to `a`.

For example a `catamorphism` is one such fold::

  cata :: Functor f => (f a -> a) -> (Fix f -> a)

There are list examples in:
https://www.schoolofhaskell.com/user/bartosz/understanding-algebras

* Just like recursive functions are defined as fixed points of regular
  functions, recursive (nested) data structures can be defined as fixed points
  of regular type constructors.
* Functors are interesting type constructors because they give rise to nested
  data structures that support recursive evaluation (generalized folding).
* An F-algebra is defined by a functor f, a carrier type a, and a function from
  f a to a.


Hylomorphism
~~~~~~~~~~~~

::

  import Data.Functor.Foldable
  import Data.List (splitAt, unfoldr)

  data TreeF c f = EmptyF | LeafF c | NodeF f f
    deriving (Eq, Show, Functor)

  mergeSort :: Ord a => [a] -> [a]
  mergeSort = hylo alg coalg where
    alg EmptyF      = []
    alg (LeafF c)   = [c]
    alg (NodeF l r) = merge l r

    coalg []  = EmptyF
    coalg [x] = LeafF x
    coalg xs  = NodeF l r where
      (l, r) = splitAt (length xs `div` 2) xs

  merge :: Ord a => [a] -> [a] -> [a]
  merge = curry $ unfoldr c where
    c ([], [])     = Nothing
    c ([], y:ys)   = Just (y, ([], ys))
    c (x:xs, [])   = Just (x, (xs, []))
    c (x:xs, y:ys) | x <= y = Just (x, (xs, y:ys))
                   | x > y  = Just (y, (x:xs, ys))

References
----------

* Recursion, traversal & folds are related
* https://en.wikipedia.org/wiki/Fixed-point_combinator

* https://en.wikipedia.org/wiki/Primitive_recursive_function
* https://en.wikipedia.org/wiki/Recursion_(computer_science)
* https://en.wikipedia.org/wiki/Corecursion
* http://www.tac-tics.net/blog/data-vs-codata
* https://www.schoolofhaskell.com/user/bartosz/understanding-algebras
* http://stackoverflow.com/questions/6941904/recursion-schemes-for-dummies

Recursion schemes:

* http://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/
* https://medium.com/@jaredtobin/practical-recursion-schemes-c10648ec1c29#.9lij6s5a8 On Kmett's recursion scheme library (has a good mergesort example)
* https://jozefg.bitbucket.io/posts/2014-05-19-like-recursion-but-cooler.html? On Kmmet's recursion schemes
* http://comonad.com/reader/2009/recursion-schemes/
* https://hackage.haskell.org/package/recursion-schemes
* http://fho.f12n.de/posts/2014-05-07-dont-fear-the-cat.html
* https://www.schoolofhaskell.com/user/edwardk/recursion-schemes/catamorphisms
* https://ulissesaraujo.wordpress.com/2009/04/09/hylomorphisms-in-haskell/
* https://ulissesaraujo.wordpress.com/2009/04/09/more-hylomorphisms-in-haskell/
* https://github.com/willtim/recursion-schemes/raw/master/slides-final.pdf

  * https://www.youtube.com/watch?v=Zw9KeP3OzpU Talk video

* https://en.wikipedia.org/wiki/Category:Recursion_schemes
* https://en.wikipedia.org/wiki/Catamorphism generalizations of folds of lists to arbitrary algebraic data types
* https://en.wikipedia.org/wiki/Anamorphism Dual of catamorphism - unfold
* https://en.wikipedia.org/wiki/Paramorphism extension of catamorphism “eats its argument and keeps it too”
* https://en.wikipedia.org/wiki/Apomorphism Dual of paramorphsim
* https://en.wikipedia.org/wiki/Hylomorphism_(computer_science) anamorphism followed by a catamorphism

* http://cgi.csc.liv.ac.uk/~grant/PS/thesis.pdf Algebraic Data Types and Program Transformation
* http://dl.acm.org/citation.cfm?id=2034807 A hierarchy of mendler style recursion combinators: taming inductive datatypes with negative occurrences".
