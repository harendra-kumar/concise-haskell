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

  x = f x -- implies f :: a -> a

We can see `x` unfold clearly by repeatedly substituting the term `x` in the
expression for its own definition::

  f x
  f (f x)           -- after substituting x by (f x)
  f (f (f x))       -- after substituting x by (f x)
  ...

This is in fact how we defined iteration earlier i.e. applying a function
repeatedly on the previous result. Though we do not have much control over it.

In the following discussion we assume that `f` is strict in `x`. If `f`
discards `x` then the definition just reduces to a trival non-recursive one.
For example::

    x = f x where f = const 10 -- x = 10

Recursive Concrete Values
-------------------------

When `x` is of concrete type and not a function, we can easily deduce the
following results for `x = f x`:

+----------------------+------------------------------------------------------+
| When                 | What happens?                                        |
+======================+======================================================+
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

A recursive definition can be constructed but does not make sense for concrete
values, it just results in an infinite loop, whereas corecursion can be used in
a fruitful manner to generate infinite data structures.

Let us see some examples:

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

When we evaluate `fixSqrt 256`, it results in a call to `fixSqrt 16` in the
first step and then `fixSqrt 4` in the next step, and so on. Finally when the
argument `x` passed to `fixSqrt` becomes very close to 1 then we hit the `True`
case and the value gets evaluated to `x` i.e. 1.0.

For termination, a recursive function must have a case where it does not
recurse further. Even then it is possible that it never hits the termination
condition.

Iterative Wrapper
~~~~~~~~~~~~~~~~~

If we look carefully the body of `fixSqrt` is a function of `fixSqrt` and `x`
the argument, we can write it explicitly in terms of a function `f` that is a
function of `fixSqrt` and `x` the argument of `fixSqrt`. If we rename `fixSqrt`
to `g` instead, we can write it as::

  g = f g
    where
      f g x =
          case (sqrt x == x) of
            True -> x
            False -> g (sqrt x)

Notice this is exactly the same as the general recursive expression `x = f x`
that we discussed earlier.  In fact, any recursive function can be expressed in
this form.

Also notice that `f` is not a recursive function.  We can read `f` as "check if
x is the same as `sqrt x`, if not call the function `g` on on `sqrt x` i.e.
perform the next iteration", there is no recursion.  `f` just represents one
step or a single iteration in the recursion process.

The explicit recursion is limited to the expression `g = f g`. As we saw
earlier this expression is equivalent to applying `f` iteratively over `g`.
However unlike concrete values the result of every iteration is a function
which may terminate when applied.

As a note, just like recursion did not make sense in case of concrete values,
corecursion does not make sense for functions as cannot be constructed using
data constructors.

Fix - Recursion by Iteration
----------------------------

We can write a utility function to iterate with a function `f`, we will call it
`fix`::

  fix :: (a -> a) -> a
  fix f = let x = f x in x

Examples
~~~~~~~~

::

  -- corecursion
  f x = 1 : 1 : zipWith (+) x (tail x)
  take 10 (fix f)

  -- recursion
  f g x =
      case (sqrt x == x) of
        True -> x
        False -> g (sqrt x)
  fix f 10

Notice that if you simply remove the `f` from the definitions above you will
get the recursive definitions.

The Y-Combinator
~~~~~~~~~~~~~~~~

fix is also called the fixed-point combinator or the Y combinator in lambda
calculus discovered by Haskell B. Curry::

  fix f = f (fix f)                -- Lambda lifted
  fix f = let x = f x in x         -- Lambda dropped

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
