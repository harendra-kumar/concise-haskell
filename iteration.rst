Recursion
=========

An expression can be defined recursively by referring to the value being
defined within the definition.

Iteration and Fix Point
-----------------------

Iteration of a function is defined as applying the function repeatedly to its
previous result.  As an example, we can apply `sqrt` to a number iteratively::

  takeWhile (/= 1) $ iterate sqrt 2

`sqrt` converges to 1 if we keep iterating it starting with any number. 1 is
called the fix-point of sqrt. In general when `c = f c`, `c` is called the fix
point of a function.

Recursion and iteration are two different ways of thinking about the same
thing.

In the next few sections we will investigate how to implement iteration using
recursion and lazy evaluation.

Recursive Expression
--------------------

Any recursive definition can be reduced to the following normalized version::

  x = f x

+----------------------+------------------------------------------------------+
| When                 | What happens?                                        |
+======================+======================================================+
| `f` does not use `x` | `x` reduces to a non-recursive definition            |
+----------------------+------------------------------------------------------+
| `f` scrutinizes `x`  | evaluation of `x` results in an infinite loop.       |
|                      | Any side effects before the scrutiny are produced    |
|                      | in the loop.                                         |
+----------------------+------------------------------------------------------+
| `f` uses `x` in      | the data type of `x` has to be recursive. `x`        |
| output construction  | represents a lazy recursive infinite data            |
|                      | construction. Pattern match on the structure         |
|                      | retrieves a component which is exactly the same      |
|                      | as original `x` and can be pattern matched again.    |
+----------------------+------------------------------------------------------+

We can see `x` unfold clearly by repeatedly substituting the term `x` in the
expression for its own definition::

  f x
  f (f x)           -- after substituting x by (f x)
  f (f (f x))       -- after substituting x by (f x)
  ...

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

    data X = Cons X Int
    let x = Cons x 1
    in let Cons y 1 = x
           Cons z 1 = y
           ...

Building a Function by Iteration
--------------------------------

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

Alternate Definition
~~~~~~~~~~~~~~~~~~~~

The higher order function `f` shown above can also be written as::

  f g x =
      case (sqrt x == x) of
        True -> x
        False -> g (sqrt x)

From this definition, it is obvious that `f` not only uses `g` but also the
argument of the output function `f g`, which is `x`, in its definition. When we
pass `g` to `f` it will return us a function which takes one argument `x` and
is defined in terms of that argument.

Note that depending on how `f` is defined in terms of `g` and `x`, the
stop condition may never be met and the iteration may never stop.

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
perhaps be iterate. The function `fixer` is more like the one which generates
an iterator with a condition to find the fixed point of a function.

Using direct recursion::

  fixit g x =
      case (g x == x) of
        True -> x
        False -> fixit g (g x)

  >> fixit sqrt 2
  1.0
  >> fixit cos 2
  0.7390851332151607

References
----------

* Recursion, traversal & folds are related
* https://en.wikipedia.org/wiki/Fixed-point_combinator

* https://en.wikipedia.org/wiki/Primitive_recursive_function
* https://en.wikipedia.org/wiki/Recursion_(computer_science)
* https://en.wikipedia.org/wiki/Corecursion

Folds with recursion:

* https://en.wikipedia.org/wiki/Catamorphism generalizations of folds of lists to arbitrary algebraic data types
* https://en.wikipedia.org/wiki/Anamorphism Dual of catamorphism - unfold
* https://en.wikipedia.org/wiki/Paramorphism extension of catamorphism “eats its argument and keeps it too”
* https://en.wikipedia.org/wiki/Apomorphism Dual of paramorphsim
* https://en.wikipedia.org/wiki/Hylomorphism_(computer_science) anamorphism followed by a catamorphism

* https://ulissesaraujo.wordpress.com/2009/04/09/hylomorphisms-in-haskell/
* https://ulissesaraujo.wordpress.com/2009/04/09/more-hylomorphisms-in-haskell/

* http://cgi.csc.liv.ac.uk/~grant/PS/thesis.pdf Algebraic Data Types and Program Transformation
* http://dl.acm.org/citation.cfm?id=2034807 A hierarchy of mendler style recursion combinators: taming inductive datatypes with negative occurrences".
