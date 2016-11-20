Haskell: A pure functional language
===================================

A functional language with denotational semantics.

Terminology
-----------

| Functional language | Functions are just like any other values, pass them as arguments or return them from functions, compose them together to create a composite function.
| Denotational Semantics | constructing mathematical objects (called denotations) that describe the
meanings of expressions from the programming language. An important tenet of denotational semantics is that
semantics should be compositional: the denotation of a program phrase should be
built out of the denotations of its subphrases.
| Equational Reasoning |
| Referential Transparency | any variable can be replaced with its actual value at any point of execution (because the variable can never change)
| Immutability |
| Side effect | A change in environment (global state, IO)  effected by a function
| Pure function | A function which always returns the same value. A
function without a side effect.
| Pure functional | No mutation of a data structure is allowed
| Lazy evaluation | Program execution is driven by IO, statements are not
executed unless they are needed by a computation driven by IO. There is no
sequential evaluation of all stattements in the control flow path.
| Bind | assign (bind) a value to a variable

Process Data
------------

TODO: replace with a picture

+-----------------------------------------------------------------------------+
| Case expressions: Map input values to output values                         |
+=============================+=================+=============================+
| Decompose and inspect input | Decision switch | Compose output              |
+-----------------------------+-----------------+-----------------------------+
| Pattern Match               | case            | Function Application        |
+-----------------------------+-----------------+-----------------------------+

+-----------------------------------------------------------------------------+
| Haskell Program: Equational composition                                     |
+============+================================================================+
| Equations  | f a b c = expr                                                 |
| (Functions)+----------------------------------------------------------------+
|            | let f a b c = expr                                             |
|            +----------------------------------------------------------------+
|            | where f a b c = expr                                           |
+------------+----------------------------------------------------------------+

Ensure Correctness
------------------

+--------------------------------------------------+
| Type Signatures: Assign types to values          |
+========================+=========================+
| Type level programming | Value level programming |
+------------------------+-------------------------+
| Specify Types          | Specify Functions       |
+------------------------+-------------------------+
| Functions              | Functions               |
+------------------------+-------------------------+

Types - Correctness
-------------------

Every chunk of data has an assigned form - a type.
Functions work only on a specific form
The form of the data (plug) and the function (socket) must match at compile time.
Picture of a plug and socket. There are millions of these plugs and sockets. Or a zigsaw puzzle which only fits correctly in one way.
Picture of a zigsaw puzzle with Haskell written on it.
Look ma it just works!

Function
--------

The essence of Haskell is the arrow (->) which represents a transform. We have
things and we have transforms between things (ding ding category theory).

thing
thing -> thing
thing -> thing -> thing
thing -> (thing -> thing)

The thing could be kinds, types or values or a function.

Haskell is about functions, the rest is details.

* Graphic to show the reflection of the word "Haskell" showing as the word "Function"
* A function is the swiss army knife of Haskell. Graphic?
* Functions are central to everything including kinds, types and values
* The only primitive in haskell is a function.
* The slide with the whole square frame around it built with the word Function or lambda.

Types of Functions
------------------

There are different types of functions:

* Transform - turn one thing into another
* Compose - put two things together - constructor (for data), composition (for functions)
* any thing (function or data) can be composed
* Decompose - pick a combined thing apart into its components - pattern match

Data and Operations
-------------------

* Data is represented by functions - constructors - compose pieces into a single type
* Operations are represented by functions - transform

* Types: Specification of data, functions reify the specification into real physical data
* Compose multiple pieces of data via constructors and decompose them via pattern matching
* Functions: take one or more data types and turn it into another output data type
* Composition - Transform and compose data via functions, compose functions

* Show the visual representation of a simple program

Polymorphism - Expressive Power
-------------------------------

* Typeclasses: attach functions to types, the function can be different for different types.
* Polymorphic functions: Same function can work on many types
* Higher order functions

* Reuse -> conciseness -  expressive power - polymorphism is about reuse - you can abstract out the common parts in processing multiple related but slightly different types
* Composition -> reuse
* typeclass hierarchy

Topics
------

* Functions

  * Recursion
* Types
* TypeClasses
* Polymorphism
* Specific Type Classes - Abstractions

  * Functor, Applicative, Monad
  * Monoid, semigroup, ...
* Higher level design patterns

  * Zippers
  * Free Monads, cofree etc.
* Data Structures (containers)
* IO
* Category Theory
* Domain specific packages

  * Web
  * Parsing

Conclusion
----------

