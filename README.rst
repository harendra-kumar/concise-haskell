Haskell in a nutshell
=====================

A concise Haskell reference.

* Why haskell
* Haskell overview
  * Program structure (branching, functions)
* Semantics
* Basic syntax (branching, function, data)
* Type theory
* Functions
* Types
* Typeclasses
* Polymorphism
* Functor family
* Abstractions

  * Algebra
  * Category theory
* Basic abstractions
* Advanced abstractions
* Basic data structures
* Advanced data structures
* Packages
* Inside haskell
* Full syntax cheatsheet?
* Full glossary
* Learning resources
* Other resources

How you learn
-------------

There are two phases in learning a programming language. In the beginner phase
you learn in bits and pieces and mostly by example or by looking at code. As
you keep learning you keep putting things together and building a bigger
picture. Many times you have your aha moments when you put the pieces of puzzle
together yourself and figure out a pattern or high level concept. This I call
bottom up learning and this is usually the way to begin.

When you reach an intermediate stage you need to build a consistent big picture
to have a deeper understanding and reach the expert stage. Big picture should
be a consistent beautiful model that you can abstract and derive things from
first principles and therefore remember more effectively. Therefore the
intermediate learning material should present that model. I call this top down
learning.

Bottom up and top down can happen simultaneously or one after other but both
need to happen nevertheless and finally the user must have a consistent model
in his head.

In this text we are presenting Haskell from the top down perspective after you
have learnt the basics.

GHC Haskell reference
---------------------

One of the goals of this text is to be a concise but comprehensive reference to
GHC Haskell including all GHC extensions. This is different from the GHC manual
in conciseness and presentation. It should essentially have all language
concepts including basics which are not in the GHC manual as well as those
which are present in the GHC manual but in more concise manner. It does not
include the operational features of the compiler itself e.g. ghc options etc.

This is written from a programmer's perspective. It tries to present solely the
material which is relevant to a user of the language and avoids explaining any
details about the rationale, theoretical fundamentals, background or pedagogic
terminology.

Note that this is not for teaching Haskell to beginners, this is a reference
for intermediate level programmers who are out of the beginner phase and have a
working knowledge of the language.

Note on Jargon
--------------

This text tries to avoid the jargon where possible but many terms which allow
you express concpets in a concise and clear manner are necessary. We define the
term before it is used so that you have an idea what it means.

By defining all the necessary vocabulary in a succinct manner it also allows
you to read any other documentation on Haskell with ease.

Format
------

We roughly describe the following for each concept:

* Name & link to details
* GHC extension & version where the feature is available (optional)
* Concise description (cheatsheet)
* Description
* Examples
* Important points to note (optional)
* External references
* Games or a learning system to build intuition quickly

Multiple views
--------------

The idea is to build multiple views from the same master text, including a
detailed reference and just a cheatsheet.

Contributions
-------------

Contributions are welcome! I especially need help in examples and writing games
or apps to learn the concepts.
