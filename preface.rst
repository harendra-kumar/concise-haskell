How you learn?
--------------

In my experience, there are two phases in learning a programming language. In
the beginner phase you learn in bits and pieces and mostly by example or by
looking at code. As you keep learning you keep putting things together and
building a bigger picture. Many times you have your aha moments when you put
the pieces of puzzle together yourself and figure out a pattern or high level
concept. This I call bottom up learning and this is usually the way to begin.

When you reach an intermediate stage you need to build a consistent big picture
to have a deeper understanding and reach the expert stage. Big picture should
be a consistent beautiful model that you can abstract and derive things from
first principles and therefore remember more effectively. The intermediate
learning material should help build and understand that model. I call this top
down learning.

Bottom up and top down can happen simultaneously or one after other but both
need to happen nevertheless and finally the learner must have a consistent
model in his head.

In this text we are presenting Haskell from the top down perspective assuming
you have learnt the basics.

Building Intuition
------------------

This text goes from the basic first principles to higher level abstractions
incrementally, building higher level stuff on lower level stuff.  Each chapter
is self contained and has a comprehensive reference for everything related to
that concept.  In addition to systematically building new concepts on top of
the previous concepts we also try to discuss the dual concpets alongside each
other for a better and deeper understanding.  We want to create a deeper
understanding of the concepts and inculcate intuition about abstractions and
how they are built in the first place so that the reader can easily apply the
same process to novel situations.  You will basically learn how to learn
Haskell.

Haskell is overwhelming, there is too much to imbibe. However it is
overwhelming because the boon of Haskell is also the curse of Haskell. It has
systematic building blocks and you can use any permutations or combinations of
those and as they multiply the numbers increase rapidly. However, if we know
the building blocks, correspondences, duals etc.  systematically we can just
abstract out most of stuff  and derive it rather than remembering in an ad-hoc
manner. That is the approach we are taking in this book.

For example if we applicatives, arrows and monads, their duals, free/cofree
versions it easily becomes tens of concepts to learn but in fact they are all
manifestations of some sort of transformation and composition and if we know
how they relate to the basic concepts and to each other they are easy to
remember.

One important aspect to building a deeper understanding is knowing the big
picture and how things relate to each other. For example how recursion, folds,
lists, monoids are related? When we know the precise relationships it is easier
to understand them better and remember them easily.

GHC Haskell reference
---------------------

One of the goals of this text is to be a concise but comprehensive reference to
GHC Haskell including all GHC extensions. This is different from the GHC manual
in conciseness and presentation. The GHC manual only describes the GHC specific
language extensions. They are described mostly from the GHC developer's
perspective and in the sequence they were developed rather than in a holistic
view of the language which makes it hard to find relevant material and build a
consistent picture quickly.

The goal of this text is to build one holistic view of all available language
features, starting from Haskell basics to GHC extensions, present all related
concepts at one place so as to provide the full consistent picture. We want to
provide maximum information in minimum space using a cheat sheet like format to
keep it concise.

It focuses purely on language and not other things in the Haskell ecosystem for
example operational features of the compiler or build tools etc.

This is written from a programmer's perspective who wants to learn the language
quickly and needs a handy reference to be able to search useful stuff quickly
when he needs it.  It tries to present the minimal concepts which are relevant
to a user of the language and avoids explaining detailed rationale, theoretical
underpinnings, background or pedagogic material unless when necessary.
Essential concepts are separated from deeper treatments where necessary.  The
descriptions of concepts are not formal or rigorous in any sense, they are only
for an intuitive understanding of the concepts.

Note on Jargon
--------------

Haskell provides several dimensions of abstraction and beautiful mathematical
concepts that fit together elegantly. To think about the concepts
systematically it is important to get familiarised with the vocabulary and know
the precise meanings of the terms involved.

This text tries to avoid jargon where possible but many terms that allow you to
express concepts in a concise and clear manner are necessary. We define the
terminology succinctly in each chapter. Sometimes terms are included just so
that the reader is able to understand other material on the topic, e.g. the GHC
manual.
