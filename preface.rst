About this book
---------------

The goal of the book is to systematically explain GHC Haskell from a
programmer's perspective so that he/she can easily find all the details
required for using the language at one place.  It is not the goal of this book
to describe the theory behind the implementation or the language. It explains
concepts or theory only where it is important for the effective use of the
language for practical programming.

..
  This is intended to be an open source, live and evolving book. I intend to keep
  uploading new material as it becomes ready. I will try to keep things in order
  but sometimes it may be out of order. I expect to keep improving the text over
  time. I also expect the readers to help me out in improving it by providing
  feedback and contributions in content.

How you learn?
--------------

The big picture vs tiny details.

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

This text goes from the first principles to higher level abstractions
incrementally, building higher level stuff on lower level stuff.  Each chapter
is self contained and has a comprehensive reference for everything related to
that concept.  In addition to systematically building new concepts on top of
the previous concepts we also try to discuss dual concepts alongside each
other for a better and deeper understanding.  We want to create a deeper
understanding of the concepts and inculcate intuition about abstractions and
how they are built in the first place so that the reader can easily apply the
same process to novel situations.  It is important to learn how to learn
Haskell!

When I started learning Haskell I dived into things as I encountered them. In
the absence of a good comprehensive reference it led to grokking code, reading
papers, struggling with category theory. I got completely bogged down with
stuff that I did not need for using the language, leading to big diversions,
wasting a lot of time. However, if I had access to a good reference where all
the related concepts are condensed together and shown in relation to each other
I could have learnt the big picture faster and then looked at details when I
needed. What I felt lacking is what this book tries to present.  Everything at
one place with a top down big picture.  Fully self-contained, including full
Haskell and GHC language reference so that the reader does not need to keep
searching for missing stuff at different places.

Haskell is overwhelming, there is a lot to imbibe. However, it is overwhelming
because the boon of Haskell is also the curse of Haskell. It has systematic
building blocks and you can use permutations or combinations of those building
blocks and as combinations multiply they grow rapidly. However, if we know the
building blocks, correspondences, duals etc. systematically we can just
abstract out most of stuff  and derive it rather than remembering everything in
an ad-hoc manner. That is the approach we are taking in this book.

For example if we use applicatives, arrows and monads, their duals, free/cofree
versions it easily becomes tens of concepts to learn but in fact they are all
manifestations of some sort of transformation and composition and if we know
how they relate to the basic concepts and to each other they are easy to
follow and remember.

One important aspect to building a deeper understanding is knowing the big
picture and how things relate to each other. For example how recursion, folds,
lists, monoids are related? When we know the precise relationships it is easier
to understand them better and remember them.

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

Forward and Reverse Reading Styles
----------------------------------

This book has been written in a dependency order, that is the most fundamental
concepts are introduced first and those building upon those are introduced
later, in that sequence, wherever possible. When you start to learn Haskell the
natural reading style is read the book from start to end. However, that is not
how it works practically. While learning Haskell and being an expert at the
language, you would still want to be able to read existing code written by
others and understand it, which may not be possible unless you know all the
features, concepts, syntax used in that piece of code.  This creates difficulty
until you have read and understood a lot of concepts.  To overcome this problem
this books introduces a reverse index as well. When you encounter a particular
syntax, concept or feature you can look it up in the index, which has a small
description as well as reference to full explanation of the concept in the
book. That way with the help of this book working as a dictionary you can
deciper any Haskell code even if you are not yet an expert in Haskell. This
helps you learn Haskell on the job.

Note on Jargon
--------------

Haskell has several dimensions and layers of abstraction and beautiful
mathematical concepts that fit together elegantly. However, to think about the
concepts systematically it is important to get familiarised with the vocabulary
and know the precise meanings of the terms involved.

This text tries to avoid jargon where possible but many terms that allow you to
express concepts in a concise and clear manner are necessary. We define the
terminology succinctly in each chapter. Sometimes terms are included just so
that the reader is able to understand other material on the topic, e.g. the GHC
manual.
