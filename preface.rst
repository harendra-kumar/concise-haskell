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

GHC Haskell reference
---------------------

One of the goals of this text is to be a concise but comprehensive reference to
GHC Haskell including all GHC extensions. This is different from the GHC manual
in conciseness and presentation. The GHC manual only describes the GHC specific
language extensions. They are described mostly from the GHC developer's
perspective and in the sequence they were developed rather than in a holistic
view of the language which makes it hard to find relevant things and build a
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
The descriptions of concepts are not formal in any sense, they are only for an
intuitive understanding of the concepts.

Note on Jargon
--------------

This text tries to avoid the jargon where possible but many terms which allow
you to express concepts in a concise and clear manner are necessary. We define the
term before it is used so that you have an idea what it means.

By defining all the necessary vocabulary in a succinct manner it also allows
you to read any other documentation on Haskell (e.g. GHC manual) with ease.

Format
------

We roughly describe the following for each concept:

* Name & link to details
* GHC extension & version where the feature is available (optional)
* Concise description (cheat sheet)
* Description
* Examples
* Important points to note (optional)
* External references
