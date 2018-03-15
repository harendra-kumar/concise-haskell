Optimization
------------

Three powerful and interrelated optimizations that require programmer awareness
are:

* INLINING
* SPECIALIZATION
* Rewrite-rules

Another powerful optimization is strictness(UNPACK, Bang patterns).
These four optimizations can make or break performance. Other than these most
other optimizations are mostly simple choices enabled or disabled.

One option that you can use to gain some performance for free is to use the
llvm backend.

What options each level enables?

* https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#optimisation-levels

Inlining
--------

What Happens in Inlining?
-------------------------

Also called unfolding. The compiler pass that performs inlining is called the
"simplifier".

GHC guarantees to inline precisely the code that you wrote, no more and no
less. It does this by capturing a copy of the definition of the function to use
for inlining (we call this the “inline-RHS”), which it leaves untouched, while
optimising the ordinarily RHS as usual. For externally-visible functions the
inline-RHS (not the optimised RHS) is recorded in the interface file.

What gets inlined?
------------------

GHC only inlines the function if:

* there is some reason (no matter how slight) to suppose that it is useful to
  do so.
* it is fully applied

* Mutually recursive functions: GHC ensures that inlining cannot go on forever:
  every mutually-recursive group is cut by one or more loop breakers that is
  never inlined. GHC tries not to select a function with an INLINE pragma as a
  loop breaker, but when there is no choice even an INLINE function can be
  selected, in which case the INLINE pragma is ignored.

* Recursive functions: For a self-recursive function, the loop breaker can only
  be the function itself, so an INLINE pragma is always ignored.

How to inline?
--------------

INLINE and INLINABLE/INLINEABLE pragmas retain a copy of the original RHS for
inlining purposes, and persists it in the interface file, regardless of the
size of the RHS.

Unlike INLINE, it is OK to use an INLINABLE pragma on a recursive function. The
principal reason do to so to allow later use of SPECIALISE.  If you mark
function f as INLINABLE, then you can subsequently SPECIALISE it in another
module because the definition is available in the interface file.

A function not marked INLINE or INLINABLE can also be inlined by GHC depending
on its size, inlining threshold etc. However, by annotating f as INLINABLE, you
ensure that f‘s original RHS is inlined, rather than whatever random optimised
version of f GHC’s optimiser has produced.

At the definition site: An INLINE pragma for a function can be put anywhere its
type signature could be put.

* {-# INLINE f #-} - “please inline me wherever I am used”
* {-# NOINLINE f #-} - noinline can be useful to move infrequently called code
  out of the way which can speedup the instruction caching and fetching.

At the call site:

* {-# INLINABLE f #-} - “feel free to inline me at the call site, my definition
  is available in the interface file". The decision at the call site will be
  affected by the inlining threshold, optimisation level etc.

* inline (f x) - “please inline this call”. To guarantee a try to inline, the
  function has to be INLINABLE so that its definition is exported via the
  interface file.

INLINE is equivalent to INLINABLE + "inline" at all the call sites.

Phase Control
~~~~~~~~~~~~~

Phase 0 is the last phase.

“INLINE[k] f” means: do not inline f until phase k, but from phase k onwards be
very keen to inline it.
“INLINE[~k] f” means: be very keen to inline f until phase k, but from phase k
onwards do not inline it.
“NOINLINE[k] f” means: do not inline f until phase k, but from phase k onwards
be willing to inline it (as if there was no pragma).
“NOINLINE[~k] f” means: be willing to inline f until phase k, but from phase k
onwards do not inline it.

Compiler Options
~~~~~~~~~~~~~~~~

::

  -fexpose-all-unfoldings - equivalent to marking everything INLINABLE
  -fignore-interface-pragmas - Do not use the interface pragmas available in
  other modules.
  -fomit-interface-pragmas - Do not generate any interface pragmas for consumers
  to use. equivalent to marking everyhting NOINLINE,
  NOSPECIALIZE.

  -funfolding-creation-threshold=⟨n⟩
  -funfolding-dict-discount=⟨n⟩
  -funfolding-fun-discount=⟨n⟩
  -funfolding-keeness-factor=⟨n⟩
  -funfolding-use-threshold=⟨n⟩

  -fmax-inline-alloc-size=⟨n⟩
  -fmax-inline-memcpy-insn=⟨n⟩
  -fmax-inline-memset-insns=⟨n⟩
  -fno-pre-inlining

  -fsimplifier-phases=⟨n⟩
  -fmax-simplifier-iterations=⟨n⟩

  -fstatic-argument-transformation

  -ddump-inlinings (use with -dverbose-core2core for detailed output)
  -ddump-simpl
  -ddump-simpl-iterations
  -ddump-simpl-stats

It is not clear what all is exposed by default when no INLINE pragams are
specified. Are small functions still liable to be inlined? Are class operations
still liable to be inlined?

Specialization
--------------

Definition side options:

+---------------------------+-------------------------------------------------+
| SPECIALISE                | Specialise this globally at the given type      |
+---------------------------+-------------------------------------------------+
| INLINABLE                 | Can be specialised at the callsite              |
+---------------------------+-------------------------------------------------+

Use side options:

+---------------------------+-------------------------------------------------+
| SPECIALISE                | Specialise this at the given type in this module|
+---------------------------+-------------------------------------------------+
| -fspecialise              | Specialise all calls to locally defined         |
|                           | functions in this module                        |
+---------------------------+-------------------------------------------------+
| -fcross-module-specialise | Specialise calls to imported functions marked   |
|                           | INLINABLE at the definition. Needs -fspecialise |
+---------------------------+-------------------------------------------------+
| -fspecialise-aggressively | Specialise regardless of size, local or         |
|                           | imported functions marked INLINABLE             |
+---------------------------+-------------------------------------------------+

::

  -fspec-constr
  -fspec-constr-keen
  -fspec-constr-count=⟨n⟩
  -fspec-constr-threshold=⟨n⟩

  -fliberate-case
  Default:off but enabled with -O2.
  Turn on the liberate-case transformation. This unrolls recursive function once
  in its own RHS, to avoid repeated case analysis of free variables. It’s a bit
  like the call-pattern specialiser (-fspec-constr) but for free variables rather
  than arguments.
  -fliberate-case-threshold=⟨n⟩

  -Wmissed-specialisations
  -Wall-missed-specialisations
  -ddump-spec

Specialization is not possible with polymorphic recursion.

* https://stackoverflow.com/questions/18341146/specialization-of-polymorphic-functions

Rewrite Rules
-------------

Rewrite rules can interact with the inliner (also called simplifier).

::

  -fenable-rewrite-rules
  -fno-enable-rewrite-rules -- note that GHC builtin rules still fire.
        However rules in GHC.Base or any other libraries are disabled.

  -ddump-simpl-stats
  -ddump-rule-firings
  -ddump-rule-rewrites
  -ddump-rules
  -frule-check
  -Winline-rule-shadowing

Strictness
----------

using a bang pattern in a function argument does not seem to have exactly the
same effect as using $! when calling the function. Using bang pattern in
argument seems to be more efficient. Also, if we use both together the effect
is much adverse. Need more research on this.

::

  -fstrictness
  -fstrictness-before=⟨n⟩

  -funbox-small-strict-fields
  -funbox-strict-fields

  -ffun-to-thunk
  -fmax-worker-args=⟨n⟩
  -fno-state-hack
  -fpedantic-bottoms

  -flate-dmd-anal
  Default:off
  Run demand analysis again, at the end of the simplification pipeline. We found
  some opportunities for discovering strictness that were not visible earlier;
  and optimisations like -fspec-constr can create functions with unused arguments
  which are eliminated by late demand analysis. Improvements are modest, but so
  is the cost. See notes on the Trac wiki page.

  -ddump-stranal
  -ddump-str-signatures
  -ddump-worker-wrapper

Batching, CSE
-------------

::

  -fcse
  -fstg-cse
  -fcmm-elim-common-blocks
  -fcmm-sink

  -ffloat-in
  -ffull-laziness

  -ddump-cse

Concurrency, Parallelism
-------------------------

::

  -feager-blackholing
  -fomit-yields

Code generation
---------------

::

  -fregs-graph
  -fregs-iterative
  -fvectorise
  -fvectorisation-avoidance

  -fllvm

General diagnostics
-------------------

::

  * -dppr-debug - works in conjunction with other options
  * -dverbose-core2core - works in conjunction with other options (-ddump-inlinings)
  * -dshow-passes
  * -ddump-core-stats
  * -ddump-simpl-stats
  * -dverbose-stg2stg
  * -dstg-stats
