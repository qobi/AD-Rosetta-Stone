# AD Rosetta Stone

Examples of Automatic Differentiation (AD) in many different languages and systems.

## Automatic Differentiation Examples Table

We are making a table whose columns consist of AD example programs and whose rows are implementations.  The purpose of this table is to provide a resource comparing APIs and convenience across AD systems: a bit of an AD Rosetta Stone.  It may also be useful for comparing speed across implementations, e.g., for identifying and diagnosing outliers.

This repository holds source code files, each of which implements one example in one AD system, along with a variety of support files and scaffolding.

## The Example Programs

There are five examples.

* A toy multilayer perceptron, or **mlp** (F / R)

  This is a tiny classic vanilla multilayer perceptron neural network, trained by gradient descent.  (The gradient of such a system would typically be computed by backpropagation, aka reverse-mode AD.)

* **saddle** point finding (FF / FR / RF / RR)

  This example finds a saddle point by nested optimization. It does not make use of the “fixedpoint” trick.

* **particle** path optimization (FF / FR / RF / RR)

  An inner process integrates an ordinary differential equation of a charged particle in a potential field, where the force on the particle is calculated from the field using AD.  An outer process alters control parameters of the potential field using gradient descent on the squared distance between the particle's intersection with a plate and a target location.

* **probabilistic lambda calculus** interpreter (F / R)

  Free parameters of a probabilistic lambda calculus program are optimized by gradient descent on the log likelihood of a dataset induced by running the program using an interpreter.

* **probabilistic prolog** interpreter (F / R)

  Free parameters of a probabilistic prolog program are optimized by gradient descent on the log likelihood of a dataset induced by running the program using an interpreter.

## Forward vs Reverse Mode

Two of the examples (*saddle* and *particle*) involve nested use of AD.  Each AD operation can be done using either forward or reverse, leading to variants listed in parenthesis above.  Some systems support both scalar and vector forward mode, the latter sometimes called “stacked tangents.”  When this would make sense for a particular benchmark (currently only *mlp*) instead of just an F variant, there are Fs and Fv variants.

## Coding Style

### Between-System Isomorphic Code, Data, and Names

Stylistically, there is an attempt to keep the code as precisely isomorphic as possible between implementations, at the expense of idiomatic constructions.  E.g., `foldl (+) 0` instead of `sum` in Haskell.  To the extent possible, the code should be me a mechanical-like translation, preserving code and data organization, and the abstract syntax.

### Type Declarations

Where relevant, and to the extent possible, type inference is used instead of explicit declarations.

### Between-Example Common AD Interface and Utility Functions for Each System

For each system, if the AD interface or other utility functions need tweaking, this is done in a common file shared between all the examples used in that system.

### Forward vs Reverse Mode API

The F vs R variants are kept as identical as possible, ideally changing only `gradient_R` to `gradient_F` and such.

## Please Help!

We'd like to add rows to the table.  Since there are already a bunch of systems fleshed out (Haskell ad, sml, Scheme R6RSAD, ocaml, VLAD/Stalingrad), this should be straightforward for anyone conversant in a particular language and an appropriate AD subsystem.  You don't even have to actually understand the innards of the interpreters in order to port them.

Note that some of the examples have been implemented in Fortran or C-based AD systems (Tapenade, ADIFOR, ADIC, ADOLC, etc).  The more dynamic examples can be difficult to port to languages like that, at least without Procrustean effort.  So if you're interested in adding a language to the table but don't want to do all the examples, that is just fine.  We are happy with a sparse table.

We would also welcome new examples.  Higher dimensional examples would be of particular interest, given that all but one of the current examples are extremely low dimensional.

## Idiomatic Variants and Other Coding Choices

If people really want to make more idiomatic implementations of the examples, to show off language facilities to better effect, we'd be happy to make a new branch for that.  But we'd like to maintain the above criteria on the master branch.

We are of course aware that the uses of Forward and Reverse mode here are often inappropriate.  In particular, using Reverse on a scalar function is silly.  These examples are expository and for testing, and are not intended as showcases of efficient code.
