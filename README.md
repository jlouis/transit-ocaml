# Encode/Decoder for the Transit format, OCaml version

This repository implements the Transit format for OCaml. Current status is that it doesn't work entirely as expected. It is still under construction, so please bear this in mind. All signatures are subject to change at the moment if I realize something is not possible.

## Implementation strategy

Use YoJson to parse data into an polymorphic variant. Then process this one by hydrating it into a Transit polymorphic variant. The opposite path does the inverse. Use Core's Int.Map.t for a cache. Use a Functor to provide for extensions. Allow chained functor invocations to add extensions in a layered fashion.

## Rationale for the implementation

Transit has been implemented for:

* Python - Dynamic typing, Object Orientation
* Ruby - Dynamic typing, Object Orientation
* Clojure - Dynamic typing, Objects al'a Carte-orientation
* Javascript - Dynamic typing, Prototypical OO
* Java - Static typing, Something not-quite-Alan-Kay-objects-but-more-like-Stoustrup-perhaps-Gosling/Steele-objects

OCaml fits nicely because it has everything different:

* Static typing discipline
* Practical functional
* Algebraic Data Types with an expressive type system

So by implementing Transit in OCaml, we can find oddities of the format w.r.t a whole different class of programming languages.

