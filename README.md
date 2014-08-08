# Encode/Decoder for the Transit format, OCaml version

This repository implements the Transit format for OCaml.

# WARNING

This is currently *experimental*. Everything is subject to change. It is not clear at this point if the approach is the right one. Details had
to be rewritten a couple of times whenever I learned something new about how to handle this.

## Implementation strategy

Use yajl bindings to parse data into an polymorphic variant, running hydration as we go along on the path. The encoding simple walks the tree and outputs the right tags. The decoder makes use of a current context which is essentially a typed stack of what we are currently doing. End-of-array and End-of-map callbacks then inspect the current stack contents in order to decide what to do.

Extensions are implemented by plugging in a functor with the extension types on top of the existing implementation.

Correctness is had by stealing transit-pythons test framework and implementing tests by means of oUnit2 in the same style as the
python code does. Implement the format by handling each of the tests one by one, extending the implementation from a base as we
go along and find things we can't really handle.

To test correctness, we use the sexp library to derive a canonical s-expression for our transit-polymorphic-variant. A simple equality check
on the sexps then provides the needed equality in our implementation. It also provides a very easy way to format discrepancies among
values.

## Rationale for the implementation

Transit has been implemented for:

* Python - Dynamic typing, Object Orientation
* Ruby - Dynamic typing, Object Orientation
* Clojure - Dynamic typing, Objects al'a Carte-orientation
* Javascript - Dynamic typing, Prototypical OO
* Java - Static typing, Something not-quite-Alan-Kay-objects-but-more-like-Stoustrup-perhaps-Gosling/Steele-objects

OCaml fits nicely because it has everything different:

* Static typing discipline
* Practical functional — In the sense of supporting imperative features directly in the language
* Algebraic Data Types with an expressive type system
* An extremely powerful module system which allows you to program with functions at the module level (functors)

So by implementing Transit in OCaml, we can find oddities of the format w.r.t a whole different class of programming languages. I have tried to implement the spec clean-room and not looking at the existing implementations at all. This is in order to weed out eventual missing things in the spec.

# Oddities found

The format is generally sane and straightforward to work with. Though when implementing it, certain interesting things happen along
the way. The flowchart diagrams help a lot in order to figure out when to make the cache lookup and when to process the data in question.

· I started out by operating directly on a JSON structure. This didn't work well at all. Switching to yajl helped the implementation
  a lot since transit is so much better suited to stream-based parsing. Perhaps this should be mentioned explicitly you should be looking
  at a streaming parser over operating on a parsed format.
· What is the distinction between ground types and extension types, really? Yes, one is encoded isomorphically and the other is built out of
  ground composites. But you can't really implement the format without handling all extensions properly. Also, the distinction breaks down
  once you start considering caching. Since some extensions (keywords, symbols) are cachable. Can you define new primitives to cache as
  extensions, I wonder…
· 64 bit Integers in OCaml is a pain. The language uses the lowest bit as a tag-bit so it can discriminate pointers in the garbage collector.
  In effect, 63 bit values are easy to represent, but 64 bit values requires a separate notion of Int64.t all over the place. I'd definitely consider
  building variants of Transit which are not spec-conforming, but ignores that last bit or maps them into BigInts directly.
· Stratifying ground types and extension types was something I did in the beginning. But it looks like that is a mistake. Better just define
  everything in one big type.
· Why the "^ " and "~#cmap" distinction? Are there certain languages which can't handle this easily?
· It took some time before I figured out strings of the form "~#<TAG>" is part of the spec. Could be me reading the spec like the devil
  reads the bible, but perhaps a clarification around the representation of tags in strings would help.
· The interaction of caching and eventual substitution together with string decoding was not entirely clear when I read the spec. It seems
  caching comes first, and indeed this makes the code work as intended.
· The hardest part of the specification has to do with the self-referential nature of Sets and Maps. To implement these, you need a
  comparator-function over the transit term. But you need the term to define the comparator and you need the comparator in order to
  define the types of Sets and Maps! My solution was to utilize Polymorphic compare—a feature in OCaml—where comparison happens
  structurally on the internal representation of the values. This solves the problem, because Poly-comparators work for any type and gives
  a "canonical" comparator for any valid term.
· It is not entirely clear how to define a system which allow for extension yet in OCaml. I feel extension must happen statically by the correct
  use of functors, but I have yet to figure out a satisfactory representation of this.
· The keyword/symbol distinction seem quite odd to a programmer who has not seen lots of Common Lisp nor Clojure. In Erlang, of which I   am deeply familar, we only have "atoms" which are roughly symbols. Yet, they run the problem of being system-global so if you generate e  nough of those, you will eventually exhaust the systems atom-table and the node() will cease operating. Is the reason for the KW/Symbol distinction a way to solve this limitation? I read keywords evaluate to themselves, whereas symbols do not, but I would really like to know more about you feel the need for both representations in the transit format.
· One strength of OCaml is that we can just invent a new type for Transit. This makes it very easy to map Transit onto ocaml since every we can simply abstract away the underlying representation of leaf values in the transit AST. If we did not have a native URI or UUID implementation—which we have—we could have opted for a string representation. The power here is we can exploit the type system and provide a rich description of what a transit term really is. In a more dynamic system (with less abstraction features) we would have forced to map transit values onto the underlying language directly. Here we can defer that decision to the user of the library.
· I couldn't really read the spec the first time around and messed up cache generation. I don't really have a test for the cache wrapping but it could be good to have since I have no idea if the cache is wrapping as it should.
· What happens if "^ " is in the middle of an array?
· What happens if "~#list" and the like is in the middle of an array or somewhere it is not recognized as special?
· The AST structure can be described by an EBNF grammar. Why isn't it?
· How are maps with multiplicity on keys handled ? That is the map

	{ 1: 3
	, 1: 5 } ?
	
· Where are string caching done? "^ "-maps only or also in "~#cmap"-maps?
