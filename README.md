# Encode/Decoder for the Transit format, OCaml version

This repository implements the Transit format for OCaml.

# WARNING

This is currently *experimental*. Everything is subject to change. It is not clear at this point if the approach is the right one. Details had
to be rewritten a couple of times whenever I learned something new about how to handle this.

# Completeness

The current version implements most of the spec including all tests. But it omits:

* Arbitrary precision decimals (Tag ~f). Simply because I have not got around to implement them yet. Do note there are no test cases requiring these yet.
* Links — While i believe these are highly important in the long run, they are not part of the examples yet and as such are kept out of the current implementation. The goal has been to target other aspects of the spec first.

## Implementation strategy

Use yajl bindings to parse data into an polymorphic variant, running hydration as we go along on the path. The encoding simple walks the tree and outputs the right tags. The decoder makes use of a current context which is essentially a typed stack of what we are currently doing. End-of-array and End-of-map callbacks then inspect the current stack contents in order to decide what to do.

Extensions are implemented by plugging in a functor with the extension types on top of the existing implementation.

Correctness is had by stealing transit-pythons test framework and implementing tests by means of oUnit2 in the same style as the
python code does. Implement the format by handling each of the tests one by one, extending the implementation from a base as we
go along and find things we can't really handle.

To test correctness, we use the sexp library to derive a canonical s-expression for our transit-polymorphic-variant. A simple equality check
on the sexps then provides the needed equality in our implementation. It also provides a very easy way to format discrepancies among
values.

To test rendering correctness, we verify that the JSON representation is string-equal to our rendering. This doesn't work for maps where the key order is arbitrary or for sets. Finally, what I believe is a bug in the official exemplar int renderer is ignored, and also are Double values since there are many ways to render at represent those.

## Rationale for the implementation

Transit has been implemented for:

* Python - Dynamic typing, Object Orientation
* Ruby - Dynamic typing, Object Orientation
* Clojure - Dynamic typing, Objects à la Carte-orientation
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

## Benign errors

When implementing the spec, you will invariably run into simple trouble because you misunderstood the specification, or because the specification says something you just read incorrectly. I don't believe these errors are problematic, but I list them here anyway:

* I started out by operating directly on a JSON structure. This didn't work well at all. Switching to yajl helped the implementation
  a lot since transit is so much better suited to stream-based parsing. Perhaps this should be mentioned explicitly you should be looking
  at a streaming parser over operating on a parsed format.
* What is the distinction between ground types and extension types, really? Yes, one is encoded isomorphically and the other is built out of
  ground composites. But you can't really implement the format without handling all extensions properly. Also, the distinction breaks down
  once you start considering caching. Since some extensions (keywords, symbols) are cachable. Further, string representations of a lot of values are cacheable when used as map keys. It also means user-extensions can be cached…
* 64 bit Integers in OCaml is a pain. The language uses the lowest bit as a tag-bit so it can discriminate pointers in the garbage collector.
  In effect, 63 bit values are easy to represent, but 64 bit values requires a separate notion of Int64.t all over the place. I'd definitely consider
  building variants of Transit which understands this and maps into 63 bit integers, breaking to Big integers for the remaining values. But it makes the encoder a bit more involved because it has to map back and forth between the representations.
* Stratifying ground types and extension types was something I did in the beginning. But it looks like that is a mistake. Better just define
  everything in one big polymorphic invariant (so it can be extended easily later on)
* Why the "^ " and "~#cmap" distinction? Are there certain languages which can't handle this easily?
* It took some time before I figured out strings of the form "~#<TAG>" is part of the spec. Could be me reading the spec like the devil
  reads the bible, but perhaps a clarification around the representation of tags in strings would help.
* The interaction of caching and eventual substitution together with string decoding was not entirely clear when I read the spec. It seems
  caching comes first, and indeed this makes the code work as intended.
* The hardest part of the OCaml implementation has to do with the self-referential nature of Sets and Maps. To implement these, you need a
  comparator-function over the transit term. But you need the term to define the comparator and you need the comparator in order to
  define the types of Sets and Maps! My solution was to utilize Polymorphic compare—a feature in OCaml—where comparison happens
  structurally on the internal representation of the values. This solves the problem, because Poly-comparators work for any type and gives
  a "canonical" comparator for any valid term.
* It is not entirely clear how to define a system which allow for extension yet in OCaml. I feel extension must happen statically by the correct
  use of functors, but I have yet to figure out a satisfactory representation of this.
* The keyword/symbol distinction seem quite odd to a programmer who has not seen lots of Common Lisp nor Clojure. In Erlang, of which I   am deeply familar, we only have "atoms" which are roughly symbols. Yet, they run the problem of being system-global so if you generate e  nough of those, you will eventually exhaust the systems atom-table and the node() will cease operating. Is the reason for the KW/Symbol distinction a way to solve this limitation? I read keywords evaluate to themselves, whereas symbols do not, but I would really like to know more about you feel the need for both representations in the transit format.
* One strength of OCaml is that we can just invent a new type for Transit. This makes it very easy to map Transit onto ocaml since every we can simply abstract away the underlying representation of leaf values in the transit AST. If we did not have a native URI or UUID implementation—which we have—we could have opted for a string representation. The power here is we can exploit the type system and provide a rich description of what a transit term really is. In a more dynamic system (with less abstraction features) we would have forced to map transit values onto the underlying language directly. Here we can defer that decision to the user of the library.
* What happens if "^ " is in the middle of an array?
* What happens if "~#list" and the like is in the middle of an array or somewhere it is not recognized as special?
* The AST structure can be described by an EBNF grammar. Why isn't it?

## Serious errors

These are things I believe would need addressing before the format can be marked as done and frozen.

* How are maps with multiplicity on keys handled ? That is the map `{ 1: 3, 1: 5 }`? Is it allowed, or is it rejected? How are implementations supposed to handle this? I would definitely add that the behaviour is undefined as in JSON.
* Where are string caching done? "^ "-maps only or also in "~#cmap"-maps? It is not entirely clear.
* Canonical representation of integers? There are 3 ways: JSON Int, "~i…" and "~n…". Is there a canonical way to represent 123 or should implementations be prepared to handle "~n123" ?
* The cache needs to contain two kinds of values: Transit values and Array tag values. Array tags are not transit-values, they are special, and if you only want to convert values once, you have to add these into the cache. The solution in OCaml is very easy and straightforward. But it is an interesting curiosity when implementing this.
* What is the format specification of floats when outputting them? Different YAJL-renderers might do different things here.
* If we have a map, then a key has to be formatted as a string, even though the map is written as an array and thus easily accepts non-string keys. Why is that not specified in the spec directly? I had to look at the examples to figure this one out.
* What *precisely* constitutes the "^ "/"~#cmap" distinction?
* Float representations: NaN Infinity -Infinity ?
