Shape Shifters
=============

A simple typechecker for a simple object-oriented language with shapes and shape shifters.

Source
------
The typechecker is spread divided across a few files.
- `definitions.ml` has the type definitions for the language and a few utility functions (`to_string`, etc)
- `context.ml` is the interface to program state. It organizes `classContext.ml`, `shifterContext.ml`, and `typeContext.ml`.
- `subtype.ml` decidable subtyping algorithm. See [Greenman et al. 2014](http://www.cs.cornell.edu/~ross/publications/shapes/) and [Tate 2013](http://www.cs.cornell.edu/~ross/publications/mixedsite/) for details.
- `well_formed.ml` checking class/interface/shape well-formedness.
- `extension_methods.ml` an extension for extension methods. Joins the caller and argument types, then finds the least parent satisfying all restrictions. (Needs some work.)
- `pretty_print.ml` has the formatting functions used for "code generation".

Testing
-------
Running `make test` will invoke [ocamlbuild](http://caml.inria.fr/pub/docs/manual-ocaml-400/manual032.html) to compile the source and all tests, then run the tests and print representative code.
To experiment on a smaller scale, you can build and run individual test files.
For example, to see what's up with `number.ml`, you can use `ocamlbuild test/number.d.byte` to compile and `./number.d.byte` to run.
Here's sample output:
```
shape Addable
{
  THIS plus (THIS that);
}

interface Boolean<>
{
  Boolean<> and (Boolean<> that);
  Boolean<> not ();
}

shape Equatable
{
  Boolean<> equals (THIS that);
}

shape Comparable
    satisfies Equatable
{
  Boolean<> leq (THIS that);
}

interface Number<>
    satisfies Comparable, Addable
{
  Number<> max3 (Number<> num1, Number<> num2);
  Integer<> intValue ();
  Long<> longValue ();
  Boolean<> isZero ();
}

interface Integer<>
    implements Number<>
    satisfies Comparable, Addable
{

}

interface Long<>
    implements Number<>
    satisfies Comparable, Addable
{

}
```

It prints representative code for the interfaces defined within `number.ml` and all the interfaces and shapes they depend on.

To create new classes and interfaces, you need to build a record following the rules in `definitions.ml`.
The catch is that subtyping and other judgments require a context, and you must construct a fully-defined class and type context manually.

Caveats
-------
As of 2014-11-12:

- No parameterized shapes
- Variance not enforced, it's all manual
- Don't distinguish inherited/not inherited shapes
- Shifters not allowed to provide implementations
- Interfaces not allowed to provide implementations
- No duplicate methods
- No duplicate type parameters (across the entire hierarchy)
- inherited methods must have same arg names in same positions
- multiple instances of super T in the same class are not identified
