Shape Shifters
=============

A simple typechecker for a simple object-oriented language with shapes and shape shifters.

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
