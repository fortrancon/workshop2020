Moving average:
--------------

The implementation is straightforward: no attempt to change the window
and retain the existing data. No extended version, but that should be
little more than extending the base type with a few methods.

Three versions:
- Use a fixed buffer that acts as a circular buffer. Probably the most
  efficient version: no data movement, only individual elements
  replaced.

- Alternative version that lets the buffer grow. The "advantage" is that
  you need very little additional data - the buffer and the maximum size
  is all.

- Alternative version that shifts the previous values and sets the new
  value as the first value of the buffer. No need to keep track of
  an insertion point like with the first version.

If you ask for the moving average without having added any data, then a
NaN is returned.


Universal storage:
-----------------

The implementation is not very smart: you have to keep track of the
index where an item is stored and at initialisation time you specify the
maximum number of items that can be stored, which cannot be
extended.

A more important issue is that the retrieval routines are type-specific:
for any type you want to store you need a separate routine, due to the
fact that Fortran has no straightforward generic programming features.

You could retrieve the data via an unlimited polymorphic variable, but
that must be converted into a specific type at some point.
