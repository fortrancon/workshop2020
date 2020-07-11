General remarks:
---------------

The exercises have been described in the accompanying document
"exercises.pdf". All in all, doing these exercises from scratch may take
a few hours -- the exercises are not, in themselves, complicated, and
the sample solutions given here only take up a few hundred lines of
code, including the comments. It is simply that you have to think about
how to implement what is being asked and then type in the code,
build the program and correct any mistakes.


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


Replicating objects:
-------------------

The objects do not have much "behaviour" or attributes -- the
implementation could be more consistent in hiding the attributes in each
object (type item_data) and providing suitable methods for retrieving
and setting them.

There is no need to allocate them one by one (though that is certainly
an option for implementing this exercise). Instead we use a simple
attribute to see if the item is "alive".

The method step_simulation is a trifle lengthy, especially the loop to
"spawn" the items, it could be put in a separate routine.

The ASSOCIATE construct is used to shorten the fairly lengthy expression
to get to the attributes of the items.

The mortality rate in the example is 0.118 -- a rather arbitrary seeming
number, but it gives a simulation that is very near to the boundary
between a decaying population and an ever-expanding one. Might be
interesting to try and find a mathematical theory for this ...


Thermostat:
----------

The sample implementation of the thermostat system is quite
straightforward:

- Two objects of the type "temperature_data" are used for the outside
  temperature and the set temperature for the room. They each use a
  specific routine to determine the temperature in question.

- One object that determines whether the heating should be turned on or
  not. It is a very straightforward object, a more sophisticated version
  might want to know the time of day and keep track of the
  actual temperature in the room etc. to anticipate the required
  heating. That means an extension of the interface.

The simulation loop is very simple: it uses the Euler method for
integrating the differential equation. All coefficients have been
determined with some trial and error to get interesting behaviour. There
was no attempt to make them realistic.


Abstract framework:
------------------

There are a few shortcuts in the implementation:

- Only the Euler method for integration is implemented.

- It is implemented as a simple case. A more ambitious implementation
  would be to use a procedure pointer, pointing to separate routines
  or a set of classes, extending from a common class. That is left as
  a further exercise.

The ODE system to be used is in a separate module, as an internal
routine for evaluate_oscillator was not accepted by the gfortran
compiler.

The implementation does not take care of the case where no print options
are defined or where the framework has not been properly initialised
with an actual ODE system. Its purpose is to demonstrate the power of
object-oriented programming in Fortran, not to provide a ready-made
framework.
