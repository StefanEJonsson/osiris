# osiris

Open source Stepwise Improvement Routines In Scala (osiris) is an API for building and training neural networks and similar machine learning architectures. 

## Getting Started

1. Install java and scala if you haven't already: https://www.scala-lang.org/download/
1. Download osiris.jar
1. Add osiris.jar to your classpath/libraryDependencies (how you do this depends on which IDE/build tool you are using)

A simple example of how the API is used can be found in examples/SineRecognizer.scala. It contains a simple neural network that is trained to distinguish between sinewaves with random phase and completely random data.

## Tutorial

The API has the following basic building blocks

* Shapes
* Scalars
* Vectors
* Pins
* Morphisms
* ScalarFunctions
* VectorFunction

A scalar is simply a number (represented either as float or double).
A vector is a list of scalars, table of scalars or some other structure containing scalars.
Every vector has a shape. A shape is a set of indicies. If a vector `x` has {1 2 3} as its shape, then `x(1)`, `x(2)` and `x(3)` are scalars. `x(0)` however is not defined since `0` is not included in the shape of `x`.

### Shapes

There are four basic ways to construct shapes:
* `upto(n)` represents the set of natural numbers smaller than n. `upto(5)` = {0 1 2 3 4}
* `range(a,b)` represents all integers between a and b (inclusive). `range(-1,2)` = {-1 0 1 2}
* `O` represents the empty set.
* `I` represents a set containing only one element. {()}

Shapes can be combined using addition and multiplication to create more complex shapes. Addition in this case means tagged union and multiplication means carteasean product.

If `a = range(-1,1)` and `b = upto(2)` then

* `a + b` has {Left(-1) Left(0) Left(1) Right(0) Right(1)} as its elements
* `a * b` has {(-1,0) (-1,1) (0,0) (0,1) (1,0) (1,1)} as its elements

### Vector- and Scalar Spaces

You can pick a scalar space by writing `R = F32` (or F64) at the beginning of your code. In most projects you will probably use the same scalarspace for all computations but it is possible to switch if you want to tailor the precision to each part of your computation graph.

You can create vector spaces by combining a shape with a scalarspace as follows: `val space = upto(5) --> R`. `R^n` can be used as shorthand for `upto(n) --> R`.

`(I --> R)` is a vector space that is basically equivalent to the scalar space R. The only difference is that now it is treated as a vector space and not a scalar space.

If you use a shape that is the cartesean product of two simpler shapes, then the resulting vector space is called a matrix space. The space of all 3 by 5 (zero-indexed) matricies is created as follows: `(until(3) * until(5)) --> R`.

Vectorspaces can be combined in the same way as shapes:

`(a --> R) + (b --> R)` is equivalent to `(a + b) --> R`. 

`(a --> R) * (b --> R)` is equivalent to `(a * b) --> R`.

This means that a matrix space can also be constructed by simply multiplying together the corresponding vectorspaces.

### Vectors

If `V` is a vector space, the following methods can be used to construct vectors belonging to V:

* `V.zeros` - constructs a vector containing only zeros
* `V.ones` - constructs a vector containing only ones
* `V.fill(x)` - constructs a vector containing only the value of x
* `V.unit(i)` - constructs a vector containing a one at index i and zeros at all other indicies
* `V.units(predicate)` - constructs a vector containing ones everywhere where predicate returns true and zeros elsewhere. The predicate must be a morphism. More on morphisms later.
* `V(f)` - constructs a vector containing the scalar f(i) at index i. f can be a function of lambda expression
* `V.open(file)` - opens a vector saved in a file

If `V` is also a matrix space, the following methods can also be used:

* `V.rows(f)` - constructs a matrix containing the vector f(i) at row index i.
* `V.cols(f)` - constructs a matrix containing the vector f(j) at column index j.
* `V.identity` - constructs the identity matrix (only works for square spaces).

If `x` and `y` are vectors of spaces `V` and `U` respectively then `x | y` is the concatenation of the two vectors and belongs to the vector space `U + V`.

There are various arithmetic operations defined on vectors, such as addition, subtraction multiplication with scalars, summation and negation. The following notation is used for the different types of products:

* `*` is used to multiply vectors by matricies, matricies by matricies or matricies by vectors. It is also used to multiply vectors by scalars.
* `o` is elementwise multiplication between two vectors of the same shape.
* `<>` is the inner product of two vectors of the same shape.
* `><` is the outer product of two vectors of the same shape.

#### Morphisms and Reindexing

A morphism is a function between two shapes. They are used to modify the shape of a vector. 

Let `a` and `b` be shapes.

Let `f` be a morphism from `a` to `b`.

Let `x` be a vector of shape `b`.

If we define the vector `y` as: 

`val y = x.reindex(f)`

then `y` have shape `a` and for every index `i` in this shape `y(i)` will be equal to `x(f(i))`.

Basically `y` has the same elements as `x` but a new shape.

### Pins

There are four types of pins:

* Constants
* Variables
* Parameters
* Output Pins

Constants represent vectors that are not supposed to be changed after they are created. Variables on the other hand can be changed. When a variable is changed, all pins that depend on that variable are automatically updated. Parameters are also vectors that can change. But parameters are not supposed to be changed by the programmer. Instead, parameters are adjusted automatically by the machine learning algorithm (improver).

Constants, variables and parameters are constructed almost the same way as ordinary vectors. If V is a vector space, the following lines can be used to construct constants, variables and parameters.

* `val myConstant = V.constant.ones` - creates a constant containing only ones.
* `val myVariable = V.variable.unit(i)` - creates a variable initialized with a vector containing a one at index i and zeros elsewhere.
* `val myParameter = V.parameter("myParameter")(i => 3*i + 1)` - creates a parameter containing `3*i + 1` at any index i.

Note that parameters have to be assigned a name. This is so that they can be saved in files during/after training.

If a file already exists with the same name as your parameter, the initialization is discarded and the parameter is initialized using whatever is stored in the file instead. Note that parameters are opened automatically, but they are not saved automatically. In order to save a parameter, use `myParameter.save()`.

### Output Pins

Output pins are created when operations are performed on other pins. Pins have the same operations as vectors. For example, if x and y are pins of the same shape, then `val z = x + y` creates a new pin `z` that represents the sum of x and y. Output pins are updated when parameters or variables that they depend on change. The following is okay:

```scala
var z = x1
z = z + x2
z = z + x3
z = z + x4
```

Changing a variable referring to a pin does not change the underlying pin. Instead a new pin is constructed as a result of the operation and stored in same variable. The old pin will still exist in the background and the new pin will have a reference to the old pin so that changing any of the variables x1 through x4 will have the desired effect on z and so that the backpropagation algorithm will work as it should.

#### Objectives

An objective represents a goal that you want to achieve with your algorithm. An objective is created as follows:

`val goal = new Objective(strength)`

and you can connect a pin to it:

`myPin ->- goal`

The pin has to represent a single number.

If a pin is connected to an objective with positive strength, the improver will try to maximize it. (more on improvers later)

If a pin is connected to an objective with negative strength, the improver will try to minimize it.

If multiple objectives are used in the same model, a linear combination of the objectives will be optimized (determined by strength).

#### Static Models

Static models are models whose structure do not change depending on the input data. Examples of this would be ordinary neural networks or convolutional neural networks (if we assume that all images in our dataset have the same dimension).

I suggest this procedure:

1. Define all your parameters
1. Create variables for the data (usually one for the inputs and one for the labels)
1. Write computations in a imperative style
1. When you have created the output pin (the pin representing the final result of your computation) combine it with your label pin to produce an error pin
1. Connect your error pin to an Objective

Note that even though you never have to create helper functions when creating static models, doing so might sometimes simplify your code. I would recomend structuring your code in the same way you would if you were writing an ordinary program (not involving deep learning). For example:

```scala
def layer[I,J](x:Pin[J,R],weight:MatrixPin[I,J,R],bias:Pin[I,R]):Pin[I,R] = 
  (weight*x + bias).map(relu)
```

Might be an excelent way to remove code duplication.

In example/SineRecognizer.scala there is another example of how helper functions can be used. In that example, the helper function describes the whole computation of the network but only on a single data point. The output pin is then obtained by applying that function to an input variable representing a whole batch of data points using row map.

#### Dynamic Models

Dynamic models are models that change their structure depending on the input data. An example of this is a recurrent neural network (the depth of network is equal to the length of the input list). In this case variables won't help you much. It is better to represent input data with constants (create new ones every time you use new data). Write one function that does the whole computation (takes a pin as input and generates pin as output). This also means that you have to construct new Objectives every time you use new data. Extra care has to be taken so that old objectives are not kept in place as the new ones are added (continuing to generate feedback to the parameters based on old data). This can be done in four ways:

* Don't construct new objectives. Use a global value representing an objective that can be reused throughout multiple iterations. Old pins are automatically disconnected when a new pin is connected to an objective.
* Disconnect manually using `pin -/- objective`
* Use `osiris.evaluator.delete(myPin)` - Deletes pin along with all pins that myPin depends on but don't have any other pins depending on them. 
* Use `myImprover.disconnect()` - disconnects your parameters from everything, giving you a fresh start.

### Scalar and Vector Functions

osiris.function.ScalarFunction contains a collection of standard functions R --> R such as Sin, Exp. They can be composed with `<<`.

`val f = Exp(R) << Sin(R)`

They can also be added, subtracted, multplied and divided to form new functions. For f(x) = x^2 write:

`val f = Id(R)^2`

For relu write:

`val relu = Heaviside(R) * Id(R)`

Scalar functions are used for map operations. If `x` is a vector and `f` is a scalar function, then `x.map(f)` creates a new vector by applying `f` on every element of `x`. This can also be done using a lambda expression (Pin[Unit,R] => Pin[Unit,R]).

The rest of the osiris.function package consists of vector functions. You usually don't have to use these Instead use the ordinary arithmetic operations on pins (they use vector functions in the background). There are however some functions (convolution, complex multiplication, fourier transform) that can not be accessed that way. 

Convolution operators are constructed as follows:

`val myConvOp = new osiris.function.bilinear.Convolution(range(-3,3) --> R, until(10) --> R, until(8) --> R)`

This results in a convolution operator that uses a filter of size 7 (-3 to 3) to transforms a vector of size 10 to a new vector of size 8. In most application you will want to use a filter shape that is symmetric around zero.


```scala
val filter = (range(-3,3) --> R).parameter("filter")(_ => random()) //construct a filter parameter and initalize it randomly

val x = (upto(10) --> R).variable.zeros   //construct a data variable and intialize it to zero

val y = myConvOp(filter,x)                //analyze data using filter
```
#### Bilinear Functions and Tensor Products

There is a special kind of function called bilinear functions. They take two (vector) input arguments and produce one (vector) output. If the first input arguments of a bilinear function is held constant the function is linear with respect to the second argument. If the second is held constant it is linear with respect to the first. Examples of such functions include:

* Ordinary Multiplication of Numbers
* Elementwise Multiplication of Vectors
* Inner Products
* Outer Products
* Matrix Products
* Convolution

There is a special operator called the tensor product that takes two bilinear operations and produces a new bilinear operation. I will not go in to too much detail here, but the following is good to know:

* In this API it is denoted `&`
* `val conv2d = myConvOp & myConvOp` constructs 2 dimensional convolution
* `val filterOp = new osiris.function.bilinear.MatrixVectorProduct(upto(n) --> R, upto(m) --> R) & conv2d` constructs the transformation that is usually used to build convolutional neural networks.

### Evaluators and Improvers

osiris.evaluator contains the algorithms needed to actually compute the values of pins.

Use `val myEvaluator = osiris.evaluator.Evaluator()` for single threaded evaluation.

Use `val myEvaluator = osiris.evaluator.Evaluator(n)` for multithreaded evaluation with n threads.

I recomend using multithreaded evaluation with n = #virtual cores on your computer. However this is only faster if there is a significant amount of computations to do in parallell. If your computation graphs are small or consist of very sequential computations, use singlethreaded instead.

Use `val xVal = myEvaluator.value(x)` to get the value of the pin x. (Note that xVal is of type vector and x of type pin).

Use `val xGrad = myEvaluator.gradient(x)` to get the gradient of the objectives with respect to x. You usually won't have to do this.

osiris.improver contains algorithms for optimizing the objectives by adjusting the parameters in your model.

Use `val myImprover = new osiris.improver.Adam(1f,0.9f,0.999f,0.0001,myEvaluator)` it is usually best. (I will however add more and better algorithms when I have time)

Use `myImprover.step(parameter1,parameter2,...)` to adjust your parameters by a small amount to get closer to your objectives.

Use `myImprover.save()` to save all the parameters along with any extra state variables used by the improver so that if you shut off the program and start it later it can just continue where it left off.

Check out example/SineRecognizer.scala to get a better overview!
Good Luck!
