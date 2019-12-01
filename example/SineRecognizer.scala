package example

import osiris._
import function._
import vector.Vector
import osiris.pin.{Pin, SinglePin}

object SineRecognizer {

  type R = Float
  val R = F32                                           // use 32 bit floating point numbers to represent reals

  val relu = Heaviside(R) * Id(R)                       // activation function
  val sigmoid = (Const(1f) + (Exp(R) << -Id(R))).inv    // squash function

  val batchSize   = 100
  val xSpace      = range(-10,10) --> R                 // x values consist of vectors indexed -10 to 10
  val hiddenSpace = R^10                                // hidden layer consists of ten neurons
  val ySpace      = (I --> R)                           // y values consist of a single neuron
  val inputSpace  = ((R^batchSize) * xSpace)            // input space is tensor product of batch space and xSpace


  def random():Float = (Math.random()-0.5).toFloat      // random numbers

  /**
    * Generates random data-point.
    * A data-point is an x y pair.
    * x is completely random with probability 0.5
    * and sine with random phase and some random noise with probability 0.5.
    * y is one if x is a random sine wave and zero if x is completely random.
    *
    * The goal of our machine learning algorithm is to predict y from x.
    */
  def randomData():(Vector[Int,R],R) = {
    val sine = util.Random.nextBoolean()
    if (sine) {
      val phase = util.Random.nextInt(5)
      val x = xSpace((i:Int) => Math.sin(i + phase).toFloat + random()*0.1f)
      (x,1f)
    } else {
      val x = xSpace((_:Int) => random()*2.2f)
      (x,0f)
    }
  }

  //initialize parameters
  val weights1 = (hiddenSpace * xSpace).parameter("w1")(_ => random())
  val weights2 = hiddenSpace.parameter("w2")(_ => random())

  val bias1 = hiddenSpace.parameter("b1")(_ => random())
  val bias2 = (I --> R).parameter("b2")(_ => random())

  // Tries to predict y given x
  def predict(x:Pin[Int,R]):SinglePin[R] = {
    val hidden = (weights1.asMatrix * x + bias1).map(relu)
    ((weights2 <> hidden) + bias2).map(sigmoid)
  }

  //initialize variables
  val input = inputSpace.variable.zeros
  val correct = (R^batchSize).variable.zeros

  //define output as the prediction function applied to every row
  val output = input.asMatrix[Int,Int,(Int,Int)].rowMap(x => predict(x))

  //define error ass sum of squared differences between predictions and correct answer
  val error = (output - correct.toColVector).map(Id(R)^2).sum
  //define parameterSize as sum of the square of all parameters
  val parameterSize = (weights1 | weights2 | bias1 | bias2).map(Id(R)^2).sum

  //define the objectives of the neural net
  val minimizeError = new Objective(-1f)
  val keepFinite = new Objective(-1f)

  //connect objectives to computation graph
  error ->- minimizeError
  parameterSize ->- keepFinite

  //select evaluation algorithm and improver
  val evaluator = osiris.evaluator.Evaluator()                             //orinary single threaded evaluation
  val improver = new osiris.improver.Adam(1f,0.9f,0.999f,0.0001f,evaluator)//adaptive moment estimation gradient descent

  //Learns parameters by doing 100 improvement steps
  def learn(): Unit = {
    for (_ <- 0 until 100) {
      val data = Vector.tabulate(batchSize)(_ => randomData()) //generate batch
      input.set(inputSpace.rows(i => data(i)._1))              //set variables to batch
      correct.set(correct.space((i:Int) => data(i)._2))
      improver.step(weights1,weights2,bias1,bias2)             //do one step of improvement
      println(evaluator.value(error))                          //print error
    }
    improver.save()                                             //save parameters on disk
  }

  def printParameters: Unit = {
    println(weights1)
    println(weights2)
    println(bias1)
    println(bias2)
  }

  def main(args:Array[String]): Unit = {
    learn()
    printParameters
  }

}
