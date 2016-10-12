package org.ialab.hmm

import scala.util.Random

/**
 * Created by hselvaggi on 17/8/16.
 * @param states A value that represent the number of states in the chain
 * @param outputs Specify how many different output values can exist
 * @param initialStates An initial estimatino of the initial state probabilities
 * @param initialTransition An initial estimation of the transition probabilites between states
 * @param initialOutput An initial estimation of the probabilities for symbol output based on the current state
 */
class HMM(val states: Int, val outputs: Int, val initialStates: Array[Float], val initialTransition: Array[Array[Float]], val initialOutput: Array[Array[Float]]) {
  import HMM._
  require(states > 0, "The state space needs to have at least 1 state")
  require(outputs > 0, "There needs to be at least one possible output symbol")

  val stateTransition: Array[Array[Float]] = Array.ofDim(states, states)

  val symbolOutput: Array[Array[Float]] = Array.ofDim(states, outputs)

  Array.copy(initialTransition, 0, stateTransition, 0, states)
  Array.copy(initialOutput, 0, symbolOutput, 0, states)


  private def forward(observations: Array[Int]) = {
    val forwardMatrix: Array[Array[Float]] = Array.ofDim(observations.length, states)

    for {
      currentTime <- 0 until observations.length
      finalState <- 0 until states
    } {
      forwardMatrix(currentTime)(finalState) =
        ∑(stateTransition(_: Int)(finalState), forwardMatrix(currentTime - 1), 1, states)(_ * _) * symbolOutput(finalState)(observations(currentTime))
    }

    forwardMatrix
  }

  private def backward(observations: Array[Int]) = {
    val backwardMatrix: Array[Array[Float]] = Array.tabulate(observations.length, states)((x,y) => 1f)

    for {
      time <- observations.length to 1
      prevState <- 1 to states
    } {
      backwardMatrix(time)(prevState) = 
        ∑(stateTransition(prevState), backwardMatrix(time + 1), symbolOutput(_)(observations(time + 1)), 1, states)(_ * _ * _)
    }

    backwardMatrix
  }

  def evaluate(observations: Array[Int]) : Float = {
    val forwardMatrix = forward(observations)
    val lastTime = observations.length

    forwardMatrix(lastTime).sum
  }

  /**
   * Use this operation to get the sequence of states and the probability of seeing a specific sequence of
   * outputs
   * @param observations The outputs that has been observed
   * @return (probability, state sequence)
   */
  def viterbi(observations: Array[Int]): (Float, Array[Float]) = {
    val delta: Array[Array[Float]] = Array.tabulate(observations.length, states)((x,y) => 0f)
    val phsi: Array[Array[Float]] = Array.tabulate(observations.length, states)((x,y) => 0f)

    (0 until states).foreach(i => delta(0)(i) = initialStates(i) * symbolOutput(i)(observations(0)))

    for {
      time <- 1 until observations.length
      next <- 0 until states
    } {
      val temp = (0 until states).map(i => delta(time-1)(i) * stateTransition(i)(next))
      val maxValue = temp.max
      delta(time)(next) = maxValue * symbolOutput(next)(observations(time))
      phsi(time)(next) = temp.indexOf(maxValue)
    }

    val probability = delta(observations.length-1).max

    val sequence: Array[Float] = new Array[Float](observations.length)
    sequence(observations.length-1) = delta(observations.length-1).indexOf(probability)

    //FIXME: fishy range to 0. You probably want Range(observations.length-2, -1, step = -1) note that this construct is always non inclusive, hence the -1
    (observations.length-2 to 0).foreach(i => sequence(i) = phsi(i + 1)(sequence(i+1).toInt))

    (probability, sequence)
  }

  /**
   * Call this operation with a sequence of observations to learn about the different properties in the system and
   * then being able to generate or predict new sequences
   * @param observations Sequence of observations from the environment
   */
  def learn(observations: Array[Int]) = {
    val alpha: Array[Array[Float]] = forward(observations)
    val beta: Array[Array[Float]] = backward(observations)
    val eta: Array[Array[Array[Float]]] = Array.ofDim[Float](observations.length, states, states)
    val gamma: Array[Array[Float]] = Array.tabulate(observations.length, states)((x,y) => 0f)

    for {
      time <- 0 until observations.length
      i <- 0 until states
      j <- (0 until states).par
    } {
      val numerator = alpha(time)(i) * stateTransition(i)(j) * symbolOutput(j)(observations(time)) * beta(time + 1)(j)
      val denominator = ∑(beta(time), alpha(time), 1, states)(_ * _)
      eta(time)(i)(j) = numerator / denominator

      gamma(time)(i) = (1 to states-1).map(e => eta(time)(i)(e)).sum
    }

    for (i <- 0 until states; j <- 0 until states) {
      val numerator = (0 to observations.length -1).map(t => eta(t)(i)(j)).sum
      val denominator = (0 to observations.length -1).map(t => gamma(t)(i)).sum
      stateTransition(i)(j) = numerator / denominator
    }

    for (i <- 0 until states; o <- (0 to outputs).par) {
      val numerator = symbolOutput(i)(o) * (0 to observations.length -1).map(t => gamma(t)(i)).sum
      val denominator = (0 to observations.length -1).map(t => gamma(t)(i)).sum
      symbolOutput(i)(o) = numerator / denominator
    }
  }

  /**
   * generalization of generateOutput and nextState
   */
  def next(probVec: Array[Float]): Int = {
    val prob = Random.nextFloat()

    val (_, output) = (0 until outputs).foldLeft((0f, 0)) {
      case ((currentProb, output), newOutput) =>
        val newProb = currentProb + probVec(newOutput)
        (newProb, if (newProb < prob) newOutput else output)
    }
    output
  }

  // Temporal code
  // Abstrat out this two operations (generateOutput and nextState)
//  private def generateOutput(currentState: Int) = {
//    val generator: Random = new Random()
//    val prob = generator.nextFloat()
//    var currentProb: Float = 0
//    var output: Int = 0
//
//    (0 to outputs-1).foreach(o => {
//        currentProb += symbolOutput(currentState)(o)
//        if(currentProb < prob) {
//          output = o
//        }
//      })
//
//    output
//  }
//
//  private def nextState(currentState: Int) = {
//    val generator: Random = new Random()
//    val prob = generator.nextFloat()
//    var currentProb: Float = 0
//    var output: Int = 0
//
//    (0 to states-1).foreach(o => {
//        currentProb += stateTransition(currentState)(o)
//        if(currentProb < prob) {
//          output = o
//        }
//      })
//
//    output
//  }

  /**
   * Once this HMM has been trained you can generate a sequence of any length with the same properties of the
   * initial sequence
   * @param sequenceLength Length of the generated sequence
   * @return A sequence of Int representing the different outputs generated from this stochastic process
   */
  def generate(sequenceLength: Int) = {
    var currentState = 0
    val output: Array[Int] = Array.fill(sequenceLength)(0)

    (0 until sequenceLength).foreach { i =>
      output(i) = next(symbolOutput(currentState))
      currentState = next(stateTransition(currentState))
    }

    output
  }

}

object HMM {
  @inline def ∑(v1: Int => Float, v2: Int => Float, i_0: Int, i_n: Int)(op: (Float, Float) =>  Float): Float = {
    var i = i_0 - 1
    var acc = 0f
    while ({i += 1; i < i_n}) {
      acc += op(v1(i), v2(i))
    }
    acc
  }
  @inline def ∑(v1: Int => Float, v2: Int => Float, v3: Int => Float, i_0: Int, i_n: Int)(op: (Float, Float, Float) =>  Float): Float = {
    var i = i_0 - 1
    var acc = 0f
    while ({i += 1; i < i_n}) {
      acc += op(v1(i), v2(i), v3(i))
    }
    acc
  }
  def create() = {

  }
}
