package org.ialab.hmm

import scala.util.Random

/**
  * Created by hselvaggi on 17/8/16.
  * @param states A value that represent the number of states in the chain
  * @param outputs Specify how many different output values can exist
  * @param initialStates An initial estimatino of the initial state probabilities
  * @param stateTransition An initial estimation of the transition probabilites between states
  * @param symbolOutput An initial estimation of the probabilities for symbol output based on the current state
  */
class HMM(val states: Int, val outputs: Int, val initialStates: Array[Float], val stateTransition: Array[Array[Float]], val symbolOutput: Array[Array[Float]]) {
  import HMM._
  require(states > 0, "The state space needs to have at least 1 state")
  require(outputs > 0, "There needs to be at least one possible output symbol")

  private def forward(observations: Array[Int]) = {
    val forwardMatrix: Array[Array[Float]] = Array.ofDim(observations.length, states)

    R(0 until observations.length, 0 until states, forwardMatrix) {
      (currentTime, finalState) => {
        ∑(1, states) { i => stateTransition(i)(finalState) * forwardMatrix(currentTime- 1)(i) } * symbolOutput(finalState)(observations(currentTime))
      }
    }

    forwardMatrix
  }

  private def backward(observations: Array[Int]) = {
    val backwardMatrix: Array[Array[Float]] = Array.tabulate(observations.length, states)((x,y) => 1f)

    R(observations.length to 1, 1 to states, backwardMatrix)
    { (time, prevState) =>
      ∑(1, states) { i => stateTransition(prevState)(i) * backwardMatrix(time + 1)(i) * symbolOutput(i)(observations(time + 1)) }
    }

    backwardMatrix
  }

  def evaluate(observations: Array[Int]) : Float = forward(observations)(observations.length).sum

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

    R(1 until observations.length, 0 until states, delta, phsi) {
      (time, next) =>
        val temp = (0 until states).map(i => delta(time - 1)(i) * stateTransition(i)(next))
        val maxValue = temp.max

        (maxValue * symbolOutput(next)(observations(time)), temp.indexOf(maxValue))
    }

    val probability = delta(observations.length-1).max

    val sequence: Array[Float] = new Array[Float](observations.length)
    sequence(observations.length-1) = delta(observations.length-1).indexOf(probability)


    R(Range(observations.length-2, -1, step = -1), sequence) { i => phsi(i + 1)(sequence(i+1).toInt) }

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
      val denominator = ∑(1, states)( i => beta(time)(i) * alpha(time)(i) )
      eta(time)(i)(j) = numerator / denominator

      gamma(time)(i) = ∑(1, states-1)(e => eta(time)(i)(e))
    }

    R(0 until states, 0 until states, stateTransition) {
      (i,j) => (0 to observations.length -1).map(t => eta(t)(i)(j)).sum / (0 to observations.length -1).map(t => gamma(t)(i)).sum
    }

    R(0 until states, 0 to states, symbolOutput) {
      (i, o) => symbolOutput(i)(o) * (0 to observations.length -1).map(t => gamma(t)(i)).sum / (0 to observations.length -1).map(t => gamma(t)(i)).sum
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

  def ∑(i_0: Int, i_n: Int)(fn: Int => Float) = {
    var i = i_0 - 1
    var acc = 0f
    while ({i += 1; i < i_n}) {
      acc += fn(i)
    }
    acc
  }

  def R(r1: Range, vec: Array[Float])( op: (Int) => Float) = {
    for {
      i <- r1
    } {
      vec(i) = op(i)
    }
  }

  def R(r1: Range, r2: Range, vec: Array[Array[Float]])( op: (Int, Int) => Float) = {
    for {
      i <- r1
      j <- r2
    } {
      vec(i)(j) = op(i, j)
    }
  }

  def R(r1: Range, r2: Range, v1: Array[Array[Float]], v2: Array[Array[Float]])( op: (Int, Int) => (Float, Float)) = {
    for {
      i <- r1
      j <- r2
    } {
      val res = op(i, j)
      v1(i)(j) = res._1
      v2(i)(j) = res._2
    }
  }

  def create() = {

  }
}
