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
  require(states > 0, "The state space needs to have at least 1 state")
  require(outputs > 0, "There needs to be at least one possible output symbol")

  val stateTransition: Array[Array[Float]] = Array.tabulate(states, states)((x, y) => 0)

  val symbolOutput: Array[Array[Float]] = Array.tabulate(states, outputs)((x, y) => 0)

  Array.copy(initialTransition, 0, stateTransition, 0, states)
  Array.copy(initialOutput, 0, symbolOutput, 0, states)

  private def forward(observations: Array[Int]) = {
    val forwardMatrix: Array[Array[Float]] = Array.tabulate(observations.length, states)((x,y) => 0f)

    (0 to observations.length-1).foreach(currentTime => {
      (0 to states-1).foreach(finalState => {
        forwardMatrix(currentTime)(finalState) = (1 to states).map(prevState => {
          stateTransition(prevState)(finalState) * forwardMatrix(currentTime - 1)(prevState)
        }).sum * symbolOutput(finalState)(observations(currentTime))
      })
    })

    forwardMatrix
  }

  private def backward(observations: Array[Int]) = {
    val backwardMatrix: Array[Array[Float]] = Array.tabulate(observations.length, states)((x,y) => 1f)

    (observations.length to 1).foreach( time => {
      (1 to states).foreach(prevState => {
        backwardMatrix(time)(prevState) = (1 to states).map( finalState => { stateTransition(prevState)(finalState) * backwardMatrix(time + 1)(finalState) * symbolOutput(finalState)(observations(time + 1)) } ).sum
      })
    })

    backwardMatrix
  }

  def evaluate(observations: Array[Int]) : Float = {
    val forwardMatrix = forward(observations)
    val lastTime = observations.length

    (0 to states-1).map( state => { forwardMatrix(lastTime)(state) }).reduce((a,b) => a + b)
  }

  /**
    * Use this operation to get the sequence of states and the probability of seeing a specific sequence of
    * outputs
    * @param observations The outputs that has been observed
    * @return (probability, state sequence)
    */
  def viterbi(observations: Array[Int]) = {
    val delta: Array[Array[Float]] = Array.tabulate(observations.length, states)((x,y) => 0f)
    val phsi: Array[Array[Float]] = Array.tabulate(observations.length, states)((x,y) => 0f)

    (0 to states-1).foreach(i => delta(0)(i) = initialStates(i) * symbolOutput(i)(observations(0)))

    (1 to observations.length-1).foreach((time) => {
      (0 to states-1).par.foreach( next => {
        val temp = (0 to states-1).map(i => delta(time-1)(i) * stateTransition(i)(next))
        val maxValue = temp.max
        delta(time)(next) = maxValue * symbolOutput(next)(observations(time))
        phsi(time)(next) = temp.indexOf(maxValue)
      })
    })

    val probability = (0 to states-1).map(i => delta(observations.length-1)(i)).max

    val sequence: Array[Float] = Array.fill[Float](observations.length)(0)
    sequence(observations.length-1) = delta(observations.length-1).indexOf(probability)

    (observations.length-2 to 0).foreach( i => sequence(i) = phsi(i + 1)(sequence(i+1).toInt))

    (probability, sequence)
  }

  /**
    * Call this operation with a sequence of observations to learn about the different properties in the system and
    * then being able to generate or predict new sequences
    * @param observations Sequence of observations from the environment
    */
  def learn(observations: Array[Int]) = {
    var alpha: Array[Array[Float]] = forward(observations)
    var beta: Array[Array[Float]] = backward(observations)
    var eta: Array[Array[Array[Float]]] = Array.ofDim[Float](observations.length, states, states)
    var gamma: Array[Array[Float]] = Array.tabulate(observations.length, states)((x,y) => 0f)

    (0 to observations.length-1).foreach(time => {
      (0 to states-1).foreach(i => {
        (0 to states-1).par.foreach(j => {
          val numerator = alpha(time)(i) * stateTransition(i)(j) * symbolOutput(j)(observations(time)) * beta(time + 1)(j)
          val denominator = (1 to states-1).map(e => beta(time)(e) * alpha(time)(e)).sum
          eta(time)(i)(j) = numerator / denominator
          gamma(time)(i) = (1 to states-1).map(e => eta(time)(i)(e)).sum
        })
      })
    })

    (0 to states-1).foreach(i => {
      (0 to states-1).par.foreach(j => {
        val numerator = (0 to observations.length -1).map(t => eta(t)(i)(j)).sum
        val denominator = (0 to observations.length -1).map(t => gamma(t)(i)).sum
        stateTransition(i)(j) = numerator / denominator
      })
    })

    (0 to states-1).foreach(i => {
      (0 to outputs).par.foreach(o => {
        val numerator = symbolOutput(i)(o) * (0 to observations.length -1).map(t => gamma(t)(i)).sum
        val denominator = (0 to observations.length -1).map(t => gamma(t)(i)).sum
        symbolOutput(i)(o) = numerator / denominator
      })
    })
  }

  // Temporal code
  // Abstrat out this two operations (generateOutput and nextState)
  private def generateOutput(currentState: Int) = {
    val generator: Random = new Random()
    val prob = generator.nextFloat()
    var currentProb: Float = 0
    var output: Int = 0

    (0 to outputs-1).foreach(o => {
      currentProb += symbolOutput(currentState)(o)
      if(currentProb < prob) {
        output = o
      }
    })

    output
  }

  private def nextState(currentState: Int) = {
    val generator: Random = new Random()
    val prob = generator.nextFloat()
    var currentProb: Float = 0
    var output: Int = 0

    (0 to states-1).foreach(o => {
      currentProb += stateTransition(currentState)(o)
      if(currentProb < prob) {
        output = o
      }
    })

    output
  }

  /**
    * Once this HMM has been trained you can generate a sequence of any length with the same properties of the
    * initial sequence
    * @param sequenceLength Length of the generated sequence
    * @return A sequence of Int representing the different outputs generated from this stochastic process
    */
  def generate(sequenceLength: Int) = {
    var currentState = 0
    val output: Array[Int] = Array.fill(sequenceLength)(0)

    (0 to sequenceLength - 1).foreach(i => {
      output(i) = generateOutput(currentState)
      currentState = nextState(currentState)
    })

    output
  }

}

object HMM {
  def create() = {

  }
}
