package org.ialab.hmm

//import org.scalatest._
import org.scalatest.{FlatSpec, PrivateMethodTester}

import org.ialab.hmm.FloatMatrix.ConstantMatrix

/**
  * Created by hselvaggi on 24/9/16.
  */
class HMMTest extends FlatSpec with PrivateMethodTester {

  it should "find the state sequence 0, 1 - 0, 0 and 1, 1 for 3 different observations" in {
    val hmm = new HMM(2, 2, Array[Double](0.5f, 0.5f), ConstantMatrix(2, 2, 0.5f),
      FloatMatrix(Array[Double](1, 0, 0, 1), 2))

    assert(hmm.viterbi(Array[Int] (0, 1))._2.sameElements(Array[Float](0,1)))

    assert(hmm.viterbi(Array[Int] (0, 0))._2.sameElements(Array[Float](0,0)))

    assert(hmm.viterbi(Array[Int] (1, 1))._2.sameElements(Array[Float](1,1)))

  }

  it should "find the state sequence 0, 1, 1" in {
    val hmm = new HMM(3, 2, Array[Double](0.5f, 0.5f, 0), FloatMatrix(Array[Double](0.5f, 0.5f, 0.5f, 0, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f), 3),
      FloatMatrix(Array[Double](1, 0, 0, 1, 1, 0), 2))

    assert(hmm.viterbi(Array[Int] (0, 1, 1))._2.sameElements(Array[Double](0,1,1)))
    assert(hmm.viterbi(Array[Int] (0, 1, 0))._2.sameElements(Array[Double](0,1,2)))
  }

  it should "Learn a simple two state where the first states emits 0 and the second emits 1" in {
    val observations = Array[Int](0,0,0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,1)
    val hmm = new HMM(2, 2, Array[Double](0.9f, 0.1f), ConstantMatrix(2, 2, 0.5f),
      FloatMatrix(Array[Double](0.7f, 0.3f, 0.3f, 0.7f), 2))

    assert(hmm.viterbi(Array[Int] (0,0,0,0,1))._2.sameElements(Array[Double](0,0,0,0,1)))
    assert(hmm.viterbi(Array[Int] (0,0,0,1,1))._2.sameElements(Array[Double](0,0,0,1,1)))
  }

  it should "It should be unlikely to see a chain with many 1" in {
    val observations = Array[Int](0,0,0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,1)
    val hmm = new HMM(2, 2, Array[Double](0.5f, 0.5f), ConstantMatrix(2, 2, 0.5f),
      FloatMatrix(Array[Double](0.7f, 0.3f, 0.3f, 0.7f), 2))

    hmm.learn(observations, 20)

    assert(hmm.viterbi(Array[Int] (0,0,0,0,1))._1 > hmm.viterbi(Array[Int] (1,1,1,1,0))._1)

    assert(hmm.viterbi(Array[Int] (0,0,0,0,1))._1 > hmm.viterbi(Array[Int] (1,1,1,0,0))._1)

    assert(hmm.viterbi(Array[Int] (0,0,0,0,1))._1 > hmm.viterbi(Array[Int] (0,0,1,1,1))._1)

    assert(hmm.viterbi(Array[Int] (1,0,0,0,0))._1 > hmm.viterbi(Array[Int] (1,1,1,1,0))._1)
  }



  it should "Learn a simple three state where the first state emits 0, the second emits 1 and the third emits 0, no return from second state to first one" in {
    val observations = Array[Int](0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    val hmm = new HMM(3, 2, Array[Double](1f, 0f, 0f), FloatMatrix(Array[Double](0.5f, 0.5f, 0f,
                                                                                0f, 0.5f, 0.5f,
                                                                                0f, 0f, 1f), 3),
      FloatMatrix(Array[Double](1f, 0f,
                              0f, 1f,
                              1f, 0f), 2))


    hmm.learn(observations, 20)


    assert(hmm.viterbi(Array[Int] (0,0,0,0,1,0))._2.sameElements(Array[Double](0,0,0,0,1,2)))

    assert(hmm.viterbi(Array[Int] (0,0,0,0,1))._1 > hmm.viterbi(Array[Int] (1,1,1,1,0))._1)
  }


}
