package com.scilari.systematic_alias_sampling.core

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * Kronmal and Peterson 1979 implementation of alias method by Walker (1974, 1977) as described in
  * "An Analysis of the Alias Method for Discrete Random-Variate Generation" by Smith and Jacobson (2005)
  *
  * @param pmf    Probability mass function.
  * @param values Values corresponding to the probability mass function.
  * @tparam T
  */
class AliasTable[T: ClassTag](val pmf: Array[Double], val values: Array[T]) {
  require(math.abs(pmf.sum - 1.0) < 1e-6, "Probability masses do not sum to one.")
  require(pmf.length == values.length, "Probability mass and value arrays are not the same size")
  val (aliasedIndices, aliasProbabilities) = create(pmf)
  val aliasedValues = aliasedIndices.map{values(_)}

  /**
    * Creates the alias table structure
    *
    * @param pmf    Probability mass function.
    * @return Tuple of aliasedIndices and corresponding aliasProbabilities.
    */
  def create(pmf: Array[Double]): (Array[Int], Array[Double]) = {
    val n = pmf.length
    val q = pmf.map {
      n.toDouble * _
    }
    // Using stacks to retain spatial order
    val (g, h) = mutable.Stack(0 until n: _*).partition { i => q(i) >= 1.0 } // indices of greater and smaller items
    val a = (0 until n).toArray // aliasIndices
    val s = new Array[Double](n) // aliasProbabilities

    // Checking both because of possible numerical inaccuracies large may be emptied first
    while (h.nonEmpty && g.nonEmpty) {
      val j = h.pop()
      val k = g.top
      a(j) = k
      s(j) = 1.0 - q(j)
      q(k) = (q(k) + q(j)) - 1.0 // numerically more stable

      if (q(k) < 1.0) {
        g.pop()
        h.push(k)
      }
    }

    (a, s)
  }
}

