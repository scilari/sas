package com.scilari.systematic_alias_sampling.util

import Helpers._

/**
  * Basic goodness-of-fit tests
  * Created by iv on 5/25/2016.
  */
object StatisticalTests {
  val invLogOf2: Double = 1.0 / math.log(2.0)
  def log2(x: Double): Double = math.log(x) * invLogOf2

  /**
    * Jensen-Shannon divergence (by Pauli Rikula)
    *
    * @param P distribution
    * @param Q distribution
    * @return
    */
  def jsd(P: Array[Double], Q: Array[Double]): Double = {
    def h(P: Array[Double]): Double = {
      -1.0 * (P map { p => if (p > 0.0) p * log2(p) else 0.0 }).sum
    }
    h((P, Q).zipped map ((p, q) => {
      (p + q) / 2.0
    })) - 0.5 * h(P) - 0.5 * h(Q)
  }


  /**
    * Kullback-Leibler divergence
    *
    * @param P distribution
    * @param Q distribution
    * @return
    */
  def kl(P: Array[Double], Q: Array[Double]): Double = {
    val dP = P.sum - 1.0
    val dQ = Q.sum - 1.0
    P(P.indexWhere(_ > dP)) -= dP
    Q(Q.indexWhere(_ > dQ)) -= dQ
    ((P, Q).zipped map ((p, q) => {
      if (p > 0.0 && q > 0.0) math.log(p / q) * p else 0.0
    })).sortWith((x, y) => math.abs(x) < math.abs(y)).sum
  }

  /**
    * Hellinger distance
    *
    * @param P distribution
    * @param Q distribution
    * @return
    */
  def hellingerD(P: Array[Double], Q: Array[Double]): Double = {
    1.0 / math.sqrt(2) * math.sqrt(((P, Q).zipped map ((p, q) => math.pow(math.sqrt(p) - math.sqrt(q), 2.0))).sum)
  }

  /**
    * Chi squared
    *
    * @param P distribution
    * @param Q distribution
    * @return
    */
  def chiSquared(P: Array[Double], Q: Array[Double]): Double = {
    (P, Q).zipped.map { (p, q) => {
      val n = P.length; math.pow(math.abs(n * p - n * q), 2) / (n * q)
    }
    }.sum
  }

  /**
    * Kolmogorov-Smirnov statistic
    *
    * @param P distribution
    * @param Q distribution
    * @return
    */
  def kss(P: Array[Double], Q: Array[Double]): Double = {
    val F1 = cumsum(P)
    val F2 = cumsum(Q)
    val differences = (F1, F2).zipped map { (x1, x2) => math.abs(x1 - x2) }
    differences.max
  }

  /**
    * Modified Cramer-Von Mises
    *
    * @param P distribution
    * @param Q distribution
    */
  def cramerVonMises(P: Array[Double], Q: Array[Double]): Double = {
    val squaredDiffs = (cumsum(P), cumsum(Q)).zipped map { (x1, x2) => (x1 - x2) * (x1 - x2) }
    squaredDiffs.sum / squaredDiffs.length
  }

  def rootCramerVonMises(P: Array[Double], Q: Array[Double]): Double = {
    Math.sqrt(cramerVonMises(P, Q))
  }

  def andersonDarling(P: Array[Double], Q: Array[Double]): Double = {
    val squaredDiffs = (cumsum(P), cumsum(Q)).zipped map { (p, q) => (p - q) * (p - q) / (q * (1 - q)) }
    squaredDiffs.sum / squaredDiffs.length
  }

}
