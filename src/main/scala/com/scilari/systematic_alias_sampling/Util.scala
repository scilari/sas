package com.scilari.systematic_alias_sampling

import org.apache.commons.math3.random.Well512a

import scala.reflect.ClassTag
import scala.{specialized => spec}

/**
 * Created by iv on 21.1.2014.
 */
object Util {
  def almostDivides(x: Double, y: Double, eps: Double): Boolean = {
    @inline def distanceFromInt(x: Double) = { val i = x - x.toInt; math.min(i, 1.0 - i) }
    val q = x/y
    val d = distanceFromInt(q)
    d < eps
  }

  val invLogOf2 = 1.0 / math.log(2.0)

  def log2(x: Double) = math.log(x) * invLogOf2

  val random = new Well512a()

  /**
    * Jensen-Shannon divergence (by Pauli Rikula)
    *
    * @param P
    * @param Q
    * @return
    */
  def JSD(P: Array[Double], Q: Array[Double]): Double = {
    def H(P: Array[Double]): Double = {
      -1.0 * (P map { p => if (p > 0.0) p * log2(p) else 0.0 }).sum
    }
    H((P, Q).zipped map ((p, q) => {
      (p + q) / 2.0
    })) - 0.5 * H(P) - 0.5 * H(Q)
  }


  /**
    * Kullback-Leibler divergence
    *
    * @param P
    * @param Q
    * @return
    */
  def KL(P: Array[Double], Q: Array[Double]): Double = {
    //    println("P.sum = " + P.sum)
    //    println("Q.sum = " + Q.sum)
    val dP = P.sum - 1.0
    val dQ = Q.sum - 1.0
    P(P.indexWhere(_ > dP)) -= dP
    Q(Q.indexWhere(_ > dQ)) -= dQ
    ((P, Q).zipped map ((p, q) => {
      if (p > 0.0 && q > 0.0) math.log(p / q) * p else 0.0
    })).sortWith((x, y) => math.abs(x) < math.abs(y)).sum
    //if(kld < 0){
    //      println("KLD fail")
    //      println("P.sum = " + P.sum)
    //      println("Q.sum = " + Q.sum)
    //      println(P.mkString("P: [", "\n", "]"))
    //      println(Q.mkString("Q: [", "\n", "]"))
    //0
    //} else kld
  }

  /**
    * Hellinger distance
    *
    * @param P
    * @param Q
    * @return
    */
  def HellingerD(P: Array[Double], Q: Array[Double]): Double = {
    1.0 / math.sqrt(2) * math.sqrt(((P, Q).zipped map ((p, q) => math.pow(math.sqrt(p) - math.sqrt(q), 2.0))).sum)
  }

  /**
    * Chi squared
    *
    * @param P
    * @param Q
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
    * @param P
    * @param Q
    * @return
    */
  def KSS(P: Array[Double], Q: Array[Double]): Double = {
    val F1 = cumsum(P)
    val F2 = cumsum(Q)
    val differences = (F1, F2).zipped map { (x1, x2) => math.abs(x1 - x2) }
    differences.max
  }

  /**
    * Modified Cramer-Von Mises
    *
    * @param P
    * @param Q
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

  def cumsum[@spec(Float, Double) T](a: Array[T])(implicit num: Fractional[T], tag: ClassTag[T]): Array[T] = {
    import num._
    val cs = new Array[T](a.length)
    cs(0) = a(0)
    for (i <- 1 until cs.length) cs(i) = cs(i - 1) + a(i)
    cs
  }

  def normalizeSum[@spec(Float, Double) T](a: Array[T], to: T)(implicit num: Fractional[T], tag: ClassTag[T]): Array[T] = {
    import num._
    val invSum = to / a.sum
    a map {
      _ * invSum
    }
  }

  def linspace[@spec(Float, Double) T](minX: T, maxX: T, pointCount: Int)
                                      (implicit num: Fractional[T], tag: ClassTag[T]): Array[T] = {
    import num._
    val step = (maxX - minX) / fromInt(pointCount - 1)
    val a = new Array[T](pointCount)
    for (i <- 0 until pointCount) a(i) = minX + fromInt(i) * step
    a
  }

  def pointToIndex[T](x: T, minX: T, maxX: T, binCount: Int)(implicit num: Fractional[T], tag: ClassTag[T]): Int = {
    import num._
    val ix = ((x - minX) / (maxX - minX) * fromInt(binCount)).toInt
    0 max ix min (binCount - 1)
  }

  def buildHistogram[T](samples: Array[T], minX: T, maxX: T, binCount: Int)(implicit num: Fractional[T], tag: ClassTag[T]): Array[T] = {
    import num._
    val indexes = samples map {
      pointToIndex(_, minX, maxX, binCount)
    }
    val histogram = new Array[T](binCount)
    for (i <- indexes) {
      histogram(i) += one
    }
    histogram
  }

  def sampleNormal(dev: Double): Double = {
    dev * sampleNormal
  }

  def sampleNormal(): Double = {
    var sum = 0.0
    for (i <- 0 until 12) sum += random.nextDouble()
    sum - 6.0
  }

  def sampleNormal(dev: Double = 1.0, k: Int): Array[Double] = {
    val a = new Array[Double](k)
    if (dev != 1.0)
      for (i <- 0 until k) a(i) = sampleNormal(dev)
    else
      for (i <- 0 until k) a(i) = sampleNormal()
    a
  }

  class ArrayShuffler[@spec(Float, Double) T](val n: Int)(implicit tag: ClassTag[T]) {
    val indices = util.Random.shuffle((0 until n).toList).toArray

    //println(indices.mkString(" "))
    def shuffle(array: Array[T]): Array[T] = {
      val shuffled = new Array[T](n)
      val offset = random.nextInt(n)
      for (i <- array.indices) {
        val ix = i + offset
        if (ix < n)
          shuffled(i) = array(indices(ix))
        else
          shuffled(i) = array(indices(ix - n))

      }
      shuffled
    }
  }
}
