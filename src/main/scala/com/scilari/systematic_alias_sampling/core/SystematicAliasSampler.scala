package com.scilari.systematic_alias_sampling.core

import com.scilari.systematic_alias_sampling.models.Random
import com.scilari.systematic_alias_sampling.util.Helpers

import scala.reflect.ClassTag
import scala.{specialized => sp}

/**
  * An implementation of the Systematic Alias Sampling described in
  *
  * Vallivaara et al.
  * "Systematic Alias Sampling: an efficient and low-variance way to sample from a discrete distribution"
  *
  * ACM Transactions on Mathematical Software (TOMS)
  * Volume 43 Issue 3, August 2016
  * Article No. 18
  *
  * http://dl.acm.org/citation.cfm?id=2935745
  *
  */
trait SystematicAliasSampler[@sp(Int, Double, Float) T] extends AliasSampler[T] {
  val parameters: Parameters = new Parameters()
  // init parameters
  import parameters._
  override val random: Random = parameters.random

  /**
    * Systematic sampling of sampleCount samples.
    *
    * @param sampleCount Number of samples.
    * @return Batch of possibly partially ordered samples.
    */
  override def sample(sampleCount: Int): Array[T] = {
    sampleSystematic(sampleCount, new Array[T](sampleCount))
  }

  /**
    * Systematic sampling of sampleCount samples using a pre-created output array.
    *
    * @param sampleCount Number of samples.
    * @param output Pre-created output array.
    * @return Batch of possibly partially ordered samples.
    */
  override def sample(sampleCount: Int, output: Array[T]): Array[T] = sampleSystematic(sampleCount, output)

  /**
    * Systematic sampling of sampleCount samples with control to provide the output array. Also uses an index to keep
    * track of computed values to avoid explicit array concatenation in recursive calls.
    *
    * @param sampleCount Number of samples.
    * @param samples Output array.
    * @param fillFrom Index to keep track of computed values.
    * @return Batch of possibly partially ordered samples.
    */
  def sampleSystematic(sampleCount: Int, samples: Array[T], fillFrom: Int = 0): Array[T] = {
    if(sampleCount > minBatchSize && isDivisibilityProblem(_binCount, sampleCount)){
      val splitIndex: Int =
        if(sampleCount <= minRecurSize)
          minBatchSize
        else
          sampleCount * batchSplitNumerator / batchSplitDenominator
      sampleSystematic(sampleCount - splitIndex, samples, fillFrom)
      sampleSystematic(splitIndex, samples, fillFrom + sampleCount - splitIndex)
    }
    else {
      val step =  _binCount/sampleCount.toDouble
      val r = step * (1.0 - random.nextDouble()) // (0, step]
      // going backwards in order to prevent index out of bounds error (this is fine, because sample(-0, -epsilon) = sample(0, 0))
      var x = _binCount - r
      var i = fillFrom
      val fillTo = fillFrom + sampleCount

      while(i < fillTo){
        val ri = x.toInt
        val rf = x - ri
        samples(i) = sample(ri, rf)
        x -= step
        i += 1
      }
      samples
    }
  }
}

object SystematicAliasSampler{
  def almostDivides(x: Double, y: Double, eps: Double): Boolean = {
    @inline def distanceFromInt(x: Double) = { val i = x - x.toInt; math.min(i, 1.0 - i) }
    val q = x/y
    val d = distanceFromInt(q)
    d < eps
  }

  // primes close to typical bin counts
  val BIN_COUNT_50 = 53
  val BIN_COUNT_100 = 101
  val BIN_COUNT_250 = 251
  val BIN_COUNT_1000 = 1009
  val BIN_COUNT_10000 = 10007
  val BIN_COUNT_100000 = 100003

  def apply[@sp(Int, Double, Float) T : ClassTag](pmf: Array[Double], values: Array[T]): AliasSampler[T] = new SystematicAliasSamplerBase[T](pmf, values)


  type IntSampler = SystematicAliasSamplerBase[Int]
  type DoubleSampler = SystematicAliasSamplerBase[Double]
  type FloatSampler = SystematicAliasSamplerBase[Float]

  class SystematicAliasSamplerBase[@sp(Int, Double, Float) T](pmf: Array[Double], values: Array[T])(implicit val classTag: ClassTag[T]) extends SystematicAliasSampler[T]{
    init(pmf, values)
  }

  /**
    * Convenience method to create Alias Sampler from a continuous distribution by generating a corresponding
    * discrete approximation automatically.
    *
    * @param distribution Function describing the distribution.
    * @param minX Minimum point for approximated values.
    * @param maxX Maximum point for approximated values.
    * @param binCount Number of values in the approximation.
    * @param parameters Parameter class instance.
    * @return SystematicAliasSampler instance initialized with approximate of the given distribution.
    */
  def fromDistribution(
    distribution: Double => Double,
    minX: Double,
    maxX: Double,
    binCount: Int = SystematicAliasSampler.BIN_COUNT_1000,
    parameters: Parameters = new Parameters()
  ): SystematicAliasSampler[Double] = {

    val (pmf, points) = Helpers.distributionApproximation(distribution, minX, maxX, binCount)
    new SystematicAliasSamplerBase[Double](pmf, points)
  }


}
