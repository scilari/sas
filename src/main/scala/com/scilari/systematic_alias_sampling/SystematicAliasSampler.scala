package com.scilari.systematic_alias_sampling

import org.apache.commons.math3.random.RandomGenerator

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * An implementation of the Systematic Alias Sampling described in
  *
  * Vallivaara et al.
  * "Systematic Alias Sampling: an efficient and low-variance way to sample from a discrete distribution"
  *
  * (accepted to and to be published in ACM TOMS in 2016)
  *
  *
  * @param pmf_ Probability mass function values. Does not need to be normalized, as it is normalized by the class.
  * @param values_ Values where the pmf is defined. Probability of values(i) is pmf(i).
  * @param parameters Parameter class instance.
  * @param tagV Implicit parameter for specializations.
  * @tparam VALUE_T Value type parameter.
  */
class SystematicAliasSampler[@specialized(Double, Float, Int) VALUE_T]
(
  pmf_ : Array[Double],
  values_ : Array[VALUE_T],
  parameters: Parameters = new Parameters()
)( implicit tagV: ClassTag[VALUE_T] ) {

  require(pmf_.length == values_.length, "Probability mass function array and values array are not the same length.")
  require(pmf_.length >= 2, "Distribution does not have at least two values.")

  private[this] val binCount: Int = pmf_.length
  // only protected because specialization generates access errors otherwise (for some reason)
  protected[this] val pmf: Array[Double] = Util.normalizeSum(pmf_.clone(), 1.0)
  private[this] val values: Array[VALUE_T] = values_.clone()

  private[this] val (aliasedValues, aliasProbabilities) = SystematicAliasSampler.createAlias(pmf, values)

  // init parameters
  private[this] val random: RandomGenerator = parameters.random
  private[this] val isDivisibilityProblem: (Int, Int) => Boolean = parameters.isDivisibilityProblem
  private[this] val minBatchSize: Int = parameters.minBatchSize
  private[this] val minRecurSize: Int = parameters.minRecurSize
  private[this] val batchSplitNumerator: Int = parameters.batchSplitNumerator
  private[this] val batchSplitDenominator: Int = parameters.batchSplitDenominator

  // getters and convenience methods e.g. for testing
  def getPmf = pmf.clone()
  def getValues = values.clone()
  def getAliasedValues = aliasedValues.clone()
  def getAliasProbabilities = aliasProbabilities.clone()
  def cdf: Array[Double] = Util.cumsum(pmf)


  /**
    * Samples a value from the underlying alias table structure. The returned value is deterministic with given, fixed
    * input.
    * @param randomInt  Random or otherwise generated integer (corresponding to bin)
    * @param randomDouble  Random or otherwise generated double (corresponding to selection between upper and lower bin)
    * @return
    */
  @inline
  def sample(randomInt: Int = random.nextInt(binCount), randomDouble: Double = random.nextDouble()): VALUE_T = {
    if(randomDouble <= aliasProbabilities(randomInt))
      aliasedValues(randomInt)
    else
      values(randomInt)
  }

  /**
    * Convenience method to sample by using a single Double value in [0, binCount[ that is used to extract the integer
    * and fractional parts for sampling.
 *
    * @param randomDouble Double value in [0, binCount[
    * @return
    */
  def sample(randomDouble: Double): VALUE_T = {
    val intPart = randomDouble.toInt
    val fracPart = randomDouble - intPart
    sample(intPart, fracPart)
  }

  /**
    * Systematic sampling of sampleCount samples.
    * @param sampleCount Number of samples.
    * @return
    */
  def sample(sampleCount: Int): Array[VALUE_T] = sampleSystematic(sampleCount)

  /**
    * Systematic sampling of sampleCount samples with control to provide the output array. Also uses an indexto keep
    * track of computed values to avoid explicit array concatenation in recursive calls.
    * @param sampleCount Number of samples.
    * @param results Output array.
    * @param fillFrom Index to keep track of computed values.
    * @return
    */
  def sampleSystematic(sampleCount: Int, results: Array[VALUE_T] = null, fillFrom: Int = 0): Array[VALUE_T] = {
    val samples = if(results == null) new Array[VALUE_T](sampleCount) else results
    if(sampleCount > minBatchSize && isDivisibilityProblem(binCount, sampleCount)){
      val splitIndex: Int =
        if(sampleCount <= minRecurSize)
          minBatchSize
        else
          sampleCount * batchSplitNumerator / batchSplitDenominator
      sampleSystematic(sampleCount - splitIndex, samples, fillFrom)
      sampleSystematic(splitIndex, samples, fillFrom + sampleCount - splitIndex)
    }
    else {
      val step =  binCount/sampleCount.toDouble
      val r = step * random.nextDouble()
      // going backwards in order to prevent index out of bounds error (this is fine, because sample(-0, -epsilon) = sample(0, 0))
      val rk = binCount - r
      var x = rk
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

  override def toString = {
    "AliasSampler with " + binCount + " bins: \n" +
      pmf.mkString("pdf = [", " ", "]\n") +
      values.mkString("points = [", " ", "]\n") +
      aliasedValues.mkString("aliasedPoints = [", " ", "]\n") +
      //aliasIndexes.mkString("aliasIndexes = [", " ", "]\n") +
      aliasProbabilities.mkString("aliasProb = [", " ", "]\n") +
      cdf.mkString("cdf = [", " ", "]\n")
  }

}

object SystematicAliasSampler{
  // primes close to typical bin counts
  val BIN_COUNT_50 = 53
  val BIN_COUNT_100 = 101
  val BIN_COUNT_250 = 251
  val BIN_COUNT_1000 = 1009
  val BIN_COUNT_10000 = 10009
  val BIN_COUNT_100000 = 100003

  // Specialized subclasses to allow easier inheritance (with more probable specialization)
  class DoubleSampler(pmf: Array[Double], values: Array[Double], parameters: Parameters) extends SystematicAliasSampler[Double](pmf, values, parameters)
  class FloatSampler(pmf: Array[Double], values: Array[Float], parameters: Parameters) extends SystematicAliasSampler[Float](pmf, values, parameters)
  class IntSampler(pmf: Array[Double], values: Array[Int], parameters: Parameters) extends SystematicAliasSampler[Int](pmf, values, parameters)

  /**
    * Convenience method to create Systematic Alias Sampler from a continuous distribution by generating a corresponding
    * discrete approximation automatically.
    * @param distribution Function describing the distribution.
    * @param minX Minimum point for approximated values.
    * @param maxX Maximum point for approximated values.
    * @param binCount Number of values in the approximation.
    * @param parameters Parameter class instance.
    * @return SystematicAliasSampler instance initialized with approximate of the given distribution.
    */
  def apply(
             distribution: Double => Double,
             minX: Double,
             maxX: Double,
             binCount: Int = BIN_COUNT_1000,
             parameters: Parameters = new Parameters()
           ): DoubleSampler = {

    val (pmf, points) = Util.distributionApproximation(distribution, minX, maxX, binCount)
    new DoubleSampler(pmf, points, parameters)
  }

  /**
    * Initializes the alias table.
    * Kronmal and Peterson 1979 implementation of alias method by Walker (1974, 1977) as described in
    * "An Analysis of the Alias Method for Discrete Random-Variate Generation" by Smith and Jacobson (2005)
    * @param pmf Probability mass function.
    * @param values Values corresponding to the probability mass function.
    * @return Tuple of aliasedValues and corresponding aliasProbabilities.
    */
  def createAlias[@specialized(Double, Int) T]( pmf: Array[Double], values: Array[T] )(implicit tag: ClassTag[T] ): (Array[T], Array[Double]) = {
    val n = pmf.length
    val q = (0 until n).map{ n.toDouble*pmf(_) }.toArray
    // using stacks to retain spatial order
    val (g, h) = mutable.Stack(0 until n : _*).partition{ i => q(i) >= 1.0 } // indexes of greater and smaller items
    val a = (0 until n).toArray // aliasIndexes
    val s = new Array[Double](n) // aliasProbabilities

    // Checking both because of possible numerical inaccuracies large may be emptied first
    while(h.nonEmpty && g.nonEmpty){
      val j = h.pop()
      val k = g.top
      a(j) = k
      s(j) = 1.0 - q(j)
      q(k) = (q(k) + q(j)) - 1.0 // numerically more stable

      if( q(k) < 1.0 ){
        g.pop()
        h.push(k)
      }
    }

    val aliasedValues = a.map{values(_)}
    (aliasedValues, s)
  }

}
