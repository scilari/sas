package com.scilari.systematic_alias_sampling.core

import com.scilari.systematic_alias_sampling.models.{Random, DiscreteDistribution, Sampler}
import com.scilari.systematic_alias_sampling.util.Helpers

import scala.reflect.ClassTag
import scala.{specialized => sp}


/**
  * Created by iv on 8/22/2016.
  */
trait AliasSampler[@sp(Int, Double, Float) T] extends Sampler[T] with DiscreteDistribution{
  protected var _pmf: Array[Double] = _
  protected var _values: Array[T] = _
  protected var _aliasedValues: Array[T] = _
  protected var _aliasProbabilities: Array[Double] = _
  protected var _binCount: Int = _

  val random: Random = Parameters.Default.random

  /**
    * Samples a random value from the underlying alias table structure.
    *
    * @return Single sample
    */
  def sample(): T = sample(random.nextInt(_binCount), random.nextDouble())

  /**
    * Samples a value from the underlying alias table structure. The returned value is deterministic with given, fixed
    * input.
    *
    * @param randomInt  Random or otherwise generated integer (corresponding to bin)
    * @param randomDouble  Random or otherwise generated double (corresponding to selection between upper and lower bin)
    * @return Single (determimistic) sample.
    */
  def sample(randomInt: Int, randomDouble: Double): T = {
    if(randomDouble <= _aliasProbabilities(randomInt))
      _aliasedValues(randomInt)
    else
      _values(randomInt)
  }


  /**
    * Initializes the data structures.
    *
    * @param pmf Probability mass function.
    * @param values Masses corresponding to the probability mass function.
    */
  protected def init(pmf: Array[Double], values: Array[T]): Unit ={
     this._pmf = Helpers.normalizeSum(pmf, 1.0)
     this._values = values.clone()
     val aliasTable = new AliasTable[T](this._pmf, this._values)
     this._aliasedValues = aliasTable.aliasedValues
     this._aliasProbabilities = aliasTable.aliasProbabilities
     this._binCount = this._pmf.length
  }

  /**
    * Convenience method to sample by using a single Double value in [0, binCount[ that is used to extract the integer
    * and fractional parts for sampling.
    *
    * @param randomDouble Double value in [0, binCount[
    * @return Single (deterministic) sample.
    */
  def sampleFromDouble(randomDouble: Double): T = {
    val intPart = randomDouble.toInt
    val fracPart = randomDouble - intPart
    sample(intPart, fracPart)
  }

  // getters and convenience methods e.g. for testing
  def pmf: Array[Double] = _pmf.clone()
  def values: Array[T] = _values.clone()
  def aliasedValues: Array[T] = _aliasedValues.clone()
  def aliasProbabilities: Array[Double] = _aliasProbabilities.clone()

}

object AliasSampler{
  def apply[@sp(Int, Double, Float) T: ClassTag](pmf: Array[Double], values: Array[T]): AliasSampler[T] = new AliasSamplerBase[T](pmf, values)

  class AliasSamplerBase[@sp(Int, Double, Float) T](pmf: Array[Double], values: Array[T])(implicit val classTag: ClassTag[T]) extends AliasSampler[T]{
    init(pmf, values)
  }

}










