package com.scilari.systematic_alias_sampling

import scala.reflect.ClassTag

/**
  * Created by iv on 5/23/2016.
  */
class GoldenRatioAliasSampler[VALUE_T](pmf: Array[Double], values: Array[VALUE_T])(implicit tagV: ClassTag[VALUE_T]) extends SystematicAliasSampler[VALUE_T](pmf, values){
  val generator = new GoldenRatioAliasSampler.GoldenRatioSequenceGenerator()
  private[this] val binCountAsDouble = pmf.size.toDouble
  override def sample(sampleCount: Int): Array[VALUE_T] = sampleGolden(sampleCount)

  def sampleGolden(sampleCount: Int, array: Array[VALUE_T] = null): Array[VALUE_T] = {
    val a = if(array == null) new Array[VALUE_T](sampleCount) else array
    var i = 0
    while(i < sampleCount){
      a(i) = sample(binCountAsDouble*generator.next())
      i += 1
    }
    a
  }


}


object GoldenRatioAliasSampler{
  class GoldenRatioSequenceGenerator(seed: Double = Parameters.Default.random.nextDouble()) {
    private[this] var x = seed
    private[this] val gr = 0.6180339887498948482 // golden ratio conjugate
    def next(): Double = {
      x += gr
      x - x.toInt
    }

    def next(k: Int): Array[Double] = {
      Array.fill(k)(next())
    }

  }
}
