package com.scilari.systematic_alias_sampling.core

import scala.reflect.ClassTag

/**
  * Created by iv on 5/23/2016.
  */

trait GoldenRatioAliasSampler[@specialized(Int, Double, Float) T] extends AliasSampler[T] {
  val generator = new GoldenRatioAliasSampler.GoldenRatioSequenceGenerator()

  override def sample(sampleCount: Int): Array[T] = sampleGolden(sampleCount)

  @inline
  def sampleGolden(sampleCount: Int, array: Array[T] = null)(implicit classTag: ClassTag[T]): Array[T] = {
    val a = if(array == null) new Array[T](sampleCount) else array
    val b = _binCount.toDouble
    var i = 0
    while(i < sampleCount){
      a(i) = sampleFromDouble(b*generator.next())
      i += 1
    }
    a
  }

}

object GoldenRatioAliasSampler{
  class GoldenRatioAliasSamplerBase[@specialized(Int, Double, Float) T: ClassTag](pmf: Array[Double], values: Array[T])(implicit val classTag: ClassTag[T]) extends GoldenRatioAliasSampler[T]{
    init(pmf, values)
  }

  def apply[@specialized(Int, Double, Float) T : ClassTag](pmf: Array[Double], values: Array[T]): AliasSampler[T] = new GoldenRatioAliasSamplerBase[T](pmf, values)

  class GoldenRatioSequenceGenerator(seed: Double = Parameters.Default.random.nextDouble()) {
    private[this] var x = seed
    private[this] val gr = 0.6180339887498948482 // golden ratio conjugate
    @inline
    def next(): Double = {
      x += gr
      x -= x.toInt
      x
    }

    def next(k: Int): Array[Double] = {
      Array.fill(k)(next())
    }
  }
}
