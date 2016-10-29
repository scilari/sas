package com.scilari.systematic_alias_sampling.models

/**
  * Created by iv on 10/18/2016.
  */
trait DiscreteDistribution {
  def pmf: Array[Double]
  def cdf: Array[Double] = pmf.scanLeft(0.0)(_+_).tail
}
