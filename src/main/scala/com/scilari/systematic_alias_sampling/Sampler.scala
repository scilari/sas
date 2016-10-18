package com.scilari.systematic_alias_sampling

import scala.reflect.ClassTag

import scala.{specialized => sp}

/**
  * Created by iv on 8/22/2016.
  */
trait Sampler[@sp(Int, Double, Float) T] {
  implicit val classTag: ClassTag[T]
  def sample: T
  def sample(k: Int): Array[T] = Array.fill(k)(sample)
  def sample(k: Int, output: Array[T]) : Array[T] = {
    for(i <- output.indices) output(i) = sample
    output
  }
}

