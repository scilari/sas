package com.scilari.systematic_alias_sampling.models


/**
  * Created by iv on 10/29/2016.
  */
trait Random {
  def nextDouble(): Double
  def nextInt(n: Int): Int
}

object Random{
  def default: Random = new Random{
    val random = new scala.util.Random()
    def nextDouble() = random.nextDouble()
    def nextInt(n: Int) = random.nextInt(n)
  }
}
