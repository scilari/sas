package com.scilari.systematic_alias_sampling

import org.apache.commons.math3.distribution.NormalDistribution
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by iv on 6/1/2016.
  */
class AliasTableTests extends FlatSpec{
  val normal = new NormalDistribution()
  val distribution = (x: Double) => normal.density(x)

  val binCount = 100
  val (pmf, _) = Util.distributionApproximation(distribution, -4.0, 4.0, binCount)
  val sas = new SystematicAliasSampler[Int](pmf, (0 until binCount).toArray)

  val values = sas.getValues
  val aliasedValues = sas.getAliasedValues
  val aliasProbabilities = sas.getAliasProbabilities

  val probabilitySum = new Array[Double](binCount)

  for(i <- 0 until binCount){
    probabilitySum(i) += (1.0 - aliasProbabilities(i))/binCount
    probabilitySum(aliasedValues(i)) += aliasProbabilities(i)/binCount
  }

  "Alias table probabilities" should "sum up to the original distribution" in {
    (pmf zip probabilitySum).foreach{ case (p1: Double, p2: Double) =>
        p1 should equal (p2 +- 0.0000001)
    }

    val totalDiff = (pmf zip probabilitySum).map{ case (p1: Double, p2: Double) => math.abs(p1 - p2)}.sum
    info(f"Total difference between aliased probabilities and original pmf: $totalDiff%f")
  }

}
