package com.scilari.systematic_alias_sampling

import org.apache.commons.math3.random.{RandomGenerator, Well512a}

/**
  * Parameter class for the SystematicAliasSampler
  * @param random
  * @param isDivisibilityProblem
  * @param minBatchSize
  * @param minRecurSize
  * @param batchSplitNumerator
  * @param batchSplitDenominator
  */
case class Parameters(
                       val random: RandomGenerator = Parameters.Default.random,
                       isDivisibilityProblem: (Int, Int) => Boolean = Parameters.Default.isDivisibilityProblem,
                       val minBatchSize: Int = Parameters.Default.minBatchSize,
                       val minRecurSize: Int = Parameters.Default.minRecurSize,
                       val batchSplitNumerator: Int = Parameters.Default.batchSplitNumerator,
                       val batchSplitDenominator: Int = Parameters.Default.batchSplitDenominator
                     ) {
}

object Parameters{
  object Default{
    def random = new Well512a()
    val divEpsilon = 0.07
    /**
      * Checks if sampleCount or sampleCount/2 divide binCount or measurements near it (closer than range).
      * The set {1, 4, 5, 6} has been found to give very good results with the experimented bin counts
      */
    import Util.almostDivides
    def isDivisibilityProblem(binCount: Int, sampleCount: Int): Boolean = {
      almostDivides(binCount, sampleCount, divEpsilon) ||
        almostDivides(binCount*4, sampleCount, divEpsilon) ||
        almostDivides(binCount*5, sampleCount, divEpsilon) ||
        almostDivides(binCount*6, sampleCount, divEpsilon)
    }

    val minBatchSize = 16
    val minRecurSize = 4*minBatchSize

    val batchSplitNumerator = 7
    val batchSplitDenominator = 13
  }

  }


