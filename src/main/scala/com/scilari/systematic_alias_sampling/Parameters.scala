package com.scilari.systematic_alias_sampling

import org.apache.commons.math3.random.{RandomGenerator, Well512a}

/**
  * Parameter class for the SystematicAliasSampler
  * @param random RNG to produce the needed random numbers.
  * @param isDivisibilityProblem Function to define whether ot split the computation recursively into smaller batches.
  * @param minBatchSize Threshold for minimum batch size in recursion.
  * @param minRecurSize Threshold for using a multiplication with batchSplitNumerator/batchSplitDenominator to define
  *                     the recursive batch sizes.
  * @param batchSplitNumerator Numerator to define the ratio in which the batch sizes are determined in the recursion.
  * @param batchSplitDenominator Denominator to define the ratio in which the batch sizes are determined in the recursion.
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
    def isDivisibilityProblem(binCount: Int, sampleCount: Int): Boolean = {
      import Util.almostDivides
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


