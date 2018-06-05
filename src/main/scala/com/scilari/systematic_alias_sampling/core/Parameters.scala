package com.scilari.systematic_alias_sampling.core

import com.scilari.systematic_alias_sampling.models.Random

/**
  * Parameter class for the SystematicAliasSampler
  *
  * @param random RNG to produce the needed random numbers.
  * @param isDivisibilityProblem Function to define whether to split the computation recursively into smaller batches.
  * @param minBatchSize Threshold for minimum batch size in recursion.
  * @param minRecurSize Threshold for using a multiplication with batchSplitNumerator/batchSplitDenominator to define
  *                     the recursive batch sizes.
  * @param batchSplitNumerator Numerator to define the ratio in which the batch sizes are determined in the recursion.
  * @param batchSplitDenominator Denominator to define the ratio in which the batch sizes are determined in the recursion.
  */
case class Parameters(
  random: Random = Parameters.Default.random,
  isDivisibilityProblem: (Int, Int) => Boolean = Parameters.Default.isDivisibilityProblem,
  minBatchSize: Int = Parameters.Default.minBatchSize,
  minRecurSize: Int = Parameters.Default.minRecurSize,
  batchSplitNumerator: Int = Parameters.Default.batchSplitNumerator,
  batchSplitDenominator: Int = Parameters.Default.batchSplitDenominator
) {
}

object Parameters{
  object Default{
    def random: Random = Random.default

    val divEpsilon: Double = 0.07

    /**
      * Checks if sampleCount or sampleCount/2 divide binCount or measurements near it (closer than range).
      * The set {1, 4, 5, 6} has been found to give very good results with the experimented bin counts
      */
    def isDivisibilityProblem(binCount: Int, sampleCount: Int): Boolean = {
      import SystematicAliasSampler.almostDivides
      almostDivides(binCount, sampleCount, divEpsilon) ||
        almostDivides(binCount*4, sampleCount, divEpsilon) ||
        almostDivides(binCount*5, sampleCount, divEpsilon) ||
        almostDivides(binCount*6, sampleCount, divEpsilon)
    }

    val minBatchSize: Int = 16
    val minRecurSize: Int = 4*minBatchSize

    val batchSplitNumerator: Int = 7
    val batchSplitDenominator: Int = 13
  }

}


