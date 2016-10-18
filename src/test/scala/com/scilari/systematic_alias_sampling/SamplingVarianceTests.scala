package com.scilari.systematic_alias_sampling

import com.scilari.systematic_alias_sampling.core.{SystematicAliasSampler, AliasSampler}
import com.scilari.systematic_alias_sampling.util.{Helpers, StatisticalTests}
import StatisticalTests._
import SystematicAliasSampler.SystematicAliasSamplerBase
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import TestUtil.Misc._


/**
  * Created by iv on 8/25/2016.
  */
class SamplingVarianceTests extends FlatSpec{

  val fitSampleCount = 110
  val fitRunCount = 100

  // Helper methods
  def empiricalDistributionSystematic(sampler: AliasSampler[Double]) = {
    val samples = sampler.sample(fitSampleCount)
    val values = sampler.values
    Helpers.normalizeSum(Helpers.buildHistogram(samples.toArray, values.min, values.max, values.length), 1.0)
  }

  def empiricalDistributionIid(sampler: AliasSampler[Double]) = {
    val samples = for(i <- 0 until fitSampleCount) yield sampler.sample()
    val values = sampler.values
    Helpers.normalizeSum(Helpers.buildHistogram(samples.toArray, values.min, values.max, values.length), 1.0)
  }

  def meanSystematicCvM(runCount: Int, sampler: AliasSampler[Double]): Double = {
    val cvm = for(run <- 0 until runCount) yield rootCramerVonMises(sampler.pmf, empiricalDistributionSystematic(sampler))
    cvm.sum/cvm.size
  }

  def meanIidCvM(runCount: Int, sampler: AliasSampler[Double]): Double = {
    val cvm = for(run <- 0 until runCount) yield rootCramerVonMises(sampler.pmf, empiricalDistributionIid(sampler))
    cvm.sum/cvm.size
  }

  val meanIid = meanIidCvM(fitRunCount, normalSas)
  val qualityThreshold = 0.75*meanIid

  "SystematicAliasSampler goodness-of-fit" should "be clearly better than i.i.d. sampling by Cramer-von-Mises" in {
    val meanSas = meanSystematicCvM(fitRunCount, normalSas)
    val ratioSas = meanSas/meanIid
    info(f"SAS: Cramer-von-Mises test ratio (smaller is better): $ratioSas%.2f")
    meanSas should be < qualityThreshold
  }

  it should "be clearly better than i.i.d. sampling by Cramer-von-Mises when using Golden Ratio sequence" in {
    val meanGolden = meanSystematicCvM(fitRunCount, normalGolden)
    val ratioGolden = meanGolden/meanIid
    info(f"SAS (Golden): Cramer-von-Mises test ratio (smaller is better): $ratioGolden%.2f")
    meanGolden should be < qualityThreshold
  }

  it should "be clearly better than i.i.d. sampling by Cramer-von-Mises with random distributions " + 3.0 in {
    val n = SystematicAliasSampler.BIN_COUNT_100
    def randomDistribution = Array.fill(n)(Helpers.random.nextDouble())
    val runCount = 20

    for(randomDistributions <- 0 until 5){
      val sas = new SystematicAliasSamplerBase[Double](randomDistribution, Helpers.linspace(-1.0, 1.0, n))
      val meanSas = meanSystematicCvM(runCount, sas)
      val meanIid = meanIidCvM(runCount, sas)
      //val meanIid = meanSystematicCvM(runCount, sas)
      val ratioSas = meanSas/meanIid
      info(f"Random distribution $randomDistributions%d. SAS: Cramer-von-Mises test ratio (smaller is better): $ratioSas%.2f")
      ratioSas should be < 0.8
    }

  }



}
