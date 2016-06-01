package com.scilari.systematic_alias_sampling

import com.scilari.systematic_alias_sampling.StatisticalTests._
import org.apache.commons.math3.distribution.NormalDistribution
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Very basic and crude performance tests to check the SAS characteristics: time performance and goodness-of-fit
  * Created by iv on 5/23/2016.
  */
class PerformanceTests extends FlatSpec{
  val normal = new NormalDistribution()
  val distribution = (x: Double) => normal.density(x)

  val sas = SystematicAliasSampler(distribution, -4.0, 4.0)
  val golden = new GoldenRatioAliasSampler(sas.getPmf, sas.getValues)

  def nanosToMillis(nanos: Long): Double = nanos/1e6

  // This is simple and probably flawed way to micro benchmark anything but gives consistent results with Google Caliper
  // (may as well be flawed too).
  // TODO: consider rewriting the benchmarks using http://openjdk.java.net/projects/code-tools/jmh/
  def measureTime(block: => Any, count: Int = 1): Double = {
    var dummy: Any = null
    val t0 = System.nanoTime()
    for(i <- 0 until count) dummy = block
    nanosToMillis(System.nanoTime() - t0)
  }

  def warmUpAndMeasureTime(block: => Any, count: Int): Double = {
    val dummy = measureTime(block, count)
    measureTime(block, count)
  }

  val perfSampleCount = 110
  val perfRunCount = 20000

  "SystematicAliasSampler performance" should "be at least 3X faster than Apache Commons Math NormalDistribution.sample()" in {
    val sasTime = warmUpAndMeasureTime({val results = sas.sample(perfSampleCount)}, perfRunCount)
    val acmTime = warmUpAndMeasureTime({val results = normal.sample(perfSampleCount)}, perfRunCount)
    val timeRatio = acmTime/sasTime
    info(f"Speed improvement of SAS over ACM NormalDistribution.sample(): $timeRatio%.2f")
    timeRatio should be > 3.0
  }

  it should "be faster than Apache Commons Math NormalDistribution.sample(), when using Golden Ratio sequence" in {
    val sasTime = warmUpAndMeasureTime({val results = golden.sample(perfSampleCount)}, perfRunCount)
    val acmTime = warmUpAndMeasureTime({val results = normal.sample(perfSampleCount)}, perfRunCount)
    val timeRatio = acmTime/sasTime
    info(f"Speed improvement of SAS (Golden) over ACM NormalDistribution.sample(): $timeRatio%.2f")
    timeRatio should be > 1.0
  }


  /*-----------------------------------
   * Goodness-of-fit tests
   */
  val fitSampleCount = 110
  val fitRunCount = 100

  // Helper methods
  def empiricalDistributionSystematic(sampler: SystematicAliasSampler[Double]) = {
    val samples = sampler.sample(fitSampleCount)
    val values = sampler.getValues
    Util.normalizeSum(Util.buildHistogram(samples.toArray, values.min, values.max, values.length), 1.0)
  }

  def empiricalDistributionIid(sampler: SystematicAliasSampler[Double]) = {
    val samples = for(i <- 0 until fitSampleCount) yield sampler.sample()
    val values = sampler.getValues
    Util.normalizeSum(Util.buildHistogram(samples.toArray, values.min, values.max, values.length), 1.0)
  }

  def meanSystematicCvM(runCount: Int, sampler: SystematicAliasSampler[Double]): Double = {
     val cvm = for(run <- 0 until runCount) yield rootCramerVonMises(sampler.getPmf, empiricalDistributionSystematic(sampler))
     cvm.sum/cvm.size
  }

  def meanIidCvM(runCount: Int, sampler: SystematicAliasSampler[Double]): Double = {
    val cvm = for(run <- 0 until runCount) yield rootCramerVonMises(sampler.getPmf, empiricalDistributionIid(sampler))
    cvm.sum/cvm.size
  }

  val meanIid = meanIidCvM(fitRunCount, sas)

  "SystematicAliasSampler goodness-of-fit" should "be better than i.i.d. sampling by Cramer-von-Mises" in {
    val meanSas = meanSystematicCvM(fitRunCount, sas)
    val ratioSas = meanSas/meanIid
    info(f"SAS: Cramer-von-Mises test ratio (smaller is better): $ratioSas%.2f")
    meanSas should be < meanIid
  }

  it should "be better than i.i.d. sampling by Cramer-von-Mises when using Golden Ratio sequence" in {
    val meanGolden = meanSystematicCvM(fitRunCount, golden)
    val ratioGolden = meanGolden/meanIid
    info(f"SAS (Golden): Cramer-von-Mises test ratio (smaller is better): $ratioGolden%.2f")
    meanGolden should be < meanIid
  }

  it should "be better than i.i.d. sampling by Cramer-von-Mises with random distributions" in {
    val n = SystematicAliasSampler.BIN_COUNT_1000
    def randomDistribution = Array.fill(n)(Util.random.nextDouble())
    val runCount = 20

    for(randomDistributions <- 0 until 5){
      val sas = new SystematicAliasSampler[Double](randomDistribution, Util.linspace(-1.0, 1.0, n))
      val meanSas = meanSystematicCvM(runCount, sas)
      val meanIid = meanIidCvM(runCount, sas)
      val ratioSas = meanSas/meanIid
      info(f"Random distribution $randomDistributions%d. SAS: Cramer-von-Mises test ratio (smaller is better): $ratioSas%.2f")
      meanSas should be < meanIid
    }

  }

}
