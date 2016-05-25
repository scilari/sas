package com.scilari.systematic_alias_sampling

import org.apache.commons.math3.distribution.NormalDistribution
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Very basic and crude performance tests to check the SAS characteristics
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
  val perfRunCount = 100000

  "SystematicAliasSampler performance" should "be at least 3X faster than Apache Commons Math NormalDistribution.sample()" in {

    val sasTime = warmUpAndMeasureTime({val results = sas.sample(perfSampleCount)}, perfRunCount)
    val acmTime = warmUpAndMeasureTime({val results = normal.sample(perfSampleCount)}, perfRunCount)
    val timeRatio = acmTime/sasTime
    info(f"Speed improvement of SAS over ACM NormalDistribution.sample(): $timeRatio%.2f")
    timeRatio should be > 3.0
  }

  "SystematicAliasSampler (Golden) performance" should "be at faster than Apache Commons Math NormalDistribution.sample()" in {
    val sasTime = warmUpAndMeasureTime({val results = golden.sample(perfSampleCount)}, perfRunCount)
    val acmTime = warmUpAndMeasureTime({val results = normal.sample(perfSampleCount)}, perfRunCount)
    val timeRatio = acmTime/sasTime
    info(f"Speed improvement of SAS (Golden) over ACM NormalDistribution.sample(): $timeRatio%.2f")
    timeRatio should be > 1.0
  }

  "SystematicAliasSampler goodness-of-fit" should "be better than i.i.d. sampling by Cramer-von-Mises" in {
    val sampleCount = 100
    val runCount = 100
    val pmf = sas.getPmf
    val values = sas.getValues

    def empiricalDistributionSystematic = {
      val samples = sas.sample(sampleCount)
      Util.normalizeSum(Util.buildHistogram(samples.toArray, values.min, values.max, values.size), 1.0)
    }

    def empiricalDistributionIid = {
      val samples = for(i <- 0 until sampleCount) yield sas.sample()
      Util.normalizeSum(Util.buildHistogram(samples.toArray, values.min, values.max, values.size), 1.0)
    }

    def empiricalDistributionGolden = {
      val samples = golden.sample(sampleCount)
      Util.normalizeSum(Util.buildHistogram(samples.toArray, values.min, values.max, values.size), 1.0)
    }

    import StatisticalTests.rootCramerVonMises
    val cvmSas = for(run <- 0 until runCount) yield rootCramerVonMises(pmf, empiricalDistributionSystematic)
    val cvmIid = for(run <- 0 until runCount) yield rootCramerVonMises(pmf, empiricalDistributionIid)
    val cvmGolden = for(run <- 0 until runCount) yield rootCramerVonMises(pmf, empiricalDistributionGolden)

    val meanSas = cvmSas.sum/cvmSas.size
    val meanIid = cvmIid.sum/cvmIid.size
    val meanGolden = cvmGolden.sum/cvmGolden.size
    val ratioSas = meanSas/meanIid
    val ratioGolden = meanGolden/meanIid

    info(f"SAS: Cramer-von-Mises test ratio (smaller is better): $ratioSas%.2f")
    info(f"SAS_Golden: Cramer-von-Mises test ratio (smaller is better): $ratioGolden%.2f")

    meanSas should be < meanIid
    meanGolden should be < meanIid
  }


}
