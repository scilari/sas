package com.scilari.systematic_alias_sampling

import org.apache.commons.math3.distribution.NormalDistribution
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

/**
  * Created by iv on 5/23/2016.
  */
class PerformanceTests extends FlatSpec{
  val normal = new NormalDistribution()
  val distribution = (x: Double) => normal.density(x)

  val sas = SystematicAliasSampler(distribution, -4.0, 4.0)
  val golden = new GoldenRatioAliasSampler(sas.getPmf, sas.getValues)

  def nanosToMillis(nanos: Long): Double = nanos/1e6

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

  "SystematicAliasSampler" should "be at least 3X faster than Apache Commons Math NormalDistribution.sample()" in {
    val sampleCount = 111
    val runCount = 100000
    val sasTime = warmUpAndMeasureTime({val results = sas.sample(sampleCount)}, runCount)
    val acmTime = warmUpAndMeasureTime({val results = normal.sample(sampleCount)}, runCount)
    val timeRatio = acmTime/sasTime
    println("Time ratio (speed improvement): " + timeRatio)
    timeRatio should be > 3.0
  }

  "SystematicAliasSampler" should "have better fit than i.i.d. sampling by Cramer-von-Mises" in {
    val sampleCount = 100
    val runCount = 10000
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


    val cvmSas = for(run <- 0 until runCount) yield Util.rootCramerVonMises(pmf, empiricalDistributionSystematic)
    val cvmIid = for(run <- 0 until runCount) yield Util.rootCramerVonMises(pmf, empiricalDistributionIid)
    val cvmGolden = for(run <- 0 until runCount) yield Util.rootCramerVonMises(pmf, empiricalDistributionGolden)

    val meanSas = cvmSas.sum/cvmSas.size
    val meanIid = cvmIid.sum/cvmIid.size
    val meanGolden = cvmGolden.sum/cvmGolden.size
    val ratioSas = meanSas/meanIid
    val ratioGolden = meanGolden/meanIid

    println("SAS: Cramer-von-Mises test ratio (smaller is better): " + ratioSas )
    println("SAS_Golden: Cramer-von-Mises test ratio (smaller is better): " + ratioGolden )

    meanSas should be < meanIid
    meanGolden should be < meanIid
  }


}
