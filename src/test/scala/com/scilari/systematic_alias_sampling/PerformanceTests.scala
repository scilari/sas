package com.scilari.systematic_alias_sampling

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import TestUtil.Timing._
import TestUtil.Misc._

/**
  * Very basic and crude performance tests to check the SAS time performance characteristics
  * Created by iv on 5/23/2016.
  */
class PerformanceTests extends FlatSpec{
  val sampleCount = 1100
  val runCount = 200
  val warmupCount = 100

  val acmTime = warmUpAndMeasureTime({BlackHole.consume(normal.sample(sampleCount))}, runCount, warmupCount)

  "SystematicAliasSampler performance" should "be at least 3X faster than Apache Commons Math NormalDistribution.sample()" in {
    val sasTime = warmUpAndMeasureTime({BlackHole.consume(normalSas.sample(sampleCount))}, runCount, warmupCount)
    val timeRatio = acmTime/sasTime
    info(f"Speed improvement of SAS over ACM NormalDistribution.sample(): $timeRatio%.2f")
    timeRatio should be > 3.0
  }

  "GoldenRatioSampler performance" should "be faster than Apache Commons Math NormalDistribution.sample()" in {
    val sasTime = warmUpAndMeasureTime({BlackHole.consume(normalGolden.sample(sampleCount))}, runCount, warmupCount)
    val timeRatio = acmTime/sasTime
    info(f"Speed improvement of SAS (Golden) over ACM NormalDistribution.sample(): $timeRatio%.2f")
    timeRatio should be > 1.0
  }

}
