package com.scilari.systematic_alias_sampling

import com.scilari.systematic_alias_sampling.core.{SystematicAliasSampler, AliasSampler, GoldenRatioAliasSampler}
import com.scilari.systematic_alias_sampling.util.Helpers
import org.apache.commons.math3.distribution.NormalDistribution

/**
  * Created by iv on 8/22/2016.
  */
object TestUtil {
  object Timing {
    def nanosToMillis(nanos: Long): Double = nanos / 1e6

    // This is simple and probably flawed way to micro benchmark anything but gives consistent results with Google Caliper
    // (may as well be flawed too).
    // TODO: consider rewriting the benchmarks using http://openjdk.java.net/projects/code-tools/jmh/
    def measureTime[T](block: => T, count: Int = 1): Double = {
      val t0 = System.nanoTime()
      for (i <- 0 until count) {
        val dummy: T = block
        //BlackHole.consumeAny(dummy)
      }
      nanosToMillis(System.nanoTime() - t0)
    }

    val defaultWarmupCount = 100

    def warmUpAndMeasureTime(block: => Any, count: Int, warmupCount: Int = defaultWarmupCount): Double = {
      for (i <- 0 until warmupCount) BlackHole.consumeDouble(measureTime(block, count))
      measureTime(block, count)
    }

    // naive black hole implementation
    object BlackHole {
      var state: Double = 0
      var anyState: Any = _

      def consume(array: Array[Double]): Unit = {
        state = array(0)
      }

      def consumeDouble(value: Double): Unit = {
        state = value
      }

      def consumeAny(value: Any): Unit = {
        anyState = value
      }
    }

  }

  object Misc{
    val normal = new NormalDistribution()
    val normalDistributionDensity = (x: Double) => normal.density(x)

    val minX = -4
    val maxX = 4
    val binCount = SystematicAliasSampler.BIN_COUNT_10000
    val (pmf, values) = Helpers.distributionApproximation(normalDistributionDensity, minX, maxX, binCount)
    //val normalSas = SystematicAliasSampler.fromDistribution(normalDistributionDensity, minX, maxX, binCount)
    val normalSas: AliasSampler[Double] = SystematicAliasSampler(pmf, values)
    val normalGolden: AliasSampler[Double] = GoldenRatioAliasSampler(pmf, values)

  }

}
