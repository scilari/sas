# Systematic Alias Sampling: an efficient and low-variance way to sample from a discrete distribution

![Alt text](./sas.png?raw=true "Illustration of Systematic Alias Sampling")


The code here implements the method described in paper 

Vallivaara et al. 
*"Systematic Alias Sampling: an efficient and low-variance way to sample from a discrete distribution"*

ACM Transactions on Mathematical Software (TOMS)
Volume 43 Issue 3, August 2016
Article No. 18

http://dl.acm.org/citation.cfm?id=2935745

The method achieves 5-20X speed up compared to Apache Commons Math NormalDistribution.sample() when sampling in batches. The empirical distribution of the batches also have significantly better goodness-of-fit according to Cramer-Von-Mises statistic.

## Usage
#### Constructing the sampler:
In order to sample, first create the needed structures based on the given discrete distribution. 
You can provide the values and their probabilities either manually or by using the helper methods.

**Manual way:**
```
import com.scilari.systematic_alias_sampling.core._

val values = Array(-0.2, -0.1, 0.0, 0.1, 0.3) // possible values
val pmf = Array(0.1, 0.1, 0.5, 0.2, 0.1) // corresponding probabilities
val sampler = SystematicAliasSampler(pmf, values)
         
```

**Using helpers:**
```
import com.scilari.systematic_alias_sampling.core._
import com.scilari.systematic_alias_sampling.util.Helpers

val density: Double => Double = /* provide your density function here */
val (pmf, values) = Helpers.distributionApproximation(
        distribution = density, 
        minX = -4.0, maxX = 4.0, // support
        binCount = SystematicAliasSampler.BINCOUNT_1000 // prime close to 1000
        )

val sampler = SystematicAliasSampler(pmf, values)
```

#### Sampling:
In order to benefit from the better goodness-of-fit and performance characteristics, generate samples in batches.
The sampler provides several methods to generate batches
```
// batch sampling
val batchA = sampler.sample(100)
val batchB = sampler.sample(273)

// single sample, not efficient but sometimes convenient
val sample = sampler.sample()

// providing the output array
val output = new Array[Double](100)
sampler.sample(100, output)
```