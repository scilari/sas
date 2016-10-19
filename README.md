# Systematic Alias Sampling: an efficient and low-variance way to sample from a discrete distribution
The code here implements the method described in paper 

Vallivaara et al.: "Systematic Alias Sampling: an efficient and low-variance way to sample from a discrete distribution"

The method achieves 5-20X speed up compared to Apache Commons Math NormalDistribution.sample() when sampling in batches. The empirical distribution of the batches also have significantly better goodness-of-fit according to Cramer-Von-Mises statistic.
