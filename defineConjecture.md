# SIR variance conservation conjecture

## ODE SIR
- Provided $R_0>1$ and $I_0 \approx 0$
- Variance in cases per case is constant (1)
- We can decompose contributions to total variance into the contribution from within- and between- cohorts of simultaneously-infected fractions of the population and:
  - total within-cohort variance, driven by the exponentially-distributed durations [discounted by falling effective reproduction number] varies monotonically from a maximum of 1 to a minimum of 0 as $R_0$ increases from $1$ to $\infty$
  - and between-cohort variance = 1-within_cohort, driven by changing cohort reproduction numbers over time, increasing from a minimum of 0 $R_0 \to 1$ to a maximum of 1 $R_0 \to \infty$  
- as a corrolary, we expect that when $CV(duration) <1$, variance in cases per case decreases monotonically from $CV(duration)^2$ to 0 as $R_0$ increases from 1 to $\infty$
- as a guess, when $CV(duration) >1$, variance in cases per case is hump-shaped, initially increasing from $CV(duration)^2$

## Individual-based stochastic SIR model (potentially equivalent to a random poisson-degree-distribution graph)
- Same provisos as above... plus the epidemic goes extinct?
- Expected variance in cases per case still conserved, at 2 (1 plus mean)