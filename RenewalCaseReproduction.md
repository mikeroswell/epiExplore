---
output:
  pdf_document: default
  html_document: default
---
# Case reproduction number distribution using the renewal equation framework

[MER: just writing some equations from Park et al 2020 PNAS to get started on this]

Our goal is to find higher moments of the case reproduction number $\mathcal{R}_{c}$, or really, to identify how variance (Which we believe to be 1 in the canonical SIR with exponential durations) depends on the distribution of generation intervals/infectiousness kernel. 

We begin with an epidemic model without demographics, with $x$ fraction susceptible, $y$ infectious, $z$ removed, and define incidence $i(t)$ and final size $Z = \int{dt\, i(t)}$. We assume susceptible depletion only occurs via infection, so  
$$\frac{dx}{dt} = -i(t)$$.

At any given time $t$, we have incidence given by the instantaneous reproduction number, multiplied by the proportion of the population infectious at time $t$, and a generation interval distribution $g(\tau)$:
% I copied limits of integration from Park et al. 2020 PNAS forward-looking serial intervals, but I don't know why the upper limit is infinity instead of t here (their eq 12)
$$i(t) = \mathcal{R}_{t} \int_{0}^{\infty}{d\tau\, i(t-\tau) g(\tau)}$$

Here, we further assume that infectiousness, which may change over the course of an individuals infection, doesn't otherwise evolve over time, so
$$  \mathcal{R}_{t} =  \mathcal{R}_{0} x(t) $$,

And the random variable whose distribution we want to interrogate is the case reproduction number $\mathcal{R}_{c}$:

$$\mathcal{R}_{c} = \mathcal{R}_{0} \int_{0}^{\infty}{d\tau\, g(\tau) x(t+\tau)}$$

## mean case reproduction number

We feel comfortable (assuming $y(0)$ negligible) that $\bar{\mathcal{R}_{c}} = 1$
[still not sure why not upper limit of sigma = t]:
\begin{eqnarray}
Z \bar{\mathcal{R}_{c}} 
  \\ = \mathcal{R}_{0} \int{dt\, i(t) \int_{0}^{\infty}{d\tau\, g(\tau) x(t+\tau)}} 
  \\ = \mathcal{R}_{0}^2\int{dt\, \int_{0}^{\infty}{d\sigma\, i(t-\sigma) g(\sigma) \int_{0}^{\infty}{d\tau\, g(\tau) x(t+\tau)}}}
  \\ = Z
\end{eqnarray}

