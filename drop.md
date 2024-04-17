

Replicating superspreader dynamics with compartmental models
* drop/meehan.pdf
* drop/meehanSupp.pdf

Seems overconfident but interesting, based on abstract so far.

MER notes: Meehan et al. consider ways to make a simple compartment model
sufficiently complex to match realized individual reproductive number
(distribution of new infections per infectee). They suggest that by having a
probabilistic sorting of susceptible individuals into one of two
infected/infectious classes, and by having two infectious stages, they can
recapitulate observed variation in infections per infectee at least as well as a
more phenomenological negative binomial a la Jamie Lloyd-Smith et al. 2005

Their SIIR model requires four parameters, which I will describe with their
notation, the corresponding variables in `myMeehan.R`, and a brief verbal gloss.

$R$ (`R0`): R naught

$\sigma$ (`iRat`): This parameter tunes the infection dynamics of the serial
compartments. The idea is that the ratio in average reproductive number between
I_1 and I_2 are the same for both infectious classes, so this parameter is that
ratio. When $\sigma>1$ the first serial compartment has higher reproductive
number and when $\sigma<1$, less, though I think Meehan et al. only consider $0
\leq \sigma \leq 1$ because for the purposes of fitting the distribution of
individual reproductive numbers, it is equivalent to have the faster compartment
first vs. second. When $\sigma = 0$ this collapses to an S[E]IR model where E is
not relevant for the focal distribution of individual reproductive number, where
as with R0, we assume we're perturbing from the disease-free equilibrium without
suceptible depletion (call that D0; MER hasn't thought about how to compute
D_effective yet)

$c$ (`pRat`): probability a susceptible individual moves to the superspreader
infectious class. $c = 0$ is a single-class model if $c = \sigma = 0$ we get a
single-class S[E]IR model.

$\rho$ (`sRat`): the ratio of the mean sub-spreader reproductive number to the
mean superspreader reproductive number. When $\rho = 1$ there shouldn't be
differences in the classes.


They use this model to generate the latent distribution $v$, the distribution of 
idealized individual reproductive numbers at the start of the epidemic, and then 
use Poisson sampling of $v$ to get integer counts, which they then fit to real-world data.

w.r.t. JD's comment about overconfidence, maybe the issue is that they only fit
the observed distribution of infections per infectee and not the timeseries of
observed incidence, a recorded final size, or any other info that would inform
whether the models they fit are any good for the epidemic as a whole vs. just the
distribution of $Z ~ Poisson(v)$



