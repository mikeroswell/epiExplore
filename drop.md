
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

$\rho$ (`sRat`): the ratio of the mean sub-spreader reproductive number to the mean superspreader reproductive number. When $\rho = 1$ there shouldn't be differences in the classes.

They use this model to generate the latent distribution $v$, the distribution of idealized individual reproductive numbers at the start of the epidemic, and then use Poisson sampling of $v$ to get integer counts, which they then fit to real-world data.

w.r.t. JD's comment about overconfidence, maybe the issue is that they only fit the observed distribution of infections per infectee and not the timeseries of observed incidence, a recorded final size, or any other info that would inform whether the models they fit are any good for the epidemic as a whole vs. just the distribution of $Z ~ Poisson(v)$

----------------------------------------------------------------------

I think that Joshua's vision would be a paper in the mold drop/parkPropagating.pdf of the attached JRSI paper, but focusing on estimating D0 rather than R0...  that is, a way of computing or estimating D0 (the distributions of individual reproductive numbers about R0 when perturbing from the disease-free equilibrium with no susceptible depletion) -- and perhaps D_effective, given the structure and parameters of a compartment model.

Concretely, he is envisioning the contribution being something like a method to reframe the complexities added to compartment models in terms of what they say about individual differences in disease transmission and "superspreading," with a goal that Joe Modeller could think about what his model said about the degree of "superspreading" or something like that, with clarity approaching his ability to think about R0.

This is different from Meehan b/c rather than aiming to generate simple models that can fit observed data reasonably well with parseable D0, Joshua is hoping to build tools to compute D0 from fairly complex models encountered "in the wild." But the Meehan seems like a really great place for grounding this in terms of how having different classes and more complex temporal dynamics contribute, separately and jointly, to generating individual-level heterogeneity in infections per infectee.

One question JSW and I discussed last week is how complicated it is to go from 2 to N (classes or infectious stages). If there are independent

I classes , we think it's pretty easy to think about (as the distributions should compose as a weighted sum) and when there are serial infectious compartments, it might be harder.

I don't think I've done much with Joshua on the front of separating the idealized, continuous distribution from the realized, integer one. He and I have actually not had a very direct conversation about why it might be interesting to think about the the stochasticity from the Poisson process outside of how to compute/estimate the underlying distribution, though I've tried to (very) gently push in that direction.

So now, you're up to date, I think! I'm looking forward to today's meeting.

