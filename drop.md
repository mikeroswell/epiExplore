
Replicating superspreader dynamics with compartmental models
* drop/meehan.pdf
* drop/meehanSupp.pdf

Seems overconfident but interesting, based on abstract so far.

----------------------------------------------------------------------

I think that Joshua's vision would be a paper in the mold drop/parkPropagating.pdf of the attached JRSI paper, but focusing on estimating D0 rather than R0...  that is, a way of computing or estimating D0 (the distributions of individual reproductive numbers about R0 when perturbing from the disease-free equilibrium with no susceptible depletion) -- and perhaps D_effective, given the structure and parameters of a compartment model.

Concretely, he is envisioning the contribution being something like a method to reframe the complexities added to compartment models in terms of what they say about individual differences in disease transmission and "superspreading," with a goal that Joe Modeller could think about what his model said about the degree of "superspreading" or something like that, with clarity approaching his ability to think about R0.

This is different from Meehan b/c rather than aiming to generate simple models that can fit observed data reasonably well with parseable D0, Joshua is hoping to build tools to compute D0 from fairly complex models encountered "in the wild." But the Meehan seems like a really great place for grounding this in terms of how having different classes and more complex temporal dynamics contribute, separately and jointly, to generating individual-level heterogeneity in infections per infectee.

One question JSW and I discussed last week is how complicated it is to go from 2 to N (classes or infectious stages). If there are independent

I classes , we think it's pretty easy to think about (as the distributions should compose as a weighted sum) and when there are serial infectious compartments, it might be harder.

I don't think I've done much with Joshua on the front of separating the idealized, continuous distribution from the realized, integer one. He and I have actually not had a very direct conversation about why it might be interesting to think about the the stochasticity from the Poisson process outside of how to compute/estimate the underlying distribution, though I've tried to (very) gently push in that direction.

So now, you're up to date, I think! I'm looking forward to today's meeting.
