# Spreadiness

When we discussed the spreadiness project previously, I think I rushed into the 
intuition that individual-level spreadiness was a single feature of a model, and 
I think I now see that there are at least 3 levels of spreadiness in a canonical
SIR ODE model with constant recovery and transmission rates, Gamma and Beta. 

The first level is the underlying assumptions of the continuous deterministic
model. Is it fair to say that at this level, to the extent that we imagine that 
individuals are represented here, all individuals have the same intrinsic 
spreadiness? 

The next level is that we can estimate the distribution of recovery times/
infectious periods, which in the classical model is an exponential (gamma with
shape = 1) distribution. This is a continuous expectation of individual
spreadiness, what Lloyd-Smith et al. 2005 calls the "individual reproduction
number" and parameterizes as $v$

Finally, we can think about an integer population size, and the distribution of 
the number of infectees per individual in a stochastic realization of that model.
In this case, we need to place individuals onto that exponential [gamma] expectation
and sample from it (this is the Poisson part of the Poisson-gamma/negative binomial)

I think the level at which other continuous compartmental models might be
differing is kind of hard to map onto here? I like how Lloyd-Smith et al. 2005
fit a single heterogeneity parameter to describe what's going on (the shape
parameter of the gamma, which is the same as the continuous size parameter for
the negative binomial). That gives a family of distributions with the same mean
(R0) but different possible degrees of dispersion in expected spreadiness. 

I can see how the gamma assumption may be limiting, and some structured
compartmental models will not have gamma-like recovery time distributions (or
sort of equivalently, have variation in individual transmission rates), so the
LS appraoch is simplistic in that sense. But I am still pretty vague on what
else is practical to do... other than maybe techniques to estimate the
distribution of something appropriate, maybe infectious periods? or generation
times? or something else? from the complicated models.

Anyways, since I'm still vague on what that would be, and the underlying 
research questions, I thought I'd write up what I thought the idea in the 
Lloyd-Smith et al. 2005 paper was (despite still not working through it carefully!)
to map out the 3 levels of expectation about individual spreadiness, and see if 
we were on the same page up that point. 

The code takes a set of dispersions, a set of R0s, and computes a realized 
distribution of infectees per individual, and then sorts it into a cumulative 
fraction as you did in your code. I plot the results so that R0 is color, and 
each level of dispersion gets its own panel. Finally I had some more abbreviated
observations/notes at the end. 
