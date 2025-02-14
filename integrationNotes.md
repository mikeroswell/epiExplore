---
output:
  pdf_document: default
  html_document: default
---

# Notes about numerical integration to evaluate kappa_R_c

\newcommand{\Rx}[1]{\ensuremath{\mathcal{R}_{#1}}}
\newcommand{\Rc}{\Rx{c}}
\newcommand{\Ro}{\Rx{0}}
\newcommand{\Reff}{\Rx{e}}
\newcommand{\Ri}{\Rx{i}}

## Background
We have the normalized SIR, defined by 
$$\frac{ds}{dt} = -\beta i s$$
$$\frac{di}{dt} = \beta i s - \gamma i$$
$$\frac{dr}{dt} = \gamma i$$
$$s + i + r = 1$$

The reproduction number (instantaneous expected cases per case) is 
$\Reff = \beta s / \gamma$, which, assuming $s(0) \approx 1$, also gives 
$\Ro = \beta/\gamma$.

Assuming again that $i(0) \approx 0$, over the course of the entire epidemic, the mean *case* reproduction number \Rc\ must be 1. Here, we're going to try to write down expressions that show this from the above. 

## Stuff I already understand before starting this document

We understand a few things about this already. 

- First, if we assumed that individual durations had super short timescales w.r.t. the dynamics in $s$ (i.e., $\Reff$ not changing over the course of an individual infection), and that durations were all identical, we could simply write
$\mathbf{E}(\Rc) = \int_{0}^{\infty}(\beta i s )* (\beta s / \gamma) dt/\int_0^{\infty}{\beta is}\ dt$, i.e. the effective reproduction number, weighted by incidence. 

	* A few points here. There's a simpler approach, which is summing over the infections _in order_, so that you just have a uniform distribution of $\Rc.$ The first $\beta$ cancels with that in the denominator, and the $\beta/\gamma$ can be pulled out of the integral.
	* The uniform value is $>1$; we have an approximation for it, and it gives some indication of the importance of the depletion we're ignoring.

- Second, we have some ideas about the effect of longer, exponentially-distributed duration.

For a given cohort, a falling $\Reff$ over the exponentially-distributed durations means both that $\mathbf{E}(\Rc) < \Re(\textrm{time of infection})$, and that the variance within a cohort is _less_ than the geometric expectation (since longer infections are not as disprorportionately effective as you would expect). 

* You can use textsl for textrm I think if you want that text to look mathier.

- Third, we have some ideas about another counterfactual, in which we can construct \Ri\ (instantaneous reproduction number) and a corresponding $\kappa_i$:

	* I kind of feel this is the same counter-factual, but with the infector-based approach above (we should possibly drop \Reff\

We define $s_f$ as the final fraction of susceptibles when the epidemic ends; this can be computed as a function of \Ro\
$$\Ro = \frac{log(s_f)}{(s_f-1)}$$
and then,

$$\mathbf{E}(\Ri) = \Ro * (1+s_f)/2$$
$$\mathbf{V}(\Ri) = \mathbf{E}(\Ri^2)-
\mathbf{E}(\Ri)^2 = \Ro^2(1-s_f)^2/12$$

This gives us 
$$\kappa_i =\frac{(\frac{1-s_f}{1+s_f})^2}{3}$$

Approaching 0 when $s_f, \Ro$ both $\approx 1$ and $1/3$ as \Ro\ grows and $s_f \to 0$

## An approach to enumerating $\kappa_c$ over the entire epidemic

### first, symbolically

If we look at our previous expression that assumed no variation in duration between individuals, and no change in $\Reff$ over the course of a given infection, $\mathbf{E}(\Rc) = \int_{0}^{\infty}(\beta i s - \gamma i)* (\beta s / \gamma) dt/\int_0^{\infty}{\beta is}\ dt$, we may have some openings to expand. This *is* something like the mean instantaneous reproduction number at time of infection across all infectors. It is omitting the fact that each infection has a duration. In the next step, we want to think carefully about what we're integrating to see if we can come up with something that makes more sense. 

We have new infections at time t
$\inc(t) = (\beta i(t) s(t) )$
We'll average across them via integration

They stay infected on average for time $1/\gamma$, which we want to describe inside that big integral, so now we might be $du$ instead of $dt$
and for any time inside *both integrals*, the ones who haven't recovered are spreading at rate $\frac{\beta s}{\gamma}$

maybe the inner integral is over something like $\Reff$ times the fraction of those infected at time t still infectious at time u. 
$$\int_{t}^{\infty} \frac{\beta s(u)}{\gamma}*(\gamma e^{-\gamma(u-t)})du$$
 
So, putting it together, we have 
$$\mathbf{E}(\Rc) = \int_{0}^{\infty}\beta i(t) s(t) \int_{t}^{\infty} \frac{\beta s(u)}{\gamma}*(\gamma e^{-\gamma(u-t)}) du \ dt$$
[actually, I have no idea if we can write this down this way, i.e. whether the use of $u$ is clear enough]

Before I write down the next things, I'm wondering...

- are there any tricks to either writing down or computing variance? 

- How do I evaluate this?
  - I think the relationship between incidence and the case-effective R_effective is just multiplication. So, we know how to numerically evaluate incidence, and my first impression is we should. 
  - It looks like the only dynamic variable in the inner integral is $s(u)$, which we also know how to evaluate via integration, which will need to happen at a level not specified in my expression (i.e., for each time $u$ we need to solve for $s(u)$ and then, for each $dt$ we integrate over those values times the exponential density up to $u-t$, or something) 
  - I think we will want top approximate the integral with a Riemann sum, but this is way more calculus than I actually remember. I guess the idea is we pick fine $\delta t$ and $\delta u$ and get a bunch of values that we sum up correctly. I think the idea is also we can square appropriately and do the same thing to get variance estimates.
  
### pseudo code???
  



