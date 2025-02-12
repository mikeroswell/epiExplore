---
output:
  pdf_document: default
  html_document: default
---
# Notes about numerical integration to evaluate kappa_R_c

## JD verbal description
>I'm also thinking that there's probably a simple numerical calculation
we could do: if we can integrate incidence and susceptibles through
time across an epidemic, we could integrate those results again to
calculate the expected kappas directly, and then average across them
appropriately. I'm a tiny bit stuck on how to discretize the first
integration to best accomplish the second integration -- or whether
there's some super-clever trick that means you need to integrate only
once. But I think if we needed to just do this we could.

## Background
We have the normalized SIR, defined by 
$$\frac{ds}{dt} = -\beta i s$$
$$\frac{di}{dt} = \beta i s - \gamma i$$
$$\frac{dr}{dt} = \gamma i$$
$$s + i + r = 1$$

The reproduction number (instantaneous expected cases per case) is 
$\mathcal{R}_e = \beta s / \gamma$, which, assuming $s(0) \approx 1$, also gives 
$\mathcal{R}_0 = \beta/\gamma$.

Assuming again that $i(0) \approx 0$, over the course of the entire epidemic, the mean *case* reproduction number $\mathcal{R}_c$ must be 1. Here, we're going to try to write down expressions that show this from the above. 

## Stuff I already understand before starting this document

We understand a few things about this already. 

- First, if we assumed that individual durations had super short timescales w.r.t. the dynamics in $s$ (i.e., $\mathcal{R}_e$ not changing over the course of an individual infection), and that durations were all identical, we could simply write
$\mathbf{E}(\mathcal{R}_c) = \int_{0}^{\infty}(\beta i s )* (\beta s / \gamma) dt$, i.e. the effective reproduction number, weighted by incidence. 

- Second, we have some ideas about the effect of longer, exponentially-distributed duration.

For a given cohort, a falling $\mathcal{R}_e$ over the exponentially-distributed durations means both that $\mathbf{E}(\mathcal{R}_c) < \mathcal{R}_e(time\ of\ infection)$, and a relatively sharper decrease in variance (i.e, lower kappa) than in the naive branching process model in which $\mathcal{R}_e$ is constant. 

c.f. decreasingRe.Rout

- Third, we have some ideas about another counterfactual, in which we can construct $\mathcal{R}_i$ (instantaneous reproduction number) and a corresponding $\kappa_i$:

We define $sf$ as the final faction of susceptibles when the epidemic ends; this can be computed as a function of $\mathcal{R}_0$
$$\mathcal{R}_0 = \frac{log(sf)}{(sf-1)}$$
and then,

$$\mathbf{E}(\mathcal{R}_i) = \mathcal{R}_0 * (1+sf)/2$$
$$\mathbf{V}(\mathcal{R}_i) = \mathbf{E}(\mathcal{R}_i^2)-
\mathbf{E}(\mathcal{R}_i)^2 = \mathcal{R}_0^2(1-sf)^2/12$$

This gives us 
$$\kappa_i =\frac{(\frac{1-sf}{1+sf})^2}{3}$$

Approaching 0 when $sf, \mathcal{R}_0$ both $\approx 1$ and $1/3$ as $\mathcal{R}_0$ grows and $sf \to 0$

## An approach to enumerating $\kappa_c$ over the entire epidemic

### first, symbolically

If we look at our previous expression that assumed no variation in duration between individuals, and no change in $\mathcal{R}_e$ over the course of a given infection, $\mathbf{E}(\mathcal{R}_c) = \int_{0}^{\infty}(\beta i s - \gamma i)* (\beta s / \gamma) dt$, we may have some openings to expand. This *is* something like the mean instantaneous reproduction number at time of infection across all infectors. It is omitting the fact that each infection has a duration. In the next step, we want to think carefully about what we're integrating to see if we can come up with something that makes more sense. 

We have new infections at time t
$\frac{di/dt}(t) = (\beta i(t) s(t) )$
We'll average across them via integration

They stay infected on average for time $1/\gamma$, which we want to describe inside that big integral, so now we might be $du$ instead of $dt$
and for any time inside *both integrals*, the ones who haven't recovered are spreading at rate $\frac{\beta s}{\gamma}$

maybe the inner integral is over something like $\mathcal{R}_e$ times the fraction of those infected at time t still infectious at time u. 
$$\int_{t}^{\infty} \frac{\beta s(u)}{\gamma}*(\gamma e^{-\gamma(u-t)})du$$
 
So, putting it together, we have 
$$\mathbf{E}(\mathcal{R}_c) = \int_{0}^{\infty}\beta i(t) s(t) \int_{t}^{\infty} \frac{\beta s(u)}{\gamma}*(\gamma e^{-\gamma(u-t)}) du \ dt$$
[actually, I have no idea if we can write this down this way, i.e. whether the use of $u$ is clear enough]

Before I write down the next things, I'm wondering...

- are there any tricks to either writing down or computing variance? 

- How do I evaluate this?
  - I think the relationship between incidence and the case-effective R_effective is just multiplication. So, we know how to numerically evaluate incidence, and my first impression is we should. 
  - It looks like the only dynamic variable in the inner integral is $s(u)$, which we also know how to evaluate via integration, which will need to happening at a level not specified in my expression (i.e., for each time $u$ we need to solve for $s(u)$ and then, for each $dt$ we integrate over those values times the exponential density up to $u-t$, or something) 
  - I think we will want top approximate the integral with a Riemann sum, but this is way more calculus than I actually remember. I guess the idea is we pick fine $\delta t$ and $\delta u$ and get a bunch of values that we sum up correctly. I think the idea is also we can square appropriately and do the same thing to get variance estimates.
  
### pseudo code???
  



