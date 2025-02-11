---
output:
  pdf_document: default
  html_document: default
---
# Notes about numerical integration to evaluate kappa_R_c

## JD verbal description
I'm also thinking that there's probably a simple numerical calculation
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

- First, if we assumed that individual durations had super short timescales w.r.t. the dynamics in $s$ (i.e., $\mathcal{R}_e$ not changing over the course of an individual infection), we could simply write
$\mathbf{E}(\mathcal{R}_c) = \int_{0}^{\infty}(\beta i s - \gamma i)* (\beta s / \gamma) dt$, i.e. the effective reproduction number, weighted by incidence. 

- Second, we have some ideas about the effect of longer, exponentially-distributed duration.

For a given cohort, a falling $\mathcal{R}_e$ over the exponentially-distributed durations means both that $\mathbf{E}(\mathcal{R}_c) < \mathcal{R}_e(time of infection)$, and a relatively sharper decrease in variance (i.e, lower kappa) than in the naive branching process model in which $\mathcal{R}_e$ is constant. 

c.f. decreasingRe.Rout

- Third, we have some ideas about another counterfactual, in which we can construct $\mathcal{R}_i$ (instantaneous reproduction number) and a corresponding $\kappa_i$:

$\mathbf{E}(\mathcal{R}_i) = \mathcal{R}_0 * (1+sf)/2$


