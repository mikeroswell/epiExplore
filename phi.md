---
title: Universality of κ
---

Start with a distribution of activities $f(\lambda)$.

The probability of observing a realized value $n$ is then:

$$p_n = \int{\frac{\lambda^n e^{-\lambda}}{n!} f(\lambda) d\lambda}$$

Define a generating function:

$$\phi(x) = \sum_n{p_n x^n} = \sum_n x^n \int{\frac{\lambda^n e^{-\lambda}}{n!} f(\lambda) d\lambda}$$

$$ =   \int{\sum_n \frac{(\lambda x)^n e^{-\lambda}}{n!} f(\lambda) d\lambda}$$

$$ =   \int{e^{\lambda(x-1)} f(\lambda) d\lambda}$$

The generating-function moments are the nth-derivatives of $\phi$ wrt $x$, evaluated at 1:

$$ \phi^{(n)}(1) =   \int{\lambda^n e^{\lambda(x-1)} f(\lambda) d\lambda}$$

$$ =   \left.\int{\lambda^n e^{\lambda(x-1)} f(\lambda) d\lambda}\right|_{x=1}$$

$$ =  \int{\lambda^n f(\lambda) d\lambda}$$

Thus, the generating-function moments of $p$ are exactly the non-central moments of $f$.

There is fiddly book-keeping to do now, since the central moments, non-central moments and generating-function moments are all slightly different, but I am going to refuse on principle to do it: any set of the first $n$ moments determines any other set; thus the relationship we already know between $\textrm{CV}_p$ and $\kappa$ must be universal.
