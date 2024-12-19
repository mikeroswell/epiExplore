## This is a epiExplore (Roswell/Weitz heterogeneity)(

current: target
-include target.mk

-include makestuff/perl.def

vim_session:
	bash -cl "vmt README.md drop.md"

######################################################################

Sources += $(wildcard *.R)

autopipeRcall = defined

######################################################################

Sources += $(wildcard *.md)
## drop.filemerge: drop.md
## mirrors += drop

######################################################################
## utilities
spreadHelpers.Rout: spreadHelpers.R
secondaryDistributionPlots.Rout: secondaryDistributionPlots.R
### generateds distribution functions assuming branching process
pdfFromRates.Rout: pdfFromRates.R

## Lloyd
### reproduces main conceptual ideas from Lloyd-Smith et al. 2005, plots
### realized and idealized inequality curves given kappa
myLloyd-Smith.Rout: myLloyd-Smith.R spreadHelpers.rda
	$(pipeRcall)

## Meehan
### uses the branching process reasoning from Meehan et al. 2021. Caution, slow.
### this was an early attempt and there are more recent files below that use the
### same logic but might produce more relevant outputs. However, plot from
### MPOPHC grant generated here.
myMeehan.Rout: myMeehan.R spreadHelpers.rda secondaryDistributionPlots.rda
	$(pipeRcall)
######################################################################

## kappa mystery: naive kappa estimation is downwards biased
### superceded, I suspect, but note error likely in optim
why_low_kappa.Rout: why_low_kappa.R
### show that navie kappa is no good, and synthetic kappa is fine
low_kappa.Rout: low_kappa.R

## emergent heterogeneity in compartmental models
kappas_in_3-class.Rout: kappas_in_3-class.R
plotEmergent.Rout: plotEmergent.R kappas_in_3-class.rda

## foray into individual-based stochastic compartmental models
IBM_sketch.Rout: IBM_sketch.R
Sources += IBM_sketch.md

## Do a simple sim (but with three activity levels)
IBM_3-way.Rout: IBM_3-way.R kappas_in_3-class.rda
	$(pipeRcall)
IBM_sketch_sketch.Rout: IBM_sketch_sketch.R
IBM_faster.Rout: IBM_faster.R

## use IBM to check conjecture that secondary case dist variance = 2 (kappa_discrete = 1)
IBM.%.pars.Rout: IBM_%_pars.R
	$(pipeRcall)

## IBM.highGamma.Rout: IBM.R 
IBM.%.Rout: IBM_minimal.R IBM_%_pars.rda
	$(pipeRcall)

## conjecture.highGamma.Rout: conjecture.R 
conjecture.%.Rout: conjecture.R IBM.%.rda
	$(pipeRcall)

######################################################################

## Weird chaining stuff

## Looks like you did everything right; make has some ancient rules that stop it from chaining sometimes (to avoid infinite recursion)
## I always get around this with the same idiom (but there are other ways).
## impmakeR calls some deep machinery in pipeR.mk
## It might be worth investigating if we can just turn off this behaviour from make!

## IBM.highGamma.Rout: IBM_minimal.R
impmakeR += IBM
%.IBM.Rout: IBM_minimal.R IBM_%_pars.rda
	$(pipeRcall)

impmakeR += conjecture
## highGamma.conjecture.Rout: conjecture.R 
%.conjecture.Rout: conjecture.R %.IBM.rda
	$(pipeRcall)

######################################################################

## Rose investigations

sculpt.Rout: sculpt.R

######################################################################

Sources += $(wildcard *.tex)

## Notes on mean and variance in R_c

## RcNotes.pdf: RcNotes.tex

######################################################################
## Robust maximum likelihood estimation when kappa is near boundary
nbinom_z.Rout: nbinom_z.R
kapWrap.Rout: kapWrap.R

## experiments with nbinom numerics as theta → ∞ (kappa → 0)
nbinom_stability.Rout: nbinom_stability.R

## input simulation distributions
sim_lnorm.Rout: sim_lnorm.R

## Different versions of early mle attempts
MLESketch.Rout: MLESketch.R
mleRepeat.Rout: mleRepeat.R

## heads up that `maxsteps = 1e4` here means this can take approximately forever
## decent for lnorm and exp but terrible for current "gamma" (nb with kappa>1)
## MLE_start.lnorm.Rout: MLE_start.R sim_lnorm.R
## MLE_start.exp.Rout: MLE_start.R sim_exp.R
MLE_start.%.Rout: MLE_start.R kapWrap.rda nbinom_z.rda sim_%.rda
	$(pipeRcall)

## Questions for Ben
breakTMB.Rout: breakTMB.R kapWrap.rda nbinom_z.rda
maxstepsIssue.Rout: maxstepsIssue.r nbinom_z.rda

######################################################################

## General distribution stuff
Learn_distributions.Rout: Learn_distributions.R

geometric.Rout: geometric.R

nbtest.Rout: nbtest.R

#######################################################################
## plots for MS?

ineqPlots_for_emergent.Rout: ineqPlots_for_emergent.R spreadHelpers.rda kappas_in_3-class.rda pdfFromRates.rda
	$(pipeRcall)

Ignore += *.html

Sources += emergentKeyIdeas.Rmd
## Roswell attempt to get a paper outline

Ignore += emergentKeyIdeas.tex emergentKeyIdeas.log
emergentKeyIdeas.html: emergentKeyIdeas.Rmd ineqPlots_for_emergent.Rout.pdf
	$(rmdh_r)

Sources += GiniKappaK.Rmd
## Roswell notes on $a$, the nb/realized dispersion parameter (inverse),
##  $\kappa$, and a bit on the Gini ceofficient
GiniKappaK.html: GiniKappaK.Rmd
	$(rmdh_r)

## JD generating function notes
phi.html: phi.md
	pandoc $< --mathjax -s -o $@

## 80-20

## Roswell-Weitz pgeom
eightyPercentGeometric.Rout: eightyPercentGeometric.R

## Simpler from JD
80.Rout: 80.R

######################################################################
## JD show that the relationship between $1/a$, $\kappa$ generally holds between
## activity distributions and the Poisson-process realized distributions
kappaSims.Rout: kappaSims.R
	$(wrapR)

saturate.Rout: saturate.R

######################################################################

### Makestuff

## Sources += $(wildcard *.mk)
## include $(wildcard *.mk)

Sources += Makefile
Ignore += makestuff

Makefile: makestuff/01.stamp
makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone --depth 1 $(msrepo)/makestuff
	touch $@

msrepo = https://github.com/dushoff
Makefile: makestuff/Makefile
makestuff/Makefile:
	ln -s ../makestuff .
	ls makestuff/Makefile

-include makestuff/os.mk

-include makestuff/pipeR.mk
-include makestuff/texj.mk
-include makestuff/rmd.mk
## -include makestuff/ldrop.mk
-include makestuff/mirror.mk

-include makestuff/git.mk
-include makestuff/visual.mk
-include makestuff/projdir.mk
