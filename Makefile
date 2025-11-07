## This is a epiExplore (Roswell/Weitz heterogeneity)(

current: target
-include target.mk

-include makestuff/perl.def

vim_session:
	bash -cl "vmt README.md drop.md"

######################################################################

Sources += $(wildcard *.R)

######################################################################

Sources += $(wildcard *.md)
## drop.filemerge: drop.md
## mirrors += drop
Sources += $(wildcard slow/*)
######################################################################
## utilities
spreadHelpers.Rout: spreadHelpers.R
secondaryDistributionPlots.Rout: secondaryDistributionPlots.R
### generateds distribution functions assuming branching process
pdfFromRates.Rout: pdfFromRates.R
kappaFns.Rout: kappaFns.R
# very rough peak time approximation
tpeak.Rout: tpeak.R
### plotting
densHist.Rout: densHist.R
ggplot_limits.Rout: ggplot_limits.R

## Lloyd
### reproduces main conceptual ideas from Lloyd-Smith et al. 2005, plots
### realized and idealized inequality curves given kappa
myLloyd-Smith.Rout: myLloyd-Smith.R spreadHelpers.rda
	$(pipeCom)

## Meehan
### uses the branching process reasoning from Meehan et al. 2021. Caution, slow.
### this was an early attempt and there are more recent files below that use the
### same logic but might produce more relevant outputs. However, plot from
### MPOPHC grant generated here.
myMeehan.Rout: myMeehan.R spreadHelpers.rda secondaryDistributionPlots.rda
	$(pipeCom)

plotMPOPHC.Rout: plotMPOPHC.R myMeehan.rda
	$(pipeCom)

impmakeR += plotPMF_PDF_ineq

## Densities and density histograms
### deadSimple.R is doing too much work right now... it generates data from
### branching processes in fully susceptible populations. The data are then
### plotted by the plotPMF_PDF_ineq.R script, which looks basically fine but
### probably want to make a bit more modular for tapan's talk.
plotPMF_PDF_ineq.Rout: plotPMF_PDF_ineq.R myMeehan.rda deadSimple.rda densHist.rda
	$(pipeCom)

######################################################################


## epichains
### learn how the package epichains works and whether it provides the mechanisms
### for susceptible depletion that I feel we need to better link the two worlds:
### branching processes for (early-phase outbreak) superspreading, and
### compartmental models, for longer-term, deterministic trajectories.
loseSus.Rout: loseSus.R kappaFns.rda
	$(pipeCom)

## kappa mystery: naive kappa estimation is downwards biased
### superceded, I suspect, but note error likely in optim
why_low_kappa.Rout: why_low_kappa.R
### show that naive kappa is no good, and synthetic kappa is fine
low_kappa.Rout: low_kappa.R

## dead simple emergent plots
deadSimple.Rout: deadSimple.R spreadHelpers.rda
	(pipeCom)

## emergent heterogeneity in compartmental models
kappas_in_3-class.Rout: kappas_in_3-class.R
plotEmergent.Rout: plotEmergent.R kappas_in_3-class.rda

## foray into individual-based stochastic compartmental models
IBM_sketch.Rout: IBM_sketch.R
Sources += IBM_sketch.md

## Do a simple sim (but with three activity levels)
IBM_3-way.Rout: IBM_3-way.R kappas_in_3-class.rda myMeehan.rda nbinom_z.rda
	$(pipeCom)
# match to an Actual Meehan model
hundredFiveHundred.Rout: hundredFiveHundred.R tpeak.rda IBM_3-way.rda myMeehan.rda nbinom_z.rda
IBM_sketch_sketch.Rout: IBM_sketch_sketch.R
IBM_faster.Rout: IBM_faster.R
######################################################################

## notes on enumerating kappa_c
## Had to do some makestuff hacking to make this rule work; in general fake rules should work
# integrationNotes.pdf: integrationNotes.md

## use IBM to check conjecture that secondary case dist variance = 2 (kappa_discrete = 1)

pipeCom = $(pipeR)

## highGamma.Rout: IBM_minimal.R
impmakeR += IBM
## base.IBM.Rout: IBM_minimal.R IBM_base_pars.rda
%.IBM.Rout: IBM_minimal.R recFun.rda IBM_%_pars.rda
	$(pipeRcall)

recFun.Rout: recFun.R
	$(pipeR)

IBM_for_v1.Rout: IBM_for_v1.R recFun.rda finalSize.rda
	$(pipeCom)

slowtarget/IBM_for_v1_pars.Rout: IBM_for_v1_pars.R IBM_for_v1.rda
	$(pipeCom)

IBM_change_%_pars.Rout: change_%.R IBM_base_pars.rda
	$(pipeCom)
%.Rout: %.R
	$(pipeCom)
impmakeR += conjecture
## lowGamma.conjecture.Rout: conjecture.R
## base.conjecture.Rout: IBM_minimal.R conjecture.R IBM_base_pars.R
%.conjecture.Rout: conjecture.R %.IBM.rda
	$(pipeCom)

Ignore += figs
MEASURE ?= proportion
## v1.hist.Rout: v1.hist.R
figs/v1.%.Rout: slow/IBM_for_v1_pars.rda  v1.%.R "$(MEASURE)"  | figs
	$(pipeCom)

figs:
	$(mkdir)

impmakeR += toPeak
%.toPeak.Rout: toPeak.R tpeak.rda %.conjecture.rda nbinom_z.rda
	$(pipeCom)

## base.hundredFiveHundred.Rout.nom.dd.mg.pdf:
## change_12.hundredFiveHundred.Rout: hundredFiveHundred.R
## base.hundredFiveHundred.Rout: hundredFiveHundred.R
impmakeR += hundredFiveHundred
%.hundredFiveHundred.Rout: hundredFiveHundred.R tpeak.rda %.conjecture.rda nbinom_z.rda
	$(pipeCom)

# kind of want to go with some Meehan models plus dynamics.


## how does the CV change in an exponential-something mixture
# lognormal_exp_sim.Rout: lognormal_exp_sim.R spreadHelpers.rda
#	$(pipeCom)

## show that kappa can be less than 1 when Re is falling
### linear example
decreasingRe.Rout: decreasingRe.R spreadHelpers.rda
	$(pipeCom)

######################################################################

## JD messes with code

## test.sim.Rout: jdMess.R test.pars.R
%.sim.Rout: jdMess.R %.pars.rda
	$(pipeCom)
%.pars.Rout: %.pars.R
	$(pipeCom)

######################################################################

## Rose investigations

sculpt.Rout: sculpt.R

######################################################################

Sources += $(wildcard *.tex)

## Notes on mean and variance in R_c

## proof.pdf: proof.tex
## forward.pdf: forward.tex
## RcNotes.pdf: RcNotes.tex
## RenewalCaseReproduction.pdf: RenewalCaseReproduction.md
## RenewalCaseReproduction.html: RenewalCaseReproduction.md
## defineConjecture.pdf: defineConjecture.md

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
	$(pipeCom)

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
	$(pipeCom)

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
## Shows that we add Poisson variance to kappa-like variance
## (this is maybe a proof of an application of the rule of total variance? check)
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
## -include makestuff/rmd.mk
## -include makestuff/pandoc.mk
## -include makestuff/ldrop.mk
-include makestuff/slowtarget.mk
-include makestuff/mirror.mk
-include makestuff/makegraph.mk

-include makestuff/git.mk
-include makestuff/visual.mk
