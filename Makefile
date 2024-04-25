## This is a epiExplore (Roswell/Weitz heterogeneity)(

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt drop.md"

######################################################################

Sources += $(wildcard *.R)

autopipeR = defined

######################################################################

Sources += $(wildcard *.md)

## drop.filemerge: drop.md

######################################################################

## Lloyd

myLloyd-Smith.Rout: myLloyd-Smith.R
myMeehan.Rout: myMeehan.R

pass.Rout: pass.R

######################################################################

Learn_distributions.Rout: Learn_distributions.R

geometric.Rout: geometric.R

nbtest.Rout: nbtest.R

######################################################################

Ignore += *.html

Sources += GiniKappaK.Rmd
GiniKappaK.html: GiniKappaK.Rmd
	$(rmdh_r)

phi.html: phi.md
	pandoc $< --mathjax -s -o $@

## 80-20

## Roswell-Weitz pgeom
eightyPercentGeometric.Rout: eightyPercentGeometric.R

## Simpler from JD
80.Rout: 80.R

######################################################################

kappaSims.Rout: kappaSims.R

######################################################################

### Makestuff

## Sources += $(wildcard *.mk)
## include $(wildcard *.mk)

Sources += Makefile
Ignore += makestuff
msrepo = https://github.com/dushoff
Makefile: makestuff/Makefile
makestuff/Makefile:
	ln -s ../makestuff .
	ls makestuff/Makefile

-include makestuff/os.mk

-include makestuff/pipeR.mk
-include makestuff/rmd.mk
-include makestuff/ldrop.mk

-include makestuff/git.mk
-include makestuff/visual.mk
-include makestuff/projdir.mk
