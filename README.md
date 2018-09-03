# PTCA4CATA .0.1.0

An  `R`-package: Implements Partial Triadic Correspondence Analysis (PTCA)
for the analysis of data coming from "Check All That Apply" (CATA) and related designs.
Soc the developed acronym `PTCA4CATA` means:

*Partial Triadic Correspondence Analysis for Check All That Apply*


## Introduction

`PTCA4CATA` is a collection of  functions for the analysis of *Partial Triadic Correspondence Analysis*: An extension of Correspondence Analysis to 3-ways data (*e.g.*, Stimuli by Descriptors by Participants). 

## Installation

To install `PTCA4CATA` from `Github` use the function `install_github()`  from the package `devtools`:

```{r}
#install.packages("devtools") # decomment this line if devtools is not yet intalled
devtools::install_github("HerveAbdi/PTCA4CATA") # install PTCA4CATA
```
Note, `PTCA4CATA` uses other packages, to make sure that these pacakges are all installed,
the first time you install `PTCA4CATA` use this command intead:
```
devtools::install_github('HerveAbdi/PTCA4CATA', dependencies = TRUE,
```
The complete installation may take quite some time, so it could be good time for a coffee (or whatever else) break.




