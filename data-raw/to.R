#!/usr/bin/env Rscript --no-save

###
#	to.R
#	/Users/abarbour/shakabrah/development/R/packages/pborepo/data-raw
#	Created by 
#		/Users/abarbour/bin/ropen ( v. 2.6.7 )
#	on 
#		2018:338 (04-December)
#
#	[ Explain what this script does, broadly ]
#
###

## local functions
#try(source('funcs.R'))

## libs

library(tools)

#if (!require("pacman")) install.packages("pacman", dependencies=TRUE)
#pacman::p_load(package1, package2, package_n)

# loads core tidy packages:  ggplot2, tibble, tidyr, readr, purrr, and dplyr
library(tidyverse)
#tidyverse_conflicts()
#tidyverse_update(TRUE)

## local/github libs
# devtools::install_github("abarbour/kook")
#library(kook)
#Set1 <- brew.set1()
#Set1l <- brew.set1(TRUE)
#Dark2 <- brew.dark2()
#Dark2l <- brew.dark2(TRUE)
#Dark2ll <- brew.dark2(TRUE,TRUE)

#+++++++++++

shake <- FALSE
redo <- TRUE
inter <- interactive()

to_na <- function(x) as.numeric(ifelse(x=='Unknown', NA, x))

if (!exists("coords") | redo){
	readr::read_table2('bsm_metadata.txt') %>% 
	dplyr::mutate(`CH0(EofN)` = to_na(`CH0(EofN)`),
		`SEISMOMETER_Depth(m)` = to_na(`SEISMOMETER_Depth(m)`),
		L_DATE = as.Date(as.POSIXct(L_DATE, "%Y:%j", tz="UTC"))) -> coords
}

if (!exists("spotl") | redo){
	readr::read_table2('spotl.tides.txt', comment="#") -> spotl
}

# As of Dec 4 2018 there are some discrepancies between the two
# ELEV(m) columns, leading to duplicates in the full_join
#
# full_join(coords, dplyr::select(spotl, -`ELEV(m)`))

#+++++++++++

bsmmeta3 <- list(
	coords=coords,
	tides=list(spotl=spotl)
)

#+++++++++++

message("saving bsmmeta3.rda ...")
save(bsmmeta3, file="bsmmeta3.rda")

