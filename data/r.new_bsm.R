#!/usr/bin/env Rscript --no-save

###
#	r.new_bsm.R
#	/Users/abarbour/survey.processing/development/R/packages/pborepo/data
#	Created by 
#		/Users/abarbour/bin/ropen ( v. 2.6.0 )
#	on 
#		2016:314 (09-November)
###

## local functions
#try(source('funcs.R'))

## libs

#if (!require("pacman")) install.packages("pacman", dependencies=TRUE)
#pacman::p_load(package1, package2, package_n)

library(plyr)
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
redo <- FALSE

#if (!exists("something") | redo){
#	fi <- "something.txt"
#	fi %>% read_table(., col_names=TRUE)
#}

countSpaces <- function(st) { sapply(gregexpr(" ", st), function(p) { sum(p>=0) } ) }

get16_SiteName <- function(x){
	sapply(strsplit(x, ","), function(.x.){
		tx <- trimws(.x.)
		sta16 <- tx[nchar(tx)==16]
		# ignore those with spaces [ ]
		sta16[countSpaces(sta16)==0]
	})
}

#+++++++++++

read_delim("http://service.iris.edu/fdsnws/station/1/query?network=PB&format=text&includeavailability=true",
	delim="|", col_names=TRUE, trim_ws=TRUE) %>%
	select(., -ends_with('Network')) %>%
	dplyr::filter(., nchar(Station)==4 & substr(Station,1,1) %in% c('A','B')) %>%
	transmute(., sta4 = Station, sta16 = get16_SiteName(SiteName), nlat=Latitude, elon=Longitude, elev.m = Elevation, StartTime, EndTime) -> X

X

#+++++++++++

#FIG <- function(x, ...){}
#if (shake){
#	FIG() 
#} else {
#	figfi <- "some_figure"
#	h <- 7
#	w <- 7
#	niceEPS(figfi, h=h, w=w, toPDF=TRUE)
#	try(FIG())
#	niceEPS()
#	nicePNG(figfi, h=h, w=w)
#	try(FIG())
#	nicePNG()
#}

###

#kook::warn_shake()
#if (require('kook') & packageVersion("kook") < '1.0.17') stop("update kook -- some defaults have changed!")

