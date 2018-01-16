#!/usr/bin/Rscript

library(randomForest)

source("common.R")
source("fun-methods.R")

args = commandArgs(TRUE)
if (NA %in% args[1]) {
    stop("NA")
}
drug = args[1]
funmeth.randomForest(drug)

