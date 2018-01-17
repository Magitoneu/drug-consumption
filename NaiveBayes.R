#!/usr/bin/Rscript

source("fun-methods.R")
load("ws.rdata")

args = commandArgs(TRUE)
if (NA %in% args[1]) {
    stop("NA")
}

drug = args[1]
funmeth.naiveBayes(data.factor, drug)
