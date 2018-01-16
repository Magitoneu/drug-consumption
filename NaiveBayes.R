#!/usr/bin/Rscript

library(e1071)
library(ggplot2)
source("common.R")
load("ws.rdata")


args = commandArgs(TRUE)
if (NA %in% args[1]) {
    stop("NA")
}
drug = args[1]

df = data.factor[, c(2:13, grep(drug, colnames(data.factor)))]

N = nrow(df)

learn = sample(1:N, round(2/3*N))


model = naiveBayes(as.formula(paste(drug, " ~ .")), data = df[learn,])


print("Training:")
common.getConfusion(df[learn, drug], predict(model, newdata=df[learn,]))
print("Test:")
common.getConfusion(df[-learn, drug], predict(model, newdata=df[-learn,]))

common.crossval(10, df, 'naivebayes')
