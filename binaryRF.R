#!/usr/bin/Rscript
library(randomForest)

source("common.R")
load('ws.rdata')

args = commandArgs(TRUE)
if (NA %in% args[1] | NA %in% args[2]) {
  stop("NA")
}
drug = args[1]
min_cl = grep(args[2], levels(data.factor[,drug]))

df = data.factor[, c(2:13, grep(drug, colnames(data.factor)))]
df[, drug][as.numeric(df[, drug]) < min_cl] = "CL0"
df[, drug][df[, drug] != "CL0"] = "CL1"
df[, drug] = factor(df[, drug])

N <- nrow(df)

learn <- sample(1:N, round(0.9*N))
nlearn <- length(learn)
ntest <- N - nlearn

harm <- function (a,b) { 2/(1/a+1/b) }
#sampsize=c(yes=3000, no=3000)
summary(df[learn,13])


model.rf3 <- randomForest(as.formula(paste(drug, " ~ .")), data = df[learn, ], ntree=100, proximity=FALSE,sampsize=c(CL0=150, CL1=41), strata=df[learn, 13])

pred.rf3 <- predict (model.rf3, df[-learn,], type="class")

tab <- table(df[-learn, 13], pred.rf3)

common.compare(df[-learn, drug], pred.rf3)
(F1 <- harm (prop.table(tab,1)[1,1], prop.table(tab,1)[2,2]))
