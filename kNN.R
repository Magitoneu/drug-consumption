#!/usr/bin/Rscript
library(MASS)
library (class)
load('ws.rdata')


args = commandArgs(TRUE)
if (NA %in% args[1]) {
  stop("NA")
}
drug = args[1]

df = data.factor[, c(2:13, grep(drug, colnames(data.factor)))]
df[, drug] = factor(df[, drug])

N <- nrow(df)
learn <- sample(1:N, round(0.67*N))
nlearn <- length(learn)
ntest <- N - nlearn

learn.inputs <- df[learn, 1:12]
learn.classes <- df[learn, drug]
test.inputs <- df[-learn, 1:12]
test.classes <- df[-learn, drug]

set.seed (23)
neighbours <- c(1:sqrt(nrow(learn.inputs)))
errors <- matrix (nrow=length(neighbours), ncol=2)
colnames(errors) <- c("k","LOOCV error")

for (k in neighbours)
{
  myknn.cv <- knn.cv (learn.inputs ,learn.classes, k = neighbours[k])
  
  # fill in no. of neighbours and LOO validation error
  errors[k, "k"] <- neighbours[k]
  
  tab <- table(myknn.cv, learn.classes)
  errors[k, "LOOCV error"] <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab)
}

best.k <- which.min(errors[,2])

myknn <- knn (learn.inputs, test.inputs, learn.classes, k = best.k, prob=TRUE) 

tab <- table(myknn, test.classes) 
1 - sum(tab[row(tab)==col(tab)])/sum(tab)
tab