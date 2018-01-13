library(MASS)
library(class)
library(knncat)
load('ws.rdata')


args = commandArgs(TRUE)
if (NA %in% args[1]) {
  stop("NA")
}
drug = args[1]

df = data.factor[, c(2:13, grep(drug, colnames(data.factor)))]
df[, drug] = factor(df[, drug])

N <- nrow(df)
learn <- sample(1:N, round(0.9*N))
nlearn <- length(learn)
ntest <- N - nlearn

learn.inputs <- df[learn, 1:12]
learn.classes <- df[learn, drug]
test.inputs <- df[-learn, 1:12]
test.classes <- df[-learn, drug]

ks <- c(1:sqrt(nrow(learn.inputs)))

myknn <- knncat(df[learn,], df[-learn,], k = ks, xvals = 10, classcol = 13, verbose = 1)
synpred <- predict(myknn, df[learn,], df[-learn,], train.classcol = 13, newdata.classcol = 13)

tab <- table(synpred, df[-learn, drug])
1 - sum(tab[row(tab)==col(tab)])/sum(tab)
tab

common.crossval(10, df,'knn')