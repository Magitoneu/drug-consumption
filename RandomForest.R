#!/usr/bin/Rscript

library(randomForest)

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

#(ntrees = round(10^seq(1,3,by=0.2)))
#rf.results = matrix (rep(0,2*length(ntrees)),nrow=length(ntrees))
#colnames (rf.results) = c("ntrees", "OOB")
#rf.results[,"ntrees"] <- ntrees
#rf.results[,"OOB"] <- 0
#
#ii <- 1
#
#for (nt in ntrees)
#{ 
#  print(nt)
#  model.rf <- randomForest(as.formula(paste(drug, " ~ .")), data = df[learn,], ntree=nt, proximity=FALSE)
#  rf.results[ii,"OOB"] <- model.rf$err.rate[nt,1]
#  ii <- ii+1
#}
#
#rf.results
#
#lowest.OOB.error <- as.integer(which.min(rf.results[,"OOB"]))
#(ntrees.best <- rf.results[lowest.OOB.error,"ntrees"])
#
#
##model = randomForest(as.formula(paste(drug, " ~ .")), data = df[learn,], ntree=ntrees.best, proximity=FALSE)
model = randomForest(as.formula(paste(drug, " ~ .")), data = df[learn,], ntree=150, proximity=FALSE)

print("Training:")
common.compare(df[learn, drug], predict(model, newdata=df[learn,]))
print("Test:")
common.compare(df[-learn, drug], predict(model, newdata=df[-learn,]))

