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

#n0 = nrow(df[intersect(learn, which(df[,drug] == "CL0")),])
#n1 = nrow(df[intersect(learn, which(df[,drug] == "CL1")),])
#n2 = nrow(df[intersect(learn, which(df[,drug] == "CL2")),])
#n3 = nrow(df[intersect(learn, which(df[,drug] == "CL3")),])
#n4 = nrow(df[intersect(learn, which(df[,drug] == "CL4")),])
#n5 = nrow(df[intersect(learn, which(df[,drug] == "CL5")),])
#n6 = nrow(df[intersect(learn, which(df[,drug] == "CL6")),])
#
#nm = max(c(n0, n1, n2, n3, n4, n5, n6))
#
#d0 = df[rep_len(row.names(df[intersect(learn, which(df[,drug] == "CL0")),]), length.out=nm),]
#d1 = df[rep_len(row.names(df[intersect(learn, which(df[,drug] == "CL1")),]), length.out=nm),]
#d2 = df[rep_len(row.names(df[intersect(learn, which(df[,drug] == "CL2")),]), length.out=nm),]
#d3 = df[rep_len(row.names(df[intersect(learn, which(df[,drug] == "CL3")),]), length.out=nm),]
#d4 = df[rep_len(row.names(df[intersect(learn, which(df[,drug] == "CL4")),]), length.out=nm),]
#d5 = df[rep_len(row.names(df[intersect(learn, which(df[,drug] == "CL5")),]), length.out=nm),]
#d6 = df[rep_len(row.names(df[intersect(learn, which(df[,drug] == "CL6")),]), length.out=nm),]
#
#dr = rbind(d0, d1, d2, d3, d4, d5, d6)
#
#model = randomForest(as.formula(paste(drug, " ~ .")), data = dr, ntree=150, proximity=FALSE, sampsize=c(CL0=nm, CL1=nm, CL2=nm, CL3=nm, CL4=nm, CL5=nm, CL6=nm), strata=dr[,drug])


print("Training:")
common.compare(df[learn, drug], predict(model, newdata=df[learn,]))
print("Test:")
common.compare(df[-learn, drug], predict(model, newdata=df[-learn,]))

