library(e1071)
library(FactoMineR)
library(randomForest)
library(ggplot2)
source("common.R")

funmeth.randomForest = function (data, drug) {
    df = data[, c(2:13, grep(drug, colnames(data)))]
    N = nrow(df)
    learn = sample(1:N, round(0.9*N))
    #(ntrees = round(10^seq(1,3,by=0.2)))
    #rf.results = matrix (rep(0,2*length(ntrees)),nrow=length(ntrees))
    #colnames (rf.results) = c("ntrees", "OOB")
    #rf.results[,"ntrees"] = ntrees
    #rf.results[,"OOB"] = 0
    #
    #ii = 1
    #
    #for (nt in ntrees)
    #{ 
    #  print(nt)
    #  model.rf = randomForest(as.formula(paste(drug, " ~ .")), data = df[learn,], ntree=nt, proximity=FALSE)
    #  rf.results[ii,"OOB"] = model.rf$err.rate[nt,1]
    #  ii = ii+1
    #}
    #
    #rf.results
    #
    #lowest.OOB.error = as.integer(which.min(rf.results[,"OOB"]))
    #(ntrees.best = rf.results[lowest.OOB.error,"ntrees"])
    #
    #
    #model = randomForest(as.formula(paste(drug, " ~ .")), data = df[learn,], ntree=ntrees.best, proximity=FALSE)
    model = randomForest(as.formula(paste(drug, " ~ .")), data = df[learn,], ntree=150, proximity=FALSE)
    print("Training:")
    common.getConfusion(df[learn, drug], predict(model, newdata=df[learn,]))
    print("Test:")
    conf = common.getConfusion(df[-learn, drug], predict(model, newdata=df[-learn,]))

    print(importance(model))
    print(varImpPlot(model))
    conf = model$confusion[,-which(colnames(model$confusion) == "class.error")]
    return(as.table(conf))
}

funmeth.randomForest.weighted = function (data, drug) {
    df = data[, c(2:13, grep(drug, colnames(data)))]
    N = nrow(df)
    learn = sample(1:N, round(0.9*N))
    n0 = nrow(df[intersect(learn, which(df[,drug] == "CL0")),])
    n1 = nrow(df[intersect(learn, which(df[,drug] == "CL1")),])
    n2 = nrow(df[intersect(learn, which(df[,drug] == "CL2")),])
    n3 = nrow(df[intersect(learn, which(df[,drug] == "CL3")),])
    n4 = nrow(df[intersect(learn, which(df[,drug] == "CL4")),])
    n5 = nrow(df[intersect(learn, which(df[,drug] == "CL5")),])
    n6 = nrow(df[intersect(learn, which(df[,drug] == "CL6")),])
    nm = max(c(n0, n1, n2, n3, n4, n5, n6))
    d0 = df[rep_len(row.names(df[intersect(learn, which(df[,drug] == "CL0")),]), length.out=nm),]
    d1 = df[rep_len(row.names(df[intersect(learn, which(df[,drug] == "CL1")),]), length.out=nm),]
    d2 = df[rep_len(row.names(df[intersect(learn, which(df[,drug] == "CL2")),]), length.out=nm),]
    d3 = df[rep_len(row.names(df[intersect(learn, which(df[,drug] == "CL3")),]), length.out=nm),]
    d4 = df[rep_len(row.names(df[intersect(learn, which(df[,drug] == "CL4")),]), length.out=nm),]
    d5 = df[rep_len(row.names(df[intersect(learn, which(df[,drug] == "CL5")),]), length.out=nm),]
    d6 = df[rep_len(row.names(df[intersect(learn, which(df[,drug] == "CL6")),]), length.out=nm),]
    dr = rbind(d0, d1, d2, d3, d4, d5, d6)
    model = randomForest(as.formula(paste(drug, " ~ .")), data = dr, ntree=150, proximity=FALSE, sampsize=c(CL0=nm, CL1=nm, CL2=nm, CL3=nm, CL4=nm, CL5=nm, CL6=nm), strata=dr[,drug])
    print("Training:")
    common.getConfusion(df[learn, drug], predict(model, newdata=df[learn,]))
    print("Test:")
    conf = common.getConfusion(df[-learn, drug], predict(model, newdata=df[-learn,]))

    print(importance(model))
    print(varImpPlot(model))

    return(conf)
}

funmeth.randomForest.comparison = function(data) {
    load("ws.rdata")
    drugs = colnames(data[,14:31])
    for (drug in drugs) {
        invisible(capture.output(conf.normal <- funmeth.randomForest(data, drug)))
        invisible(capture.output(conf.weighted <- funmeth.randomForest.weighted(data, drug)))
        subs.normal = common.getSubstimateds(conf.normal) / sum(conf.normal)
        subs.weighted = common.getSubstimateds(conf.weighted)/sum(conf.weighted)

        print(drug)
        print(paste("Substimateds normal:", subs.normal))
        print(paste("Substimateds weigth:", subs.weighted))
    }
}
