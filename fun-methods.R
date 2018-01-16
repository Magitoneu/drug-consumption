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
    lvls = levels(df[,drug])
    nm = 0
    for (lvl in lvls) {
        print(lvl)
        print(nrow(df[intersect(learn, which(df[,drug] == lvl)),]))
        nm = max(nm, nrow(df[intersect(learn, which(df[,drug] == lvl)),]))
    }
    dr = data.frame()
    for (lvl in lvls) {
        dlvl = df[rep_len(row.names(df[intersect(learn, which(df[,drug] == lvl)),]), length.out=nm),]
        dr = rbind(dr, dlvl)
    }
    model = randomForest(as.formula(paste(drug, " ~ .")), data = dr, ntree=150, proximity=FALSE)
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


funmeth.logisticRegression = function(data, drug){
    
    df = data[, c(2:13, grep(drug, colnames(data)))]
    conf = common.crossval(10, df, 'logisticregression')
    return(conf)
}

