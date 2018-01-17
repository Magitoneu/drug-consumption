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
    conf = common.crossval(10, df, 'logisticregression', drug = drug)
    return(conf)
}


funmeth.knn = function(data, drug){
    df = data[, c(2:13, grep(drug, colnames(data)))]
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
    conf = common.crossval(10, df, 'knn', best.k, drug = drug)
    return(conf)
}

funmeth.mlp = function(data, drug){
    df = data.factor[, c(2:13, grep(drug, colnames(data.factor)))]
    df[, drug] = factor(df[, drug])
    
    N <- nrow(df)
    learn <- sample(1:N, round(0.9*N))
    nlearn <- length(learn)
    ntest <- N - nlearn
    
    trc <- trainControl (method="repeatedcv", number=5, repeats=5)
    sizes <- seq(1,40,by=2)+1
    
    print('Start with 5x5CV looking for best size')
    model.10x10CV <- train (as.formula(paste(drug, " ~ .")), data = df, subset=learn, method='nnet', maxit = 800, trace = F,
                            tuneGrid = expand.grid(.size=sizes,.decay=0), trControl=trc, MaxNWts = 1500)
    best.size <- model.10x10CV$bestTune[1,1]
    
    p2 <- as.factor(predict (model.10x10CV, newdata=df[-learn,], type="raw"))
    t2 <- table(pred=p2,truth=df[-learn, drug])
    error_rate.test <- 100*(1-sum(diag(t2))/ntest)
    error_rate.test
    
    decays <- 10^seq(-3,0,by=0.05)
    
    print('Start with 5x5CV looking for best decay')
    model.10x10CV <- train (as.formula(paste(drug, " ~ .")), data = df, subset=learn, method='nnet', maxit = 800, trace = F,
                            tuneGrid = expand.grid(.size=best.size,.decay=decays), trControl=trc, MaxNWts = 1500)
    
    best.decay <- model.10x10CV$bestTune[1,2]

    conf = common.crossval(10, df, 'mlp', best.size = 2, best.decay = 1, drug = drug)
    return(conf)
}

funmeth.naiveBayes = function(data, drug){
    df = data[, c(2:13, grep(drug, colnames(data)))]
    conf = common.crossval(10, df, 'naivebayes', drug = drug)
    return(conf)
}

