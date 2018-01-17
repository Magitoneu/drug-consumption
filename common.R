library(e1071)
library(FactoMineR)
library(randomForest)
library(nnet)
library(MASS)
library(class)
library(ggplot2)


common.getConfusion = function(real, pred) {
    lvls = union(levels(real), levels(pred))
    real = factor(real, levels=lvls)
    pred = factor(pred, levels=lvls)
    t = table(real, pred)
    print(t)
    print(100*(1-sum(diag(t))/length(real))) 
    t
}


common.getSubstimateds = function(table) {
    df = data.frame(table)
    select = as.numeric(df[,1]) > as.numeric(df[,2])
    subs = sum(df[select, 3])
}

common.crossval = function(k.folds, data, class.method, best.k = NULL, best.size = NULL, best.decay = NULL, drug = NULL){
    shuffled.data <- data[sample(nrow(data)),]
    folds <- cut(seq(1,nrow(shuffled.data)), breaks = k.folds, labels = FALSE)
    error.cv <- 0
    for(i in 1:k.folds){
        testIndexes <- which(folds==i,arr.ind=TRUE)
        if(class.method == 'logisticregression'){
            model <- glm(as.formula(paste(drug, " ~ .")), data=shuffled.data[-testIndexes,], family=binomial)
            pred <- predict(model, newdata=shuffled.data[testIndexes,], type = "response")
            gl1predt <- NULL
            gl1predt[pred<0.5] <- 0
            gl1predt[pred>=0.5] <- 1
            pred <- factor(gl1predt, labels=c("CL0","CL1"), levels=c(0, 1))
        }
        else if(class.method == 'naivebayes'){
            model <- naiveBayes(as.formula(paste(drug, " ~ .")), data = shuffled.data[-testIndexes,])
            pred <- predict(model, newdata=shuffled.data[testIndexes,])
        }
        else if(class.method == 'knn'){
            model <- knn (shuffled.data[-testIndexes,1:12], shuffled.data[testIndexes,1:12], shuffled.data[-testIndexes,13], k = best.k, prob=TRUE)
            pred <- model
        }
        else if(class.method == 'mlp'){
            model <- nnet(as.formula(paste(drug, " ~ .")), data = shuffled.data[-testIndexes,], size = best.size, decay = best.decay, trace = F, maxit = 1000, MaxNWts = 1500)
            pred <- predict(model, newdata=shuffled.data[testIndexes,], type = 'class')
        }
        else {
            print('Not implemented method.')
        }
        real = shuffled.data[testIndexes, 13]
        lvls = union(levels(real), levels(pred))
        real = factor(real, levels=lvls)
        pred = factor(pred, levels=lvls)

        t  <- table(real, pred)
        if(i == 1) table.cv = t
        error.cv <- error.cv + (1-sum(diag(t))/length(shuffled.data[testIndexes, 13]))
        table.cv <- table.cv + t
    }
    print((error.cv/k.folds)*100)
    #print(table.cv / k.folds)
    #print(paste('Cross-validation', class.method, 'Error:', (error.cv/k.folds)*100, sep = ' '))
    ret = table.cv / k.folds
    return(ret)
}


common.plotConfusion = function (table, title, axis=FALSE) {
    table.df = data.frame(table)
    total = sum(table)
    subs = common.getSubstimateds(table)/total
    error = (1 - sum(diag(table))/total)
    colnames(table.df) <- c("Var1", "pred", "Freq")
    p = ggplot(table.df, aes(pred, Var1)) + 
        geom_raster(aes(fill = Freq)) + 
        scale_y_discrete(limits = rev(levels(table.df$Var1))) + 
        guides(fill=FALSE) +
        labs(x=NULL, y=NULL, title=title, subtitle=paste("Error: ", round(error*100, 2), "%   Subs: ", round(subs*100, 2), "%", sep="")) +
        scale_fill_gradient(low = "white", high = "red", limits=c(0, total)) +
        geom_text(aes(label = round(Freq/total*100, digits=1)))
    if (!isTRUE(axis)) {
        p = p + theme(
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()
        )
    }
    p
}
