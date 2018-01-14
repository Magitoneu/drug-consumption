#library(ggplot2)


common.compare = function(real, pred) {
    real = factor(real, levels=c("CL0", "CL1", "CL2", "CL3", "CL4", "CL5", "CL6"))
    pred = factor(pred, levels=c("CL0", "CL1", "CL2", "CL3", "CL4", "CL5", "CL6"))
    t = table(real, pred)
    print(t)
    print(100*(1-sum(diag(t))/length(real))) 
}

#knnCAT, nnet,  RandomForest no necesiten la funció ja que la tenen incorporada (nnet té el metode train, RandomForest l'error OBS, knnCAT en la mateixa funció)
common.crossval = function(k.folds, data, class.method){
  shuffled.data <- data[sample(nrow(data)),]
  folds <- cut(seq(1,nrow(shuffled.data)), breaks = k.folds, labels = FALSE)
  error.cv <- 0
  for(i in 1:k.folds){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    print(nrow(shuffled.data[-testIndexes,]))
    print(nrow(shuffled.data[testIndexes,]))
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
    else {
      print('Not implemented method.')
    }
    t  <- table(shuffled.data[testIndexes, 13], pred)
    if(i == 1) table.cv = t
    error.cv <- error.cv + (1-sum(diag(t))/length(shuffled.data[testIndexes, 13]))
    table.cv <- table.cv + t
  }
  print(table.cv / k.folds)
  print(paste('Cross-validation', class.method, 'Error:', (error.cv/k.folds)*100, sep = ' '))
}
