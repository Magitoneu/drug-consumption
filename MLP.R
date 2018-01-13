library(nnet)
library(caret)
library(doMC)
require(doMC)
registerDoMC(4)
load('ws.rdata')

# args = commandArgs(TRUE)
# if (NA %in% args[1]) {
#   stop("NA")
# }
# drug = args[1]
drug = 'Alcohol'
df = data.factor[, c(2:13, grep(drug, colnames(data.factor)))]
df[, drug] = factor(df[, drug])



N <- nrow(df)

learn <- sample(1:N, round(0.67*N))
nlearn <- length(learn)
ntest <- N - nlearn

## specify 10x10 CV
trc <- trainControl (method="repeatedcv", number=5, repeats=5)
sizes <- seq(1,40,by=2)+1

## WARNING: this takes some minutes
print('Start with 5x5CV looking for best size')
model.10x10CV <- train (as.formula(paste(drug, " ~ .")), data = df, subset=learn, method='nnet', maxit = 1000, trace = T,
                        tuneGrid = expand.grid(.size=sizes,.decay=0), trControl=trc, MaxNWts = 1500)
best.size <- model.10x10CV$bestTune[1,1]
p1 <- as.factor(predict (model.nnet, type="class"))
t1 <- table(p1,df[learn, drug])
error_rate.learn <- 100*(1-sum(diag(t1))/nlearn)
error_rate.learn

p2 <- as.factor(predict (model.10x10CV, newdata=df[-learn,], type="raw"))
t2 <- table(pred=p2,truth=df[-learn, drug])
error_rate.test <- 100*(1-sum(diag(t2))/ntest)
error_rate.test

decays <- 10^seq(-3,0,by=0.05)

## WARNING: this takes some minutes
print('Start with 5x5CV looking for best decay')
model.10x10CV <- train (as.formula(paste(drug, " ~ .")), data = df, subset=learn, method='nnet', maxit = 1000, trace = T,
                        tuneGrid = expand.grid(.size=best.size,.decay=decays), trControl=trc, MaxNWts = 1500)

best.decay <- model.10x10CV$bestTune[1,2]


nnet.fit <- nnet(as.formula(paste(drug, " ~ .")), data = df[learn,], size = best.size, decay = best.decay, trace = T, maxit = 1000, MaxNWts = 1500)

nnet.predict <- predict(nnet.fit, newdata = df[-learn,], type = 'class')
b <- nnet.predict == df[-learn, drug]
summary(b)

tab <- table(nnet.predict, df[-learn, drug]) 
1 - sum(tab[row(tab)==col(tab)])/sum(tab)


