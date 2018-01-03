library('FactoMineR')

load('ws.rdata')
data.alcohol[,14] <- factor(data.alcohol[,14])
N <- nrow(data.alcohol)

learn <- sample(1:N, round(0.67*N))
nlearn <- length(learn)
ntest <- N - nlearn

dataM1 <- glm(Alcohol ~ ., data=data.alcohol[learn,], family=binomial)
dataM1.AIC <- step(dataM1)

P=0.5
dataM1.AICpred <- NULL
dataM1.AICpred[dataM1.AIC$fitted.values<P] <- 0
dataM1.AICpred[dataM1.AIC$fitted.values>=P] <- 1
dataM1.AICpred <- factor(dataM1.AICpred, labels=c("CL0","CL1"))

print(M1.TRtable <- table(Truth=data.alcohol[learn,]$Alcohol,Pred=dataM1.AICpred))

print(100*(1-sum(diag(M1.TRtable))/nlearn))

gl1t <- predict(dataM1.AIC, newdata=data.alcohol[-learn,],type="response")
gl1predt <- NULL
gl1predt[gl1t<P] <- 0
gl1predt[gl1t>=P] <- 1
gl1predt <- factor(gl1predt, labels=c("CL0","CL1"))

print(M1.TEtable <- table(Truth=data.alcohol[-learn,]$Alcohol,Pred=gl1predt))
print(100*(1-sum(diag(M1.TEtable))/ntest)) 






