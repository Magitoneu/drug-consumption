

# drug-consumption

Abstract: Classify type of drug consumer by personality data

[Dataset](http://archive.ics.uci.edu/ml/datasets/Drug+consumption+%28quantified%29)

All the project is build with R. 

### Required libraries
```
ggplot2
reshape
gridExtra
e1071
FactoMineR
randomForest
nnet
MASS
class
caret
```

### Execution
	./SCRIPTNAME DRUG

This will show the prediciton error and the confusion matrix at the end of the execution. 

    SCRIPTNAME   Each script executes a different method:
    
                LogisticRegression.R    : Logistic regression
                kNN.R                   : k-Nearest Neighbours
                NaiveBayes.R            : Naive Bayes
                MLP.R                   : one-hidden-layer MLP
                RandomForest.R          : Random Forest
				RandomForestW.R         : Random Forest with weights
                BinaryRandomForest.R    : Binary Random Forest
                BinaryRandomForestW.R   : Binary Random Forest with weights

    DRUG    The drug about we want the prediciton: 
    
                Alcohol
                Amphet
                Amyl
                Benzos
                Caff
                Cannabis
                Choc
                Coke 
                Crack
                Ecstasy
                Heroin
                Ketamine
                Legalh
                LSD 
                Meth
                Mushrooms 
                Nicotine
                VSA
								

Other scripts:
		- plots.R, it creates all the plots of the final document. 
		- fun-methods.R, where all the methods are implemented.
		- common.R, util functions. 

In order to load all the data:
    
    >load("ws.rdata")
