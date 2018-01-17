-- Incloem diversos fitxers --

Llibreries requerides:
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

Els scripts individuals s'executen de la següent manera:

	./SCRIPT DROGA

Que escriurà per pantalla l'error de predicció calculat i la matriu de confusió generada. 

    SCRIPT   L'script corresponent al mètode que es vol fer servir:
    
                LogisticRegression.R    : Regressió logística
                kNN.R                   : k-Nearest Neighbours
                NaiveBayes.R            : Naive Bayes
                MLP.R                   : one-hidden-layer MLP
                RandomForest.R          : Random Forest
				RandomForestW.R         : Random Forest with weights
                BinaryRandomForest.R    : Binary Random Forest
                BinaryRandomForestW.R   : Binary Random Forest with weights

    DROGA    La droga que es vol predir: 
    
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
								

També conté altres scripts:
		- plots.R, que genera tots els plots utilitzats a l'informe.
		- fun-methods.R, que conté les funcions on estàn implementats els mètodes.
		- common.R conté funcions de comparació, i càlcul d'errors entre d'altres.

Les dades del data set ja normalitzades i ben estructurades es troben a ws.rdata,
es poden carregar desde R amb la comanda
    
    >load("ws.rdata")

Les variables definides són:
    data                        - El data set original
    data.factor                 - El data set amb les variables nominals factoritzades
    data.factor.normalized      - El data set amb les variables nominals quantificades
    data.binary                 - El data set amb el consum de drogues binaritzat

