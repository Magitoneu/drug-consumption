-- Incolem diversos fitxers --

mètodes : {LogisticRegression.R, kNN.R, NaiveBayes.R, MLP.R, RandomForest.R, 
					 RandomForestW.R, BinaryRandomForest.R, BinaryRandomForestW.R}
					 
Els scripts individuals es poden executar de la seguent manera:

	Rscript mètode droga

Que escriure per pantalla l'error de predicció calculat i la matriu de confusió generada. 

Al argument droga i poden anar:{Alcohol, Amphet, Amyl, Benzos, Caff, 
								Cannabis, Choc, Coke, Crack, Ecstasy, Heroin, 
								Ketamine, Legalh, LSD, Meth, Mushrooms, Nicotine, VSA}
								

També conté altres scripts:
		- plots.R, que genera tots els plots utilitzats a l'informe.
		- fun-methods.R que conté les funcions on estàn implementats els mètodes.
		- common.R conté funcions de comparació, i càlcul d'errors entre d'altres.

Les dades del data set ja normalitzades i ben estructurades es troben a ws.rdata. 


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

