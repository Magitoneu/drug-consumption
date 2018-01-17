#!/usr/bin/Rscript
library(ggplot2)
library(reshape)
library(gridExtra)
source("common.R")
source("fun-methods.R")
load("ws.rdata")

plotsdir = "plots-dir"
dir.create(plotsdir)

# ==================================================================================
# ==================================================================================
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
 if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
    }
  }
}
# ==================================================================================
print("DISTRIBUTION ALL CSV")
# ==================================================================================
a = lapply( data.factor[,14:31], function(x) rbind(
    CL0 = sum(x == "CL0") ,
    CL1 = sum(x == "CL1") ,
    CL2 = sum(x == "CL2") ,
    CL3 = sum(x == "CL3") ,
    CL4 = sum(x == "CL4") ,
    CL5 = sum(x == "CL5") ,
    CL6 = sum(x == "CL6")
))
write.csv(data.frame(a), paste(plotsdir, "/all.csv", sep=""))
# ==================================================================================
print("DISTRIB FULL")
# ==================================================================================
df = data.factor[,14:31]
pl = lapply(seq_along(df), function(x) {
    a = data.frame(df[,x])
    ggplot(a, aes_string(x=colnames(a)[1])) + geom_bar() + labs(x=names(df)[x], y=NULL) + ylim(0, 1700)
})
png(filename=paste(plotsdir, "/all.png", sep=""), width=210, height=297, units="mm", res=200)
multiplot(plotlist=pl, cols=2)
dev.off()
# ==================================================================================
print("DISTRIB HALF 1")
# ==================================================================================
df = data.factor[,14:23]
pl = lapply(seq_along(df), function(x) {
    a = data.frame(df[,x])
    ggplot(a, aes_string(x=colnames(a)[1])) + geom_bar() + labs(x=names(df)[x], y=NULL) + ylim(0, 1700)
})
png(filename=paste(plotsdir, "/all-sub-00.png", sep=""), width=210, height=250, units="mm", res=200)
multiplot(plotlist=pl, cols=2)
dev.off()
# ==================================================================================
print("DISTRIB HALF 2")
# ==================================================================================
df = data.factor[,24:31]
pl = lapply(seq_along(df), function(x) {
    a = data.frame(df[,x])
    ggplot(a, aes_string(x=colnames(a)[1])) + geom_bar() + labs(x=names(df)[x], y=NULL) + ylim(0, 1700)
})
png(filename=paste(plotsdir, "/all-sub-01.png", sep=""), width=210, height=200, units="mm", res=200)
multiplot(plotlist=pl, cols=2)
dev.off()
# ==================================================================================
print("CONFUSIONS")
# ==================================================================================
makeConfusions = function(data, methodName, func=NULL, cols=3, width=210, height=297) {
    drugs = colnames(data[,14:31])
    pl = list()
    i = 1
    for (drug in drugs) {
        if (is.null(func)) {
            assign("drug", drug, envir = .GlobalEnv)
            df = data.factor[, c(2:13, grep(drug, colnames(data)))]
            invisible(capture.output(table <- common.crossval(10, df, methodName, drug)))
        } else {
            invisible(capture.output(table <- func(data, drug)))
        }
        table.df = data.frame(table)
        p = common.plotConfusion(table, drug, axis=TRUE)
        pl[[i]] = p
        i = i + 1
    }
    png(filename=paste(plotsdir, "/confusion-", methodName, ".png", sep=""), width=width, height=height, units="mm", res=200)
    multiplot(plotlist=pl, cols=cols)
    dev.off()
}


print("NAIVE BAYES")
makeConfusions(data.factor, 'naivebayes', fun=funmeth.naiveBayes)
print("RANDOM FOREST")
makeConfusions(data.factor, "randomforest", func=funmeth.randomForest)
print("RANDOM FOREST WEIGHTED")
makeConfusions(data.factor, "randomforest-weighted", func=funmeth.randomForest.weighted)
print("RANDOM FOREST BINARY")
makeConfusions(data.binary, "randomForestBinary", func=funmeth.randomForest)
print("RANDOM FOREST BINARY WEIGHTED")
makeConfusions(data.binary, "randomForestBinaryWeighted", func=funmeth.randomForest.weighted)
print("KNN")
makeConfusions(data.factor.normalized, "knn", func=funmeth.knn)
print("MLP")
makeConfusions(data.factor, "mlp", func=funmeth.mlp)
print("Logistic Regression")
makeConfusions(data.binary, "logisticRegression", func=funmeth.logisticRegression)

print("SMALLS")
print("Logistic Regression")
makeConfusions(data.binary, "logisticRegression-xs", func=funmeth.logisticRegression, cols=4, width=210, height=148.5)
print("RANDOM FOREST BINARY")
makeConfusions(data.binary, "randomForestBinary-xs", func=funmeth.randomForest, cols=4, width=210, height=148.5)
print("RANDOM FOREST BINARY WEIGHTED")
makeConfusions(data.binary, "randomForestBinaryWeighted-xs", func=funmeth.randomForest.weighted, cols=4, width=210, height=148.5)

#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

makeComparisons = function(datas, methodNames, funcs, filename) {
    drugs = colnames(datas[[1]][,14:31])
    df.error = data.frame(data.frame(matrix(nrow=length(drugs), ncol=length(funcs) + 1)))
    df.subs = data.frame(data.frame(matrix(nrow=length(drugs), ncol=length(funcs) + 1)))
    colnames(df.error) = c("drug", methodNames)
    colnames(df.subs) = c("drug", methodNames)
    df.error[,1] = drugs
    df.subs[,1] = drugs
    i = 1
    for (drug in drugs) {
        j = 2
        errors = c()
        subs = c()
        for (func in funcs) {
            print(paste(i, j))
            invisible(capture.output(table <- func(datas[[j-1]], drug)))
            total = sum(table)
            error = 1 - (sum(diag(table)) / total)
            subs = common.getSubstimateds(table) / total
            df.error[i,j] = error
            df.subs[i,j] = subs
            j = j + 1
        }
        i = i + 1
    }
    df.melted <- melt(df.error, id = "drug")
    p.error = ggplot(data = df.melted, aes(x = drug, y = value, color = variable, group = variable)) +
        geom_line() + geom_point() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="bottom", legend.title=element_blank()) + 
        labs(x = "Drug", y = "Error")
    df.melted <- melt(df.subs, id = "drug")
    p.subs = ggplot(data = df.melted, aes(x = drug, y = value, color = variable, group = variable)) +
        geom_line() + geom_point() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = "Drug", y = "Substimateds")
    pl = list(p.error, p.subs)

    mylegend = g_legend(p.error)
    png(filename=paste(plotsdir, "/", filename, ".png", sep=""), width=210, height=148.5, units="mm", res=200)
    p = grid.arrange(arrangeGrob(p.error + theme(legend.position="none"),
                         p.subs + theme(legend.position="none"),
                         nrow=1),
             mylegend, nrow=2,heights=c(10, 1))
    dev.off()
}

datas = list(data.factor, data.factor, data.factor, data.factor.normalized, data.factor)
methodNames = c("naivebayes", "randomforest","randomforestweighted", "knn", "mlp")
funcs = c(funmeth.naiveBayes, funmeth.randomForest, funmeth.randomForest.weighted, funmeth.knn, funmeth.mlp)
makeComparisons(datas, methodNames, funcs, "all-classes")

datas = list(data.binary, data.binary, data.binary)
methodNames = c("logisticregression", "randomforest","randomforestweighted")
funcs = c(funmeth.logisticRegression, funmeth.randomForest, funmeth.randomForest.weighted)
makeComparisons(datas, methodNames, funcs, "bin-classes")

