#!/usr/bin/Rscript
library(ggplot2)
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
makeConfusions = function(methodName, func=NULL) {
    drugs = colnames(data.factor[,14:31])
    pl = list()
    i = 1
    for (drug in drugs) {
        assign("drug", drug, envir = .GlobalEnv)
        df = data.factor[, c(2:13, grep(drug, colnames(data.factor)))]
        if (is.null(func)) {
            invisible(capture.output(table <- common.crossval(10, df, methodName)))
        } else {
            invisible(capture.output(table <- func(drug)))
        }
        table.df = data.frame(table)
        p = common.plotConfusion(table, drug)
        pl[[i]] = p
        i = i + 1
    }
    png(filename=paste(plotsdir, "/confusion-", methodName, ".png", sep=""), width=210, height=297, units="mm", res=200)
    multiplot(plotlist=pl, cols=3)
    dev.off()
}
print("NAIVE BAYES")
makeConfusions('naivebayes')
print("RANDOM FOREST")
makeConfusions("randomforest", func=funmeth.randomForest)
print("RANDOM FOREST WEIGHTED")
makeConfusions("randomforest-weighted", func=funmeth.randomForest.weighted)
