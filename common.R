#library(ggplot2)


common.compare = function(real, pred) {
    t = table(real, pred)
    print(t)
    print(100*(1-sum(diag(t))/length(real))) 
}
