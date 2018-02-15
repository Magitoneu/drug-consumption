
load('ws.rdata')

data.binary <- data.factor
a <- colnames(data.factor)
a <- a[14:31]
min_cl <- c("CL4","CL3","CL3","CL3","CL4","CL3","CL4","CL3","CL3","CL3","CL3","CL3","CL3","CL3","CL3","CL4","CL3","CL3")

for (i in 1:18) {
    min_cl = grep(min_cl[i], levels(data.factor[,a[i]]))
    data.binary[, a[i]][as.numeric(data.binary[, a[i]]) < min_cl[i]] = "CL0"
    data.binary[, a[i]][data.binary[, a[i]] != "CL0"] = "CL1"
    data.binary[, a[i]] = factor(data.binary[, a[i]])
}


