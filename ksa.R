library(proxyC, philentropy, quanteda, data.table)

dta <- read.csv(file = 'ksa.csv', row.names = 'SOC_code')

# Uso que max(Jaccard distance) = 1 ~ Jaccard Similarity
x <- 1 - distance(dta, method = "jaccard")
colnames(x) <- rownames(dta)
rownames(x) <- rownames(dta)

# Ahora lo dejamos en formato largo (774^2)x3
simMat <- data.frame(SOC1=NA, SOC2=NA, Sim=NA)
simMat <- as.data.table(simMat)
for (i in 1:nrow(x)) {
  for (k in 1:ncol(x)) {
    simMat[i*k, ]=c(rownames(x)[i], colnames(x)[k], x[i, k])
  }
}
fwrite(simMat, 'simKSA.csv')
