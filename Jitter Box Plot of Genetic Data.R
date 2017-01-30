readpheno2=function(fname) {
  y=read.table(fname)
  return (y)
}

X <- readgeno("lm1.geno")
Y <- readpheno2("lm2.pheno")
phenotype1 = Y[,1]
phenotype2 = Y[,2]
phenotype3 = Y[,3]
phenotype4 = Y[,4]

X <- t(as.matrix(X))
X <- cbind(1,X)

model = lm(phenotype4 ~ X)
model
my.summary <- summary(model)

table <- my.summary$coefficients
hist(table[,4], main= "Phenotype 1 Histogram of P Values",xlab= "P Values", breaks=20)
subset(table, table[,4] < 0.0125)
SNP = 251
boxplot(phenotype1 ~ X[,SNP], ylab = "Phenotype", xlab = "Genotype", main= paste("SNP ", SNP, " / Phenotype 4 Relationship"))
stripchart(phenotype1 ~ X[,SNP], vertical=TRUE, method="jitter", col = "orange", add=TRUE)
length(X[1,])
