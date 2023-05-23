 
##Create a dataset
df <- data.frame(Religious = c(10, 30, 50), nonReligious = c(25, 15, 5) )
rownames(df) <- c("young", "midAged", "old")
df <- as.matrix(df)

allSum <- sum(df)
nrow <- nrow(df)
ncol <- ncol(df)
ncell <- nrow*ncol

## Initialize
rowSum <- rep(0, nrow)
colSum <- rep(0, ncol)

for (i in 1:nrow) {
  rowSum[i] <- sum(df[i,])
}

for (j in 1:ncol) {
  colSum[j] <- sum(df[,j ])
}

## Computed the expected value assuming independence
expected <- matrix(data = rep(0, ncell), nrow = nrow, ncol = ncol)
diff <- matrix(data = rep(0, ncell), nrow = nrow, ncol = ncol)
  for (j in 1:ncol){
    for (i in 1:nrow) {
      expected[i, j] <- (rowSum[i]*colSum[j])/allSum
      diff[i, j] <- (df[i, j]- expected[i, j])^2/(expected[i, j])
    }   
  }

#Hypothesis testing using p-value
sigLevel <- 0.05
sumDiff <- round(sum(diff), 3)
degFree <- (nrow-1)*(ncol-1)
pValue <- round(pchisq(sumDiff, degFree, lower.tail = F), 3)

if (pValue <= sigLevel ){
  print("Reject the null.")
} else {
  print("Fail to reject the null.")
}



##Hypothesis testing using using critical value
criticalVal <- round(qchisq(sigLevel, degFree, lower.tail = F), 3)

if (sumDiff > criticalVal ){
  print("Reject the null.")
} else {
  print("Fail to reject the null.")
}



