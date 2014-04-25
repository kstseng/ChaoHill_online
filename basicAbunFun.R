basicAbunFun <- function(x, b){
  x <- x[which(x > 0)]
  n <- sum(x)
  D <- length(x)
  f1 <- length(x[which(x == 1)])
  f2 <- length(x[which(x == 2)])
  B <- b
  C <- 1 - f1/n*((n - 1)*f1/((n - 1)*f1 + 2*f2))
  tab1 <- as.matrix(c("n", "D", "f1", "f2", "C", "B"), ncol=1)
  tab2 <- round(as.matrix(c(n, D, f1, f2, C, B), ncol=1), 3)
  tab <- cbind(tab1, tab2)
  colnames(tab) <- c("Variable", "Value")
  rownames(tab) <- c("Number of observed individuals", "Number of observed species", 
                     "Number of singletons", "Number of doubletons", 
                     "Estimated sample coverage", "Bootstrap replications for s.e. estimate")
  as.data.frame(tab)
}