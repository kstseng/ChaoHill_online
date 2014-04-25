basicInciFun <- function(Y, b){
  t <- Y[1]; y <- Y[-1]
  y <- y[which(y > 0)]
  U <- sum(y)
  D <- length(y)
  Q1 <- length(y[which(y == 1)])
  Q2 <- length(y[which(y == 2)])
  B <- b
  C <- 1 - Q1/U*(t - 1)*Q1/((t - 1)*Q1 + 2*Q2)
  
  tab1 <- as.matrix(c("t", "D", "Q1", "Q2", "C", "B"), ncol=1)
  tab2 <- round(as.matrix(c(t, D, Q1, Q2, C, B), ncol=1), 3)
  tab <- cbind(tab1, tab2)
  colnames(tab) <- c("Variable", "Value")
  rownames(tab) <- c("Number of observed samplings units", "Number of observed species", 
                     "Number of singletons", "Number of doubletons", 
                     "Estimated sample coverage", "Bootstrap replications for s.e. estimate")
  as.data.frame(tab)
}