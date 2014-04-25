Phat.dete <- function(x){
  n <- sum(x)  
  f1 <- sum(x==1)
  f2 <- sum(x==2)
  f3 <- sum(x==3)
  if(f2==0){
    f1 <- max(f1 - 1, 0)
    f2 <- 1
  }
  A1 <- f1 / n * ((n-1)*f1 / ((n-1)*f1 + 2*max(f2,1)))
  A2 <- f2 / choose(n, 2) * ((n-2)*f2 / ((n-2)*f2 + 3*max(f3,1)))^2
  x <- x[x>0]
  q.solve <- function(q){
    e <- A1 / sum(x/n*exp(-q*x))
    out <- sum((x/n * (1 - e * exp(-q*x)))^2) - sum(choose(x,2)/choose(n,2)) + A2
    abs(out)
  }
  #q <- tryCatch(uniroot(q.solve, lower=0, upper=1)$root, error = function(e) {1})
  q <- tryCatch(optimize(q.solve, c(0,1))$min, error = function(e) {1})
  e <- A1 / sum(x/n*exp(-q*x))
  o <- x/n * (1 - e * exp(-q*x))
  o
}


prob.SC <- function(x){
  n = sum(x)
  f1 = sum(x==1)
  o = x/n*(1-f1/n)
}

prob.CSC <- function(x){
  n = sum(x)
  nstr = sum(x[x<=10])
  f1 = sum(x==1)
  o = x/n*(1-f1/nstr)
  o[x>10] = x[x>10]/n
  o
}
RRSE <- function(x){
  sqrt(sum((x-p)^2)/sum((p-mean(p))^2))
}

Csd <- function(x){
  x <- x[x>0]
  sd(x)
}
conf.reg=function(x,LCL,UCL,...) {
  x.sort <- order(x)
  x <- x[x.sort]
  LCL <- LCL[x.sort]
  UCL <- UCL[x.sort]
  polygon(c(x,rev(x)),c(LCL,rev(UCL)), ...)
}

RB <- function(x){
  mean(abs(x-p)/p)
}


Phat.zipf <- function(x){
  n <- sum(x)  
  f1 <- sum(x==1)
  f2 <- sum(x==2)
  f3 <- sum(x==3)
  f0.hat <- ifelse(f2 == 0, (n - 1) / n * f1 * (f1 - 1) / 2, (n - 1) / n * f1 ^ 2/ 2 / f2)
  if(f0.hat < 0.5){
    return(NULL)
  }
  f0.hat <- ceiling(f0.hat)
  A1 <- f1 / n * ((n-1)*f1 / ((n-1)*f1 + 2*max(f2,1)))
  A2 <- f2 / choose(n, 2) * ((n-2)*f2 / ((n-2)*f2 + 3*max(f3,1)))^2
  R <- A1^2/A2
  j <- 1:f0.hat+sum(x>0)
  f.solve <- function(x){
    #out <- sum(1/(j+x))^2 / sum(1/(j+x)^2) - R
    1/j^x
    out <- sum(exp(-x*log(j)))^2 / sum(exp(-2*x*log(j))) - R
    #out <- sum(1/j^x)^2 / sum(1/j^(2*x)) - R
    abs(out)
  }  
  b <- tryCatch(optimize(f.solve, lower=0, upper=100)$min, error=function(e) 1)
  a <- A1/sum(1/j^b)
  p <- a / j^b
  if(f0.hat ==1 ) p <- A1
  p
}

Phat.unde <- function(x){
  n <- sum(x)  
  f1 <- sum(x==1)
  f2 <- sum(x==2)
  f3 <- sum(x==3)
  f4 <- max(sum(x == 4), 1)
  f0.hat <- ceiling(ifelse(f2 == 0, (n - 1) / n * f1 * (f1 - 1) / 2, (n - 1) / n * f1 ^ 2/ 2 / f2))  #estimation of unseen species via Chao1
  #f0.hat <- f0.hat + ceiling(f3 / f4 / 4 * max(f1 - f2 * f3 / f4 / 2, 0))
  if(f0.hat < 0.5){
    return(NULL)
  } 
  if(f2==0){
    f1 <- max(f1 - 1, 0)
    f2 <- 1
  }
  A1 <- f1 / n * ((n-1)*f1 / ((n-1)*f1 + 2*max(f2,1)))
  A2 <- f2 / choose(n, 2) * ((n-2)*f2 / ((n-2)*f2 + 3*max(f3,1)))^2
  R <- A1^2/A2
  j <- 1:f0.hat
  f.solve <- function(x){ 
    out <- sum(x^j)^2 / sum((x^j)^2) - R
    abs(out)
  }
  b <-  tryCatch(optimize(f.solve, lower=(R-1)/(R+1), upper=1, tol=1e-5)$min, error = function(e) {(R-1)/(R+1)})
  a <- A1 / sum(b^j)
  p <- a * b^j
  if(f0.hat ==1 ) p <- A1
  p
}

EstComDis <- function(x){
  phat <- sort(c(Phat.unde(x), Phat.dete(x)), d=TRUE)
  phat <- phat[phat>0]
  phat
}

qDJade <- function(x, q, B){
  qDjade <- function(x, q){
    Phat.dete(x) 
    Phat.unde(x) 
    p.all <- c(Phat.dete(x), Phat.unde(x))
    if (q == 1){
      qD <- exp(-sum(p.all*log(p.all)))
    }else{
      qD <- (sum((p.all)^q))^(1/(1 - q))
    }
    return(qD)
  }
  ibootstrap <- function(y, fun, B, q){      # improved bootstrap
    n <- sum(y)
    Q <- function(r, data){sum(data == r)} 
    y <- y[which(y != 0)]; n <- sum(y)
    q1 <- Q(1, y); q2 <- Q(2, y)
    
    if (q2 > 0){
      alfa1 <- 2*q2/((n - 1)*q1 + 2*q2)
    } else if (q2 == 0 & q1 != 0){
      alfa1 <- 2/((n - 1)*(q1 - 1) + 1)
    } else {
      alfa1 <- 1  
    }
    Chat <- 1 - q1/n*(1 - alfa1)
    
    W <- (1 - Chat)/sum(y/n*(1 - y/n)^n)
    pi.hat <- y/n*(1 - W*(1 - y/n)^n)
    
    if (q2 > 0){
      Q0_hat <- (n - 1)/n*q1^2/(2*q2)
    }else{
      Q0_hat <- (n - 1)/n*q1*(q1 - 1)/(2*(q2 + 1))
    }
    Q0_hat <- ceiling(Q0_hat)
    pi.hat.r <- rep((1 - Chat)/Q0_hat, Q0_hat)         
    pi.star <- c(pi.hat, pi.hat.r)     
    
    Y <- matrix(rmultinom(B, n, pi.star), ncol=B)
    
    b <- 1:B
    sd(sapply(b, function(b){fun(Y[, b], q)}), na.rm=T)
  }
  i <- 1:length(q)
  est.value <- sapply(i, function(i) qDjade(x, q[i]))
  est.sd <- sapply(i, function(i) ibootstrap(x, qDjade, 10, q[i]))
  return(matrix(c(est.value, est.sd), nrow=2, byrow=T))
}