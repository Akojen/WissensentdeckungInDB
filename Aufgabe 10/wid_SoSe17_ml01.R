## Aufgabe 3

# theta[1] = c
# theta[2:14] = a
# theta[15:end] = b
#theta <- rnorm(1+13+13*13)
theta <- rep(0, 1+13+13*14/2) # Note: Last part due to gaussian formula

# After some optimization it turns out that expand.grid takes the majority
# of computing time. Found this faster alternative here:
# http://stackoverflow.com/questions/10405637/use-outer-instead-of-expand-grid
#expand.grid.alt(seq_len(nrow(dat)), seq_len(ncol(dat)))
expand.grid.alt <- function(seq1,seq2) {
  cbind(rep.int(seq1, length(seq2)),
        c(t(matrix(rep.int(seq2, length(seq1)), nrow=length(seq2)))))
}

f <- function(x) {
  c = theta[1]
  as = as.numeric(x)*theta[2:14]
  tmp = outer(as.numeric(x), as.numeric(x))
  tmp = tmp[lower.tri(tmp, diag=TRUE)]
  bs = tmp * theta[15:length(theta)]
  return(sum(c(c,as,bs)))
}

df <- function(x) {
  dC = 1
  dAs = as.numeric(x)
  tmp = outer(as.numeric(x), as.numeric(x))
  dBs = tmp[lower.tri(tmp, diag=TRUE)]
  
  return(c(dC,dAs,dBs))
}

ell <- function(X, Y) {
  preds = apply(X, 1, f)
  error = 0.5*(preds - Y)*(preds-Y)
  return(sum(error))
}

dell <- function(X,Y) {
  preds = apply(X, 1, f)
  diff = preds - Y
  gradients = matrix(apply(X, 1, df), nrow=length(theta), ncol=dim(X)[1])
  tmp = t(t(gradients) * diff)
  return(rowSums(tmp))
  }

rmse <- function(X,Y) {
  error <- ell(X,Y)
  return(sqrt(sum(error)/length(error)))
}

main <- function() {
  data <- read.table("/home/jonas/Dokumente/Workspace/R/WiD_SS18/Aufgabe 05/housing.csv", header=TRUE, sep = ",")
  X <- data[1:13]
  Y <- unlist(data[14]) # needed for dell computation so that t(t(gradients) * diff) works
  
  print("still here")
  
  NSteps = 100
  Eta = 0.0002
  
  for(i in 1:NSteps) {
    del <- dell(X,Y)
    theta <<- theta - Eta * del 
    cat("RMSE: ", rmse(X,Y), "\n")
    }
}