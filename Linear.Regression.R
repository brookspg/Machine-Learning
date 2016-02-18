linear_regression <- function(data, dependent, ..., intercept = T) {
  #List of Independent Variables
  variables <- unlist(list(...))
  
  #Matrix of Independent Variable Data
  X <- (data[,variables])
  
  #Factor and Character Variables (sets first value in factor list as default)
  for(i in variables) {
    if (is.factor(X[,i])) {
      levs <- levels(X[,i])
      names <- colnames(X)
      for(j in levs[2:length(levs)]) {
        X <- cbind(X, j = ifelse(X[,i] == j, 1, 0))
      }
      X <- X[, !(names(X) %in% c(i))]
      dimnames(X)[[2]] <- c(names[1:(length(names)-1)], levs[2:length(levs)])
    }
    else if(is.character(X[,i])) {
      X[,i] <- as.factor(X[,i])
      levs <- levels(X[,i])
      names <- colnames(X)
      for(j in levs[2:length(levs)]) {
        X <- cbind(X, j = ifelse(X[,i] == j, 1, 0))
      }
      X <- X[, !(names(X) %in% c(i))]
      dimnames(X)[[2]] <- c(names[1:(length(names)-1)], levs[2:length(levs)])
    }
 }
  
  #If an intercept is desired (default is T) append an intercept column ot data
  if (intercept == T) {
    X <- cbind(intercept = rep(1, length(X[,1])), X)
  }
  
  #Make Sure X is properly formatted
  X <- as.matrix(X)
  
  #Gather dependent Column
  Y <- as.matrix(data[, dependent])
  
  #Generate Coefficients using matrix transformations
  w <- (solve(t(X) %*% X) %*% t(X)) %*% Y
  colnames(w) <- c('Coefficients')
  
  return(w)
}