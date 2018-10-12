# fit a ridge regression model

ridge_reg <- function(formula, data, lambda){
	
	x <- as.matrix(model.matrix(formula, data))
	y <- data[,as.character(formula)[2]]
	beta <- solve(t(x)%*%x+lambda*diag(dim(x)[2])) %*% t(x) %*% y

	# Format coef for export
  	coef <- as.numeric(beta)
  	names(coef) <- colnames(x)

  	# The object to export
  	ret <- list(coef = coef, lambda = lambda, formula = formula)
  	class(ret) <- "ridge_reg"
  	ret
}
