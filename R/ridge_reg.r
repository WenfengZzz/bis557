#' Fit a ridge regression model
#'
#' @description This function performs ridge regression.
#' @param formula a formula
#' @param data a data.frame
#' @param lambda a numeric parameter
#' @return An ridge_reg object
#' @import stats MASS graphics
#' @examples
#' fit <- ridge_reg(Sepal.Length ~., 1.2, iris)
#' @export

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
