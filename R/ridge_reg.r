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

ridge_reg <- function(formula, lambda, data){
  
  x <- as.matrix(model.matrix(formula, data))
	y <- data[,as.character(formula)[2]]
	
  svd_obj <- svd(x)
  U <- svd_obj[["u"]]
  V <- svd_obj[["v"]]
  svals <- svd_obj[["d"]]
  D <- diag(svals / (svals^2 + lambda))
  beta <- V %*% D %*% t(U) %*% y

  # beta <- solve(t(x)%*%x+lambda*diag(dim(x)[2])) %*% t(x) %*% y

	# Format coef for export
  coef <- as.numeric(beta)
  names(coef) <- colnames(x)

  # The object to export
  ret <- list(coef = coef, lambda = lambda, formula = formula)
  class(ret) <- "ridge_reg"
  ret
}
