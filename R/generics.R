
#' Generic
#'
#' @param object An object of class lmcp
#' @export
print.lmcp <- function(object, ...){
  stats:::print.lm(object, ...)
  cat(paste0("Change-point fitted at ", object$cp_var, "=", object$change_point,
             " with slope ", signif(object$cp_coefficients[1, 1], 5), " before and ",
             signif(object$cp_coefficients[2, 1], 5), " after.")
  )
}

#' Generic
#'
#' @param object An object of class lmcp
#' @export
summary.lmcp <- function(object, ...){
  sum_fit <- stats:::summary.lm(object, ...)
  sum_fit$cp_coefficients <- object$cp_coefficients
  sum_fit$cp_var <- object$cp_var
  sum_fit$change_point <- object$change_point
  class(sum_fit) <- c("summary.lmcp", "summary.lm")
  sum_fit
}

#' Generic
#'
#' @param object An object of class summary.lmcp
#' @export
print.summary.lmcp <- function(object, ...){
  stats:::print.summary.lm(object, ...)
  cat(paste0("\nOptimal change-point found at ", object$cp_var, "=", object$change_point, ", with coefficients: \n"))
  printCoefmat(object$cp_coefficients)
}

#' Generic
#'
#' @param object An object of class lmcp_pvalue
#' @export
print.lmcp_pvalue <- function(object, ...){
  cat(paste("Approximate P-value based on", attr(object, "n_sim"), "Monte-Carlo iterations:\n",   as.numeric(object)
))
}

