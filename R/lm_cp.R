#' Fit a linear model with a change-point in trend but preserving continuity
#'
#' @param formula An object of class formula
#' @param data A data frame containing the variables in the model
#' @param cp_var Name of term where change-point is assumed to occur, by default this is the first term on the right-hand side of formula
#' @param cp_values Values where change-points may occur
#' @param ... Additional parameters to be passed to lm
#'
#' @return An object of class lmcp (an augmented object of class lm)
#'
#' @importFrom splines bs
#' @importFrom stringr str_replace
#' @export
#' @examples
#' fit <- lm_cp(log(PFUnDA) ~ year, data = pfunda_uria)
#' fit
#' summary(fit)

lm_cp <- function(formula, data, cp_var = NULL, cp_values = NULL, ...){
  library(splines)
  cl <- match.call()
  fit0 <- lm(formula, data = data, ...)
  fit0$formula <- formula
  fit0$data <- data
  frame <- model.frame(formula, data)
  n <- nrow(frame)
  k <- length(fit0$coefficients)
  if (is.null(cp_var))
    cp_var <- attr(terms(formula), "term.labels")[1]
  cp_range <- frame[, cp_var] |> range(na.rm = TRUE)
  cp_var_values <- sort(unique(frame[, cp_var]))
  if (length(cp_var_values) < 5) stop("Too few unique values of change-point variable")
  if (is.null(cp_values)){
    cp_values <- cp_var_values[3]:cp_var_values[length(cp_var_values) - 2]
  }
  replacements <- paste0("bs(", cp_var, ", degree = 1, knots = ", cp_values, ")")
  formulas <- str_replace_all(deparse(formula), cp_var, replacements)
  fits <- lapply(formulas, function(f) lm(f, data = data, ...))
  RSS <- sapply(fits, function(fit) sum(fit$residuals^2))
  RSS0 <- sum(fit0$residuals^2)
  cp_Fstat <- (RSS0 - RSS) / (RSS) * (n - k - 1)
  best_id <- which(cp_Fstat == max(cp_Fstat))
  if (length(best_id) > 1){
    warning("Optimal change point not unique, using median")
    best_id <- median(best_id)
  }
  cp <- cp_values[best_id]
  fit <- fits[[best_id]]
  fit$change_point <- cp
  fit$null_model <- fit0
  fit$call <- cl
  fit$cp_var <- cp_var
  fit$cp_Fstat <- min(cp_Fstat)

  # Change-points trends
  bs_slopes <- c(1 / (cp - cp_range[1]), 1 / (cp_range[2] - cp))
  A <- diag(fit$rank)
  A[2, 2] <- bs_slopes[1]
  A[3, 2] <- -bs_slopes[2]
  A[3, 3] <- bs_slopes[2]
  # A <- matrix(c(1, 0, 0, 0, bs_slopes[1], -bs_slopes[2], 0, 0, bs_slopes[2]), nrow = 3)
  covmat <- A %*% vcov(fit) %*% t(A)
  cp_coeff <- as.numeric(A %*% coefficients(fit))
  se <- sqrt(diag(covmat))
  t_value <- cp_coeff / se
  p_value <- (2 * (1 - pt(abs(t_value), df = fit$df.residual)))
  fit$cp_coefficients <- matrix(c(cp_coeff[2:3], se[2:3], t_value[2:3], p_value[2:3]), nrow = 2)
  colnames(fit$cp_coefficients) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  row.names(fit$cp_coefficients) <- c("Trend 1", "Trend 2")
  class(fit) <- c("lmcp", "lm")
  fit
}
