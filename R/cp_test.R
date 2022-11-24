#' Parametric Bootstrap P-value for the no-changepoint hypothesis
#'
#' @param object An object of class lmcp
#' @param n_sim Number of simulations
#'
#' @return A Monte-Carlo P-value for the no-change-point hypothesis
#'
#' @importFrom stringr str_split
#' @export
#' @examples
#' fit <- lm_cp(log(PFUnDA) ~ year, data = pfunda_uria)
#' p_value <- cp_test(fit)
#' p_value
cp_test <- function(object, n_sim = 1000){
  formula <- as.formula(paste0("sim_1~", str_split(deparse(object$null_model$formula), "~", simplify = TRUE)[2]))
  sim_Fstat <- function() {
    sim_data <- cbind(object$null_model$data, simulate(object$null_model))
    fit <- lm_cp(formula, data = sim_data,
                 cp_var = object$cp_var)
    fit$cp_Fstat
  }
  Fstat <- replicate(n_sim, sim_Fstat())
  cat(paste("Approximate P-value based on", n_sim, "Monte-Carlo iterations:\n"))
  mean(Fstat > object$cp_Fstat)
}
