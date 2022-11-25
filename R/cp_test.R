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
  formula <- as.formula(paste0("sim_response~", str_split(deparse(object$null_model$formula), "~", simplify = TRUE)[2]))
  sim_Fstat <- function() {
    sim_response <- simulate(object$null_model)
    idx <- as.numeric(rownames(sim_response))
    data <- object$null_model$data
    data$sim_response <- NA
    data$sim_response[idx] <- sim_response[["sim_1"]]
    fit <- lm_cp(formula, data = data,
                 cp_var = object$cp_var)
    fit$cp_Fstat
  }
  Fstat <- replicate(n_sim, sim_Fstat())
  p <- mean(Fstat > object$cp_Fstat)
  class(p) <- "lmcp_pvalue"
  attr(p, "n_sim") <- n_sim
  p
}
