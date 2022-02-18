#' Calculate OTS bounds
#' Main function of \code{otsbounds}. Calculates Optimal Treatment Selection bounds
#' @param ps prognostic score
#' @param obs Observed effect, e.g. observed mean
#' @param a The lowest bound for the potential outcome
#' @export
#' @examples
#' ps = c(0.1, 0.2, 0.1, 0.4)
#' obs = data.frame(a1 = c(1, 2, 3, 4), a2 = c(4, 5, 6, 7))
#' a = 0
ots_bound <- function(ps, obs, a) {


  UB = ps*obs[1,,drop = T] - ps * a
  LB = (1-ps) * a - (1 - ps) * obs[2,,drop = T]

  return(data.frame(UB = UB, LB = LB))

}
