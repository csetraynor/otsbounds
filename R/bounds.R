#' Calculate OTS bounds
#' Main function of \code{otsbounds}. Calculates Optimal Treatment Selection bounds
#' @param ps prognostic score
#' @param y1 Observed effect on treatment 1
#' @param y0 Observed effect on treatment 0
#' @param beta The lowest bound for the potential outcome
#' @param ... ignored
#' @export
#' @examples
#' ps = c(0.1, 0.2, 0.1, 0.4)
#' obs = data.frame(a1 = c(1, 2, 3, 4), a2 = c(4, 5, 6, 7))
#' a = 0
ots_bound <- function(ps, y1, y0, beta, ...) {

  UB = ps * y1 - ps * beta
  LB = (1 - ps) * beta - (1 - ps) * y0

  return(data.frame(UB = UB, LB = LB))

}


#' Calculate No Assumptions bounds
#' Main function of \code{otsbounds}. Calculates Optimal Treatment Selection bounds
#' @param ps prognostic score
#' @param y1 Observed effect on treatment 1
#' @param y0 Observed effect on treatment 0
#' @param beta The lowest possible potential outcome
#' @param alpha The highest possible potential outcome
#' @export
#' @examples
#' ps = c(0.1, 0.2, 0.1, 0.4)
#' obs = data.frame(a1 = c(1, 2, 3, 4), a2 = c(4, 5, 6, 7))
#' a = 0
no_assumption_bound <- function(ps, y1, y0, beta, alpha) {

  UB = ps * y1 - ps * beta
  LB = (1 - ps) * beta - (1 - ps) * y0

  return(data.frame(UB = UB, LB = LB))

}
