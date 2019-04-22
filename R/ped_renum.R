#' Renumber pedigree
#'
#' @param pedigree
#' @param as_dt
#'
#' @return
#'
#' @examples
#'
#' @import data.table
#' @export ped_renum
#' @rdname ped_renum
ped_renum <- function(pedigree, ...) {
  UseMethod("ped_renum", pedigree)
}

