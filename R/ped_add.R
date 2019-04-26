#' Add parents to pedigree
#'
#' `ped_add()` takes an `R` object and assumes the first three elements
#' corresponds to the id of the individual, sire and dam, in that order. If the
#' `pedigree` object contains additional columns these will be filled with `NA`
#' by default for the added individuals.
#'
#' @param pedigree an object coercible to a `data.table` which includes the
#'   id of individual, sire and dam.
#'
#' @return a `data.table` containing the missing parents
#'
#' @examples
#'
#' @import data.table
#' @export ped_add
#' @rdname ped_add
ped_add <- function(pedigree, ...) {
  nped <- ped_prep(pedigree, na_id_rm = TRUE)

  # add missing parents and founders
  miss_parents <- unique(rbind(nped[!is.na(.sire), .(.id = .sire)],
                               nped[!is.na(.dam), .(.id = .dam)]), by = ".id")[
                                 !nped, on = ".id"]
  if ((n_miss <- nrow(miss_parents)) > 0L) {
    type <- miss_parents[, typeof(.id)]
    set(miss_parents, j = c(".sire", ".dam"), value = na_vector(type, n_miss))
    col_order <- colnames(nped)
    nped <- rbind(miss_parents, nped, fill = TRUE)
    setcolorder(nped, col_order)
  }
  return(nped)
}
