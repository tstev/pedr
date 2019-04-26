#' Renumber pedigree
#'
#' `ped_renum()` takes an `R` object and assumes the first three elements
#' corresponds to the id of the individual, sire and dam, in that order. First,
#' some assumed missing codes and recorded to NA and then all the ids are
#' renumbered by adding three additional columns `.id`, `.sire` and `.dam`.
#'
#' It is assumed that all the sire and dam ids are present in the individual
#' column. If not run `ped_add()` first.
#'
#' @param pedigree an object which includes the id of individual, sire and dam.
#'
#' @return a `data.table` containing the original ids and the renumbered ids
#'
#' @examples
#'
#' @import data.table
#' @export ped_renum
#' @rdname ped_renum
ped_renum <- function(pedigree, ...) {
  UseMethod("ped_renum", pedigree)
}

# Steps involved in renumbering the pedigree
# 1. Get rid of unknown/missing ids -> NA
# 2.
