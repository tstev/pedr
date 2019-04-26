#' Preprocess pedigree
#'
#' `ped_prep()` prepares the given pedigree by recoding missing ids to `NA`,
#' checking for duplicate records and some more checks. This is usually the
#' first step before any further calculations are performed.
#'
#' The first three columns are assumed to be the id of the individual, sire and
#' dam, respectively in that order. New columns .id, .sire and .dam are created
#' which are used by other functions in this package.
#'
#' @param pedigree an object coercible to a `data.table` which includes the
#'   id of individual, sire and dam.
#' @param na_id_rm a logical indicating whether to remove individuals coded as
#'   missing or unkown. When set to `FALSE` it will return an error.
#'
#' @return a `data.table` containing the original ids and new id columns used
#'   in further calculation steps
#' @seealso [ped_add()] is a function that relies on preprocessing before adding
#'   the missing parents, for example.
#' @export
ped_prep <- function(pedigree, na_id_rm = FALSE) {
  # deep copy pedigree to ensure no changes through ref
  nped <- copy(as.data.table(pedigree))

  # create copies of first three columns for id, sire and dam, respectively
  cols <- c(".id", ".sire", ".dam")
  for (i in seq_along(cols)) set(nped, j = cols[[i]], value = pedigree[[i]])

  # recode missing ids
  for (j in cols) set(nped, j = j, value = renum_miss(nped[[j]]))

  # check not all parents are missing
  all_sire_miss <- all(nped[, is.na(.sire)])
  all_dam_miss <- all(nped[, is.na(.dam)])
  if (all_sire_miss && all_dam_miss) {
    stop(msg_wrap("All the parents have been coded as missing/unknown.",
                  "Please check input."), call. = FALSE)
  } else if (all_sire_miss && all_dam_miss) {
    parent <- c("sires", "dams")[c(all_sire_miss, all_dam_miss)]
    warning(msg_wrap("All the ", parent, " have been coded as missing/unknown.",
                     "They will all be considered as founders. Check input."),
            call. = FALSE)
  }

  # check no missing valus in .id col
  n_miss_id <- nped[is.na(.id), .N]
  if (n_miss_id > 0L) {
    if (na_id_rm) {
      msg <- ngettext(n_miss_id,
                      msg1 = " individual coded as missing has been removed.",
                      msg2 = " individuals coded as missing have been removed.")
      warning(n_miss_id, msg, call. = FALSE)
      nped <- nped[!is.na(.id), ]
    } else {
      stop(n_miss_id, " individual(s) coded as missing/unkown detected",
           " in original pedigree. Please check input", call. = FALSE)
    }
  }

  # check for duplicated records and keep only the first
  if (anyDuplicated(nped, by = c(".id", ".sire", ".dam")) > 0L) {
    warning("Duplicated records found. Only first record kept.",
            call. = FALSE)
    nped <- nped[!duplicated(nped, by = c(".id", ".sire", ".dam"))]
  }

  # check for duplicated ids and give warning
  if (anyDuplicated(nped, by = c(".id")) > 0L) {
    warning("Duplicated ids found. Will be considered as seperate individuals.",
            "Please check.", call. = FALSE)
  }

  return(nped)
}
