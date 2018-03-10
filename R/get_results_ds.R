get_results_ds <- function(file_name,
                          elem_number = NULL,
                          del_col = NULL, del_mode = "auto",
                          col_names = NULL,
                          partial_names = TRUE,
                          col_classes = NULL, partial_classes = TRUE,
                          short_subj_ids = TRUE,
                          sprt = TRUE,
                          ...) {

  #' Get results from a "DashedSentence" controller
  #'
  #' @param file_name character. Path to the file with Ibex results
  #' @param elem_number Element number in IBEX. Only useful when a) You
  #'   specified multiple controllers within one item and b) Some of those
  #'   controllers have the same type. In this case you can specify which
  #'   exactly of those duplicate controllers you need.
  #' @param del_mode how to determine which columns have to be deleted. Can be one of:
  #'          `auto` (decide based on data), `user` (only take into account
  #'          user's requests), `mixed` (take into account user request and data).
  #'          `auto` is used by default if "del_col" is not specified; if it is,
  #'          `mixed` is used. See Details
  #' @param col_names character vector with names for the columns
  #' @param partial_names logical. If `TRUE`, the first seven columns will receive
  #'                      default names (`timestamp`, `md5_hash`, `controller`,
  #'                      `item_number`, `element_number`, `type`, `group`),
  #'                      since these columns are the same in all (default)
  #'                      Ibex controllers. So `col_names` will be taken to
  #'                      specify names for columns starting from 8.
  #'                      If `FALSE`, `col_names` will be taken as specifying names
  #'                      for all columns
  #' @param col_classes character vector with classes to be assumed for the columns
  #' @param partial_classes logical. If `TRUE`, the first seven columns will receive
  #'                      default classes:
  #'
  #'                      + timestamp, md5_hash, controller, type, group - `character`,
  #'                      + item_number, element_number - `numeric`,
  #'
  #'                      So `col_classes` will be taken to specify classes for columns
  #'                      starting from 8. If `FALSE`, `col_classes` will be taken
  #'                      as specifying classes for all columns
  #'  @param short_subj_ids logical. If `TRUE`, each subject will be assigned
  #'  a short index. If `FALSE`, a combination of timestamp and browser md5 hash
  #'  will be used to identify each subject.
  #'  @param sprt logical. Set to `TRUE` if the data comes from "DashedSentence"
  #'  controlled with `mode: "self-paced reading"` setting. Set to `FALSE` if
  #'  it had `mode: "acceptability judgment` setting.
  #'  @param ... further parameters passed to `read.csv`
  #'
  #'
  #' @return data.frame
  #' @export
  #'
  #' @details
  #'
  #' The first seven columns in IBEX results always are:
  #' \enumerate{
  #' \item Time the results are received
  #' \item MD5 hash of participant's IP
  #' \item Controller type
  #' \item Item number
  #' \item Element number
  #' \item Type
  #' \item Group
  #'}
  #' By default, the deletion mode is `auto`. It means that the function will
  #' remove columns not containing useful information. This means:
  #'
  #' \itemize{
  #' \item "md5 hash" column is always removed since it's not usually used in analyses
  #' \item "controller" column is removed if there is just one controller in the data -
  #'   it is assumed, the user will know what controller that is
  #' \item "element number" column is removed if that column only contains zeros
  #' \item "group" column is removed if it only contains `NULL` values
  #' }
  #'
  #' The above applies to all controllers. In addition:
  #'
  #' \itemize{
  #' \item for DashedSentence and DashedAccpetabilityJudgment column 11 ("line break")
  #' is removed if it only contains `FALSE` values.
  #' \item For DashedAcceptabilityJudgment only column 15 ("is.correct") is removed
  #' if it contains only `NULL` valuess (if this is the case, it means that correct
  #' answers were not specified in IBEX, so this column is of no use).
  #' }
  #'
  #' In `user` deletion mode the data will not be taken into account at all, and
  #' only the columns specified by user will be deleted.
  #'
  #' In `mixed` deletion mode the columns to delete are the union of the sets of
  #' automatically deleted columns and user-specified columns.


  if (sprt){ # if the mode is "self-paced reading"

    if (is.null(col_names)){
      col_names <- c("region",
                     "word",
                     "rt",
                     "line_break",
                     "sentence")
    }

    if (is.null(col_classes)){
      col_classes <- c("factor",
                       "character",
                       "numeric",
                       "logical",
                       "character")
    }

    res <- read_ibex(file_name, ...)
    res <- subset_ibex(res, controller = "DashedSentence", elem_number = elem_number)

    if (any(is.na(as.numeric(res[[8]])))){
      stop("Column 8 contains non-numeric data, which it shouldn't. It is possible ",
              "that you are subsetting for the wrong controller, or that the mode of ",
              "DashedSentence controller was set to `acceptability judgment` during ",
              "data collection, while you are requesting `self-paced reading` (sprt = TRUE)")
    }

    res <- format_ibex(res,
                       col_names = col_names, partial_names = partial_names,
                       col_classes = col_classes, partial_classes = partial_classes)

    res <- recode_subjects(res, short_ids = short_subj_ids)
    res <- delete_columns(res, del_col, del_mode)

  } else { # if the mode is "speeded acceptability"

    if (is.null(col_names)){
      col_names <- c("sentence")
    }

    if (is.null(col_classes)){
      col_classes <- c("character")
    }

    res <- read_ibex(file_name, ...)
    res <- subset_ibex(res, controller = "DashedSentence", elem_number = elem_number)

    if (any(res[[9]] != "")){
      stop("Column 9 contains some data, which it shouldn't. It is possible ",
           "that you are subsetting for the wrong controller, or that the mode of ",
           "DashedSentence controller was set to `self-paced reading` during ",
           "data collection, while you are requesting `acceptability judgment` (sprt = FALSE)")
    }

    res <- res[, 1:8]

    res <- format_ibex(res,
                       col_names = col_names, partial_names = partial_names,
                       col_classes = col_classes, partial_classes = partial_classes)

    res <- recode_subjects(res, short_ids = short_subj_ids)
    res <- delete_columns(res, del_col, del_mode)
  }

  return(res)
}

