read_ibex <- function(file_name,
                      col_names = NULL, partial_names = TRUE,
                      col_classes = NULL, partial_classes = TRUE, ...){

  # count the columns in the input file, set their names and classes
  n_cols <- count_columns(file_name)
  col_names <- set_column_names(n_cols, col_names, partial_names)
  col_classes <- set_column_classes(n_cols, col_classes, partial_classes)

  # read the file
  d <- read.csv(file_name, header=FALSE, fill=TRUE, comment.char="#",
                col.names=col_names,
                colClasses=col_classes,
                as.is=FALSE, ...)

  return(d)

}

get_results <- function(file_name,
                        controller,
                        elem_number = NULL,
                        del_col = NULL, del_mode = "auto",
                        col_names = NULL, partial_names = TRUE,
                        col_classes = NULL, partial_classes = TRUE,
                        recode_subjects = TRUE,
                        ...){

  #' General function for reading Ibex results.
  #'
  #' @param file_name character. File with the Ibex results (`results`
  #'   downloaded as is from Ibex)
  #' @param controller character. Name of Ibex controller to get the data for.
  #'   In principle can be a vector, but it is recommended to use single values,
  #'   since often different controllers have a different number of columns, and
  #'   the content of the columns beyond the first seven is also
  #'   controller-specific.
  #' @param elem_number Element number in IBEX. Only useful when a) You
  #'   specified multiple controllers within one item and b) Some of those
  #'   controllers have the same type. In this case you can specify which
  #'   exactly of those duplicate controllers you need.
  #' @param del_col numeric. Vector of column numbers indicating which columns are
  #'   to be deleted.
  #' @param del_mode how to determine which columns have to be deleted. Can be one of:
  #'          `auto` (decide based on data), `user` (only take into account
  #'          user's requests), `mixed` (take into account user request and data).
  #'          `auto` is used by defualt if "del_col" is not specified; if it is,
  #'          `mixed` is used. See Details
  #' @param col_names character vector with names for the columns
  #' @param partial_names logical. If `TRUE`, the first seven columns will receive
  #'                      default names (`subject`, `md5_hash`, `controller`,
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
  #'                      + subject - `factor`
  #'                      + md5_hash, controller, type, group - `character`,
  #'                      + item_number, element_number - `numeric`,
  #'
  #'                      So `col_names` will be taken to specify classes for columns
  #'                      starting from 8. If `FALSE`, `col_classes` will be taken
  #'                      as specifying classes for all columns
  #'
  #' @return data.frame with pre-processed Ibex data
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


  # ----- Check input parameters------
  if (!is.character(file_name)){stop ("file_name should be character")}
  if (is.character(controller)==FALSE) {stop ("controller should be a character")}
  if ((is.numeric(elem_number)==FALSE) &(is.null(elem_number)==FALSE)) {stop ("elem.number should be numeric")}
  # if ((is.numeric(del_col)==FALSE)&(is.null(del.col)==FALSE)) {stop ("del.col should be numeric")}
  if ((is.character(col_names)==FALSE) &(is.null(col_names)==FALSE)){stop ("col_names should be character")}


  d <- read_ibex(file_name, col_names, partial_names, col_classes, partial_classes, ...)
  #------------------------------------
  # check whether all the parameters provided are in the data. Report which ones are not

  tmp <- check_missing(controller, d[[3]], "controllers")
  check_missing(elem_number, d[[5]], "elem.numbers")

  # We can pass a vector of controllers, but since different
  # controllers create different number of columns, the
  # result may not be what we want. Let user know about it.
  if (length(controller)-tmp >1) {warning (length(controller)-tmp, " existing controllers requested.",
                                           "The columns may contain inconsistent info.\n")  }

  #------------------------------------

  # If elem.number specified subset with it; otherwise - without it
  if (!is.null(elem_number)){
    res <- d[(d[[3]] %in% controller) & (d[[5]] %in% elem_number),]}
  else {
    res <- d[d[[3]] %in% controller, ]
  }

  res <- droplevels(res)

  # we don't need old row counts, so we delete them
  rownames(res) <- NULL

  # if the number of rows of the data set is zero
  # then probably the subsetting didn't work.
  if (NROW(res) == 0) {
    stop("The subsetting resulted in empty data.frame. Check the following parameters ",
         "for correctness: `controller`, `elem_number` \n")
  }

  res[,1] <- factor(sort(paste0(res[,1], "_", res[,2])))
  colnames(res)[1] <- "subj_uid"

  res <- delete_columns(res, del_col, del_mode)

  if(recode_subjects){
    res <- cbind(subj = as.numeric(res$subj_uid), res)
  }

  return(res)

}
