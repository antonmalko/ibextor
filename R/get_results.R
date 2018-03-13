read_ibex <- function(file_name, ...){

  #' Read the csv file with Ibex results
  #'
  #' Thin wrapper around `read.csv`
  #' @param file_name character. Path to the file with Ibex results
  #' @return data frame with the contents of the file

  # count the columns in the input file, set their names and classes
  # n_cols <- count_columns(file_name)
  # col_names <- set_column_names(n_cols, col_names, partial_names)
  # col_classes <- set_column_classes(n_cols, col_classes, partial_classes)

  n_cols <- max(count.fields(file_name, sep = ","), na.rm = TRUE)
  # read the file
  d <- read.csv(file_name, header=FALSE, fill=TRUE, comment.char="#",
                col.names= paste0("V", seq_len(n_cols)), # make sure that all columns are read in
                stringsAsFactors = FALSE,
                ...)
  return(d)

}

subset_ibex <- function(d,
                        controller,
                        elem_number = NULL
                        ){

  #' Select the right subset of the Ibex data
  #'
  #' @param d data frame with Ibex results as returned by `read_ibex`
  #' @param controller character. Name of Ibex controller to get the data for.
  #'   In principle can be a vector, but it is recommended to use single values,
  #'   since often different controllers have a different number of columns, and
  #'   the content of the columns beyond the first seven is also
  #'   controller-specific.
  #' @param elem_number Element number in IBEX. Only useful when a) You
  #'   specified multiple controllers within one item and b) Some of those
  #'   controllers have the same type. In this case you can specify which
  #'   exactly of those duplicate controllers you need.
  #' @return data frame with data for requested controllers / elements

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

  return(res)
}

format_ibex <- function(d,
                        col_names = NULL, partial_names = TRUE,
                        col_classes = NULL, partial_classes = TRUE){

  #' Set the names and classes for the columns in Ibex data
  #'
  #' @param d data.frame as returned by `format_ibex`
  #'
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

  n_cols <- ncol(d)
  col_names <- make_column_names(n_cols, col_names, partial_names)
  col_classes <- make_column_classes(n_cols, col_classes, partial_classes)

  colnames(d) <- col_names
  d <- set_column_classes(d, col_classes)

}
