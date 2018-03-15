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

  # check that we have at least 8 columns - that's the minimum for Ibex dat
  check_ibex_df(d)

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

check_missing <-  function (x, string, name) {
  #' A utility function reporting if the information the user requested
  #' can indeed be found in the file.
  #' @param x the info requested by the user, e.g. the name of Ibex controller
  #'          or the element_number
  #' @param string unique values in the corresponding column of the Ibex file
  #'               (column 3 for controller, column 5 for element_number )
  #' @param name character, specifying the type of info we are checking for.
  #'             Will be inserted in the report message.
  #' @return invisibly, the number of requested pieces of info absent from the
  #'         Ibex file.


    missing_values <- which(!(x %in% string))
    missing_values <- x[missing_values]

    if (length(missing_values)==length(x)&length(x)>0)
      stop ("None of the ", name," (", paste0(missing_values, collapse=", "), ") is found in the data")

    if (length(missing_values)>0)
      message ("The following ", name, " are not found in the data: ",
               paste0(missing_values, collapse=", "))

    invisible(length(missing_values))

}

check_ibex_df <- function(d){
  if (ncol(d) < 8) {
    stop("Ibex data has at least 8 columns, and your data has fewer than that.")
  }
}

recode_subjects <- function(d, short_ids = TRUE){

  #' Set unique subject IDs
  #'
  #' Ibex provides 2 pieces of information identifying a participant: timestamp
  #' of the moment when the results and md5 hash of browser and system
  #' information. Neither of these parameters in itself is guaranteed to be
  #' unique; their combination has more chances of being so.
  #'
  #' This function merges the timestamp and md5 into a single string (separated
  #' by "_"), and puts it into `subj_uid` column (which replaces the `timestamp`
  #' column. If necessary, timestamp can be extracted by simply breaking the
  #' combined string at "_").
  #'
  #' @param d data.frame with (a subset of) Ibex data
  #' @param short_ids logical. If `FALSE`, just merge timestamp and md5 hash
  #'   together, If `TRUE` (default), add a new column with a simple index to
  #'   idenitfy subjects. One index is assigned to each combination of timestamp
  #'   and browser md5 hash. The column is put at the place of `subj_uid`
  #'   column, and `subj_uid` column is moved at the very end of the data.frame
  #'   (in order not to interfere with column deletion; otherwise we would have
  #'   to offset the indices of columns specified for deletion by 1).
  #' @return data.frame with updated subject codes

  d[,1] <- factor(sort(paste0(d[,1], "_", d[,2])))
  colnames(d)[1] <- "subj_uid"

  if(short_ids){
    d <- d[, c(setdiff(colnames(d), "subj_uid"), "subj_uid")]
    d <- cbind(subj = factor(as.numeric(d$subj_uid)), d)
  }

  return(d)
}



# numbers_only <- function (x){
#     res <- suppressWarnings(is.na(sapply((x), as.numeric)))
#     if (length(which(res)) > 0) return (which(res))
#     else return (NULL)
#   }
#
#
# which_not_numbers <-  function (x, names=TRUE){
#     NaN.ind <- numbers.only(levels(x))
#     if (!is.null(NaN.ind)) {
#       if (names) {return (as.character(levels(x)[NaN.ind]))}
#       else return (grep("[^1234567890]", x))
#     }
#     else return(NULL)
#   }
#
#
# check_numeric <-  function(x, col_name) {
#     NaN.values <- which.not.numbers (x)
#     if (!is.null(NaN.values)) {
#       message ("\nApart from numeric values ", col.name," column contains also the following values: \"",
#                paste0(NaN.values, collapse="\", \""), "\". They are coerced to NAs")
#     }
#   }


