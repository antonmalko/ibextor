
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


