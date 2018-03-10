
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

# recode_subjects <- function(d, timestamp_col = "timestamp", md5_col = "md5_hash"){
#
#   if (!timestamp_col %in% colnames(d)) stop ("timestamp column not found in the data!")
#   if (!md5_col %in% colnames(d)) stop ("md5_hash column not found in the data!")
#
#   subject_dic <- data.frame(subj_uid = factor(sort(paste0(d[[timestamp_col]], "_", d[[md5_col]]))))
#
#   # if (anyDuplicated(subject_dic)){
#   #   subject_uid_duplicates <- subject_dic$subj[duplicated(subject_dic$subj)]
#   #   stop("Subjects UIDs are not unique! The following UIDs are duplicated: ", subject_uid_duplicates)
#   # }
#
#   subject_dic$subj <- as.numeric(subject_dic$subj_uid)
#
#   return(subject_dic)
# }
