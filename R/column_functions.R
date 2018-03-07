count_columns <- function(file_name){

  #' A function to count number of columns in a file.
  #' @param file_name character. Name fo the file where the columns should be counted
  #' @return numeric. Number of columns. If an error arises, assumes that there
  #'         are 12 columns in the file.

  # input checks
  if (!is.character(file_name)) stop("file_name should be character!")

  n_cols <- max(count.fields(file_name, sep=",",comment.char="#", quote = "\""), na.rm = TRUE)
  if (is.na(n_cols)) {
    n_cols <- 12
    warning ("Error while counting columns in the file. Assuming there are 12 columns")
  }
  return(n_cols)
}

set_column_names <- function(n_cols, col_names = NULL, partial_names = TRUE){

  #' A function to provide default names for columns and ensure that
  #' every column has a name
  #' @param n_cols numeric. Number of columns in the data
  #' @param col_names character vector with names for the columns
  #' @param partial_names logical. If `TRUE`, the first seven columns will receive
  #'                      default names (`subject`, `md5_hash`, `controller`,
  #'                      `item_number`, `element_number`, `type`, `group`),
  #'                      since these columns are the same in all (default)
  #'                      Ibex controllers. So `col_names` will be taken to
  #'                      specify names for columns starting from 8.
  #'                      If `FALSE`, `col_names` will be taken as specifying names
  #'                      for all columns
  #' @return vector with column names

  if (is.null(col_names)){
    if (partial_names) {
      col_names <- c (paste0("Col",seq(from=8, to=n_cols)))
    } else {
      col_names <- c (paste0("Col",seq(from=1, to=n_cols)))
    }
  }

  # input checks
  if (!is.numeric(n_cols)) stop("n_cols should be numeric!")
  if (!is.character(col_names)) stop("col_names should be a character vector!")
  if (!is.vector(col_names)) stop("col_names should be a character vector!")

  # unless we provided a full set of column names, provide default names for
  # the first seven columns - they are the same in all default Ibex controllers
  if (partial_names){
    col_names <-  c("subject",
                    "md5_hash",
                    "controller",
                    "item_number",
                    "element_number",
                    "type",
                    "group", col_names)
  }

  # If there are more column names specified than there are columns in the data,
  # reject extra column names with a warning
  if (n_cols < length(col_names)) {
    col_names <- col_names[1:n_cols]
    # n_cols <- length(col_names)
    warning("There are fewer columns than column names provided. ",
            "Dropping extra column names")
  }

  # If there are fewer column names specified than there are columns in the data,
  # add dummy column names with a warning
  if (n_cols>length(col_names)){
    col_names <- c (col_names, paste0("Col",seq(from=length(col_names)+1, to=n_cols)))
    warning("There are more columns than column names provided. ",
            "Adding dummy column names")
  }

  return(col_names)

}

set_column_classes <- function(n_cols, col_classes = NULL, partial_classes = TRUE){

  #' A function to provide default names for columns and ensure that
  #' every column has a name
  #' @param n_cols numeric. Number of columns in the data
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
  #' @return vector with column classes

  if (is.null(col_classes)){
    if (partial_classes) {
      col_classes <- rep(NA, times = n_cols - 7)
    } else {
      col_classes <- rep(NA, times = n_cols)
    }
  }

  # input checks
  if (!is.numeric(n_cols)) stop("n_cols should be numeric!")
  if (!is.vector(col_classes)) stop("col_classes should be a vector!")

  # unless we provided a full set of column classes, provide default classes for
  # the first seven columns
  if (partial_classes){
    col_classes <-  c("factor", "character", "character",
                    "numeric","numeric", "character","character",
                    col_classes)
  }

  # If there are more column names specified than there are columns in the data,
  # reject extra column classes
  if (n_cols < length(col_classes)) {
    col_classes <- col_classes[1:n_cols]

    # do not throw a warning

    #warning("There are fewer columns than column classes provided. ",
    #        "Dropping extra column classes")
  }

  # If there are fewer column classes specified than there are columns in the data,
  # allow classes to be selected automatically by downstream functions
  if (n_cols > length(col_classes))
    col_classes <- c(col_classes, rep(NA, times = n_cols - length(col_classes)))
  #warning("There are more columns than column classes provided. ",
  #        "The non-specified classes will be chosen automatically")

  return(col_classes)
}

auto_determine_del_col <- function(d){
  #' Helper function to auto-determine columns to be deleted
  #'
  #' @param data.frame from which columns are to be deleted
  #' @return vector with number of columns to be deleted

  del_col = c(auto = 2) # remove md5 hash by default

  # The 3rd column is the controller column.
  # If only one controller is specified, we can delete the column. By the time
  # the current function is called, we have already subsetted the main dataset
  # for only the controllers that we want.
  if (length(unique(d[,3]))==1) del_col <- c(del_col, auto = 3)

  # The 5th column is the element number. If there is just one element number,
  # the column is not useful and can be safely deleted
  if (length(unique(d[,5]))==1) del_col <- c(del_col, auto = 5)

  # The 7-th column in IBEX results is the group number. It is used in Latin
  # Square designs. If it is unused, IBEX returns NULL.  The column in this case
  # is of no use.
  if (all(d[,7] == "NULL")) del_col <- c(del_col, auto = 7)

  return(del_col)
}

report_del_col <- function(del_col, d_colnames){
  #' Helper function to report which columns were deleted
  #'
  #' @param del_col Vector of column numbers to be deleted.
  #' @param d_colnames Vector of columns names (used to report more informatively
  #'                   which columns were deleted)
  #' @return data.frame with deleted columns info

  # create empty data.frame for info about deleted columns
  del_col_info <- data.frame(matrix(ncol=3, nrow=length(del_col),
                                    dimnames=list(NULL, c("Index",
                                                          "Name",
                                                          "Reason"))))

  del_col_info[1] <- del_col
  del_col_info[2] <- d_colnames[del_col]

  # these columns appear in the results for all controllers
  del_col_info[(del_col_info$Index==2), 3] <- "Auto (md5 hash usually not needed for analysis)"
  del_col_info[(del_col_info$Index==3), 3] <- "Auto (only one controller exists)"
  del_col_info[(del_col_info$Index==5), 3] <- "Auto (only one elem.number exists)"
  del_col_info[(del_col_info$Index==7), 3] <- "Auto (contains only NULL values)"

  # these ones appear only for "double" controllers such as
  # DashedAcceptabilityJudgment
  del_col_info[(del_col_info$Index==10), 3] <- "Auto (contains only NULL values)"
  del_col_info[(del_col_info$Index==15), 3] <- "Auto (contains only NULL values)"
  del_col_info[(del_col_info$Index==11), 3] <- "Auto (contains only FALSE values)"

  # Make note of which columns were requested to be deleted by the user - they
  # will have "user" as the name of the element in the vector
  del_col_info[del_col_info$Index %in% del_col[names(del_col) == "user"], 3] <-  "User request"

  return(del_col_info)

}

delete_columns <- function(d, del_col, del_mode = c("auto", "user", "mixed")){
  #' A function to delete columns from the data based on data and/or user requests.
  #'
  #' @param d data.frame the columns of which are to be deleted
  #' @param del_col numeric. Vector of column numbers to be deleted.
  #' @param del_mode how to determine which columns have to be deleted. Can be one of:
  #'          `auto` (decide based on data), `user` (only take into account
  #'          user's requests), `mixed` (take into account user request and data).
  #'          `auto` is used by defualt if "del_col" is not specified; if it is,
  #'          `mixed` is used. See Details
  #' @return data.frame `d` with deleted columns
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

  # by default, choose auto deletion
  del_mode <- match.arg(del_mode)

  # if some columns are specified for deletion, assume that auto deletion
  # should still ap[ply to unsepcified columns.
  if (!missing(del_col) & del_mode == "auto") del_mode <- "mixed"

  base::switch(del_mode,
               auto = {del_col <- auto_determine_del_col(d)}, # select columns for deletions based on the data
               user = {del_col <- sort(unique(del_col)) # use columns provided by the user
               names(del_col) <- rep("user", times = length(del_col))}, # and remember that they were provided by the user
               mixed = {del_col <- unique(del_col)
               names(del_col) <- rep("user", times = length(del_col))
               del_col_auto <- auto_determine_del_col(d)
               del_col <- sort(c(del_col, del_col_auto[!del_col_auto %in% del_col]))})

  del_col_info <- report_del_col(del_col, colnames(d))

  # delete columns
  d[del_col] <- list(NULL)

  # report deleted columns
  if (length(del_col) > 0){
    cat ("\n", NROW(del_col_info)," column(s) deleted:\n\n")
    print(del_col_info, right=FALSE, row.names=FALSE)
    cat("\n")
  } else {
    cat ("\nNo columns were deleted\n")
  }

  return(d)
}
