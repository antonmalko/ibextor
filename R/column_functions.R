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

make_column_names <- function(n_cols, col_names = NULL, partial_names = TRUE){

  #' A function to provide default names for columns and ensure that
  #' every column has a name
  #'
  #' @param n_cols numeric. Number of columns in the data
  #' @param col_names character vector with names for the columns.
  #' @param partial_names logical. If \code{TRUE}, the first seven columns will receive
  #'                      default names (\dQuote{subject}, \dQuote{md5_hash}, \dQuote{controller},
  #'                      \dQuote{presentation_order}, \dQuote{element_number},
  #'                      \dQuote{type}, \dQuote{item}),
  #'                      since these columns are the same in all (default)
  #'                      Ibex controllers. So \code{col_names} will be taken to
  #'                      specify names for columns starting from 8.
  #'                      If \code{FALSE}, \code{col_names} will be taken as specifying names
  #'                      for all columns. If there are fewer than 8 columns,
  #'                      \code{partial_names} will always be taken to be \code{FALSE}.
  #' @return vector with column names

  # If there are fewer than 8 columns, then assume that the names provided have
  # to be applied to all columns (instead of applying them starting from the
  # eighth column)
  if (n_cols < 8) {
    partial_names <- FALSE
    warning("There are fewer than 8 columns in your data! Assuming `partial_names = FALSE`",
            " (i.e. default Ibex column names are not used)")
  }

  # If columns names are not specified at all, make dummy names
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
    col_names <-  c("timestamp",
                    "md5_hash",
                    "controller",
                    "presentation_order",
                    "element_number",
                    "type",
                    "item", col_names)
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

make_column_classes <- function(n_cols, col_classes = NULL, partial_classes = TRUE){

  #' A function to provide default names for columns and ensure that
  #' every column has a name
  #' @param n_cols \code{numeric}. Number of columns in the data
  #' @param col_classes \code{character} vector with classes to be assumed for the columns.
  #'   Currently, the following classes are allowed: \dQuote{character}, \dQuote{numeric},
  #'   \dQuote{integer}, \dQuote{logical}, \dQuote{factor}. Additionally, one may
  #'   use \dQuote{asis}, if the class of a column should not be changed. If the class
  #'   for a column is not specified explicitly, \dQuote{asis} will be assumed.
  #'
  #' @param partial_classes \code{logical}. If \code{TRUE}, the first seven columns will receive
  #'                      default classes:
  #'
  #'                      + subject - \code{factor}
  #'                      + md5_hash, controller, type, item - \code{character}
  #'                       (notice that in Ibex items can be identified with character
  #'                       strings, that's why item is not \code{numeric} by default),
  #'                      + presentation_order, element_number - \code{numeric},
  #'
  #'                      So \code{col_names} will be taken to specify classes for columns
  #'                      starting from 8. If \code{FALSE}, \code{col_classes} will be taken
  #'                      as specifying classes for all columns. If there are less
  #'                      than 8 columns, \code{partial classes} will always be taken
  #'                      to be \code{FALSE}.
  #'
  #' @return vector with column classes

  # If there are fewer than 8 columns, then assume that the names provided have
  # to be applied to all columns (instead of applying them starting from the
  # eighth column)
  if (n_cols < 8) {
    partial_classes <- FALSE
    warning("There are fewer than 8 columns in your data! Assuming `partial_classes = FALSE`",
            " (i.e. default Ibex column classes are not used)")
  }

  if (is.null(col_classes)){
    if (partial_classes) {
      col_classes <- rep("asis", times = n_cols - 7)
    } else {
      col_classes <- rep("asis", times = n_cols)
    }
  }

  # input checks
  if (!is.numeric(n_cols)) stop("n_cols should be numeric!")
  if (!is.vector(col_classes)) stop("col_classes should be a vector!")

  # unless we provided a full set of column classes, provide default classes for
  # the first seven columns
  if (partial_classes){
    col_classes <-  c("character",
                      "character",
                      "character",
                      "numeric",
                      "numeric",
                      "character",
                      "factor",
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
    col_classes <- c(col_classes, rep("asis", times = n_cols - length(col_classes)))
  #warning("There are more columns than column classes provided. ",
  #        "The non-specified classes will be chosen automatically")

  return(col_classes)
}

set_column_classes <- function(d, col_classes){
  #' A function to change classes of multiple data.frame columns
  #' @param d data.frame
  #' @param col_classes character vector with classes to be assumed for the columns
  #' @return data.frame with updated column classes

  # recipe taken from
  # https://stackoverflow.com/questions/7680959/convert-type-of-multiple-columns-of-a-dataframe-at-once
  res <- lapply(1:ncol(d), function(i){
    FUN <- switch(col_classes[i],
                  character = as.character,
                  numeric = as.numeric,
                  integer = as.integer,
                  logical = as.logical,
                  factor = as.factor,
                  asis = base::identity)

    # If a name of the class is not reognized, do not change the column
    if (is.null(FUN)){
      warning("Column class name `",col_classes,"` is not recognized. The corresponding ",
              "column class will not be changed")
      FUN <- base::identity
    }

    FUN(d[,i])
  })

  names(res) <- colnames(d)
  res <- as.data.frame(res, stringsAsFactors = FALSE)
}

auto_determine_del_col <- function(d){
  #' Helper function to auto-determine columns to be deleted
  #'
  #' @param data.frame from which columns are to be deleted
  #' @return vector with number of columns to be deleted

  del_col = c(`Auto (md5 hash usually not needed for analysis)` = 2) # remove md5 hash by default

  # The 3rd column is the controller column.
  # If only one controller is specified, we can delete the column. By the time
  # the current function is called, we have already subsetted the main dataset
  # for only the controllers that we want.
  if (length(unique(d[,3]))==1) del_col <- c(del_col,
                                             `Auto (only one controller exists)` = 3)

  # The 5th column is the element number. If there is just one element number,
  # the column is not useful and can be safely deleted
  if (length(unique(d[,5]))==1) del_col <- c(del_col, `Auto (only one elem.number exists)` = 5)

  # The 7-th column in IBEX results is the group number. It is used in Latin
  # Square designs. If it is unused, IBEX returns NULL.  The column in this case
  # is of no use.
  if (all(d[,7] == "NULL")) del_col <- c(del_col, `Auto (contains only NULL values)` = 7)

  # --- Controller specific

  # For "DashedSentence" in "self-paced reading" mode Column 11 contains info
  # on whether there was a line break between the current and the following word.
  # If all sentences in the experiment fit on the same line, this column always
  # contains FALSE and thus is not necessary.
  # Same for "Acceptability Judgment", only we don't need to account for mode
  if (all(d[,3] == "DashedSentence") && ncol(d) >= 12 && all(d[,11] == FALSE)){
    del_col <- c(del_col, `Auto (contains only FALSE values)` = 11)
  }

  if (all(d[,3] == "DashedAcceptabilityJudgment") && ncol(d) >= 12 && all(d[,11] == FALSE)){
    del_col <- c(del_col, `Auto (contains only FALSE values)` = 11)
  }

  # For "Question", "AcceptabilityJudgment" and "DashedAcceptabilityJudgment" in
  # "speeded acceptability" mode, Column 10 contains the info on whether the
  # question was answered correctly. If an answer was not specified in Ibex,
  # this column will always contain NULLs and thus is not necessary

  if (all(d[,3] == "Question" ) && all(is.na(d[,10]))){
    del_col <- c(del_col, `Auto (contains only NULL values)` = 10)
  }

  if (all(d[,3] == "AcceptabilityJudgment" ) && all(is.na(d[,10]))){
    del_col <- c(del_col, `Auto (contains only NULL values)` = 10)
  }

  if (all(d[,3] == "DashedAcceptabilityJudgment" ) && all(is.na(d[,10]))){
    del_col <- c(del_col, `Auto (contains only NULL values)` = 10)
  }

  return(del_col)
}

report_del_col <- function(del_col, d_colnames){
  #' Helper function to report which columns were deleted
  #'
  #' @param del_col Vector of column numbers to be deleted.
  #' @param d_colnames Vector of columns names (used to report more informatively
  #'                   which columns were deleted)
  #' @return data.frame with deleted columns info
  #'

  if (is.null(del_col)){
    return(NULL)
  }

  # create empty data.frame for info about deleted columns
  del_col_info <- data.frame(matrix(ncol=3, nrow=length(del_col),
                                    dimnames=list(NULL, c("Index",
                                                          "Name",
                                                          "Reason"))))

  del_col_info[1] <- del_col
  del_col_info[2] <- d_colnames[del_col]
  del_col_info[3] <- names(del_col)
#
#   # these columns appear in the results for all controllers
#   del_col_info[(del_col_info$Index==2), 3] <- "Auto (md5 hash usually not needed for analysis)"
#   del_col_info[(del_col_info$Index==3), 3] <- "Auto (only one controller exists)"
#   del_col_info[(del_col_info$Index==5), 3] <- "Auto (only one elem.number exists)"
#   del_col_info[(del_col_info$Index==7), 3] <- "Auto (contains only NULL values)"
#
#   # these ones appear only for "double" controllers such as
#   # DashedAcceptabilityJudgment
#   del_col_info[(del_col_info$Index==10), 3] <- "Auto (contains only NULL values)"
#   del_col_info[(del_col_info$Index==15), 3] <- "Auto (contains only NULL values)"
#   del_col_info[(del_col_info$Index==11), 3] <- "Auto (contains only FALSE values)"
#
#   # Make note of which columns were requested to be deleted by the user - they
#   # will have "user" as the name of the element in the vector
#   del_col_info[del_col_info$Index %in% del_col[names(del_col) == "user"], 3] <-  "User request"

  return(del_col_info)

}

delete_columns <- function(d, del_col = NULL, del_mode = c("auto", "user", "mixed"),
                           verbose = TRUE){
  #'A function to delete columns from the data based on data and/or user
  #'requests.
  #'
  #'@param d data.frame the columns of which are to be deleted
  #'@param del_col numeric. Vector of column numbers to be deleted.
  #'@param del_mode how to determine which columns have to be deleted. Can be
  #'  one of: \dQuote{auto} (decide based on data), \dQuote{user} (only take into account
  #'  user's requests), \dQuote{mixed} (take into account user request and data).
  #'  \dQuote{auto} is used by default if "del_col" is not specified; if it is, \dQuote{mixed}
  #'  is used. See Details
  #'@param verbose logical. If \code{TRUE} (default), report which columns were
  #'  deleted and why.
  #'@return data.frame `d` with deleted columns
  #'
  #'@details
  #'
  #'The first seven columns in IBEX results always are: \enumerate{ \item Time
  #'the results are received \item MD5 hash of participant's IP \item Controller
  #'type \item Item number \item Element number \item Type \item Group } By
  #'default, the deletion mode is \dQuote{auto}. It means that the function will remove
  #'columns not containing useful information. This means:
  #'
  #'\itemize{ \item "md5 hash" column is always removed since it's not usually
  #'used in analyses \item "controller" column is removed if there is just one
  #'controller in the data - it is assumed, the user will know what controller
  #'that is \item "element number" column is removed if that column only
  #'contains zeros \item "group" column is removed if it only contains `NULL`
  #'values }
  #'
  #'The above applies to all controllers. In addition:
  #'
  #'\itemize{ \item for DashedSentence and DashedAccpetabilityJudgment column 11
  #'("line break") is removed if it only contains \code{FALSE} values. \item For
  #'DashedAcceptabilityJudgment only column 15 ("is.correct") is removed if it
  #'contains only \code{NULL} valuess (if this is the case, it means that correct
  #'answers were not specified in IBEX, so this column is of no use). }
  #'
  #'In \dQuote{user} deletion mode the data will not be taken into account at all, and
  #'only the columns specified by user will be deleted.
  #'
  #'In \dQuote{mixed} deletion mode the columns to delete are the union of the sets of
  #'automatically deleted columns and user-specified columns.

  # by default, choose auto deletion
  del_mode <- match.arg(del_mode)

  # if some columns are specified for deletion, assume that auto deletion
  # should still ap[ply to unsepcified columns.
  if (!is.null(del_col) & del_mode == "auto") del_mode <- "mixed"

  base::switch(del_mode,
               auto = {
                 del_col <- auto_determine_del_col(d)
               }, # select columns for deletions based on the data

               user = {
                 if (!is.null(del_col)) {
                   del_col <- sort(unique(del_col)) # use columns provided by the user
                   names(del_col) <- rep("User request", times = length(del_col)) # and remember that they were provided by the user
                 }
               },

               mixed = {
                 del_col <- unique(del_col)
                 names(del_col) <- rep("User request", times = length(del_col))
                 del_col_auto <- auto_determine_del_col(d)
                 del_col <- sort(c(del_col, del_col_auto[!del_col_auto %in% del_col]))
               })

  del_col_info <- report_del_col(del_col, colnames(d))

  # delete columns
  d[del_col] <- list(NULL)

  # report deleted columns
  if (verbose){
    if (length(del_col) > 0){
      cat ("\n", NROW(del_col_info)," column(s) deleted:\n\n")
      print(del_col_info, right=FALSE, row.names=FALSE)
      cat("\n")
    } else {
      cat ("\nNo columns were deleted\n")
    }
  }

  return(d)
}
