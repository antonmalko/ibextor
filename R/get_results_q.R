get_results_q <- function(file_name,
                          elem_number = NULL,
                          del_col = NULL, del_mode = "auto",
                          col_names = NULL, partial_names = TRUE,
                          col_classes = NULL, partial_classes = TRUE,
                          short_subj_ids = TRUE,
                          ...) {

  if (is.null(col_names)){
    col_names <- c("question",
                   "answer",
                   "is.correct",
                   "rt")
  }

  if (is.null(col_classes)){
    col_classes <- c("character",
                     "character",
                     "numeric",
                     "numeric")
  }

  res <- read_ibex(file_name, ...)
  res <- subset_ibex(res, controller = "Question", elem_number = elem_number)

  res <- res[, 1:11]

  res <- format_ibex(res,
                     col_names = col_names, partial_names = partial_names,
                     col_classes = col_classes, partial_classes = partial_classes)
  res <- recode_subjects(res, short_ids = short_subj_ids)
  res <- delete_columns(res, del_col, del_mode)
  return(res)
}
