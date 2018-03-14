get_results_aj <- function(file_name,
                           elem_number = NULL,
                           del_col = NULL, del_mode = "auto",
                           col_names = NULL, partial_names = TRUE,
                           col_classes = NULL, partial_classes = TRUE,
                           short_subj_ids = TRUE,
                           ...) {

  #' @rdname get_results
  #' @export

    if (is.null(col_names)){
      col_names <- c("question",
                     "answer",
                     "is_correct",
                     "rt",
                     "sentence")
    }

    if (is.null(col_classes)){
      col_classes <- c("character",
                       "character",
                       "numeric",
                       "numeric",
                       "character")
    }

  res <- read_ibex(file_name, ...)
  res <- subset_ibex(res, controller = "AcceptabilityJudgment", elem_number = elem_number)

  res <- res[, 1:11]

  # Reading odd (first line of code) and even (second line) raws.
  # They contain different types of info.
  res_FlashSentence <- res[(seq(1, NROW(res), by=2)), 8 ]
  if (NROW(res_FlashSentence)==0) stop ("Subsetting for sentences data failed")
  res <- res[(seq(2, nrow(res), by=2)),  ]
  if(NROW(res)==0) stop ("Subsetting for questions data failed")

  rownames(res) <- NULL

  #add FlashSentence info into the 12th column, after the main data
  res[,12] <- res_FlashSentence
  droplevels(res)

  res <- format_ibex(res,
                     col_names = col_names, partial_names = partial_names,
                     col_classes = col_classes, partial_classes = partial_classes)
  res <- recode_subjects(res, short_ids = short_subj_ids)
  res <- delete_columns(res, del_col, del_mode)
  return(res)
}

