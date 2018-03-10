get_results_ds <- function(file_name,
                          elem_number = NULL,
                          del_col = NULL, del_mode = "auto",
                          col_names = NULL,
                          partial_names = TRUE,
                          col_classes = NULL, partial_classes = TRUE,
                          short_subj_ids = TRUE,
                          sprt = TRUE,
                          ...) {

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

