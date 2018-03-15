get_results_ds <- function(file_name,
                          elem_number = NULL,
                          del_col = NULL, del_mode = "auto",
                          col_names = NULL,
                          partial_names = TRUE,
                          col_classes = NULL, partial_classes = TRUE,
                          short_subj_ids = TRUE,
                          sprt = TRUE,
                          ...) {

  #' @rdname get_results
  #' @export

  res <- read_ibex(file_name, ...)
  res <- subset_ibex(res, controller = "DashedSentence", elem_number = elem_number)

  sprt_lines <- detect_sprt_lines(res)

  if (all(sprt_lines) == TRUE & !sprt){ # if the auto detection says it sprt, but the user requested speeded acceptability
    stop("Column 10 doesn't contain numeric data or there are more than 11 columns in the data.
         It is possible that you are subsetting for the wrong controller, or that the mode of ",
         "DashedSentence controller was set to `self-paced reading` during ",
         "data collection, while you are requesting `acceptability judgment` (sprt = FALSE)")
  } else {
    if (!any(sprt_lines) == TRUE & sprt) { # if the auto detection says it speeded acceptability, but the user requested sprt
      stop("Column 8 contains non-numeric data, which it shouldn't. It is possible ",
           "that you are subsetting for the wrong controller, or that the mode of ",
           "DashedSentence controller was set to `acceptability judgment` during ",
           "data collection, while you are requesting `self-paced reading` (sprt = TRUE)")
    } else { # if the data is a mixture of both, only pick one subset
      warning("The data seems to contain a mixture of sentences in `self-paced reading` and ",
              "`speeded acceptability` mode. Will only choose those agreeing with `sprt` parameter ",
              "(`self-paced` for sprt = TRUE and `speeded acceptability` for sprt = FALSE)")
    }
  }

  if (sprt){ # if the mode is "self-paced reading"

    # if the mode is sprt, just extract sprt lines
    res <- res[sprt_lines,]

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

    res <- format_ibex(res,
                       col_names = col_names, partial_names = partial_names,
                       col_classes = col_classes, partial_classes = partial_classes)

    res <- recode_subjects(res, short_ids = short_subj_ids)
    res <- delete_columns(res, del_col, del_mode)

  } else { # if the mode is "speeded acceptability"

    sa_lines <- detect_sa_lines(res)
    res <- res[sa_lines,]

    if (is.null(col_names)){
      col_names <- c("sentence")
    }

    if (is.null(col_classes)){
      col_classes <- c("character")
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

