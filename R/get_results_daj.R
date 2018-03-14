get_results_daj <- function(file_name,
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
  res <- subset_ibex(res, controller = "DashedAcceptabilityJudgment",
                     elem_number = elem_number)

  # --------- 1. Speeded acceptability mode ----------------------
  if (sprt){

    if (is.null(col_names)){
      col_names <- c("region",
                     "word",
                     "rt",
                     "line_break",
                     "sentence",
                     "question",
                     "answer",
                     "is_correct",
                     "rt")
    }

    if (is.null(col_classes)){
      col_classes <- c("factor",
                       "character",
                       "numeric",
                       "logical",
                       "character",
                       "character",
                       "character",
                       "numeric",
                       "numeric")
    }

    if (any(is.na(as.numeric(res[[8]])))){
      stop("Column 8 contains non-numeric data, which it shouldn't. It is possible ",
           "that you are subsetting for the wrong controller, or that the mode of ",
           "DashedSentence controller was set to `acceptability judgment` during ",
           "data collection, while you are requesting `self-paced reading` (sprt = TRUE)")
    }

    # Subset for sentences
    res_sent <- res[res[,12]!="",  ]
    if (NROW(res_sent)==0) stop ("Subsetting for sentence data failed")
    rownames(res_sent) <- NULL
    res_sent <- droplevels(res_sent)

    # Subset for Questions. Get only the relevant columns, i.e 8-11
    res_quest <- res[res[,12]=="", c(4, 8:11)]
    if (NROW (res_quest)==0) stop ("Failed to get questions data")
    rownames(res_quest) <- NULL

    # merge on the 4th column, which is item number
    res <- merge(res_sent, res_quest, by = 4, sort = FALSE)

    res <- format_ibex(res,
                       col_names = col_names, partial_names = partial_names,
                       col_classes = col_classes, partial_classes = partial_classes)
    res <- recode_subjects(res, short_ids = short_subj_ids)
    res <- delete_columns(res, del_col, del_mode)
    return(res)


    } else {

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
                       "logical",
                       "numeric",
                       "character")
    }


    # if (!missing(sep.quest)) message ("Sep.quest parameter is not used if sprt=FALSE(default)\n")
    if (NCOL(res) > 11){
      stop("There are more than 11 columns in the data. It is possible ",
           "that you are subsetting for the wrong controller, or that the mode of ",
           "DashedSentence controller was set to `self-paced reading` during ",
           "data collection, while you are requesting `acceptability judgment` (sprt = FALSE)")
    }

    res <- res[,1:11]

    #------------ read the data ------------------------

    # read odd lines with the info about Sentences. Here we read only the
    # last column, which has number 8
    res_sent <- res[(seq(1, NROW(res), by=2)), 8 ]
    if (NROW(res_sent)==0) stop ("Subsetting for sentence data failed")


    # read even lines, which contain info about questions.
    # we read all the columns, so almost all the information is already extracted
    res <- res[(seq(2, NROW(res), by=2)),  ]
    if (NROW(res)==0) stop ("Subsetting for questions data failed")
    rownames(res) <- NULL

    # add column with question info after immediately after all the other info,
    res[,12] <- res_sent
    droplevels(res)

    res <- format_ibex(res,
                       col_names = col_names, partial_names = partial_names,
                       col_classes = col_classes, partial_classes = partial_classes)
    res <- recode_subjects(res, short_ids = short_subj_ids)
    res <- delete_columns(res, del_col, del_mode)

    return (res)
    }
}

