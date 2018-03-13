get_results_aj <- function(file_name,
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

# if(!is.character(d)&!is.data.frame(d)) stop ("d must be either character or data.frame")

#------------- Read the data  -----------------

    # if (is.character(d)) {d <- read.ibex(d, col.names)
    # } else {
    #   # check whether the data.frame has supposed number of columns
    #   if (NCOL(d)<11) stop ("Acceptability Judgment controller creates 11 columns,\n",
    #                         "but the number of columns in the data.frame is less than 11.\n",
    #                         "Check whether the data was read into R completely")
    # }

# # ensuring correct types of data in the columns
# check.numeric(d[[11]], col.names[4])
# d[[11]] <- as.numeric(as.character(d[[11]])) # rt should be numeric
#
#
#   #------------- Get the relevant data ------------------------
#     res.raw <- d[d[3]=="AcceptabilityJudgment",]
#     if (NROW(res.raw)==0) stop ("Subsetting for AcceptabiltyJudgment controller failed.\n")
#
#     # Reading odd (first line of code) and even (second line) raws.
#     # They contain different types of info.
#     res.FlashSentence <- res.raw[(seq(1, NROW(res.raw), by=2)), 8 ]
#     if (NROW(res.FlashSentence)==0) stop ("Subsetting for sentences data failed")
#     res <- res.raw[(seq(2, nrow(res.raw), by=2)),  ]
#     if(NROW(res)==0) stop ("Subsetting for questions data failed")
#
#     rownames(res) <- NULL
#
#     #add FlashSentence info into the 12th column, after the main data
#     res[,12] <- res.FlashSentence
#     droplevels(res)
#
#     #Check whether Is.correct column can be deleted
#     if (NROW(unique(res[10]))==1) if (res[1,10]=="NULL") del.col <- c(del.col, 10)
#
#     res <- get.results(res, controller="AcceptabilityJudgment",
#                        elem.number, del.col, col.names, list, save.to)
#
#     return (res)
#
#   }
