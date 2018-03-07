get_subjects <- function (d, form_name="intro", info=NULL,
                          browser_info=FALSE) {

  # Check the input data
  if ((is.data.frame(d)==FALSE)&(is.character(d)==FALSE)) stop ("d must be either a data.frame or a character")
  if (is.character(form_name)==FALSE) stop ("form.name must be a character")
  if ((is.character(info)==FALSE) &(is.null(info)==FALSE)) stop ("info should be character")
  #---------------------------------------

  # handle duplicate values in the input
  info <- unique(info)

  # d may be either a string or a data.frame. If it is a string,
  # it should specify the filepath. Also, only if it's a string
  # we are going to read participant's browser info (I doubt that
  # somebody would read data.file with comment lines, so...)
  #
  # Ok, what we do here is searching for USER AGENT string,
  # which indicates the browser info.

  if (browser_info){
    raw_data <- read.csv(file=d, header = FALSE)
    df_Browser_Info <- raw_data[grepl(pattern="# USER AGENT:", x=raw_data[,1]),]
    if (NROW(df_Browser_Info)==0) warning ("Browser info was not found!")
    else  df_Browser_Info[,1] <- paste(df_Browser_Info[,1],df_Browser_Info[,2],sep="")
  }

  if (is.character(d)) {
    d <- read.ibex(d, col.names=c("field","answer"))
  }

  if (NROW(d[d[3]=="Form",])==0) stop ("The Form controller isn't found in the data. ",
                                       "Possibly, the data doesn't contain subject info")


  # Select info by controller type, which is Form here,
  # because Forms gather information from participants in IBEX.
  # Also, we don't want the get_result function to print the numbers
  # of columns that were deleted (none are in this call, since del.col=NULL)
  # So we use capture.output() function

  capture.output(form_df <- get_results(d,
                                        controller="Form",
                                        col_names=c("field","answer")))

  # User may specify the name of Form controller (in case
  # there are several forms in the experiment). Default name
  # in IBEX and here as well is "intro".
  # If it is not specified at all,we subset only forms with this name.

  check_missing(x=form_name, string=form_df$type, "Forms")
  form_df <- form_df[form_df$type %in% form_name,]

  if (NROW(form_df)==0) stop ("Subsetting for form names failed")

  # We get the number of participants by dividing the number of
  # entries by number of unique entry types (e.g. age, sex, etc)
    len <- length(form_df$field)/length(unique(form_df$field))

  # If the user hasn't specified which info he needs, we gather
  # everything what is contained in Field column.
  if (is.null(info)){
    info <- levels(form_df$field)
  }


  Subject_Info <- data.frame(matrix(nrow=len, ncol=length(info),
                                    dimnames=list(NULL, info)))

  # Names starting with _ are not valid in R.
  # So R appends X in the beggining. In the IBEX Form controller
  # reaction time is marked as _REACTION_TIME_, so R would "fix it" and
  # create a column named X_REACTION_TIME_. We don't need such a column,
  # and if there is _REACTION_TIME_ in the data, we remove this column.

  if ("_REACTION_TIME_" %in% info) {Subject_Info$X_REACTION_TIME_ <- NULL}

  # Subset by different fields. If a field doesn't exist,
  # the subset will have 0 rows, and this parameter should be
  # excluded from the final table. This is what happens in "if"
  # statement.

  for (i in 1:length(info))
  {
    if (NROW(form_df[form_df$field== info[i], ]) == 0){
      message ("Parameter \"", info[i], "\" wasn't found in the data.")
      Subject_Info[info[i]] <- NULL
      next
    }
    temp <- form_df[form_df$field==info[i],]
    Subject_Info[info[i]] <- as.vector(temp$answer)

  }

  # Select unique values of Subject Codes
  Subject_Info$subject <- unique(paste0(form.df[,1], "_", form.df[,2]))

  #if (length(Subject.Info$subject)<len) warning ("Subject codes may be duplicate, check them")
  if (any(duplicated(Subject_Info$subject))) {
    (warning("Subject codes are duplicated! See details below"))
    subject_code_duplicates <- Subject_Info$subject[duplicated(Subject_Info$subject)]
  } else {
      subject_code_duplicates <- NA
  }

  if (any(duplicated(Subject_Info$Initials))) {
    warning("Subject initials are duplicated! Resolving by adding numbers")
    subject_initials_duplicates <- Subject_Info$Initials[duplicated(Subject_Info$Initials)]

    for (i in unique(subject.initials.duplicates)){
      Subject_Info$Initials[Subject_Info$Initials==i] <- paste0(Subject_Info$Initials[Subject_Info$Initials==i],"_",
                                                                seq_along(Subject_Info$Initials[Subject_Info$Initials==i]))
    }
  } else {
    subject_initials_duplicates <- NA
  }




  # if Browser Info was extracted, we add it.
  if (browser_info) if (NROW(df_Browser.Info)!=0) {
    Subject_Info$browser.info <- df_Browser_Info[,1]
  }

  # output some useful info
  cat("Number of subjects: ", len, "\n\n")
  cat("Info extracted:\n - ", paste(colnames(Subject_Info), collapse="\n - "), "\n\n")
  if (!is.na(subject_code_duplicates)) cat("Duplicate subject codes:\n", subject_code_duplicates, "\n\n")
  if (!is.na(subject_initials_duplicates)) cat("Duplicate subject initials:\n", subject_initials_duplicates, "\n\n")

  # saving the results to file, if asked
  # if (save.to != "") {write.csv(Subject.Info, file=paste(save.to,".csv",sep=""), row.names=FALSE)}

  return(Subject_Info)

}
