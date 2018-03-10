get_subjects <- function (d, form_name="intro", info=NULL,
                          browser_info=FALSE, ...) {

  # Check the input data
  if (is.character(d)==FALSE) stop ("d must be a character")
  if (is.character(form_name)==FALSE) stop ("form_name must be a character")
  if ((is.character(info)==FALSE) &(is.null(info)==FALSE)) stop ("info should be character")

  #---------------------------------------

  # Read the data in. `controller` = "Form"
  # because Forms gather information from participants in IBEX.
  # Also, we don't want the get_result function to print the numbers
  # of columns that were deleted (none are in this call, since del.col=NULL)
  # So we use capture.output() function

  capture.output(form_df <- get_results(d,
                                        controller="Form",
                                        col_names=c("field","answer"),
                                        del_col = NULL,
                                        del_mode = "user",
                                        recode_subjects = FALSE))

  # User may specify the name of Form controller (in case
  # there are several forms in the experiment). Default name
  # in IBEX and here as well is "intro".

  check_missing(x=form_name, string=form_df$type, "Forms")
  form_df <- form_df[form_df$type %in% form_name,]

  if (NROW(form_df)==0) stop ("Subsetting for form names failed")

  # We get the number of participants by dividing the number of
  # entries by number of unique entry types (e.g. age, sex, etc)
  n_subj <- length(form_df$field)/length(unique(form_df$field))

  # Create an empty df to hold subject info
  Subject_Info <- data.frame(matrix(nrow=n_subj, ncol=length(info),
                                    dimnames=list(NULL, info)))

  # Names starting with _ are not valid in R.
  # So R appends X in the beggining. In the IBEX Form controller
  # reaction time is marked as _REACTION_TIME_, so R would "fix it" and
  # create a column named X_REACTION_TIME_. We don't need such a column,
  # and if there is _REACTION_TIME_ in the data, we remove this column.

  # if ("_REACTION_TIME_" %in% info) {Subject_Info$X_REACTION_TIME_ <- NULL} # don't know why we need this condition
  Subject_Info$X_REACTION_TIME_ <- NULL

  # If the user hasn't specified which info he needs, we will gather
  # everything what is contained in Field column.
  if (is.null(info)){
    form_df$field <- factor(form_df$field) # make sure it's a factor
    info <- levels(form_df$field)
  }

  # handle potential duplicate values
  info <- unique(info)

  # Subset by different fields. If a field doesn't exist,
  # the subset will have 0 rows, and this parameter should be
  # excluded from the final table. This is what happens in "if"
  # statement.

  # TODO: Rewrite this with mutate

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


  # If we include browser info, we should look for "USER AGENT" string

  if (browser_info){
    raw_data <- read.csv(file=d, header = FALSE)
    df_Browser_Info <- raw_data[grepl(pattern="# USER AGENT:", x=raw_data[,1]),]
    if (NROW(df_Browser_Info)==0) {
      warning ("Browser info was not found!")
    } else {
      Subject_Info$browser <- paste(df_Browser_Info[,1],df_Browser_Info[,2],sep="")

    }
  }

  # if (is.character(d)) {
  #   d <- read_ibex(d, col_names=c("field","answer"), ...)
  # }
  #
  # if (NROW(d[d[3]=="Form",])==0) stop ("The Form controller isn't found in the data. ",
  #                                      "Possibly, the data doesn't contain subject info")


  # Select unique values of Subject Codes
  Subject_Info$subject_uid <- unique(paste0(form_df[,1], "_", form_df[,2]))
  # Subject_Info$subject <- recode_subjects()

  #if (length(Subject.Info$subject)<len) warning ("Subject codes may be duplicate, check them")
  if (any(duplicated(Subject_Info$subject))) {
    (warning("Subject codes are duplicated! See details below"))
    subject_code_duplicates <- Subject_Info$subject[duplicated(Subject_Info$subject)]
  } else {
      subject_code_duplicates <- NA
  }

  # if (any(duplicated(Subject_Info$Initials))) {
  #   warning("Subject initials are duplicated! Resolving by adding numbers")
  #   subject_initials_duplicates <- Subject_Info$Initials[duplicated(Subject_Info$Initials)]
  #
  #   for (i in unique(subject.initials.duplicates)){
  #     Subject_Info$Initials[Subject_Info$Initials==i] <- paste0(Subject_Info$Initials[Subject_Info$Initials==i],"_",
  #                                                               seq_along(Subject_Info$Initials[Subject_Info$Initials==i]))
  #   }
  # } else {
  #   subject_initials_duplicates <- NA
  # }

  # output some useful info
  cat("Number of subjects: ", n_subj, "\n\n")
  cat("Info extracted:\n - ", paste(colnames(Subject_Info), collapse="\n - "), "\n\n")
  if (!is.na(subject_code_duplicates)) cat("Duplicate subject codes:\n", subject_code_duplicates, "\n\n")
  # if (!is.na(subject_initials_duplicates)) cat("Duplicate subject initials:\n", subject_initials_duplicates, "\n\n")

  return(Subject_Info)

}
