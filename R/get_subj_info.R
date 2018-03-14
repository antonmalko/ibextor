get_subj_info <- function (file_name,
                          form_name="intro",
                          info=NULL,
                          browser_info=FALSE,
                          short_subj_ids = TRUE,
                          verbose = TRUE,
                          ...) {

  #' Extract subjects info from Ibex data
  #'
  #' @param file_name character. Path to the file with Ibex results
  #' @param form_name character. Useful if you have several Forms collecting
  #'   different info throughout the experiment. In this case form.name should
  #'   specify which of those Forms collected subject info you're interested in.
  #'   By default (in IBEX and here) it's "intro". You can also pass a vector
  #'   with several form names.
  #' @param info character vector, describing which info about subjects you want
  #'   to extract (i.e. the names of the fields you specified in IBEX, e.g.
  #'   "age", "sex" etc). If `NULL` (deault), all the available info will be
  #'   extracted.
  #' @param browser_info logical. Whether to extract subject's browser info or not
  #'   (defaults to `FALSE`).
  #' @param short_subj_ids logical. If `FALSE`, just merge timestamp and md5 hash
  #'   together, If `TRUE` (default), add a new column with a simple index to
  #'   idenitfy subjects. One index is assigned to each combination of timestamp
  #'   and browser md5 hash. The column is put at the place of `subj_uid`
  #'   column, and `subj_uid` column is moved at the very end of the data.frame.
  #' @param verbose logical. If `TRUE`, print the number of subjects and the kinds
  #'   of information extracted.
  #' @return data.frame with subject info
  #' @export

  # Check the input data
  if (is.character(file_name)==FALSE) stop ("file_name must be a character")
  if (is.character(form_name)==FALSE) stop ("form_name must be a character")
  if ((is.character(info)==FALSE) &(is.null(info)==FALSE)) stop ("info should be character")

  #---------------------------------------

  form_df <- read_ibex(file_name, ...)
  form_df <- subset_ibex(form_df, controller = "Form")

  form_df <- form_df[,1:9]

  form_df <- format_ibex(form_df,
                     col_names = c("field","answer"), partial_names = TRUE,
                     col_classes = c("character", "character"), partial_classes = TRUE)

  form_df <- recode_subjects(form_df, short_ids = short_subj_ids)

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
  subj_info <- data.frame(matrix(nrow=n_subj, ncol=length(info),
                                    dimnames=list(NULL, info)))

  # Names starting with _ are not valid in R.
  # So R appends X in the beggining. In the IBEX Form controller
  # reaction time is marked as _REACTION_TIME_, so R would "fix it" and
  # create a column named X_REACTION_TIME_. We don't need such a column,
  # and if there is _REACTION_TIME_ in the data, we remove this column.

  # if ("_REACTION_TIME_" %in% info) {subj_info$X_REACTION_TIME_ <- NULL} # don't know why we need this condition
  # subj_info$X_REACTION_TIME_ <- NULL

  # If the user hasn't specified which info he needs, we will gather
  # everything what is contained in Field column.
  if (is.null(info)){
    info <- unique(form_df$field)
  }

  # Subset by different fields. If a field doesn't exist,
  # the subset will have 0 rows, and this parameter should be
  # excluded from the final table. This is what happens in "if"
  # statement.

  for (i in 1:length(info))
  {
    if (NROW(form_df[form_df$field== info[i], ]) == 0){
      message ("Parameter \"", info[i], "\" wasn't found in the data.")
      subj_info[info[i]] <- NULL
      next
    }
    temp <- form_df[form_df$field==info[i],]
    subj_info[info[i]] <- as.vector(temp$answer)

  }

  # If we include browser info, we should look for "USER AGENT" string

  if (browser_info){
    raw_data <- read.csv(file=d, header = FALSE)
    df_Browser_Info <- raw_data[grepl(pattern="# USER AGENT:", x=raw_data[,1]),]
    if (NROW(df_Browser_Info)==0) {
      warning ("Browser info was not found!")
    } else {
      subj_info$browser <- paste(df_Browser_Info[,1],df_Browser_Info[,2],sep="")
    }
  }


  # output some useful info
  if (verbose){
    cat("Number of subjects: ", n_subj, "\n\n")
    cat("Info extracted:\n - ", paste(colnames(subj_info), collapse="\n - "), "\n\n")
    #if (!is.na(subject_code_duplicates)) cat("Duplicate subject codes:\n", subject_code_duplicates, "\n\n")
    # if (!is.na(subject_initials_duplicates)) cat("Duplicate subject initials:\n", subject_initials_duplicates, "\n\n")
  }

  return(subj_info)

}
