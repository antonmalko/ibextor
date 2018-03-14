#'Get results from an Ibex controller
#'
#'
#'The functions are all called \code{get_results_X}, where X corresponds to an
#'abbreviation of an Ibex controller name: \itemize{ \item get_results_aj -
#'AcceptabilityJudgment \item get_results_daj - DashedAcceptabilityJudgment
#'\item get_results_ds - DashedSentence \item get_results_q - Question \item
#'get_results_fs - FlashSentence }
#'
#'
#'@param file_name character. Path to the file with Ibex results
#'@param elem_number Element number in IBEX. Only useful when a) You specified
#'  multiple controllers within one item and b) Some of those controllers have
#'  the same type. In this case you can specify which exactly of those duplicate
#'  controllers you need.
#'@param del_mode how to determine which columns have to be deleted. Can be one
#'  of: \dQuote{auto} (decide based on data), \dQuote{user} (only take into
#'  account user's requests), \dQuote{mixed} (take into account user request and
#'  data). `auto` is used by default if \code{del_col} is not specified; if it
#'  is, \dQuote{mixed} is used. See Details
#'@param col_names character vector with names for the columns
#'@param partial_names logical. If \code{TRUE}, the first seven columns will
#'  receive default names (\dQuote{timestamp}, \dQuote{md5_hash},
#'  \dQuote{controller}, \dQuote{presentation_order}, \dQuote{element_number},
#'  \dQuote{type}, \dQuote{item}), since these columns are the same in all
#'  (default) Ibex controllers. So \code{col_names} will be taken to specify
#'  names for columns starting from 8. If \code{FALSE}, \code{col_names} will be
#'  taken as specifying names for all columns
#'@param col_classes character vector with classes to be assumed for the columns
#'@param partial_classes logical. If \code{TRUE}, the first seven columns will
#'  receive default classes:
#'
#'  \itemize{
#'
#'  \item timestamp, md5_hash, controller, type, group - \code{character}, \item
#'  item_number, element_number - \code{numeric}, }
#'
#'  So \code{col_classes} will be taken to specify classes for columns starting
#'  from 8. If \code{FALSE}, \code{col_classes} will be taken as specifying
#'  classes for all columns
#'
#'@param short_subj_ids logical. If \code{TRUE}, each subject will be assigned a
#'  short index. If \code{FALSE}, a combination of timestamp and browser md5
#'  hash will be used to identify each subject.
#'
#'@param sprt logical. Only applicable to \dQuote{DashedSentence} and
#'  \dQuote{DashedAcceptabilityJudgment} controllers. Set to \code{TRUE} if the mode
#'  was set \dQuote{self-paced reading} in Ibex.Set to \code{FALSE} if the mode
#'  was \dQuote{speeded acceptability}.
#'
#'@param ... further parameters passed to \code{\link[utils]{read.csv}}
#'
#'
#'@details
#'
#'The first seven columns in IBEX results always are: \enumerate{ \item Time the
#'results are received \item MD5 hash of participant's IP \item Controller type
#'\item Item presentation order \item Element number \item Type \item Item }
#'
#'Notice that the names given here for column 4 (\dQuote{Item presentation
#'order}) and 7 (\dQuote{Item}) are not exactly the names listed in the Ibex
#'manual (where they are instead named \dQuote{item_number} and \dQuote{Group}.
#'However, the names listed here better correspond to the nature of the data
#'contained in the corresponding column. See ch.15 (\dQuote{Terminological
#'clarifications}) in Ibex manual for clarifying remarks.
#'
#'By default, the column deletion mode is \dQuote{auto}. It means that the
#'function will remove columns not containing useful information. This means:
#'
#'\itemize{ \item \dQuote{md5 hash} column is always removed since it's not
#'usually used in analyses \item \dQuote{controller} column is removed if there
#'is just one controller in the data - it is assumed, the user will know what
#'controller that is \item \dQuote{element number} column is removed if that
#'column only contains zeros \item \dQuote{item} column is removed if it only
#'contains \code{NULL} values }
#'
#'The above applies to all controllers. In addition:
#'
#'\itemize{ \item for \dQuote{DashedSentence} and
#'\dQuote{DashedAcceptabilityJudgment} in \dQuote{self-paced reading} mode
#'column 11 (\dQuote{line_break}) is removed if it only contains \code{FALSE}
#'values (i.e. if all sentences in the experiment fit on one line and there were
#'no line breaks).
#'
#'\item For \dQuote{Question}, \dQuote{AcceptabilityJudgment} and
#'\dQuote{DashedAcceptabilityJudgment} in \dQuote{speeded acceptability} mode
#'column 15 (\dQuote{is_correct}) is removed if it contains only \code{NULL}
#'valuess (if this is the case, it means that correct answers were not specified
#'in Ibex, so this column is of no use). }
#'
#'In \dQuote{user} deletion mode the data will not be taken into account at all,
#'and only the columns specified by user will be deleted.
#'
#'In \dQuote{mixed} deletion mode the columns to delete are the union of the
#'sets of automatically deleted columns and user-specified columns.
#'
#'@return \code{data.frame} with the preprocessed data associated with the requested controller
#'
#'@name get_results
NULL
