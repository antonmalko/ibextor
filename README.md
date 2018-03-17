# ibextor

This package is intended to facilitate loading experimental data generated on [Ibex](http://spellout.net/ibexfarm) into R. Ibex data are stored in a simple .csv file,
so loading it into R per se is easy. However, multiple preprocessing steps have to
be carried on such raw data. The functions in this package automatically perform
many of such steps:

+ Columns are automatically assigned appropriate names and data types
+ Columns not containing useful info are automatically deleted (e.g. the column 
indicating whether the answer to a question was correct will contain only `NULL` 
if the correct answer was not specified inthe Ibex script)
+ Controllers adding multiple lines to the data file (e.g. "DashedSentence") are
"unrolled", so that all the info from an experimental item is contained in a single
`data.frame`
+ Participants are automatically assigned simple numeric ids (1,2, ...), instead
of being identified by strings like "1520910272_c575f9a34a249d8f9d34c59469515f14"

## Functions overview

**Data loading functions**

Several functions are available, corresponding to the main default Ibex controllers:

+ `get_results_ds()` - DashedSentence
+ `get_results_daj()` - DashedAcceptabilityJudgment
+ `get_results_aj()` - AcceptabilityJudgment
+ `get_results_q()` - Question

In the most basic case one would just pass the name of the file with Ibex result 
as the single parameter, and the functions will return pre-processed data in a 
`data.frame`. See documentation for additional options (e.g. specifying columns
to be deleted)

**Subject info function**

`get_subj_info()`

In the most basic case, again, it is enough to pass the name of the file with
Ibex results. A couple of parameters are of immediate interest, however:

+ `form_name` - the name of the form used to collect subject info. If there were
several forms, you can just pass all of their names as a vector. These names correspond
to the names you assigned to the forms in the `.js` file. This parameter
defaults to "intro" (name of the form in the Ibex experiment template).
+ `info` - name of the fields you want to extract. If `NULL` (default), all
available info will be extracted. Otherwise, pass a vector with the names
of the fields you want to get (these would be whatever you  put in the 
"name" attribute of the input fields in the HTML code for the forms).

## Installation

`devtools::install_github("antonmalko/ibextor")`

Notice that the package is still being developed and tested.





