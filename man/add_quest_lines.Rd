% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{add_quest_lines}
\alias{add_quest_lines}
\title{Find question lines in DAJ Ibex controllers}
\usage{
add_quest_lines(sent_lines)
}
\arguments{
\item{sent_lines}{\code{logical} vector indicating which lines contain sentence data}
}
\value{
\code{logical} vector indicating whcih lines contain sentence AND question data
}
\description{
DashedAcceptabilityJudgment obligatorily adds lines corresponding to the
question data immediately after the sentence data. Thus, knowing which lines
correspond to last lines of sentence data, we can find queestion data by looking at
the immediately following line.
}
