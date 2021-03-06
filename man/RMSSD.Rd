% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Time Domain HRV Functions .R
\name{RMSSD}
\alias{RMSSD}
\title{Root Mean Squared Successive Differences (RMSSD)}
\usage{
RMSSD(data)
}
\arguments{
\item{data}{Dataframe with minutes(column 1) and RR values (column 2). Preferably generated by
the Data_Input function.}
}
\value{
Single numeric object
}
\description{
RMSSD is calculated using the successive differences ellapsed between heart
beats (RR values). This is considered the most precise marker for parasympathetic
activity using heart rate, and it is the most common time-domain HRV statistic reported
in the literature. Lower HRV and RMSSD values indicate higher levels of distress.
}
