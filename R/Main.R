#' Time Domain HRV Package
#'
#' Time-Domain Heart Rate Variability Analysis is one way to analyze and interpret
#' heart rate data. Time domain indicies quantify the amount of variance in the
#' inter-beat-intervals (IBI). It is frequently used across different domains of
#' research, such as medicine, human kinetcs, and psychology. Benefits of this method
#' is that the cost of collecting this type of data is relatively low compared to other
#' methods and statistical outputs can be compared across individuals. This package
#' includes a function to organize the data input, and subsequent functions to calculate
#' Time-Domain statistical values. Each value is a different way of quantifying the HRV
#' data.
#'
#' @section Data Input
#' / This function takes the user's heart rate variation (HRV) data and turns it into a
#' useable form for the rest of this package.
#'
#' @section Descriptive HRV
#' / This function outputs a list of the descriptive statistics used in for HRV data.
#'
#' @section NN50 Count
#' /This function gives the number of intervals between heart beats that are greater than 50ms.
#'
#' @section pNN50 Percentage
#' /This function gives the percentage of IBIs that are longer than 50ms.
#'
#' @section RMSSD
#' /This function gives the root mean square of successive differences between heart beats.
#'
#' @section RR_Plot
#' /#This function plots the HRV data to give an overview of how IBIs differed over time.
#'
#' @section SDNN
#' /This function outputs the standard deviation of all IBIs.
#' 
#' 
#' @details 
#' Shaffer, F., & Ginsberg, J. P. (2017). An overview of heart rate variability metrics and norms. Frontiers in public health, 5, 258.
