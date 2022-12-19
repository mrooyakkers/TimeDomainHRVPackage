#' Input Data Organization/Cleaning Function
#'
#' This function takes the user's heart rate variation (HRV) data and turns it into a
#' useable form for the rest of this package. Output from heart rate monitors used in research
#' are typically either in RR values or MS values. RR values are the milliseconds ellapsed
#' between successive heart beats, while MS values are a chronological millisecond value that
#' marks when each heart beat occurs. All statistical values used in HRV take RR values
#' as an input. Therefore, if the user chooses to upload MS values, the function will
#' transform it into RR values.
#' 
#' The function allows the user to specify the time frame that they
#' are interested in, using the "segment" option. The function intakes a minute min/max
#' value to specify the bounds of their data that they are interested in.
#' In the literature, the HRV segments less than 30 seconds are not valid and their
#' use is not advised. Therefor, the program writes a warning if the user chooses to
#' to use a HRV segment less than 30 seconds.
#' 
#' The function assumes that the user is uploading a csv file with the ms or RR values
#' in a single row. This matches the type of output given by most heart rate monitoring
#' software.
#'
#'
#' @param imported_data Imported csv file.
#' @param input RR or MS values.
#' @param segment Specifies the upper and lower bounds by minutes.
#' @param min If segment is true, what is the minimum value.
#' @param max If segment is true, what is the maximum value.
#' @return A dataframe.
#' @export


Data_Input <- function (imported_data, input, segment, min, max) {

  if (ncol(imported_data) > 2) {
    stop("Please upload a csv file containing one column with all RR or MS values.")
  }

  if (nrow(imported_data) < 10) {
    stop("Please enter a larger segment of HRV data.")
  }

  if (is.numeric(imported_data[ ,1]) == FALSE) {
    stop("Please enter numeric data.")
  }

  if (input == "RR" && segment == FALSE) {
    x <- as.data.frame(as.numeric(imported_data[ ,1]))
    time_vector <- vector()
    for (value in 1:nrow(x)) {
      newsum <- sum(x[1:value, ])
      time_vector <- append(time_vector, newsum)
    }

    # transforming millisecond values to minutes
    Minutes <- time_vector / 6000

    # then add this to existing dataframe, so that the user can index RR values by time
    data <- cbind(Minutes, x)

  } else if (input == "MS" && segment == FALSE) {

    # diff function that takes the differences between each millisecond time point
    # turns the data into RR values
    RR <- diff(imported_data[ ,1])
    x <- as.data.frame(RR)

    # creating a vector that transforms RR values to milliseconds
    time_vector <- vector()

    for (value in 1:nrow(x)) {
      newsum <- sum(x[1:value, ])
      time_vector <- append(time_vector, newsum)
    }

    # millisecond to minutes
    Minutes <- time_vector / 6000

    # then add this to existing dataframe, so that the user can index time
    data <- cbind(Minutes, x)

  } else if (input == "RR" && segment == TRUE) {
    x <- as.data.frame(imported_data[ ,1])

    # create a new vector that converts RR values to time
    time_vector <- vector()

    for (value in 1:nrow(x)) {
      newsum <- sum(x[1:value, ])
      time_vector <- append(time_vector, newsum)
    }

    # millisecond to minutes
    Minutes <- time_vector / 6000

    # then add this to existing dataframe, so that the user can index time
    df1_plus_time <- cbind(Minutes, x)

    # referencing the row number based on the minimum value that the user inputs
    min_row <- which(grepl(min, df1_plus_time[ ,1]))
    min_row <- min_row[1]

    # referencing the row number based on the maximum value that the user inputs
    max_row <- which(grepl(max, df1_plus_time[ ,1]))
    max_row <- tail(max_row)
    max_row <- max_row[6]

    # narrowing down the data frame so that it only includes the RR values
    # within the user's specified time frame
    data <- df1_plus_time[min_row:max_row, 1:2]

  } else if (input == "MS" && segment == TRUE) {
    RR <- diff(imported_data[ ,1])
    x <- as.data.frame(RR)
    time_vector <- vector()

    for (value in 1:nrow(x)) {
      newsum <- sum(x[1:value, ])
      time_vector <- append(time_vector, newsum)
    }

    # millisecond to minutes
    Minutes <- time_vector / 6000
    
    # then add this to existing dataframe, so that the user can index time
    df1_plus_time <- cbind(Minutes, x)
    
    # referencing the row number based on the minimum value that the user input
    min_row <- which(grepl(min, df1_plus_time[ ,1]))
    min_row <- min_row[1]
    
    # referencing the row number based on the maximum value that the user inputs
    max_row <- which(grepl(max, df1_plus_time[ ,1]))
    max_row <- tail(max_row)
    max_row <- max_row[6]

    # narrowing down the data frame so that it only includes the RR values
    # within the user's specified time frame
    data <- df1_plus_time[min_row:max_row, 1:2]
  }

  # warning messages for incorrect data input
  last_minute <- tail(data$Minutes, n = 1)

  if (last_minute < .5) {
    warning("This software is not appropriate for heart rate
            segments smaller than 30 seconds.")
  }

  if (segment == TRUE) {
    if (min > max) {
      warning("Incorrect min/max values. Max value should exceed min value by 
              at least 30 seconds.")

    } else if (max - min < 0.5) {
      warning("This software is not appropriate for heart rate
            segments smaller than 30 seconds.")
    }
    
    if (max > last_minute) {
      warning("Specified max value exceeds the max value in the dataset.")
    }
  }
  return(data)
}


#' Root Mean Squared Successive Differences (RMSSD)
#'
#' RMSSD is calculated using the successive differences ellapsed between heart
#' beats (RR values). This is considered the most precise marker for parasympathetic
#' activity using heart rate, and it is the most common time-domain HRV statistic reported
#' in the literature. Lower HRV and RMSSD values indicate higher levels of distress.
#'
#' @param data Dataframe with minutes(column 1) and RR values (column 2). Preferably generated by
#' the Data_Input function.
#' @return Single numeric object
#' @export

RMSSD  <- function(data) {
  input_length <- length(data[,2]) - 1
  RR_squared <- data^2
  RR_squared_average <- sum(RR_squared) / input_length
  rmssd1 <- sqrt(RR_squared_average)
  return(rmssd1)
}



#' SDNN Function
#'
#' SDNN measurement is the standard deviation of all time ellapses heart beats.
#' This value indicates both parasympathetic and sympathetic nervous system
#' activity and is imporant measure in cardiology research and practice.
#'
#' @param data Dataframe with minutes(column 1) and RR values (column 2). Preferably generated by
#' the Data_Input function.
#' @return Single numeric object
#' @export

SDNN <- function(data) {
  last_minute <- tail(data[,1])
  last_minute <- last_minute[5]
  SDNN <- sd(data[,2])
  return(SDNN)
}



#' NN50 Calculation
#'
#' NN50 is the number of inter-beat-intervals that are greater than 50ms.
#' This is more useful when calculated as a percentage (next function).
#' @param data Dataframe with minutes(column 1) and RR values (column 2). Preferably generated by
#' the Data_Input function.
#' @return Single numeric object
#' @export

NN50_Count <- function(data = data) {
  RR <- data[ ,2]
  counter <- 0
  for (i in 1:length(RR)) {
    if (i > 50) {
      counter <- counter + 1
    }
  }
  return(counter)
}


#' pNN50 Calculation
#'
#' This value is the percentage of intervals between heart beats that are
#' greater than 50ms when compared to all heart beats in the segment.
#' Research suggests that less than 6.25% of heart beats greater than 50ms
#' indicates an elevated heart rate, putting the individual at cardiac risk or
#' indicating high levels of distress.
#'
#' @param data Dataframe with minutes(column 1) and RR values (column 2). Preferably generated by
#' the Data_Input function.
#' @return Single numeric object
#' @export

pNN50_Percentage <- function (data) {
  RR <- data[,2]
  counter <- 0
  for (i in 1:length(RR)) {
    if (i > 50) {
      counter <- counter + 1
    }
  }
  RR_length <- length(RR)
  pNN50_Declimal <- counter / RR_length
  pNN50_Percent <- pNN50_Declimal %*% 100
  return(pNN50_Percent)
}


#' Descriptive Heart Rate -- MIN + MAX + AVERAGE
#'
#' Gives the user an overview of the data.
#' HR range (HR max - HR min) is sensitive to Respiratory Sinus Arrhythmia (RSA),
#' and higher HR range reflects the hearts ability to adapt to the environment.
#' Lower HR range, in addition to other measures, can be used to diagnose cardiac
#' risk or as an indicator of distress. 
#' 
#' @param data A dataframe with minutes(column 1) and RR values (column 2). Preferably generated by
#' the Data_Input function.
#' @return List
#' @export

Descriptive_HRV <- function (data = data) {
  last_minute <- tail(data[,1])
  last_minute <- last_minute[5]
  RR <- data[ , 2]
  min_max_difference <- max(RR) - min(RR)
  return <- (list("range", min_max_difference, "mean", mean(RR), "minimum", min(RR), "maximum", max(RR)))
  return(return)
}


#' Plot
#'
#' Gives the user an overview of the data and how heart rate varation changes over time.
#' @param data A dataframe with minutes(column 1) and RR values (column 2). Preferably generated by
#' the Data_Input function.
#' @return Plot
#' @export

RR_Plot <- function (data = data) {
  plot(data, type = "p", xlab = "Minutes", ylab = "RR Values", col=rgb(0.4,0.4,0.8,0.6), pch=16 , cex=.5)
  Minutes <- as.matrix(data[,1])
  RR <- as.matrix(data[,2])
  reg <- lm(RR ~ Minutes, data = data)
  abline(reg, col="red", lwd=1)
  minval <- format(round(min(Minutes), 0), nsmall = 0)
  maxval <- format(round(max(Minutes), 0), nsmall = 0)
  axis(1, at = seq(minval, maxval, by = 1))
}




