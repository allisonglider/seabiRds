# ---------------------------------------------------------------------------------------------------------------
#' Find peak frequency
#'
#'@param data Vector of numeric values for finding peaks
#'@param time Vector of POSIXct times
#'@param frequency Sampling frequency of data, in Hz
#'@param window Size of moving window for estimating peaks, in seconds
#'@param threshold Minimum amplitude to be include as a true peak, results may differ with method
#'@param sample Only used in fft method, only calculate paeaks at sample interval, in sec, to save processing time
#'
#'@description
#'
#'The pracma method uses findpeaks() to count the number of peaks within each window above a specified treshold. Results are returned as
#'peaks per second. This approach is faster than the fft method and can be less impacted by noise in the data.If using this approach, I recommend
#'exploring different threshold values to understand how this changes the result.
#'
#'The fft method uses a Fast Fourier Transform to identify the peak frequency in the input data over a moving
#'window defined the frequency * interval. The threshold paramter filters out peaks identified in samples with an inter-quartile range less than the threshold value.
#'This helps elimnate noisy peaks with low amplitudes. This method can be slow for large data sets. Use sample to only calcualte peaks
#'at a sampling interval (default is 1 sec), this can greatly speed up the algorithm.
#'
#'@value A vector of frequencies (Hz).


# Version that accounts for unequal time intervals
getPeaks <- function(data, time, method = c('pracma', 'fft'), window, frequency = NULL,
                     threshold = 0.1, sample = 1) {

  if (is.null(frequency)) frequency <- getFrequency(time = time)

  if (method == 'pracma') {
    if (threshold == 0) warning(call. = F, 'pracma method does not work well with threshold of 0')

    tt <- rep(0, length(data))

    tt[pracma::findpeaks(data, threshold = threshold)[,2]] <- 1

    peaks <- zoo::rollsum(tt, window * frequency, sum, fill = 0)

    dd <- data.frame(peaks = peaks, time = time, starts = as.POSIXct(NA), ends =  as.POSIXct(NA))

    idx <- floor(frequency * window/2)
    dd$starts[idx:length(time)] <- time[1:(length(time) - idx + 1)]
    dd$ends[1:(length(time) - idx)] <- time[(idx + 1):(length(time))]
    totTime <- as.numeric(difftime(dd$ends, dd$starts, units = "sec"))

    peaks <- peaks/totTime
  }

  if (method == 'fft') {
    # Track processing time
    ptm <- proc.time()

    # Get the length of the data and create peaks object
    lenVar <- length(data)
    peaks <- rep(NA, lenVar)

    # Get values needed for the loop from user inputs
    sampInterval <- frequency * sample
    windowWidth <- frequency * window
    midPoint <- floor(sampInterval/2)
    calcs <- seq(from = (1 + (windowWidth/2)), to = (lenVar - (windowWidth)/2), by = sampInterval)

    for (i in calcs) {
      # Prepare data for fft
      sampInt<- (i-floor(windowWidth/2)):((i-floor(windowWidth/2)) + windowWidth - 1)
      ddd <- data[sampInt]
      ddd <- ts(data = ddd, start = 1, frequency = frequency)

      # Create indices for outputting the data
      halfwidth <- ceiling(length(sampInt)/2)
      freqs <- ((2:halfwidth/2)/(halfwidth))/(1/frequency)

      # calculate fft
      pows <- abs(fft(ddd)[2:halfwidth])^2

      # Select maximum frequency
      val <- freqs[which(pows == max(pows))[1]]

      # Exclude frequencies with very low amplitudes
      if (IQR(ddd) < threshold) val <- 1/(frequency/2)

      # Write frequency value to output vector, filling all rows within the sample interval
      myInt <- (i - midPoint):((i - midPoint) + (sampInterval - 1))
      peaks[myInt] <- rep(val, sampInterval)

      # Print a progress message
      trackProg <- seq(from = 1, to = length(calcs), length.out = 11)[2:11]
      if (i %in% calcs[trackProg]) {

      print(paste("Finished processing:", round((i/lenVar) * 100), "% at", format(Sys.time(), "%T")))

      }

    }

    prc.time <- round((proc.time() - ptm)[[3]], digits = 3)
    print(paste("Processing time:", prc.time))

  }

  return(peaks)

  #' @export getPeaks
}


# ---------------------------------------------------------------------------------------------------------------
#' Calculate pitch
#'
#' @param X Vector of accelerometer X value
#' @param Y Vector of accelerometer Y value
#' @param Z Vector of accelerometer Z value
#' @param time Vector of POSIXct times
#' @param frequency Sampling frequency of data, in Hz
#' @param window Size of moving window for estimating peaks, in seconds
#' @param standVar Variable to use in standardizing pitch values, if desired
#' @param standMin Minimum value in standVar for standarization
#' @param standMax Maximum value in standVar for standarization

getPitch <- function(X, Y, Z, window, time, frequency = NULL,
                     standVar = NULL, standMin = NULL, standMax = NULL) {

  if (sum(is.na(X))) stop('NA values in X', call. = F)
  if (sum(is.na(Y))) stop('NA values in Y', call. = F)
  if (sum(is.na(Z))) stop('NA values in Z', call. = F)
  if (!is.null(standVar) & (is.null(standMin) | is.null(standMax))) stop('standMin and standMax cannot be NULL is standVar is used', call. = F)

  if (is.null(frequency)) frequency <- getFrequency(time = time)

  # Calculate mean acceleration over a moving window
  staticX <- zoo::rollmean(X, window * frequency, fill = NA)
  staticY <- zoo::rollmean(Y, window * frequency, fill = NA)
  staticZ <- zoo::rollmean(Z, window * frequency, fill = NA)

  # Calculate pitch based on mean acceleration in each axis
  pitch <- atan(staticX/(sqrt((staticY^2)+(staticZ^2))))*(180/pi)

  if (!is.null(standVar)) {


    idx <- which(standVar > standMin & standVar < standMax)

    # Determine the median value for each axis
    standX <- mean(X[idx])
    standY <- mean(Y[idx])
    standZ <- 1-mean(Z[idx])

    # Use median values during flight to standardize the axes
    X <- X  - standX
    Y <- Y - standY
    Z <- Z + standZ

    # Calculate mean acceleration over a moving window
    staticX <- zoo::rollmean(X, window * frequency, fill = NA)
    staticY <- zoo::rollmean(Y, window * frequency, fill = NA)
    staticZ <- zoo::rollmean(Z, window * frequency, fill = NA)

    # Calculate pitch based on mean acceleration in each axis
    pitch <- atan(staticX/(sqrt((staticY^2)+(staticZ^2))))*(180/pi)
  }

  return(pitch)

  #' @export getPitch
}

# ---------------------------------------------------------------------------------------------------------------
#' Calculate dynamic acceleration
#'
#' @param X Vector of accelerometer X value
#' @param Y Vector of accelerometer Y value
#' @param Z Vector of accelerometer Z value
#' @param time Vector of POSIXct times
#' @param window Size of moving window for estimating peaks, in seconds
#' @param frequency Sampling frequency of data, in Hz

getODBA <- function(X, Y, Z, time, window, frequency = NULL) {

  if (sum(is.na(X))) stop('NA values in X', call. = F)
  if (sum(is.na(Y))) stop('NA values in Y', call. = F)
  if (sum(is.na(Z))) stop('NA values in Z', call. = F)

  if (is.null(frequency)) frequency <- getFrequency(time = time)

  # Calculate mean acceleration over a moving window
  staticX <- zoo::rollmean(X, window * frequency, fill = NA)
  staticY <- zoo::rollmean(Y, window * frequency, fill = NA)
  staticZ <- zoo::rollmean(Z, window * frequency, fill = NA)

  dynamicX <- X - staticX
  dynamicY <- Y - staticY
  dynamicZ <- Z - staticZ
  ODBA <- abs(dynamicX) + abs(dynamicY) + abs(dynamicZ)

  return(ODBA)

  #' @export
}
