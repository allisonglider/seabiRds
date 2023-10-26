# ---------------------------------------------------------------------------------------------------------------
#' Find peak frequency
#'
#'@param data Vector of numeric values for finding peaks
#'@param method Method for calculating peak frequency, options are 'pracma' or 'fft'
#'@param time Vector of POSIXct times
#'@param frequency Sampling frequency of data, in Hz, if missing the function will estimate frequency from time
#'@param window Size of moving window for estimating peaks, in seconds
#'@param threshold Minimum amplitude to be include as a true peak, results may differ with method
#'@param sample Only used in fft method, only calculate paeaks at sample interval, in sec, to save processing time
#'@param maxfreq Maximum frequency of interest using the FFT method
#'
#'@details
#'
#'The pracma method uses pracma::findpeaks() to count the number of peaks within each window
#'above a specified treshold. Results are returned as peaks per second. This approach is faster
#'than the fft method and can be less impacted by noise in the data.If using this approach, I recommend
#'exploring different threshold values to understand how this changes the result.
#'
#'The fft method uses a Fast Fourier Transform to identify the peak frequency in the input data
#'over a moving window defined the frequency * interval. The threshold paramter filters out peaks
#'identified in samples with an inter-quartile range less than the threshold value. This helps eliminate
#'noisy peaks with low amplitudes. This method can be slow for large data sets. Use sample to only
#'calcualte peaks at a sampling interval (default is 1 sec), this can speed up the algorithm.
#'
#'@return A vector of frequencies (Hz).
#'
#'@examples
#'
#' # generate data with a noisy sine wave in the middle
#' ts <- 25 # sampling rate of 25 Hz
#' time <- 500 # duration in sec
#' l <- ts * time
#' x <- seq(0, time - 1/25, 1/25)
#' f <- 8 # frequency of 8Hz
#' f <- rep(rnorm(time, f, 0.05), each = ts) # create noise to frequency
#' amp <- rnorm(ts * time, 0.5, 0.1) # create noisy amplitude
#' z <- amp*sin(2*pi*f*x) # generate sine wave
#' # add data with no frequency to start and end
#' dat <- c(rep(0, 1500) + rnorm(1500, 0, 0.02), z, rep(0, 1500) + rnorm(1500, 0, 0.04))
#' tim <- seq(Sys.time(), length.out = length(dat), by = 1/ts) # generate time vector
#'
#' # example using pracma
#' myPeaks <- getPeakFrequency(data = dat, time = tim, method = 'pracma', window = 5,
#' frequency = NULL, threshold = 0.1)
#' par(mfrow = c(2,1))
#' plot(dat ~ tim, type = 'l')
#' plot(myPeaks ~ tim, type = 'l')
#' par(mfrow = c(1,1))
#'
#' # example using fft
#' myPeaks <- getPeakFrequency(data = dat, time = tim, method = 'fft', window = 5,
#' frequency = NULL, threshold = 0.06)
#' par(mfrow = c(2,1))
#' plot(dat ~ tim, type = 'l')
#' plot(myPeaks ~ tim, type = 'l')
#' par(mfrow = c(1,1))


# Version that accounts for unequal time intervals
getPeakFrequency <- function(data, time, method = c('pracma', 'fft'), window, frequency = NULL,
                     threshold = 0.1, sample = 1, maxfreq = NULL) {

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

    if (is.null(maxfreq)) maxfreq <- frequency

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
      ddd <- stats::ts(data = ddd, start = 1, frequency = frequency)

      # Create indices for outputting the data
      halfwidth <- ceiling(length(sampInt)/2)
      freqs <- ((2:halfwidth/2)/(halfwidth))/(1/frequency)

      # calculate fft
      pows <- abs(stats::fft(ddd)[2:halfwidth])^2
      pows <- pows[freqs < maxfreq]
      freqs <- freqs[freqs < maxfreq]

      # Select maximum frequency
      val <- freqs[which(pows == max(pows))[1]]

      # Exclude frequencies with very low amplitudes
      if (stats::IQR(ddd) < threshold) val <- 1/(frequency/2)

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

  #' @export getPeakFrequency
}

# ---------------------------------------------------------------------------------------------------------------
#' Calculate amplitude in wing beats
#'
#' @param dat Vector of accelerometer X value
#' @param time Vector of POSIXct times, should use \%H:\%M:\%OS if sampling frequency >1 Hz
#' @param frequency Sampling frequency of data, in Hz, if missing the function will estimate frequency from time
#' @param window Size of moving window for estimating peaks, in seconds
#'
#' @details Calculates inter-quartile range using a moving window of time*frequency

getAmplitude <- function(dat, window, time, frequency = NULL) {


  if (sum(is.na(dat))) stop('NA values in Z', call. = F)

  if (is.null(frequency)) frequency <- getFrequency(time = time)

  # Calculate IQR over a moving window
  amp <- zoo::rollapply(dat, window * frequency, FUN = stats::IQR, fill = NA)

  return(amp)

  #' @export getAmplitude
}


# ---------------------------------------------------------------------------------------------------------------
#' Calculate pitch
#'
#' @param X Vector of accelerometer X value
#' @param Y Vector of accelerometer Y value
#' @param Z Vector of accelerometer Z value
#' @param time Vector of POSIXct times, should use \%H:\%M:\%OS if sampling frequency >1 Hz
#' @param frequency Sampling frequency of data, in Hz, if missing the function will estimate
#' frequency from time
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
#' Calculate dynamic body acceleration
#'
#' @param X Vector of accelerometer X value
#' @param Y Vector of accelerometer Y value
#' @param Z Vector of accelerometer Z value
#' @param time Vector of POSIXct times, should use \%H:\%M:\%OS if sampling frequency >1 Hz
#' @param window Size of moving window for estimating peaks, in seconds
#' @param frequency Sampling frequency of data, in Hz, if missing the function will estimate frequency from time
#' @param partial If TRUE, calculates partial dynamic body acceleration


getDBA <- function(X, Y, Z = NULL, time, window, frequency = NULL, partial = F) {

  if (sum(is.na(X))) stop('NA values in X', call. = F)
  if (sum(is.na(Y))) stop('NA values in Y', call. = F)
  if (partial == F & sum(is.na(Z))) stop('NA values in Z', call. = F)

  if (is.null(frequency)) frequency <- getFrequency(time = time)

  # Calculate mean acceleration over a moving window
  staticX <- zoo::rollmean(X, window * frequency, fill = NA)
  staticY <- zoo::rollmean(Y, window * frequency, fill = NA)
  if (partial == F) staticZ <- zoo::rollmean(Z, window * frequency, fill = NA)

  dynamicX <- X - staticX
  dynamicY <- Y - staticY
  if (partial == F) dynamicZ <- Z - staticZ
  if (partial == F) ODBA <- sqrt((dynamicX^2) + (dynamicY^2) + (dynamicZ^2))

  if (partial == T) ODBA <- sqrt((dynamicX^2) + (dynamicY^2))

  return(ODBA)

  #' @export getDBA
}

# ---------------------------------------------------------------------------------------------------------------
#' Detect and correct switched X and Y axes
#'
#' @param data Vector of accelerometer X value
#' @param ask If TRUE, function will plot roll and pitch then ask if axes should be flipped
#' @param force If TRUE, function will always flip the x and y axes whether or not they appear flipped
#'
#' @details Different models of accelerometers can have the x and y channels flipped. This is particularly
#' an issue between different configurations of Technosmart AxyTrek units. This functions provides two options for
#' switching these axes back.
#'
#' If force is set to true, then the function will automatically flip the axes, this option should only be used
#' if you know that all of your data have this issue.
#'
#' If ask is set to true, then the function will plot pitch and roll using the data, and ask the user if they want to
#' flip the axes. On most seabirds there hould be more variability in the pitch than in the roll. If the
#' roll plot appears more variable, then the axes should be flipped.


checkAxes <- function(data, ask = T, force = F) {

  if (!('time' %in% names(data)))   stop('data must contain a field named time', call. = F)
  if (class(data$time)[1] != 'POSIXct')   stop('time must be formatted as POSIXct', call. = F)
  if (!('x' %in% names(data)))   stop('data must contain a field named x', call. = F)
  if (!('y' %in% names(data)))   stop('data must contain a field named y', call. = F)
  if (!('z' %in% names(data)))   stop('data must contain a field named z', call. = F)
  if (!('dep_id' %in% names(data)))   stop('data must contain a field named dep_id', call. = F)

  if (force == T) ask <- F

  if (ask == T) {

    pp <- seabiRds::getPitch(X = data$x, Y = data$y, Z = data$z, time = data$time, window = 1)
    rr <- seabiRds::getPitch(X = data$y, Y = data$x, Z = data$z, time = data$time, window = 1)

    par(mfrow = c(2,1))
    idx <- seq(1, nrow(data), getFrequency(data$time))
    plot(pp[idx] ~ data$time[idx], type = 'l', xlab = '', ylab = 'Pitch', main = data$dep_id[1])
    plot(rr[idx] ~ data$time[idx], type = 'l', xlab = '', ylab = 'Roll', main = data$dep_id[1])
    par(mfrow = c(1,1))

    flip <- readline("Should axes be flipped(1: yes, 2: no) ?")
    if (flip == 1) force <- T
  }

  if (force == T) {
    data <- fixAxes(data)
  }

  return(data)
  #' @export checkAxes
}

# ---------------------------------------------------------------------------------------------------------------
#' Correct switched X and Y axes
#'
#' @param data A formatted dateframe of accelerometer data, including fields: x, y, z, dep_id
#'
#' @details Different models of accelerometers can have the x and y channels flipped. This is particularly
#' an issue between different configurations of Technosmart AxyTrek units. This functions takes formatted
#' accelerometer data, swaps the x and y axes and multiplies the original x-axis by -1. This makes the x, y, and z
#' fields consistent with other acceleromters.

fixAxes <- function(data) {

  if (!('x' %in% names(data)))   stop('data must contain a field named x', call. = F)
  if (!('y' %in% names(data)))   stop('data must contain a field named y', call. = F)
  if (!('z' %in% names(data)))   stop('data must contain a field named z', call. = F)
  if (!('dep_id' %in% names(data)))   stop('data must contain a field named dep_id', call. = F)

  print(paste('Fixing X and Y axes in',data$dep_id[1]))
  temp <- data$x
  data$x <- data$y
  data$y <- temp * -1

  return(data)
  #' @export fixAxes
}
