################ Data Manipulation ################ 

adjust_times <- function(dataset, minTime) { # make sure we start at t=0
  dataset$time <- dataset$time - minTime
  dataset <- subset(dataset, time <= 180)#dataset <- subset(dataset)
  return(dataset)
}

### TO DO, NOISEDATA MISSING
# Find the final positions by: foot position + noise values & add it to the DF
calc_final_pos <- function(footData, noiseData) {
  # Ensure both datasets are sorted by time to maintain order after joining
  footData <- footData %>% arrange(time)
  noiseData <- noiseData %>%
    select(time, offset_x, offset_y, offset_z) %>%
    arrange(time)
  
  # Join footData with noiseData by time
  footData <- footData %>%
    left_join(noiseData, by = "time") %>%
    rowwise() %>%
    mutate(
      magnitude = sqrt(offset_x^2 + offset_y^2 + offset_z^2),
      final_pos_x = pos_x + offset_x,
      final_pos_y = pos_y + offset_y,
      final_pos_z = pos_z + offset_z
    ) %>%
    ungroup()
  
  return(footData)
}

preprocess_data <- function(participant, trialNum){
  leftFoot = get_t_data(participant, "leftfoot", trialNum)
  rightFoot = get_t_data(participant, "rightfoot", trialNum)
  hip = get_t_data(participant, "hip", trialNum)
  targetData = get_t_data(participant, "steptargets", trialNum)
  leftDisturbance <- get_t_data(participant, "leftdisturbance", trialNum)
  rightDisturbance <- get_t_data(participant, "rightdisturbance", trialNum)
  #rightFoot <- calc_final_pos(rightFoot, rightDisturbance)
  #leftFoot <- calc_final_pos(leftFoot, leftDisturbance)
  minTime <- leftFoot$time[1] #get_p_results(participant,"start_time",trialNum)
  
  moveSpeed = get_move_speed(participant)
  leftFoot = adjust_times(leftFoot, minTime)
  leftFoot$actual_pos_z = leftFoot$pos_z + moveSpeed * leftFoot$time
  rightFoot = adjust_times(rightFoot, minTime)
  rightFoot$actual_pos_z = rightFoot$pos_z + moveSpeed * rightFoot$time
  
  return(list(
    leftFoot = leftFoot,
    rightFoot = rightFoot,
    leftDisturbance = leftDisturbance,
    rightDisturbance = rightDisturbance,
    hip = adjust_times(hip, minTime),
    targetData = adjust_times(targetData, minTime)
  ))
}

apply_padding_and_filter <- function(column, poly_order, frame_size, fs, cutoff_freq = 5) { 
  # Detect and remove outliers using a z-score method
  detect_outliers_filter <- function(column, threshold = 3) {
    # Calculate the z-scores
    z_scores <- (column - mean(column, na.rm = TRUE)) / sd(column, na.rm = TRUE)
    # Identify outliers
    outliers <- abs(z_scores) > threshold
    return(outliers)
  }
  
  # Detect outliers
  outliers <- detect_outliers_filter(column)
  
  # Replace outliers with NA
  column[outliers] <- NA
  
  # Interpolate the missing values (linear interpolation)
  column <- na.approx(column, rule = 2)
  
  # Calculate the number of points to pad (half the frame size generally works well)
  pad_width <- 20
  
  # Create mirrored padding
  padding_start <- rev(column[1:pad_width])
  padding_end <- rev(column[(length(column) - pad_width + 1):length(column)])
  
  # Pad the column
  padded_column <- c(padding_start, column, padding_end)
  
  # Apply Butterworth filter to the padded data
  b <- butter(poly_order, cutoff_freq / (fs / 2))  # 4th order Butterworth filter
  filtered_column <- filtfilt(b, padded_column)
  
  # Remove the padding
  filtered_column <- filtered_column[(pad_width + 1):(length(filtered_column) - pad_width)]
  
  return(filtered_column)
}

detect_foot_events_coordinates <- function(footData, hipData) {
  ####### FILTER
  # Apply a moving average filter to the relative foot position
  numeric_columns <- sapply(hipData, is.numeric)  # Identify numeric columns --> NOTE: actual_pos_z not filtered, but not needed here.
  
  # Apply the filter to all numeric columns
  poly_order <- 4  # Order of the polynomial (savgoy=3,butter=4)
  frame_size <- 5  # (for savgoy Must be odd)
  footData[numeric_columns] <- lapply(footData[numeric_columns], function(column) { apply_padding_and_filter(column, poly_order, frame_size, 90) } ) 
  hipData[numeric_columns] <- lapply(hipData[numeric_columns], function(column) { apply_padding_and_filter(column, poly_order, frame_size, 90) } )
  
  # Get subcomponents
  frontalFootPos <- footData$pos_z
  frontalHipPos <- hipData$pos_z
  #footHeight <- footData$pos_y ##### Could also do a height check, but looks good without it
  relFootPos <- frontalFootPos - frontalHipPos
  
  # Detect local extremes of relative foot pos - Based on https://c-motion.com/v3dwiki/index.php/Tutorial:_Gait_Events#Method_1._Coordinate_Based_Algorithm
  local_maxima <- which(diff(sign(diff(relFootPos))) == -2) + 1
  local_minima <- which(diff(sign(diff(relFootPos))) == 2) + 1
  
  if (local_minima[1] > local_maxima[1]) {
    local_maxima <- local_maxima[-1]
  }
  
  N_removed_min <- 0
  N_removed_max <- 0
  # alternation checking
  for (i in 1:(length(local_minima))) {
    if (i <= length(local_minima) && i <= length(local_maxima)) {
      while (local_maxima[i] < local_minima[i] && i <= length(local_minima) && i <= length(local_maxima)) {
        local_maxima <- local_maxima[-i] # remove the maxima, it is wrong
        N_removed_min <- N_removed_min + 1
      }
    }
    
    if (i+1 <= length(local_minima)  && i <= length(local_maxima)) {
      while (local_maxima[i] > local_minima[i+1] && i+1 <= length(local_minima)  && i <= length(local_maxima)) {
        local_minima <- local_minima[-(i+1)]
        N_removed_max <- N_removed_max + 1
      }
    }
  }
  
  # Make sure lengths match
  lMax <- length(local_maxima)
  lMin <- length(local_minima)
  if (lMax != lMin){
    if (lMax > lMin){print("Something REALLY WRONG... check detect_foot_events_coordinates()")}
    
    trimLength <- min(lMax, lMin)
    local_maxima <- local_maxima[1:trimLength]
    local_minima <- local_minima[1:trimLength]
  }
  
  # Some logging
  if (N_removed_max + N_removed_min > 0) {
    print(paste("removed",N_removed_max,"maxima, and",N_removed_min,"minima."))
  }
  if (length(local_maxima) !=length(local_minima)) {  
    print(paste("Length maxima:",length(local_maxima),"Length minima:",length(local_minima)))
  }
  
  # Extract positions and times
  heelStrikes <- data.frame(footData[local_maxima, ])
  toeOffs <- data.frame(footData[local_minima, ])
  print(paste("totalsteps: ",length(heelStrikes$time)))
  return(list(heelStrikes=heelStrikes,toeOffs=toeOffs))
}

find_foot_events <- function(participant, trialNum) {
  preprocessedData <- preprocess_data(participant, trialNum)
  
  leftFoot <- preprocessedData$leftFoot
  rightFoot <- preprocessedData$rightFoot
  hip <- preprocessedData$hip
  targetData <- preprocessedData$targetData
  
  # Detect toe-off and heelstrikes
  footEventsLeft <- detect_foot_events_coordinates(leftFoot, hip)
  footEventsRight <- detect_foot_events_coordinates(rightFoot, hip)
  
  # Add a 'foot' column to each event dataframe
  footEventsLeft$heelStrikes$foot <- 'Left'
  footEventsLeft$toeOffs$foot <- 'Left'
  footEventsRight$heelStrikes$foot <- 'Right'
  footEventsRight$toeOffs$foot <- 'Right'
  
  # Combine heel strikes and foot lifts from both feet
  combinedHeelStrikes <- rbind(footEventsLeft$heelStrikes, footEventsRight$heelStrikes)
  combinedToeOffs <- rbind(footEventsLeft$toeOffs, footEventsRight$toeOffs)
  
  # Order the events by time
  combinedHeelStrikes <- combinedHeelStrikes[order(combinedHeelStrikes$time), ]
  combinedToeOffs <- combinedToeOffs[order(combinedToeOffs$time), ]
  
  ensure_alternation <- function(data1, data2) {
    incorrect_seq <- which(diff(as.numeric(data1$foot == "Left")) == 0)
    
    if (length(incorrect_seq > 0)) {
      #print(data1[c(incorrect_seq,incorrect_seq+1),])
      data1 <- data1[-(incorrect_seq + 1),] # remove the second (later) value
      data2 <- data2[-(incorrect_seq + 1),] # remove the second (later) value
      print(paste("removed",length(incorrect_seq),"steps due to wrong alternation. At place(s):"))
      print(incorrect_seq)
      print("----------------------")
    }
    return(list(data1 = data1, data2 = data2))
  }
  
  # Apply alternation check on heelStrikes and remove index columns
  results <- ensure_alternation(combinedHeelStrikes, combinedToeOffs)
  combinedHeelStrikes <- results$data1
  combinedToeOffs     <- results$data2
  
  # Label step numbers. Assuming each heel strike represents a new step
  combinedHeelStrikes$step <- seq_len(nrow(combinedHeelStrikes))
  combinedToeOffs$step <- seq_len(nrow(combinedToeOffs))
  
  # Check if we stepped onto a target
  targetData <- targetData[order(targetData$time), ] # make sure we order by time
  combinedHeelStrikes$target <- FALSE
  for (i in 2:nrow(combinedHeelStrikes)) {
    prevStepTime <- combinedHeelStrikes$time[i-1]
    stepTime <- combinedHeelStrikes$time[i]
    
    # Find targets between the current heel strike and the next heel strike (or end of recording)
    targetsInTime <- targetData[targetData$time > prevStepTime & targetData$time <= stepTime, ]
    
    # Update the target_stepped_on flag if any targets were found
    if (nrow(targetsInTime) > 0) {
      combinedHeelStrikes$target[i] <- TRUE
    }
  }
  
  return(list(heelStrikes = combinedHeelStrikes, toeOffs = combinedToeOffs))
}

calculate_gait_parameters <- function(participant, trialNum) {
  gaitData <- find_foot_events(participant, trialNum)
  
  heelStrikesData <- gaitData$heelStrikes # should already be sorted based on time
  toeOffsData <- gaitData$toeOffs
  
  relHeelStrikesData <- gaitData$heelStrikes
  # Apply diff and padding to each numeric column
  relHeelStrikesData[] <- lapply(relHeelStrikesData, function(column) {
    if (is.numeric(column)) {
      # Calculate differences and pad with a leading zero
      c(0, diff(column))
    } else {
      column  # Return non-numeric columns unchanged
    }
  })
  
  # time-based
  stepTimes <- relHeelStrikesData$time # Calculate step times  >>> NOTE: The first heelstrike is only used as a starting point to the second
  swingTimes <- heelStrikesData$time - toeOffsData$time # Calculate swing times <<< L = N   (not N-1)
  stanceTimes <- stepTimes - swingTimes # Calculate stance times
  
  # position-based
  stepWidths <- relHeelStrikesData$pos_x # Calculate step width
  stepWidths <- ifelse(relHeelStrikesData$foot == "Left", stepWidths * -1, stepWidths) # Adjust sign based on which foot is stepping
  heelStrikeHeights <- 
    stepLengths <- relHeelStrikesData$actual_pos_z # Calculate step lengths
  
  speed <- stepLengths / stepTimes # Calculate speed
  
  # We ignore the steps onto and away from a target
  # and also add to the ignore list the outliers in the stepTimes and stepLengths (these are very obvious ways to remove the misdetected steps)
  targetSteps <- heelStrikesData$target
  targetSteps <- targetSteps | lead(heelStrikesData$target, default = FALSE) # add step before
  targetSteps <- targetSteps | lag(heelStrikesData$target, default = FALSE) | lag(heelStrikesData$target, 2, default = FALSE) # add steps after (until foot is placed back onto new position)
  heelStrikesData$targetIgnoreSteps <- targetSteps
  # Detect outliers
  heelStrikesData$outlierSteps <- detect_outliers(stepTimes, targetSteps) | detect_outliers(stepLengths, targetSteps)  | detect_outliers(gaitData$heelStrikes$pos_y, targetSteps) | detect_outliers(gaitData$toeOffs$pos_y, targetSteps)
  
  #stepsToIgnore <- outlierSteps
  # Make a list of all the gait parameters
  gaitParams <- list(
    stepTimes = stepTimes,
    stanceTimes = stanceTimes,
    swingTimes = swingTimes,
    #finalStepWidths = finalStepWidths,
    stepLengths = stepLengths,
    #finalStepLengths = finalStepLengths,
    stepWidths = stepWidths,
    speed = speed,
    heelStrikes = heelStrikesData, # We also add the heelstrikes info as it's interesting for plotting.
    toeOffs = toeOffsData,
    relHeelStrikes = relHeelStrikesData
  )
  
  return(gaitParams)
}

detect_outliers <- function(data, targetIgnoreSteps) {
  data_filtered <- data[!targetIgnoreSteps] # We don't use the target steps to calculate our interquartile ranges.
  
  Q1 <- quantile(data_filtered, 0.25)
  Q3 <- quantile(data_filtered, 0.75)
  IQR <- Q3 - Q1
  
  # Define the upper and lower bounds for outliers
  upper_bound <- Q3 + 1.5 * IQR
  lower_bound <- Q1 - 1.5 * IQR
  
  return(!(data >= lower_bound & data <= upper_bound))
}
