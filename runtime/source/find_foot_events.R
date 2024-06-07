################ Data Manipulation ################ 

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

detect_foot_events_coordinates <- function(footData, hipData) {
  ####### FILTER
  # Apply a moving average filter to the relative foot position
  numeric_columns <- sapply(hipData, is.numeric)  # Identify numeric columns --> NOTE: actual_pos_z not filtered, but not needed here.
  
  # Apply the filter to all numeric columns
  poly_order <- 4  # Order of the polynomial (savgoy=3,butter=4)
  frame_size <- 5  # (for savgoy Must be odd)
  footData_filtered <- footData
  hipData_filtered  <- hipData
  footData_filtered[numeric_columns] <- lapply(footData[numeric_columns], function(column) { apply_padding_and_filter(column, poly_order, frame_size, 90) } ) 
  hipData_filtered[numeric_columns] <- lapply(hipData[numeric_columns], function(column) { apply_padding_and_filter(column, poly_order, frame_size, 90) } )
  
  # Get subcomponents
  frontalFootPos_filtered <- footData_filtered$pos_z
  frontalHipPos_filtered <- hipData_filtered$pos_z
  #footHeight <- footData$pos_y ##### Could also do a height check, but looks good without it
  relFootPos_filtered <- frontalFootPos_filtered - frontalHipPos_filtered
  
  # Detect local extremes of relative foot pos - Based on https://c-motion.com/v3dwiki/index.php/Tutorial:_Gait_Events#Method_1._Coordinate_Based_Algorithm
  local_maxima <- which(diff(sign(diff(relFootPos_filtered))) == -2) + 1
  local_minima <- which(diff(sign(diff(relFootPos_filtered))) == 2) + 1
  
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
  
  # Extract positions and times - use UNFILTERED footData
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
  #heelStrikeHeights <- 
  stepLengths <- relHeelStrikesData$actual_pos_z # Calculate step lengths
  
  speed <- stepLengths / stepTimes # Calculate speed
  
  # We ignore the steps onto and away from a target
  # and also add to the ignore list the outliers in the stepTimes and stepLengths (these are very obvious ways to remove the misdetected steps)
  targetSteps <- heelStrikesData$target
  targetSteps <- targetSteps | lead(heelStrikesData$target, default = FALSE) # add step before
  targetSteps <- targetSteps | lag(heelStrikesData$target, default = FALSE) | lag(heelStrikesData$target, 2, default = FALSE) | lag(heelStrikesData$target, 3, default = FALSE) # add 3 steps after (until foot is placed back onto new position)
  heelStrikesData$targetIgnoreSteps <- targetSteps
  
  
  # Detect outliers
  # First, we throw out some incorrect steps that are just physically impossible (for example the first/last step sometimes is filled in as a 0.0s step, which is nonsensical)
  heelStrikesData$incorrectDetection <-  stepLengths <= 0 | stepTimes < 0.05 | gaitData$toeOffs$pos_y > 1.0 | gaitData$heelStrikes$pos_y > 1.0 | gaitData$toeOffs$pos_y < 0.42 | gaitData$heelStrikes$pos_y < 0.42 | stepLengths > 2 | stepTimes > 2 | stepWidths > 1.0 | stepWidths < -0.5
  heelStrikesData$incorrectDetection[heelStrikesData$targetIgnoreSteps] <- FALSE
  
  # no backwards stepsc | impossibly far step | impossibly fast step | impossibly long step | impossibly wide step | a little bit of negative may be possible, but not more than .5m | impossibly high TO | impossibly high HS | impossibly low TO (below floor) | impossibly low HS (below floor)
  
  alreadyIgnoredSteps <- targetSteps | heelStrikesData$incorrectDetection # we define those because we don't want to use them for calculating the IQRs for the outlier detection
  IQR_mlp <- 1.5
  heelStrikesData$outlierSteps <- detect_outliers(stepWidths, alreadyIgnoredSteps, IQR_mlp) | detect_outliers(stepTimes, alreadyIgnoredSteps, IQR_mlp) | detect_outliers(stepLengths, alreadyIgnoredSteps, IQR_mlp) #| detect_outliers(gaitData$heelStrikes$pos_y, alreadyIgnoredSteps, 3) | detect_outliers(gaitData$toeOffs$pos_y, alreadyIgnoredSteps, 3)
  
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


calculate_target_data <- function(participant, trial){
  # get the target tracker results
  targetData <- get_t_data(participant, "steptargets", trial)
  targetData$rel_x <- targetData$foot_x - targetData$target_x
  targetData$rel_z <- targetData$foot_z - targetData$target_z
  
  # calculate total target distance
  targetData$targetDist <- sqrt(targetData$rel_x^2 + targetData$rel_z^2)
  
  return(targetData)
}