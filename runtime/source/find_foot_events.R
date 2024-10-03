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
  relFootPos <- footData$pos_z - hipData$pos_z
  relFootPos_filtered <- apply_padding_and_filter(relFootPos, 4, 90, 5)

  # Detect local extremes of relative foot pos - Based on https://c-motion.com/v3dwiki/index.php/Tutorial:_Gait_Events#Method_1._Coordinate_Based_Algorithm
  local_maxima <- which(diff(sign(diff(relFootPos_filtered))) == -2) + 1
  local_minima <- which(diff(sign(diff(relFootPos_filtered))) == 2) + 1
  if (local_minima[1] > local_maxima[1]) { # Always start with a minimum (toe-off)
    local_maxima <- local_maxima[-1]
  }

  # Check for minimum time difference between consecutive maxima and minima  (erronous detections)
  time_vector <- footData$time
  pos_vector <- relFootPos_filtered
  i <- 1 # Initialize indice
  min_time_diff <- 0.1
  min_pos_diff <- 0.0 # --> Removed pos threshold, because I'm not sure which extremes to remove.
  N_removed_time <- 0
  N_removed_pos <- 0
  while (i < length(local_maxima) && i < length(local_minima)) {
    # Check time difference
    time_diff_max_to_min <- abs(time_vector[local_maxima[i]] - time_vector[local_minima[i + 1]])
    time_diff_min_to_max <- abs(time_vector[local_minima[i]] - time_vector[local_maxima[i]])
    # Check distance difference
    pos_diff_max_to_min <- abs(pos_vector[local_maxima[i]] - pos_vector[local_minima[i + 1]])
    pos_diff_min_to_max <- abs(pos_vector[local_minima[i]] - pos_vector[local_maxima[i]])

    if (time_diff_max_to_min < min_time_diff) {
      local_minima <- local_minima[-(i + 1)]
      local_maxima <- local_maxima[-i]
      N_removed_time <- N_removed_time + 1
    } else if (time_diff_min_to_max < min_time_diff) {
      local_maxima <- local_maxima[-i]
      local_minima <- local_minima[-i]
      N_removed_time <- N_removed_time + 1
    } else if (pos_diff_max_to_min < min_pos_diff) { # we need to nest these, to prevent removing twice in 1 loop
      local_minima <- local_minima[-(i + 1)]
      local_maxima <- local_maxima[-i]
      N_removed_pos <- N_removed_pos + 1
    } else if (pos_diff_min_to_max < min_pos_diff) {
      local_maxima <- local_maxima[-i]
      local_minima <- local_minima[-i]
      N_removed_pos <- N_removed_pos + 1
    } else {
      # Move to the next pair if both time and pos differences are sufficient
      i <- i + 1
    }
  }
  
  # Heelstrike only in front of hip, toe-off only behind hip
  count_wrong_side_of_hip <- length(relFootPos[local_maxima] > 0) + length(relFootPos[local_maxima] > 0)
  local_maxima <- local_maxima[relFootPos[local_maxima] > 0]
  local_minima <- local_minima[relFootPos[local_minima] < 0]
  
  # alternation checking
  N_removed_min <- 0
  N_removed_max <- 0
  for (i in 1:(length(local_minima))) {
    if (i <= length(local_minima) && i <= length(local_maxima)) {
      while (local_maxima[i] < local_minima[i] && i <= length(local_minima) && i <= length(local_maxima)) {
        local_maxima <- local_maxima[-i] # remove the maximum, it is wrong
        N_removed_min <- N_removed_min + 1
      }
    }

    if (i + 1 <= length(local_minima) && i <= length(local_maxima)) {
      while (local_maxima[i] > local_minima[i + 1] && i + 1 <= length(local_minima) && i <= length(local_maxima)) {
        local_minima <- local_minima[-(i + 1)] # remove the minimum, it is wrong
        N_removed_max <- N_removed_max + 1
      }
    }
  }

  # Make sure lengths match
  lMax <- length(local_maxima)
  lMin <- length(local_minima)
  if (lMax != lMin) {
    if (lMax > lMin) {
      print("Something REALLY WRONG... check detect_foot_events_coordinates()")
    }

    trimLength <- min(lMax, lMin)
    local_maxima <- local_maxima[1:trimLength]
    local_minima <- local_minima[1:trimLength]
  }

  # Some logging
  if (N_removed_time > 0) {
    print(paste("removed", N_removed_time, "max+min due to time constraint."))
  }
  if (N_removed_pos > 0) {
    print(paste("removed", N_removed_pos, "max+min due to pos difference constraint."))
  }
  if (count_wrong_side_of_hip > 0) {
    print(paste("removed", count_wrong_side_of_hip, " extrema due to wrong side of hip"))
  }
  if (N_removed_max + N_removed_min > 0) {
  print(paste("removed", N_removed_max, "maxima, and", N_removed_min, "minima due to wrong alternation."))
  }


  if (length(local_maxima) != length(local_minima)) {
    print(paste("WARNING: Length maxima:", length(local_maxima), "Length minima:", length(local_minima)))
  }

  # Extract positions and times - use UNFILTERED footData
  heelStrikes <- data.frame(footData[local_maxima, ])
  toeOffs <- data.frame(footData[local_minima, ])
  print(paste("--- ---totalsteps: ", length(heelStrikes$time)))
  return(list(heelStrikes = heelStrikes, toeOffs = toeOffs))
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
  footEventsLeft$heelStrikes$foot <- "Left"
  footEventsLeft$toeOffs$foot <- "Left"
  footEventsRight$heelStrikes$foot <- "Right"
  footEventsRight$toeOffs$foot <- "Right"

  # Combine heel strikes and foot lifts from both feet
  combinedHeelStrikes <- rbind(footEventsLeft$heelStrikes, footEventsRight$heelStrikes)
  combinedToeOffs <- rbind(footEventsLeft$toeOffs, footEventsRight$toeOffs)

  # Order the events by time
  combinedHeelStrikes <- combinedHeelStrikes[order(combinedHeelStrikes$time), ]
  combinedToeOffs <- combinedToeOffs[order(combinedToeOffs$time), ]

  ensure_alternation <- function(data1, data2) {
    incorrect_seq <- which(diff(as.numeric(data1$foot == "Left")) == 0)
    
    if (length(incorrect_seq > 0)) {
      # print(data1[c(incorrect_seq,incorrect_seq+1),])
      data1 <- data1[-(incorrect_seq + 1), ] # remove the second (later) value
      data2 <- data2[-(incorrect_seq + 1), ] # remove the second (later) value
      print(paste("--- ---removed", length(incorrect_seq), "steps total (wrong alternation). Removing at place:", incorrect_seq))
      # print("----------------------")
    }
    return(list(data1 = data1, data2 = data2))
  }

  # Apply alternation check on heelStrikes and remove index columns
  results <- ensure_alternation(combinedHeelStrikes, combinedToeOffs)
  combinedHeelStrikes <- results$data1
  combinedToeOffs <- results$data2

  # Label step numbers. Assuming each heel strike represents a new step
  combinedHeelStrikes$step <- seq_len(nrow(combinedHeelStrikes))
  combinedToeOffs$step <- seq_len(nrow(combinedToeOffs))

  # Check if we stepped onto a target
  targetData <- targetData[order(targetData$time), ] # make sure we order by time
  combinedHeelStrikes$target <- FALSE
  for (i in 2:nrow(combinedHeelStrikes)) {
    prevStepTime <- combinedHeelStrikes$time[i - 1]
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
      column # Return non-numeric columns unchanged
    }
  })

  # time-based
  stepTimes <- relHeelStrikesData$time # Calculate step times  >>> NOTE: The first heelstrike is only used as a starting point to the second
  swingTimes <- heelStrikesData$time - toeOffsData$time # Calculate swing times <<< L = N   (not N-1)
  stanceTimes <- stepTimes - swingTimes # Calculate stance times

  # position-based
  stepWidths <- relHeelStrikesData$pos_x # Calculate step width
  stepWidths <- ifelse(relHeelStrikesData$foot == "Left", stepWidths * -1, stepWidths) # Adjust sign based on which foot is stepping
  stepLengths <- relHeelStrikesData$actual_pos_z # Calculate step lengths
  speed <- stepLengths / stepTimes # Calculate speed

  # We ignore the steps onto and away from a target
  targetSteps <- heelStrikesData$target
  targetSteps <- targetSteps | lead(heelStrikesData$target, default = FALSE) # add step before
  targetSteps <- targetSteps | lag(heelStrikesData$target, default = FALSE) | lag(heelStrikesData$target, 2, default = FALSE) | lag(heelStrikesData$target, 3, default = FALSE) # add 3 steps after (until foot is placed back onto new position)
  heelStrikesData$targetIgnoreSteps <- targetSteps

  # Detect outliers
  # First, we throw out some incorrect steps that are just physically impossible
  heelStrikesData$incorrectDetection <- stepLengths > 2.0 | stepLengths <= 0.0 | stepWidths > 0.5 | stepWidths < -0.0
  heelStrikesData$incorrectDetection[heelStrikesData$targetIgnoreSteps] <- FALSE # prevent overlap

  # and also add to the ignore list the outliers in the step speed
  alreadyIgnoredSteps <- targetSteps | heelStrikesData$incorrectDetection # we define those because we don't want to use them for calculating the IQRs for the outlier detection
  IQR_mlp <- 3.0
  heelStrikesData$outlierSteps <- detect_outliers(speed, alreadyIgnoredSteps, IQR_mlp) | detect_outliers(stepTimes, alreadyIgnoredSteps, IQR_mlp)

  # Make a list of all the gait parameters
  gaitParams <- list(
    stepTimes = stepTimes,
    # stanceTimes = stanceTimes, ####### These are not working well atm. Toe-offs are not filtered properly
    # swingStanceRatio = swingTimes / stanceTimes,
    # swingTimes = swingTimes,
    # finalStepWidths = finalStepWidths,
    stepLengths = stepLengths,
    # finalStepLengths = finalStepLengths,
    stepWidths = stepWidths,
    speed = speed,
    heelStrikes = heelStrikesData,
    toeOffs = toeOffsData,
    relHeelStrikes = relHeelStrikesData
  )

  gaitParams <- lapply(gaitParams, function(x) {
    # Check if the element is a vector or data frame
    if (is.vector(x)) {
      # Remove the first element for vectors
      x[-1]
    } else if (is.data.frame(x)) {
      # Remove the first row for data frames
      x[-1, ]
    } else {
      x # Return the element unchanged if it is not a vector or data frame
    }
  })

  return(gaitParams)
}


calculate_target_data <- function(participant, trial) {
  # get the target tracker results
  targetData <- get_t_data(participant, "steptargets", trial)
  targetData$rel_x <- targetData$foot_x - targetData$target_x
  targetData$rel_z <- targetData$foot_z - targetData$target_z

  # calculate total target distance
  targetData$targetDist <- sqrt(targetData$rel_x^2 + targetData$rel_z^2)

  return(targetData)
}
