#### Column definitions ####
trackerPrefixes <- c("LeftFoot", "RightFoot", "TreadmillLeft", "TreadmillRight")
travelingTrackerPrefixes <- c("LeftFoot", "RightFoot")

#### Some pre-processing we use later in plotting and get_foot_events

adjust_time <- function(dataset, minTime, maxTime = 120) { # make sure we start at t=0, and trim to maxTime
  dataset$time <- dataset$time - minTime
  dataset <- subset(dataset, time >= 0 & time <= maxTime)
  return(dataset)
}

find_trial_times <- function(participant, trialNum) {
  # Retrieve trial start and end times from loko_lokodata
  lokoData <- get_t_data(participant, "loko", trialNum)

  # Find indices where trialStarted transitions from 0 to 1 (start) and 1 to 0 (end)
  transitions <- diff(lokoData$trialStarted)
  startIndices <- which(transitions == 1)
  endIndices <- which(transitions == -1)
  # Always taking the last trial: sometimes a crash happened and the trial was fully re-done within the same dataset
  startIndex <- startIndices[length(startIndices)]
  endIndex <- endIndices[length(endIndices)]
  # Ensure that both start and end indices are found
  if (is.na(startIndex) || is.na(endIndex)) {
    stop("Could not find start or end of the trial.")
  }

  # Extracting time and Lokotime at the start and end of the trial
  trialStartTime <- lokoData$time[startIndex]
  trialStartLokotime <- lokoData$Lokotime[startIndex]
  trialEndTime <- lokoData$time[endIndex]
  trialEndLokotime <- lokoData$Lokotime[endIndex]

  # Make sure the data is 120 seconds
  lengthTrial <- trialEndTime - trialStartTime
  if (lengthTrial < 119) { # Taking 1 second of play to account for sampling frequency / delays
    cat("WARNING: Participant: ", participant, " Trial: ", trialNum, " has trial length of only ", lengthTrial, "\n\n")
  } else if (lengthTrial > 121) { # Taking 1 second of play to account for sampling frequency / delays
    cat("Participant: ", participant, " Trial: ", trialNum, " trial length of ", lengthTrial, ". Trimming to 120 seconds.\n\n")
    trialEndTime <- trialStartTime + 120
    trialEndLokotime <- trialStartLokotime + 120 # this time isn't being used but lets be consistent and trim it too.
  }

  # Save the start and end times for later use or analysis
  trialTimes <- data.frame(
    Start_Time = trialStartTime,
    Start_Lokotime = trialStartLokotime,
    End_Time = trialEndTime,
    End_Lokotime = trialEndLokotime # not used but good to have
  )

  return(trialTimes)
}

slerp <- function(q1, q2, t) {
  cosTheta <- sum(q1 * q2)
  if (cosTheta < 0) {
    q1 <- -q1
    cosTheta <- -cosTheta
  }

  if (cosTheta > 0.95) {
    # If the quaternions are very close, linearly interpolate and normalize the result
    result <- (1 - t) * q1 + t * q2
    return(result / sqrt(sum(result * result)))
  }

  theta <- acos(cosTheta)
  sinTheta <- sqrt(1 - cosTheta * cosTheta)

  return((sin((1 - t) * theta) * q1 + sin(t * theta) * q2) / sinTheta)
}

interpolate_quaternions <- function(data, quatCols, artifact_indices) {
  if (length(artifact_indices) == 0) {
    return(data) # No artifacts to interpolate
  }

  ### Interpolating this data takes too much compute, but we don't need rotations anyway so lets ignore this problem for now.
  data[, quatCols] <- na.approx(data[, quatCols], na.rm = FALSE)
  return(data) ### For now, we do linear interpolation, which is way faster but not actually accurate
}

##################### CHECK THIS #####################
filter_artifacts <- function(data, trackerName, velocity_threshold = 10, rotational_velocity_threshold = 3, passes = 1) { # velocity_threshold in m/s , rotational_velocity_threshold in rad/s
  # print(paste("Filtering tracker data:", trackerName))
  pattern <- paste0("^", trackerName) # get just the position data
  trackerCols <- grep(pattern, colnames(data), value = TRUE) # Get the column names that match the pattern
  posCols <- grep("\\.pos", trackerCols, value = TRUE) # only the position columns (these are used to get the velocity)
  quatCols <- grep("\\.rot", trackerCols, value = TRUE) # Quaternion columns

  if (length(posCols) != 3 || length(quatCols) != 4) {
    stop("Error: Incorrect number of position or quaternion columns.")
  }

  # Now, we remove all the datapoints where the tracker got disconnected (at least one of the values gets set to 0)
  artifact_indices <- c()
  THRESHOLD <- 0.00001 # To account for floating point inaccuracies
  for (col in trackerCols) {
    artifact_indices <- c(artifact_indices, which(abs(data[[col]]) <= THRESHOLD))
  }
  # mark the datapoints
  artifact_indices <- sort(unique(artifact_indices))
  # data$artifact[artifact_indices] <- trackerName
  # filter them out
  data[artifact_indices, trackerCols] <- NA
  data[, posCols] <- na.approx(data[, posCols], na.rm = FALSE) # approx position
  data <- interpolate_quaternions(data, quatCols, artifact_indices) # approx rotation

  # We downsample our data and apply a simple low pass filter
  fixedRate <- 100 # Target fixed sampling rate in Hz
  newTime <- seq(from = min(data$time), to = max(data$time), by = 1 / fixedRate)
  resampledData <- data.frame(time = newTime)

  peakThreshold <- 0.01 # Define a threshold for peak detection
  for (col in colnames(data)) {
    if (col != "time" && is.numeric(data[[col]])) {
      if (sum(!is.na(data[[col]])) >= 2) {
        # Resample the data
        resampled <- na.approx(data[[col]], x = data$time, xout = newTime, na.rm = FALSE)

        # Apply a rolling median or mean
        smoothed <- rollapply(resampled, width = 5, FUN = median, fill = NA, align = "center")

        # Identify and replace peaks, handling NAs
        isPeak <- !is.na(resampled) & !is.na(smoothed) & (abs(resampled - smoothed) > peakThreshold)
        resampled[isPeak] <- smoothed[isPeak]
        # print(length(sum(isPeak)))
        resampledData[[col]] <- resampled
      } else {
        resampledData[[col]] <- rep(NA, length(newTime))
      }
    }
  }
  data <- resampledData
  for (i in 1:passes) {
    tempData <- data

    # Calculate positional differences
    diffs <- data.frame(lapply(tempData[, posCols], function(col) c(NA, diff(col))))
    names(diffs) <- paste0("diff_", posCols)
    diffs$diff_time <- c(NA, diff(data$time))

    # Calculate quaternion differences and rotational velocity
    quat_current <- tempData[, quatCols]
    quat_next <- rbind(quat_current[-1, ], rep(NA, length(quatCols)))
    dot_product <- rowSums(quat_current * quat_next, na.rm = FALSE)

    # Clamping dot product within the range [-1, 1] to avoid NaNs
    dot_product <- pmin(pmax(dot_product, -1), 1)
    rotational_diff <- 2 * acos(dot_product)
    rotational_velocity <- c(NA, rotational_diff[-1] / diffs$diff_time[-1]) # Skip first and align lengths

    # Combine diffs with data and calculate linear velocity
    tempData <- cbind(tempData, diffs)
    tempData$distance <- sqrt(rowSums(tempData[, paste0("diff_", posCols)]^2))
    tempData$velocity <- tempData$distance / tempData$diff_time
    tempData$rotational_velocity <- rotational_velocity

    # Identify velocity artifacts
    velocity_artifact_indices <- which(tempData$velocity > velocity_threshold)
    rotational_velocity_artifact_indices <- which(tempData$rotational_velocity > rotational_velocity_threshold)
    combined_artifact_indices <- unique(c(velocity_artifact_indices, rotational_velocity_artifact_indices))

    # Interpolate (use original data)
    data[combined_artifact_indices, trackerCols] <- NA
    data[, posCols] <- na.approx(data[, posCols], na.rm = FALSE)
    data <- interpolate_quaternions(data, quatCols, combined_artifact_indices)
  }

  return(data)
}

filter_all_data <- function(data, velocity_threshold = 100, rotational_velocity_threshold = 3000, passes = 1) {
  # Process and filter each dataset with a unique artifact label  --> Note, if 2 datasets both have an artefact at the same time frame, only the last one here will be saved.
  for (prefix in trackerPrefixes) {
    data <- filter_artifacts(data, prefix, velocity_threshold, rotational_velocity_threshold, passes)
  }

  return(data)
}

######### ROTATION CORRECTION #########

rotate_y <- function(data, theta_deg, prefixList) {
  theta <- theta_deg * pi / 180 # convert to radians
  # Create the rotation matrix
  rotation_matrix <- matrix(
    c(
      cos(theta), 0, sin(theta),
      0, 1, 0,
      -sin(theta), 0, cos(theta)
    ),
    nrow = 3, byrow = TRUE
  )

  # Select columns for this tracker
  pos_cols <- paste0(prefix, ".pos.", c("x", "y", "z"))
  rot_cols <- paste0(prefix, ".rot.", c("x", "y", "z"))

  # Apply the rotation to position columns
  positions <- as.matrix(data[, pos_cols])
  rotated_positions <- positions %*% t(rotation_matrix)

  # Update the dataframe with rotated positions
  data[, pos_cols] <- rotated_positions

  rotations <- as.matrix(data[, rot_cols])
  rotated_rotations <- rotations %*% t(rotation_matrix)
  data[, rot_cols] <- rotated_rotations

  return(data)
}

quaternion_to_rotation_matrix <- function(q) {
  # Ensure the quaternion is normalized
  q <- q / sqrt(sum(q^2))

  w <- q[1]
  x <- q[2]
  y <- q[3]
  z <- q[4]

  # Compute the elements of the rotation matrix
  rotMatrix <- matrix(c(
    1 - 2 * y^2 - 2 * z^2, 2 * x * y - 2 * z * w, 2 * x * z + 2 * y * w,
    2 * x * y + 2 * z * w, 1 - 2 * x^2 - 2 * z^2, 2 * y * z - 2 * x * w,
    2 * x * z - 2 * y * w, 2 * y * z + 2 * x * w, 1 - 2 * x^2 - 2 * y^2
  ), nrow = 3, byrow = TRUE)

  # Append a fourth row and column for homogeneous coordinates
  rotMatrix <- rbind(rotMatrix, c(0, 0, 0))
  rotMatrix <- cbind(rotMatrix, c(0, 0, 0, 1))

  return(rotMatrix)
}

average_quaternion <- function(quaternions) { # not sure if this makes sense
  # Ensure all quaternions are normalized
  normalizedQuaternions <- t(apply(quaternions, 1, function(q) {
    q / sqrt(sum(q^2))
  }))

  # Calculate the weighted average
  avgQ <- colMeans(normalizedQuaternions)

  # Normalize the result
  avgQ <- avgQ / sqrt(sum(avgQ^2))

  return(avgQ)
}

# Define a cross product function
vector_cross <- function(a, b) {
  c(
    a[2] * b[3] - a[3] * b[2],
    a[3] * b[1] - a[1] * b[3],
    a[1] * b[2] - a[2] * b[1]
  )
}

calculate_transformation_matrix <- function(data) {
  # Calculate the average position of the trackers over the first x frames
  # Calculate the means
  avgLeftPos <- sapply(data[, c("TreadmillLeft.pos.x", "TreadmillLeft.pos.y", "TreadmillLeft.pos.z")], mean, na.rm = TRUE)
  avgRightPos <- sapply(data[, c("TreadmillRight.pos.x", "TreadmillRight.pos.y", "TreadmillRight.pos.z")], mean, na.rm = TRUE)
  avgLeftRot <- sapply(data[, c("TreadmillLeft.rot.w", "TreadmillLeft.rot.x", "TreadmillLeft.rot.y", "TreadmillLeft.rot.z")], mean, na.rm = TRUE) # this should be done with slerp but for now this is fine.
  avgRightRot <- sapply(data[, c("TreadmillRight.rot.w", "TreadmillRight.rot.x", "TreadmillRight.rot.y", "TreadmillRight.rot.z")], mean, na.rm = TRUE) # this should be done with slerp but for now this is fine.
  avgLeftPos <- unname(avgLeftPos)
  avgRightPos <- unname(avgRightPos)

  # X Axis: Line between two trackers
  xAxis <- avgLeftPos - avgRightPos
  xAxis <- xAxis / sqrt(sum(xAxis^2)) # Normalize

  # Y Axis: Average 'up' direction of the trackers
  leftUp <- quaternion_to_rotation_matrix(avgLeftRot)[1:3, 3] # z axis of tracker is up
  rightUp <- quaternion_to_rotation_matrix(avgRightRot)[1:3, 3]
  yAxis <- (leftUp + rightUp) / 2
  yAxis <- yAxis / sqrt(sum(yAxis^2)) # Normalize

  # Project to ensure orthogonality
  projection <- sum(yAxis * xAxis) * xAxis # Project yAxis onto xAxis
  yAxisOrthogonal <- yAxis - projection # Subtract the projection from xAxis to get the orthogonal component
  yAxisOrthogonal <- yAxisOrthogonal / sqrt(sum(yAxisOrthogonal^2)) # Normalize the new X axis
  yAxis <- yAxisOrthogonal

  # Z Axis: Orthogonal to X and Y
  zAxis <- vector_cross(xAxis, yAxis)
  zAxis <- zAxis / sqrt(sum(zAxis^2)) # Normalize

  # Create the rotation matrix
  rotation_matrix <- matrix(c(xAxis, yAxis, zAxis), nrow = 3, ncol = 3)

  # Construct the translation vector
  avgPos <- -(avgLeftPos + avgRightPos) / 2
  translation_vector <- t(rotation_matrix) %*% avgPos
  translation_vector <- c(translation_vector[1], translation_vector[2], translation_vector[3])

  # Construct the full transformation matrix
  tmatrix <- rbind(cbind(rotation_matrix, translation_vector), c(translation_vector, 1))

  return(tmatrix)
}

transform_tracker_data <- function(data, trackerName, tmatrix) {
  pattern <- paste0("^", trackerName) # get just the position data
  trackerCols <- grep(pattern, colnames(data), value = TRUE) # Get the column names that match the pattern
  posCols <- grep("\\.pos", trackerCols, value = TRUE) # only the position columns (these are used to get the velocity)

  positions <- as.matrix(data[, posCols])
  positionsHomogeneous <- cbind(positions, matrix(1, nrow = nrow(positions), ncol = 1)) # Convert positions to homogeneous coordinates (x, y, z, 1)

  # Apply the transformation
  transformedPositions <- positionsHomogeneous %*% tmatrix
  data[, posCols] <- transformedPositions[, 1:3] # Only take the x, y, z components
  return(data)
}

transform_all_data <- function(data, tmatrix, trackerPrefixes) {
  for (tracker in trackerPrefixes) {
    data <- transform_tracker_data(data, tracker, tmatrix)
  }
  return(data)
}

preprocess_data <- function(participant, trialNum) {
  # Get the tracker data
  data <- get_t_data(participant, "vivetrackers", trialNum)

  # Get trial times
  trialTimes <- find_trial_times(participant, trialNum)
  data <- adjust_time(data, trialTimes$Start_Time, 120) # trialTimes$End_Time)

  # Some small fixes related to the buffer
  data <- data[data$time >= 0, ] # I believe when the device disconnects, or some problem happens with the buffer, the time defaults to some other value, which is always negative and can be filtered out like this.
  data <- data[order(data$time), ] # Due to problems with the buffer, sometimes data was loaded in incorrect order. We can simply order it by time to fix this
  # Further, there are some issues where sometimes one value is recorded twice, we filter these out as well as they cause problems when differentiating our timeseries.
  data <- data[!c(FALSE, diff(data$time) == 0), ]
  # Correct the swapped tracker names
  # This patch is required due to a mistake in data recording where TreadmillLeft and TreadmillRight trackers were swapped
  colnames(data) <- gsub("TreadmillLeft", "TempTreadmill", colnames(data)) # Temporarily rename TreadmillLeft to a placeholder
  colnames(data) <- gsub("TreadmillRight", "TreadmillLeft", colnames(data)) # Rename TreadmillRight to TreadmillLeft
  colnames(data) <- gsub("TempTreadmill", "TreadmillRight", colnames(data)) # Rename the placeholder to TreadmillRight  return(data)

  # Transform the data
  tmatrix <- calculate_transformation_matrix(data)
  data <- transform_all_data(data, tmatrix, trackerPrefixes)

  # Rotate the data if needed (rotate data around y axis to align with treadmill direction)
  rotations_file <- "./data_extra/rotations_kinematic_data.csv"
  if (file.exists(rotations_file)) {
    rotations_data <- read.csv(rotations_file)
    rotation <- rotations_data[rotations_data$participant == participant & rotations_data$trial == trialNum, "rotation"]
    if (length(rotation) == 0) {
      rotation <- rotations_data[rotations_data$participant == participant, "rotation"]
    }

    if (length(rotation) > 0) {
      rotation <- rotation[1]
      data <- rotate_y(data, rotation, trackerPrefixes)
    }
  }

  # Adjust times and add actual position
  moveSpeed <- get_move_speed(participant)
  data <- add_actual_z(data, moveSpeed, travelingTrackerPrefixes)

  return(data)
}

# You might need to update this function to work with the new data structure
add_actual_z <- function(data, moveSpeed, movingTrackerPrefixes) {
  for (prefix in movingTrackerPrefixes) {
    data[[paste0(prefix, ".actual_pos.z")]] <- data[[paste0(prefix, ".pos.z")]] + moveSpeed * data$time
  }
  return(data)
}

# Detect and remove outliers using a Modified z-score method
detect_outliers_modified_z_scores <- function(data, ignoreSteps = c(FALSE), threshold = 3.5) {
  data_filtered <- data[!ignoreSteps] # We don't use the target steps to calculate our med etc

  # Calculate the median of the column
  med <- median(data_filtered, na.rm = TRUE)

  # Calculate the Median Absolute Deviation (MAD)
  mad_value <- median(abs(data_filtered - med), na.rm = TRUE)

  # Calculate the Modified z-scores
  modified_z_scores <- 0.6745 * (data - med) / mad_value

  # Identify outliers based on the threshold
  outliers <- abs(modified_z_scores) > threshold

  return(outliers)
}

detect_outliers <- function(data, ignoreSteps, IQR_mlp = 1.5) {
  data_filtered <- data[!ignoreSteps] # We don't use the target steps to calculate our interquartile ranges.

  Q1 <- quantile(data_filtered, 0.25, na.rm = TRUE)
  Q3 <- quantile(data_filtered, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1

  # Define the upper and lower bounds for outliers
  upper_bound <- Q3 + IQR_mlp * IQR
  lower_bound <- Q1 - IQR_mlp * IQR

  return(!(data >= lower_bound & data <= upper_bound))
}

apply_padding_and_filter <- function(column, poly_order, fs, cutoff_freq = 5) {
  column <- hampel_filter(column, k = 7, t0 = 3) # this is kinda slow but makes step detection more consistent.

  # Calculate the number of points to pad (half the frame size generally works well)
  pad_width <- 20

  # Create mirrored padding
  padding_start <- rev(column[1:pad_width])
  padding_end <- rev(column[(length(column) - pad_width + 1):length(column)])

  # Pad the column
  padded_column <- c(padding_start, column, padding_end)

  # Apply Butterworth filter to the padded data
  b <- butter(poly_order, cutoff_freq / (fs / 2)) # 4th order Butterworth filter
  filtered_column <- filtfilt(b, padded_column)

  # Remove the padding
  filtered_column <- filtered_column[(pad_width + 1):(length(filtered_column) - pad_width)]

  return(filtered_column)
}

# Hampel filter function
hampel_filter <- function(x, k, t0 = 3) {
  n <- length(x)
  y <- x
  L <- 1.4826 # Scale factor for Gaussian distribution
  for (i in (k + 1):(n - k)) {
    window <- x[(i - k):(i + k)]
    median_window <- median(window)
    sigma_window <- L * median(abs(window - median_window))
    if (abs(x[i] - median_window) > t0 * sigma_window) {
      y[i] <- median_window
    }
  }
  return(y)
}

calculate_step_statistics <- function(data, group_vars = c("participant")) {
  # Group by specified variables and calculate the number of included and removed steps
  step_summary <- data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      total_steps = n(), # Use n() to count the number of rows
      included_steps = sum(!heelStrikes.incorrectDetection & !heelStrikes.outlierSteps),
      removed_steps = sum(heelStrikes.incorrectDetection | heelStrikes.outlierSteps),
      .groups = "drop"
    )

  # Calculate the average and standard deviation for included steps
  total_steps_avg <- mean(step_summary$total_steps)
  total_steps_sd <- sd(step_summary$total_steps)

  # Calculate the average and standard deviation for included steps
  included_steps_avg <- mean(step_summary$included_steps)
  included_steps_sd <- sd(step_summary$included_steps)

  # Calculate the average and standard deviation for removed steps
  removed_steps_avg <- mean(step_summary$removed_steps)
  removed_steps_sd <- sd(step_summary$removed_steps)

  # Create a result data frame
  result <- data.frame(
    Metric = c("Total Steps", "Included Steps", "Removed Steps"),
    Average = c(total_steps_avg, included_steps_avg, removed_steps_avg),
    SD = c(total_steps_sd, included_steps_sd, removed_steps_sd)
  )

  return(result)
}
