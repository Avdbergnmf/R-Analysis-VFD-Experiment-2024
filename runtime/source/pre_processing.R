#### Some pre-processing we use later in plotting and get_foot_events

adjust_times <- function(dataset, minTime, maxTime = 180) { # make sure we start at t=0
  dataset$time <- dataset$time - minTime
  dataset <- subset(dataset, time <= maxTime) # dataset <- subset(dataset)
  return(dataset)
}

preprocess_data <- function(participant, trialNum) {
  leftFoot <- get_t_data(participant, "leftfoot", trialNum)
  rightFoot <- get_t_data(participant, "rightfoot", trialNum)
  hip <- get_t_data(participant, "hip", trialNum)

  targetData <- get_t_data(participant, "steptargets", trialNum)
  leftDisturbance <- get_t_data(participant, "leftdisturbance", trialNum)
  rightDisturbance <- get_t_data(participant, "rightdisturbance", trialNum)
  rightFoot <- calc_final_pos(rightFoot, rightDisturbance)
  leftFoot <- calc_final_pos(leftFoot, leftDisturbance)
  minTime <- leftFoot$time[1] # get_p_results(participant,"start_time",trialNum)

  moveSpeed <- get_move_speed(participant)
  maxTime <- ifelse(get_p_results(participant, "practice", trialNum) == "True", 120, 180)
  leftFoot <- adjust_times(leftFoot, minTime, maxTime)
  leftFoot$actual_pos_z <- leftFoot$pos_z + moveSpeed * leftFoot$time
  rightFoot <- adjust_times(rightFoot, minTime, maxTime)
  rightFoot$actual_pos_z <- rightFoot$pos_z + moveSpeed * rightFoot$time

  data <- list(
    leftFoot = leftFoot,
    rightFoot = rightFoot,
    leftDisturbance = adjust_times(leftDisturbance, minTime, maxTime),
    rightDisturbance = adjust_times(rightDisturbance, minTime, maxTime),
    hip = adjust_times(hip, minTime, maxTime),
    targetData = adjust_times(targetData, minTime, maxTime)
  )

  data <- hardcoded_rotation_fix(data, participant, trialNum)
  return(data)
}

######## Added this later as a hardcode fix to get my data rotated properly, because with one of the participants, this was necessary
hardcoded_rotation_fix <- function(data, participant, trialNum) {
  if (participant == "15" && trialNum %in% c(5, 6)) { # saw these trials needed a 4 deg rotation
    return(rotate_preprocessed_data(data, -4 * pi / 180))
  } else {
    return(data)
  }
}

rotate_preprocessed_data <- function(data, rotation) {
  # Apply the rotation to each dataset in the list
  data$leftFoot <- rotate_y(data$leftFoot, rotation)
  data$rightFoot <- rotate_y(data$rightFoot, rotation)
  data$hip <- rotate_y(data$hip, rotation)
  return(data)
}

# one of the datasets has a wrong rotation in some of the trials, we correct that with this function.
rotate_y <- function(data, theta) {
  # Create the rotation matrix
  rotation_matrix <- matrix(
    c(
      cos(theta), 0, sin(theta),
      0, 1, 0,
      -sin(theta), 0, cos(theta)
    ),
    nrow = 3, byrow = TRUE
  )

  # Apply the rotation to position columns
  positions <- as.matrix(data[, c("pos_x", "pos_y", "pos_z")])
  rotated_positions <- positions %*% t(rotation_matrix)

  # Update the dataframe with rotated positions
  data$pos_x <- rotated_positions[, 1]
  data$pos_y <- rotated_positions[, 2]
  data$pos_z <- rotated_positions[, 3]

  # Optionally, if you need to rotate orientation vectors (rot_x, rot_y, rot_z)
  rotations <- as.matrix(data[, c("rot_x", "rot_y", "rot_z")])
  rotated_rotations <- rotations %*% t(rotation_matrix)

  data$rot_x <- rotated_rotations[, 1]
  data$rot_y <- rotated_rotations[, 2]
  data$rot_z <- rotated_rotations[, 3]

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

# lof_outliers <- function(data, ignoreSteps, threshold = 2.5) {
#   k <- 3 # LOF parameter, number of neighbors

#   # Apply LOF only to non-ignored steps
#   lof_scores <- rep(NA, nrow(data)) # Initialize lof_scores for all data
#   lof_scores[!ignoreSteps] <- lof(data[!ignoreSteps, ], k)

#   # Identify outliers based on the threshold, only for non-ignored steps
#   outliers <- rep(FALSE, nrow(data)) # Initialize outliers for all data
#   outliers[!ignoreSteps] <- lof_scores[!ignoreSteps] > threshold

#   return(outliers)
# }



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
      included_steps = sum(!heelStrikes.incorrectDetection & !heelStrikes.targetIgnoreSteps & !heelStrikes.outlierSteps),
      removed_steps = sum(heelStrikes.incorrectDetection | heelStrikes.outlierSteps),
      target_steps = sum(heelStrikes.targetIgnoreSteps),
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

  # Calculate the average and standard deviation for target steps
  target_steps_avg <- mean(step_summary$target_steps)
  target_steps_sd <- sd(step_summary$target_steps)

  # Create a result data frame
  result <- data.frame(
    Metric = c("Total Steps", "Included Steps", "Removed Steps", "Target Steps"),
    Average = c(total_steps_avg, included_steps_avg, removed_steps_avg, target_steps_avg),
    SD = c(total_steps_sd, included_steps_sd, removed_steps_sd, target_steps_sd)
  )

  return(result)
}
