################ PATH DEFINITIONS ################ 
dataFolder <- file.path(".", "data")
participants <- list.dirs(path = dataFolder, full.names = FALSE, recursive = FALSE)

# Path definitions
questionnaireInfoFolder <- file.path(".", "questionnaires")
trackerPath <- file.path(file.path(dataFolder, participants[1]), "trackers")
#trackers <- list.files(path = trackerPath, full.names = FALSE)
filenameDict <- list(
  "camera" = "camera_movement_T",
  "hip" = "hiptracker_movement_T",
  "leftfoot" = "leftfoottracker_movement_T",
  "lefthand" = "lefthandtracker_movement_T",
  "rightfoot" = "rightfoottracker_movement_T",
  "righthand" = "righthandtracker_movement_T",
  "steptargets" = "steptargetsmanager_targetsteps_T",
  "treadmillleft" = "treadmillleft_movement_T",
  "treadmillright" = "treadmillright_movement_T",
  "treadmillback" = "treadmillrightback_movement_T",
  "eye" = "eyetracking_EyeGaze_T",
  "leftdisturbance" = "leftfoot_disturbance_noise_T",
  "rightdisturbance" = "rightfoot_disturbance_noise_T"
)

trackers <- names(filenameDict)

################ Data retrieval / helper methods ################ 

# Data retrieval functions
get_p_dir <- function(pnum) {
  return(file.path(dataFolder, pnum))
}

get_p_resultsFile <- function(pnum) {
  return(file.path(get_p_dir(pnum), "trial_results.csv"))
}

get_p_results <- function(pnum, settingName, trialNumber) {
  # get the path to the settings file for the participant
  resultsFile <- get_p_resultsFile(pnum)
  results <- read.csv(resultsFile)
  
  # retrieve the value of the specific detail
  resultValue <- results[[settingName]][trialNumber]
  
  return(resultValue)
}

get_move_speed <- function(pnum) { # return move speed in m/s
  trialNum = 1 # should be the same for all trials
  return(get_p_results(pnum,"move_speed",trialNum) / 3.6)
}

get_p_detail <- function(pnum, detailName) {
  # get the path to the details file for the participant
  detailsFile <- file.path(get_p_dir(pnum), "participant_details.csv")
  
  # read the csv file into a data frame
  details <- read.csv(detailsFile)
  
  # retrieve the value of the specific detail
  detailValue <- details[[detailName]][1]
  
  return(detailValue)
}

# If true, the participant started with noise VFD enabled, otherwise without it enabled
started_with_noise <- function(pnum) {
  secondTrialHasNoise <- get_p_results(pnum,"noise_enabled",2) == "True"
  return(secondTrialHasNoise)
}

# get any type of data
get_t_data <- function(pnum, trackerType, trialNum) {
  # Validate trackerType
  if (!trackerType %in% names(filenameDict)) {
    stop("Invalid tracker type specified.")
  }
  
  filename <- paste0(filenameDict[[trackerType]], sprintf("%03d", trialNum), ".csv")
  filePath <- file.path(get_p_dir(pnum), "trackers", filename)
  
  # Use tryCatch for more robust error handling
  tryCatch(
    {
      data <- read.csv(filePath)
    },
    error = function(e) {
      message("Failed to read the file: ", e$message)
      return(NULL) # Or handle the error as appropriate for your context
    }
  )
  
  return(data)
}

# For changing up the selection inputs - bit costly, but not really needed to optimize this
getOptions <- function(tracker){
  exampleData <- get_t_data(participants[1],tracker,1)
  numericTypes <- sapply(exampleData, is.numeric)
  numeric_cols <- names(exampleData[numericTypes]) # names(numericDataTypes[numericDataTypes | logicalDataTypes])
  return(numeric_cols)
}

get_q_file <- function(pnum, qType) { # qType = IMI / SSQ / VEQ
  return(file.path(get_p_dir(pnum), "Questionnaires", paste0("questionnaireID_", qType, "_ALL_answers.csv")))
}

################ Questionnaires ################

get_q_data <- function(pnum, qType) {
  # Get the path to the questionnaire file for the participant
  questionnaireFile <- get_q_file(pnum, qType)
  
  # Read the CSV file into a data frame
  questionnaire <- read.csv(questionnaireFile)
  
  # Extract the QuestionID and the two answer columns
  result <- questionnaire[, c("QuestionID", "Answer_Participant__condition_Base", "Answer_Participant__condition_Noise")]
  
  return(result)
}

get_question_info <- function(qType) { # qType = IMI / SSQ / VEQ
  qInfopath <- file.path(questionnaireInfoFolder, paste0(qType, ".csv"))
  # Read the CSV file into a data frame
  questionnaire <- read.csv(qInfopath)
  return(questionnaire)
}

### Questionnaire results calculation
compute_scores <- function(pnum, qType) {
  qdata <- get_q_data(pnum, qType)
  
  qinfo <- get_question_info(qType)
  combined <- merge(qdata, qinfo, by = "QuestionID")
  
  # Mirror the scores if needed
  combined$Answer_Participant__condition_Base <- ifelse(combined$mirror, 8 - combined$Answer_Participant__condition_Base, combined$Answer_Participant__condition_Base)
  combined$Answer_Participant__condition_Noise <- ifelse(combined$mirror, 8 - combined$Answer_Participant__condition_Noise, combined$Answer_Participant__condition_Noise)
  
  # Compute the scores for each category
  scoresBase <- tapply(combined$Answer_Participant__condition_Base, combined$category, mean, na.rm = TRUE)
  scoresNoise <- tapply(combined$Answer_Participant__condition_Noise, combined$category, mean, na.rm = TRUE)
  
  # Compute the total score for each condition
  scoresBase["total"] <- mean(scoresBase, na.rm = TRUE)
  scoresNoise["total"] <- mean(scoresNoise, na.rm = TRUE)
  
  return(list(base = scoresBase, noise = scoresNoise))
}

calculate_all_scores <- function(qType) {
  # Initialize empty data frames to hold the results
  dfBase <- data.frame()
  dfNoise <- data.frame()
  
  # Iterate over the participants
  for (participant in participants) {
    # Compute the scores
    scores <- compute_scores(participant, qType)
    
    # Transform the scores into a data frame with a single row and bind it with the participant ID
    baseRow <- cbind(participant = participant, as.data.frame(t(scores$base)))
    noiseRow <- cbind(participant = participant, as.data.frame(t(scores$noise)))
    
    # Add the scores to the data frames
    dfBase <- rbind(dfBase, baseRow)
    dfNoise <- rbind(dfNoise, noiseRow)
  }
  
  allScores <- list(base = dfBase, noise = dfNoise)
  
  # Reshape the data somewhat
  allScores$base$VFD <- FALSE
  allScores$noise$VFD <- TRUE
  
  combinedData <- rbind(allScores$noise, allScores$base)
  
  return(combinedData)
}

get_all_questionnaire_results <- function() {
  # Calculate all scores of all participants
  allQs <- c("IMI", "VEQ", "SSQ")
  # Initialize allQResults with the first questionnaire to establish a base for merging
  initialData <- calculate_all_scores(allQs[1])[, c("participant", "total", "VFD")]
  names(initialData)[names(initialData) == "total"] <- allQs[1]
  allQResults <- initialData
  
  # Loop through the remaining questionnaires
  for (currQ in allQs[-1]) {
    # Calculate scores for current questionnaire
    qData <- calculate_all_scores(currQ)[, c("participant", "total", "VFD")]
    names(qData)[names(qData) == "total"] <- currQ  # Rename 'total' to reflect questionnaire name
    
    # Merge with allQResults based on 'participant' and 'VFD'
    allQResults <- merge(allQResults, qData, by = c("participant", "VFD"), all = TRUE)
  }
  return(allQResults)
}

#### Some pre-processing we use later in plotting and get_foot_events

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
