################ PATH DEFINITIONS ################
dataFolder <- "data"
dataExtraFolder <- "data_extra"
questionnaireInfoFolder <- "questionnaires"

participants <- list.dirs(path = dataFolder, full.names = FALSE, recursive = FALSE)
trackerPath <- file.path(file.path(dataFolder, participants[1]), "trackers")
filenameDict <- read.csv(file.path(dataExtraFolder, "filenameDict.csv"), stringsAsFactors = FALSE)
filenameDict <- setNames(as.list(filenameDict[[2]]), filenameDict[[1]])

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
  trialNum <- 1 # should be the same for all trials
  return(get_p_results(pnum, "move_speed", trialNum) / 3.6)
}

get_p_detail <- function(pnum, detail) {
  # get the path to the details file for the participant
  detailsFile <- file.path(get_p_dir(pnum), "session_info/participant_details.csv")

  # read the csv file into a data frame
  details <- read.csv(detailsFile)

  # retrieve the value of the specific detail
  detailValue <- details[[detail]][1]

  return(detailValue)
}

calculate_participant_details <- function(participants) {
  details <- data.frame(participant = participants)
  
  # Retrieve details for each participant
  details$age <- sapply(participants, get_p_detail, detail = "age")
  details$gender <- sapply(participants, get_p_detail, detail = "gender")
  details$startedWithNoise <- sapply(participants, started_with_noise)
  details$education <- sapply(participants, get_p_detail, detail = "education")
  details$vr_experience <- sapply(participants, get_p_detail, detail = "vr_experience")
  details$motion <- sapply(participants, get_p_detail, detail = "motion")
  details$noticed <- sapply(participants, get_p_detail, detail = "noticed")
  details$move_speed <- sapply(participants, get_move_speed)
  
  # Calculate statistics for age
  age_mean <- mean(details$age)
  age_sd <- sd(details$age)
  age_median <- median(details$age)
  age_min <- min(details$age)
  age_max <- max(details$age)
  age_iqr <- IQR(details$age)
  
  # Format the age results
  age_mean_sd <- sprintf("%.2f, SD: %.2f", age_mean, age_sd)
  age_median_min_max_iqr <- sprintf("%d [%.0f, %.0f], IQR=%.2f", age_median, age_min, age_max, age_iqr)
  
  # Calculate statistics for move speed
  move_speed_mean <- mean(details$move_speed)
  move_speed_sd <- sd(details$move_speed)
  move_speed_median <- median(details$move_speed)
  move_speed_min <- min(details$move_speed)
  move_speed_max <- max(details$move_speed)
  move_speed_iqr <- IQR(details$move_speed)
  
  # Format the move speed results
  move_speed_mean_sd <- sprintf("%.2f, SD: %.2f", move_speed_mean, move_speed_sd)
  move_speed_median_min_max_iqr <- sprintf("%.2f [%.2f, %.2f], IQR=%.2f", move_speed_median, move_speed_min, move_speed_max, move_speed_iqr)
  
  # Count the occurrences of each unique value in other columns
  gender_counts <- table(details$gender)
  started_counts <- table(details$startedWithNoise)
  education_counts <- table(details$education)
  vr_experience_counts <- table(details$vr_experience)
  motion_counts <- table(details$motion)
  noticed_counts <- table(details$noticed)
  
  # Create a result data frame
  result <- data.frame(
    Detail = c(
      "Age (Mean, SD)", "Age (Median [Min, Max], IQR)",
      "Move Speed (Mean, SD)", "Move Speed (Median [Min, Max], IQR)",
      rep("Gender", length(gender_counts)),
      rep("Started With Noise", length(started_counts)),
      rep("Education", length(education_counts)),
      rep("VR Experience", length(vr_experience_counts)),
      rep("Motion", length(motion_counts)),
      rep("Noticed", length(noticed_counts))
    ),
    Value = c(
      age_mean_sd, age_median_min_max_iqr,
      move_speed_mean_sd, move_speed_median_min_max_iqr,
      as.vector(gender_counts),
      as.vector(started_counts),
      as.vector(education_counts),
      as.vector(vr_experience_counts),
      as.vector(motion_counts),
      as.vector(noticed_counts)
    )
  )
  
  # Add the names of the counted categories to the details
  result$Category <- c(
    "", "", "", "",
    names(gender_counts),
    names(started_counts),
    names(education_counts),
    names(vr_experience_counts),
    names(motion_counts),
    names(noticed_counts)
  )
  
  return(result)
}

# If true, the participant started with noise VFD enabled, otherwise without it enabled
started_with_noise <- function(pnum) {
  secondTrialHasNoise <- get_p_results(pnum, "noise_enabled", 2) == "True"
  return(secondTrialHasNoise)
}

# did participant notice the VFD?
noticed_vfd <- function(participant) {
  return(get_p_detail(participant, "noticed") == "True")
}

# get any type of data
get_t_data <- function(pnum, trackerType, trialNum) {
  print(filenameDict)
  # Validate trackerType
  if (!trackerType %in% names(filenameDict)) {
    stop("Invalid tracker type specified.")
  }

  filename <- paste0(filenameDict[[trackerType]],"_T", sprintf("%03d", trialNum), ".csv")
  filePath <- file.path(get_p_dir(pnum), "trackers", filename)
  print(filePath)
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
getOptions <- function(tracker) {
  exampleData <- get_t_data(participants[1], tracker, 1)
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

get_question_weights <- function(qType) { # qType = IMI / SSQ / VEQ
  qpath <- file.path(questionnaireInfoFolder, paste0(qType, "_weights.csv"))
  # Read the CSV file into a data frame
  questionnaire <- read.csv(qpath)
  return(questionnaire)
}
