################ PATH DEFINITIONS ################
dataFolder <- "data"
dataExtraFolder <- "data_extra"
questionnaireInfoFolder <- "questionnaires"

participants <- list.dirs(path = dataFolder, full.names = FALSE, recursive = FALSE)
trackerPath <- file.path(file.path(dataFolder, participants[1]), "trackers")
filenameDict <- read.csv(file.path(dataExtraFolder,"filenameDict.csv"))
filenameDict <- setNames(filenameDict[[2]], filenameDict[[1]])

trackers <- names(filenameDict)

# List of all questionnaires
allQs <- "UserExperience" #c("IMI", "VEQ", "SSQ")
matchByIdentifiers <- "participant" #c("participant", "VFD") # used in questionnaire result calculation

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
  
  # List of possible details
  possible_details <- c("age", "gender", "height", "weight", "education", "vr_experience", "motion", "noticed")
  
  # Retrieve details for each participant if available
  for (detail in possible_details) {
    tryCatch({
      details[[detail]] <- sapply(participants, get_p_detail, detail = detail)
    }, error = function(e) {
      message(paste("Detail", detail, "not available for all participants. Skipping."))
    })
  }
  
  # Add startedWithNoise and move_speed separately as they use different functions
  tryCatch({
    details$startedWithNoise <- sapply(participants, started_with_noise)
  }, error = function(e) {
    message("startedWithNoise not available for all participants. Skipping.")
  })

  tryCatch({
    details$move_speed <- sapply(participants, get_move_speed)
  }, error = function(e) {
    message("move_speed not available for all participants. Skipping.")
  })
  
  # Calculate statistics for available numeric columns
  numeric_cols <- sapply(details, is.numeric)
  result <- data.frame(Detail = character(), Value = character(), Category = character())
  
  for (col in names(details)[numeric_cols]) {
    if (col != "participant") {
      col_mean <- mean(details[[col]], na.rm = TRUE)
      col_sd <- sd(details[[col]], na.rm = TRUE)
      col_median <- median(details[[col]], na.rm = TRUE)
      col_min <- min(details[[col]], na.rm = TRUE)
      col_max <- max(details[[col]], na.rm = TRUE)
      col_iqr <- IQR(details[[col]], na.rm = TRUE)
      
      mean_sd <- sprintf("%.2f, SD: %.2f", col_mean, col_sd)
      median_min_max_iqr <- sprintf("%.2f [%.2f, %.2f], IQR=%.2f", col_median, col_min, col_max, col_iqr)
      
      result <- rbind(result, data.frame(
        Detail = c(paste(col, "(Mean, SD)"), paste(col, "(Median [Min, Max], IQR)")),
        Value = c(mean_sd, median_min_max_iqr),
        Category = ""
      ))
    }
  }
  
  # Count the occurrences of each unique value in categorical columns
  categorical_cols <- sapply(details, is.character)
  categorical_cols["participant"] <- FALSE  # Exclude the participant column
  
  for (col in names(details)[categorical_cols]) {
    counts <- table(details[[col]])
    result <- rbind(result, data.frame(
      Detail = rep(col, length(counts)),
      Value = as.vector(counts),
      Category = names(counts)
    ))
  }
  
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
  # Validate trackerType
  if (!trackerType %in% names(filenameDict)) {
    stop("Invalid tracker type specified.")
  }

  filename <- paste0(filenameDict[[trackerType]],"_T", sprintf("%03d", trialNum), ".csv")
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

get_question_info <- function(qType) { # qType = e.g. IMI / SSQ / VEQ
  qInfopath <- file.path(questionnaireInfoFolder, paste0(qType, ".csv"))
  # Read the CSV file into a data frame
  questionnaire <- read.csv(qInfopath)
  return(questionnaire)
}

get_question_weights <- function(qType) { # qType = e.g. IMI / SSQ / VEQ
  qpath <- file.path(questionnaireInfoFolder, paste0(qType, "_weights.csv"))
  # Read the CSV file into a data frame
  questionnaire <- read.csv(qpath)
  return(questionnaire)
}
