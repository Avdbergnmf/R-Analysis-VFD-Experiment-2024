library(data.table)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(readxl)
library(rstatix)
library(plotrix)
library(car)
library(Rmisc)

# data manipulation
library(plyr)
library(dplyr)
library(purrr)

# plotting
library(tidyr)
library(devtools)
library(ggpattern) # To differentiate when printing in black and white

# LMER stuff
library(lme4)
library(emmeans)
library(sjstats)
library(lmerTest)
library(MuMIn)

# saving figures
library(svglite)

# File importing / reading
library(jsonlite) # for json files

# Reshaping data for questionnaires
library(reshape2)

library(pwr)

library(signal)

################ PATH DEFINITIONS ################ 
dataFolder <- file.path(".", "data")
participants <- list.dirs(path = dataFolder, full.names = FALSE, recursive = FALSE)
#participants <- participants[1]
# Column definitions
xOptions <- c("time", "pos_x", "pos_y", "pos_z")
xOptionsVFD <- c("time", "magnitude", "offset_x", "offset_y", "offset_z", "final_pos_x", "final_pos_y", "final_pos_z", "rel_pos_x", "rel_pos_y", "rel_pos_z")
xOptions <- c(xOptions, xOptionsVFD)

# Path definitions
questionnaireInfoFolder <- file.path(".", "questionnaires")
trackerPath <- file.path(file.path(dataFolder, participants[1]), "trackers")
trackers <- list.files(path = trackerPath, full.names = FALSE)
filenameDict <- list(
  "camera" = "camera_movement_T",
  "hip" = "hiptracker_movement_T",
  "leftfoot" = "leftfoottracker_movement_T",
  "lefthand" = "controllerleft_movement_T",
  "leftknee" = "leftkneetracker_movement_T",
  "rightknee" = "righkneetracker_movement_T",
  "rightfoot" = "rightfoottracker_movement_T",
  "righthand" = "controllerright_movement_T",
  "steptargets" = "steptargetsmanager_targetsteps_T",
  "treadmillleft" = "treadmilllefttracker_movement_T",
  "treadmillright" = "treadmillrighttracker_movement_T",
  "treadmillback" = "treadmillrightbacktracker_movement_T",
  "eye" = "eyetracking_EyeGaze_T"
)

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
  return(get_p_detail(pnum, "vfd_first"))
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

allQResults <- get_all_questionnaire_results() 

################ Data Manipulation ################ 

adjust_times <- function(dataset, minTime) { # make sure we start at t=0
  dataset$time <- dataset$time - minTime
  dataset <- subset(dataset)
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
  #get_t_data(participant, "leftnoise", trialNum),
  #get_t_data(participant, "rightnoise", trialNum),
  #rightFoot <- calc_final_pos(rightFoot, rightNoise)
  #leftFoot <- calc_final_pos(leftFoot, leftNoise)
  minTime <- min(leftFoot$time)
  
  moveSpeed = get_move_speed(participant)
  leftFoot = adjust_times(leftFoot, minTime)
  leftFoot$actual_pos_z = leftFoot$pos_z + moveSpeed * leftFoot$time
  rightFoot = adjust_times(rightFoot, minTime)
  rightFoot$actual_pos_z = rightFoot$pos_z + moveSpeed * rightFoot$time
  
  return(list(
    leftFoot = leftFoot,
    rightFoot = rightFoot,
    hip = adjust_times(hip, minTime),
    targetData = adjust_times(targetData, minTime)
  ))
}

detect_foot_events_coordinates <- function(footData, hipData) {
  frontalFootPos <- footData$pos_z
  frontalHipPos <- hipData$pos_z
  relFootPos <- frontalFootPos - frontalHipPos
  footHeight <- footData$pos_z
  
  # Apply a moving average filter to the relative foot position
  #N = 4
  #smoothRelFootPos <- stats::filter(relFootPos, rep(1/N, N), sides=1)
  fs <- 90  # Sampling frequency
  cutoff <- 10  # Cutoff frequency
  w <- cutoff / (fs / 2)  # Normalized frequency
  b <- butter(4, w)  # 4th order Butterworth filter
  smoothRelFootPos <- filtfilt(b, relFootPos)  # Zero phase filtering
  smoothFootHeight <- filtfilt(b, footHeight)
  # Detect local extremes
  local_maxima <- which(diff(sign(diff(smoothRelFootPos))) == -2) + 1
  local_minima <- which(diff(sign(diff(smoothRelFootPos))) == 2) + 1
  
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
  
  # Apply alternation check and remove index columns
  results <- ensure_alternation(combinedHeelStrikes, combinedToeOffs)
  combinedHeelStrikes <- results$data1
  combinedToeOffs     <- results$data2
  #print(which(diff(as.numeric(combinedToeOffs$foot == "Left")) == 0))
  #results <- ensure_alternation(combinedToeOffs, combinedHeelStrikes) # remove from both since they are paired
  #combinedToeOffs     <- results$data1
  #combinedHeelStrikes <- results$data2
  
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
  
  # time-based
  stepTimes <- diff(heelStrikesData$time) # Calculate step times  >>> NOTE: The first heelstrike is only used as a starting point to the second
  swingTimes <- heelStrikesData$time - toeOffsData$time # Calculate swing times <<< L = N   (not N-1)
  swingTimes <- tail(swingTimes, -1) # remove the FIRST swingTime (we have to sacrifice the first step to be able to do diff's)
  stanceTimes <- stepTimes - swingTimes # Calculate stance times
  
  # position-based
  stepWidths <- diff(heelStrikesData$pos_x) # Calculate step width
  stepWidths <- ifelse(head(heelStrikesData$foot, -1) == "Right", stepWidths * -1, stepWidths) # Adjust sign based on which foot is stepping
  #finalStepWidths <- diff(heelStrikesData$final_pos_x)
  #finalStepWidths <- ifelse(head(heelStrikesData$foot, -1) == "Right", finalStepWidths * -1, finalStepWidths)
  
  stepLengths <- diff(heelStrikesData$actual_pos_z) # Calculate step lengths
  #finalStepLengths <- diff(heelStrikesData$actual_final_pos_z) # Calculate step lengths
  
  speed <- stepLengths / stepTimes # Calculate speed
  
  # We ignore the steps onto and away from a target
  # and also add to the ignore list the outliers in the stepTimes and stepLengths (these are very obvious ways to remove the misdetected steps)
  heelStrikesData <- tail(heelStrikesData, -1) # to match our diffs
  toeOffsData <- tail(toeOffsData, -1) # to match our diffs
  targetSteps <- heelStrikesData$target
  targetSteps <- targetSteps | lead(heelStrikesData$target, default = FALSE) # add step before
  targetSteps <- targetSteps | lag(heelStrikesData$target, default = FALSE) | lag(heelStrikesData$target, 2, default = FALSE) # add steps after (until foot is placed back onto new position)
  heelStrikesData$targetIgnoreSteps <- targetSteps
  # Detect outliers
  
  heelStrikesData$outlierSteps <- detect_outliers(stepTimes, targetSteps) | detect_outliers(stepLengths, targetSteps)
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
    toeOffs = toeOffsData
  )
  
  return(gaitParams)
}


detect_outliers <- function(data, targetIgnoreSteps) {
  data_filtered <- data[targetIgnoreSteps]
  
  # We don't use the target steps to calculate our interquartile ranges.
  Q1 <- quantile(data_filtered, 0.25)
  Q3 <- quantile(data_filtered, 0.75)
  IQR <- Q3 - Q1
  
  # Define the upper and lower bounds for outliers
  upper_bound <- Q3 + 1.5 * IQR
  lower_bound <- Q1 - 1.5 * IQR
  
  return(!(data >= lower_bound & data <= upper_bound))
}

################ GET RESULTS ################ 

allTrials <- c(1,2,3,4,5,6) # VFD conditions

calc_all_gait_params <- function(){
  # Initialize an empty data frame
  allGaitParams <- data.frame()
  # Loop over all participants
  for (participant in participants) {
    print(participant)
    # Loop over all trials
    for (trial in allTrials) {
      print(trial)
      # Calculate gait data and parameters
      gaitParams <- calculate_gait_parameters(participant, trial)
      
      # Convert the list of gait parameters to a data frame
      gaitParamsDf <- as.data.frame(gaitParams)
      
      # Add a column for the participant identifier
      gaitParamsDf$participant <- participant
      gaitParamsDf$trialNum <- trial
      gaitParamsDf$VFD <- get_p_results(participant,"noise_enabled",trial) == "True"
      gaitParamsDf$practice <- get_p_results(participant,"practice",trial) == "True"
      
      # Bind this participant's gait parameters to the overall data frame
      allGaitParams <- rbind(allGaitParams, gaitParamsDf)
    }
  }
  return(allGaitParams)
}

#allGaitParams <- calc_all_gait_params()

########################### PLOTTING FUNCTIONS ##############################

plot_steps <- function(participant, trialNum, start=1, end=500, x_axis = "time", y_axis = "pos_z") { # start=first step to plot, end=last step to plot
  filteredGaitParams <- allGaitParams[allGaitParams$participant == participant & allGaitParams$trialNum == trialNum, ]
  
  preprocessedData <- preprocess_data(participant, trialNum) 
  rightData <- preprocessedData$rightFoot
  leftData <- preprocessedData$leftFoot
  
  # Filter out a subset of the steps if needed
  if (start > 1) {start <- start - 1} # make sure we get the step before
  steps <- start:end
  
  if (length(steps) > 1) {
    filteredGaitParams <- filteredGaitParams[filteredGaitParams$heelStrikes.step %in% steps,]
  }
  
  timeMin <- min(filteredGaitParams$heelStrikes.time)
  timeMax <- max(filteredGaitParams$heelStrikes.time)
  
  rightData <- rightData %>% filter(time > timeMin & time < timeMax)
  leftData <- leftData %>% filter(time > timeMin & time < timeMax)
  
  # Add a new column to both dataframes to identify the foot
  rightData$foot <- "Right"
  leftData$foot <- "Left"
  # Combine the dataframes
  both <- rbind(rightData, leftData)
  both <- both[order(both$time), ] # Order by time
  
  rParams <- filteredGaitParams[filteredGaitParams$heelStrikes.foot == "Right", ]
  lParams <- filteredGaitParams[filteredGaitParams$heelStrikes.foot == "Left", ]
  rTargets <- rParams[rParams$heelStrikes.target, ]
  lTargets <- lParams[lParams$heelStrikes.target, ]
  
  # Create the plot
  targetSize <- 5
  footEventSize <- 2
  p <- ggplot(both, aes(x = .data[[x_axis]], y = .data[[y_axis]], color = .data$foot)) +
    geom_path() +
    # toeOffs
    geom_point(data = rParams, aes(x = .data[[paste0("toeOffs.", x_axis)]], y = .data[[paste0("toeOffs.", y_axis)]]), shape = 24, color = "red", size = footEventSize) +
    geom_point(data = lParams, aes(x = .data[[paste0("toeOffs.", x_axis)]], y = .data[[paste0("toeOffs.", y_axis)]]), shape = 24, color = "blue", size = footEventSize) + # 12=empty square
    # heelstrikes
    geom_point(data = rParams, aes(x = .data[[paste0("heelStrikes.", x_axis)]], y = .data[[paste0("heelStrikes.", y_axis)]]), shape = 25, color = "red", size = footEventSize) + # 16=ball
    geom_point(data = lParams, aes(x = .data[[paste0("heelStrikes.", x_axis)]], y = .data[[paste0("heelStrikes.", y_axis)]]), shape = 25, color = "blue", size = footEventSize) +
    # targets
    geom_point(data = rTargets, aes(x = .data[[paste0("heelStrikes.", x_axis)]], y = .data[[paste0("heelStrikes.", y_axis)]]), shape = 10, color = "red", size = targetSize) +
    geom_point(data = lTargets, aes(x = .data[[paste0("heelStrikes.", x_axis)]], y = .data[[paste0("heelStrikes.", y_axis)]]), shape = 10, color = "blue", size = targetSize) + # 10=target
    scale_color_manual(values = c("Right" = "black", "Left" = "grey"))
  
  if (x_axis != "time" && y_axis != "time") {
    p <- p + coord_equal()
  }
  
  return(p)
}


add_lines <- function(p, footEvents, rightData, leftData, start, end, x_axis = "time", y_axis = "pos_z") { # start=first step to plot, end=last step to plot
  heelStrikes <- footEvents$heelStrikes
  
  # Filter out a subset of the steps if needed
  if (start > 1) {
    start <- start - 1
  } # make sure we get the step before
  steps <- start:end
  
  if (length(steps) > 1) {
    heelStrikes <- subset(heelStrikes, step %in% steps)
    # Filter out the steps we want to plot
    rHeelStrikes <- subset(heelStrikes, .data$foot == "Right")
    timeMin <- min(rHeelStrikes$time)
    timeMax <- max(rHeelStrikes$time)
    rightData <- subset(rightData, time > timeMin & time < timeMax)
    leftData <- subset(leftData, time > timeMin & time < timeMax)
  }
  
  # Add a new column to both dataframes to identify the foot
  rightData$foot <- "Right"
  leftData$foot <- "Left"
  
  # Create the plot
  p <- p + geom_path(data = rightData, aes(x = .data[[x_axis]], y = .data[[y_axis]]), color = "pink") +
    geom_path(data = leftData, aes(x = .data[[x_axis]], y = .data[[y_axis]]), color = "lightblue")
  if (x_axis != "time" && y_axis != "time") {
    p <- p + coord_equal()
  }
  return(p)
}

############################# SUMMARY TABLE ###################################

categories <- c("participant", "VFD","trialNum")#, "heelStrikes.foot")
categoriesInputs <- append(categories, "None")
numericDataTypes <- sapply(allGaitParams, is.numeric)
logicalDataTypes <- sapply(allGaitParams, is.logical)
dataTypes <- names(numericDataTypes[numericDataTypes | logicalDataTypes])
dataTypes <- setdiff(dataTypes, categories)

get_summ <- function(dataType, categories, filteredData) {
  filteredData %>%
    group_by(across(all_of(categories))) %>%
    summarise(
      mean = mean(.data[[dataType]], na.rm = TRUE),
      sd = sd(.data[[dataType]], na.rm = TRUE),
      cv = sd(.data[[dataType]], na.rm = TRUE) / mean(.data[[dataType]], na.rm = TRUE),
      .groups = "drop"
    )
}

# This table is huge (like 160 columns)
get_full_mu <- function(){
  # Assuming mu is a list of data frames, each corresponding to a dataType
  mu <- lapply(dataTypes, get_summ, categories = categories, filteredData = allGaitParams)
  mu <- setNames(mu, dataTypes)
  
  # Convert list to a single data frame
  mu_long <- bind_rows(mu, .id = "dataType") %>%
    pivot_longer(
      cols = -c(participant, VFD, trialNum, dataType), 
      names_to = "statistic", 
      values_to = "value"
    )
  
  # Create new column names and pivot to a wider format
  mu_wide <- mu_long %>%
    unite("new_col_name", dataType, statistic, sep = ".") %>%
    pivot_wider(
      names_from = new_col_name, 
      values_from = value
    )
  
  merged_results <- merge(allQResults, mu_wide, by = c("participant", "VFD"), all = TRUE)
  
  # Initialize new columns for deltaCv to 0.0 for each CV column
  cv_columns <- grep("\\.cv$", colnames(merged_results), value = TRUE)
  delta_cv_columns <- paste(cv_columns, "deltaCv", sep = ".")
  merged_results[delta_cv_columns] <- 0.0
  # Calculate avgNoVFD for each participant for non-practice VFD==FALSE trials
  noVFDTrials <- merged_results[merged_results$practice.mean == 0 & merged_results$VFD == FALSE, ]
  avgNoVFD <- noVFDTrials %>%
    group_by(participant) %>%
    summarise(across(all_of(cv_columns), \(x) mean(x, na.rm = TRUE)), .groups = 'drop')
  
  # Join avgNoVFD with the main data frame
  names(avgNoVFD)[-1] <- paste(sub("\\.cv$", "", names(avgNoVFD)[-1]), "avgNoVFD", sep = ".")
  mu_full <- merge(merged_results, avgNoVFD, by = c("participant"), all = TRUE)
  
  # Calculate deltaCv for each .cv column
  for (cv_col in cv_columns) {
    avg_col <- paste(sub("\\.cv$", "", cv_col), "avgNoVFD", sep = ".")  # Construct the avgNoVFD column name
    delta_col <- paste(cv_col, "deltaCv", sep = ".")  # Construct the deltaCv column name
    
    mu_full[[delta_col]] <- mu_full[[cv_col]] - mu_full[[avg_col]]
  }
  
  return(mu_full)
}

mu <- get_full_mu()
numericDataTypes <- sapply(mu, is.numeric)
logicalDataTypes <- sapply(mu, is.logical)
muDataTypes <- names(numericDataTypes[numericDataTypes | logicalDataTypes])
muDataTypes <- setdiff(muDataTypes, categories)


make_pie_chart <- function(data){
  #data <- filteredParams()
  targetIgnoreSteps <- length(data[data$heelStrikes.targetIgnoreSteps==TRUE & data$heelStrikes.outlierSteps == FALSE,]$VFD)
  outlierSteps <- length(data[data$heelStrikes.targetIgnoreSteps==FALSE & data$heelStrikes.outlierSteps == TRUE,]$VFD)
  bothSteps <- length(data[data$heelStrikes.targetIgnoreSteps==TRUE & data$heelStrikes.outlierSteps == TRUE,]$VFD)
  total_steps <- length(data$VFD)
  
  # Create a data frame for ggplot
  df_filtered <- data.frame(
    StepType = c("Target Ignore", "Outlier", "Both"),
    TotalCount = c(targetIgnoreSteps, outlierSteps, bothSteps)
  )
  
  # Calculate label positions for the pie chart
  df_filtered$label_pos <- cumsum(df_filtered$TotalCount) - df_filtered$TotalCount/2
  
  # Generate the pie chart
  p <- ggplot(df_filtered, aes(x = "", y = TotalCount, fill = StepType)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y", start = 0) +
    theme_void() +
    scale_fill_brewer(palette = "Pastel1") +
    geom_text(aes(label = TotalCount, y = label_pos), color = "black")
  #+
  #ggtitle(paste("Participant =", participant_value, "| Total steps =", total_steps))
  
  return(p)
}

make_pie_chart(allGaitParams)
