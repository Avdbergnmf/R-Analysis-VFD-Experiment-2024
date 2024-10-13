####### DEFINITIONS
trials <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11) # List of trials
# Getting types for later use
xOptions <- c("time", "pos.x", "pos.y", "pos.z")
xOptions2D <- colnames(get_t_data(participants[1], "leftfoot", 1)) # options for pos rot trackers
categories <- c("participant", "trialNum", "freqHigh", "freqLow", "gain")
categoriesInputs <- append(categories, "None")

getTypes <- function(dt) {
  numericDataTypes <- sapply(dt, is.numeric)
  logicalDataTypes <- sapply(dt, is.logical)
  dataTypes <- names(numericDataTypes[numericDataTypes | logicalDataTypes])
}

############ FUNCTIONS
# Helper function to load or calculate and save data
load_or_calculate <- function(filePath, calculate_function) {
  if (file.exists(filePath)) {
    data <- readRDS(filePath)
  } else {
    data <- calculate_function()
    saveRDS(data, filePath)
  }
  return(data)
}

add_category_columns <- function(data, participant, trial) {
  # Add a column for the participant identifier
  data$participant <- participant
  data$trialNum <- trial
  data$freqHigh <- get_p_results(participant, "freqHigh", trial)
  data$freqLow <- get_p_results(participant, "freqLow", trial)
  data$gain <- get_p_results(participant, "gain", trial)
  data$treadmillSpeed <- get_p_results(participant, "treadmillSpeed", trial)

  return(data)
}

get_data_from_loop_parallel <- function(get_data_function, ...) {
  # Create a data frame of all participant and trial combinations
  combinations <- expand.grid(participant = participants, trial = trials)

  # Set up parallel backend to use multiple processors
  numCores <- detectCores() - 1 # Leave one core free for system processes
  cl <- makeCluster(numCores)
  registerDoParallel(cl)

  # Export necessary variables and functions to the cluster
  clusterExport(cl, c("participants", "trials", "get_data_function", "add_category_columns"), envir = environment())

  # Run the loop in parallel using foreach
  data_list <- foreach(
    i = 1:nrow(combinations),
    .combine = rbind,
    .packages = c() # "data.table", "readxl", "dplyr", "purrr", "jsonlite", "signal", "zoo", "" ... just did whole setup.R for now
  ) %dopar% {
    # Source the scripts containing your functions
    source("source/setup.R", local = FALSE)
    source("source/data_loading.R", local = FALSE)
    source("source/pre_processing.R", local = FALSE)
    source("source/find_foot_events.R", local = FALSE)
    source("source/calc_all_gait_params.R", local = FALSE)

    # Extract participant and trial for this iteration
    participant <- combinations$participant[i]
    trial <- combinations$trial[i]

    # Calculate gait data and parameters with optional arguments
    newData <- get_data_function(participant, trial, ...)
    newData <- add_category_columns(as.data.frame(newData), participant, trial)
    newData # Return the new data frame
  }

  # Stop the cluster
  stopCluster(cl)

  # Combine the list of data frames into one data frame
  data <- as.data.frame(data_list)

  return(data)
}

get_data_from_loop <- function(get_data_function, ...) {
  # Initialize an empty data frame
  data <- data.frame()

  # Variables for progress bar
  total_iterations <- length(participants) * length(trials)
  current_iteration <- 0

  for (participant in participants) {
    print(paste("Participant:", participant))

    for (trial in trials) {
      # PRINT PROGRESS BAR
      current_iteration <- current_iteration + 1 # Increment the iteration count
      progress_percent <- (current_iteration / total_iterations) * 100 # Calculate the percentage of progress
      progress_bar_length <- 50 # Length of the progress bar
      num_hashes <- floor(progress_bar_length * progress_percent / 100)
      num_dashes <- progress_bar_length - num_hashes
      progress_bar <- paste0("[", paste(rep("#", num_hashes), collapse = ""), paste(rep("-", num_dashes), collapse = ""), "]")

      # Print progress bar with the percentage
      cat(sprintf("\rProgress: %s %.2f%% On Participant: %s, Trial: %s\n", progress_bar, progress_percent, participant, trial))
      flush.console() # Ensure the console updates immediately

      # Calculate gait data and parameters
      newData <- get_data_function(participant, trial, ...)
      newData <- add_category_columns(as.data.frame(newData), participant, trial) # Convert to DF and add categories
      data <- rbind(data, newData) # Bind this participant's gait parameters to the overall data frame
    }
  }

  # Print final progress bar to indicate completion
  cat("\n")

  return(data)
}


calc_all_gait_params <- function() {
  return(get_data_from_loop_parallel(calculate_gait_parameters))
}
