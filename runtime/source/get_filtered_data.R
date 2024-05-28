# Initialize reactiveVal to store the filtered parameters
filteredParams <- reactiveVal()

filter_data <- function(allGaitParams, filterParticipants, filterVFD, filterTrials, filterTargets, filterOutliers, filterPractice, filterStartCondition, additionalArg) {
  included <- allGaitParams[["participant"]] %in% filterParticipants
  included <- included & allGaitParams[["VFD"]] %in% filterVFD
  included <- included & allGaitParams[["trialNum"]] %in% filterTrials
  included <- included & allGaitParams[["heelStrikes.targetIgnoreSteps"]] %in% filterTargets
  included <- included & allGaitParams[["heelStrikes.outlierSteps"]] %in% filterOutliers
  included <- included & allGaitParams[["practice"]] %in% filterPractice
  included <- included & allGaitParams[["started_with_noise"]] %in% filterStartCondition
  return(allGaitParams[included, ])
}

# Update the filtered parameters whenever any input changes
observeEvent({
  list(input$filterParticipants, input$filterVFD, input$filterTrials,
       input$filterTargets, input$filterOutliers, input$filterPractice, input$filterStartCondition, input$additionalArg)
}, {
  filtered_data <- filter_data(
    allGaitParams, 
    input$filterParticipants, 
    input$filterVFD, 
    input$filterTrials, 
    input$filterTargets, 
    input$filterOutliers, 
    input$filterPractice,
    input$filterStartCondition,
    input$additionalArg  # The additional argument
  )
  filteredParams(filtered_data)
})

filteredTargetParams <- reactive({
  included <- allTargetParams[["participant"]] %in% input$filterParticipants
  included <- included & allTargetParams[["VFD"]] %in% input$filterVFD
  included <- included & allTargetParams[["trialNum"]] %in% input$filterTrials
  included <- included & allTargetParams[["practice"]] %in% input$filterPractice
  included <- included & allTargetParams[["started_with_noise"]] %in% input$filterStartCondition
  
  return(allTargetParams[included, ])
})

get_mu_dyn <- reactive({
  mu_dyn <- lapply(dataTypes, get_summ, categories = categories, filteredData = filteredParams())
  mu_dyn <- setNames(mu_dyn, dataTypes)
  
  return(mu_dyn)
})

get_mu_dyn_long <- reactive({
  mu_dyn <- get_mu_dyn()
  
  # Convert list to a single data frame
  mu_long <- bind_rows(mu_dyn, .id = "dataType") %>%
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
  merged_results <- merged_results[!(merged_results$trialNum %in% c(1,4)),]
  
  ### Extra stuff to get relative CV from mean of NoVFD condition compared to VFD trials
  
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
    mu_full[mu_full$VFD == FALSE, delta_col] <- NA # these values make no sense so we remove them from the dataset
  }
  #mu_full$trialNum <- as.factor(mu_full$trialNum)
  return(mu_full)
})
