get_summ <- function(dataType, categories, data) {
  data %>%
    group_by(across(all_of(categories))) %>%
    summarise(
      mean = mean(.data[[dataType]], na.rm = TRUE),
      sd = sd(.data[[dataType]], na.rm = TRUE),
      cv = sd(.data[[dataType]], na.rm = TRUE) / mean(.data[[dataType]], na.rm = TRUE),
      .groups = "drop"
    )
}

get_mu <- function(data, types, categories){
  mu <- lapply(types, get_summ, categories = categories, data = data)
  mu <- setNames(mu, types)
  
  return(mu)
}

# This table is huge (like 160 columns)
summarize_table <- function(data, allQResults, categories){
  dataTypes <- setdiff(getTypes(data), categories)
  
  # Define the list of columns to remove - We add these later, but remove them here so they are not considered for the summary table
  data <- data %>% select(-all_of(columns_to_not_summarize)) # Remove the specified columns from the data
  types <- setdiff(dataTypes, columns_to_not_summarize) # also remove them from our types list
  
  # Assuming mu is a list of data frames, each corresponding to a dataType
  mu <- get_mu(data, types, categories)
  
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
  
  mu_full <- merge(allQResults, mu_wide, by = c("participant", "VFD"), all = TRUE)
  
  # Create the new column using mutate and sapply
  mu_full$trialNumWithinCondition <-(mu_full$trialNum - 1) %% 3
  mu_full$startedWithNoise <- sapply(mu_full$participant, started_with_noise)
  mu_full$practice <- mapply(get_p_results, mu_full$participant, "practice", mu_full$trialNum)
  mu_full$startedWithNoise <- sapply(mu_full$participant, started_with_noise)
  mu_full$noticed <- sapply(mu_full$participant, noticed_vfd)
  
  mu_full <- mu_full %>%
    select(participant, trialNum, columns_to_not_summarize, everything())
  
  return(mu_full)
}

get_full_mu <- function(allGaitParams, allTargetParams, allQResults, categories) {
  targetColumnsToAdd <- c("score", "targetDist", "rel_x", "rel_z")
  
  # Join the columns
  muGait <- summarize_table(allGaitParams, allQResults, categories)
  muTarget <- summarize_table(allTargetParams, allQResults, categories)
  
  # Find columns that partially match the names listed in targetColumnsToAdd
  matched_columns <- unlist(lapply(targetColumnsToAdd, function(x) grep(x, names(muTarget), value = TRUE)))
  
  # Select only the columns we are interested in
  matchByList <- c("participant", "trialNum")
  muTarget <- muTarget %>% select(all_of(matchByList), all_of(matched_columns))
  
  # Rename matched columns with the prefix "target."
  muTarget <- muTarget %>%
    rename_with(~ paste0("target.", .), all_of(matched_columns))
  
  # Combine the data frames
  combined_mu <- merge(muGait, muTarget, by = matchByList, all = TRUE)
  
  return(combined_mu)
}
