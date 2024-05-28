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
get_full_mu <- function(data, allQResults, dataTypes, categories){
  # Define the list of columns to remove - We add these later, but remove them here so they are not considered for the summary table
  columns_to_remove <- c("practice", "startedWithNoise")
  data <- data %>% select(-all_of(columns_to_remove)) # Remove the specified columns from the data
  types <- setdiff(dataTypes, columns_to_remove) # also remove them from our types list
  
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
  
  merged_results <- merge(allQResults, mu_wide, by = c("participant", "VFD"), all = TRUE)
  #merged_results <- merged_results[!(merged_results$trialNum %in% c(1,4)),]
  
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
  
  # Create the new column using mutate and sapply
  mu_full$startedWithNoise <- sapply(mu_full$participant, started_with_noise)
  mu_full$practice <- mapply(get_p_results, mu_full$participant, "practice", mu_full$trialNum)
  
  mu_full <- mu_full %>%
    select(participant, trialNum, startedWithNoise, practice, everything())
  
  
  return(mu_full)
}
