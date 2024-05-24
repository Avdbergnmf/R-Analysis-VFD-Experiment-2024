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
get_full_mu <- function(allGaitParams, allQResults, dataTypes, categories){
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
