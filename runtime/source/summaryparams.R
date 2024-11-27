get_summ_by_foot <- function(dataType, categories, data, avg_feet = TRUE) {
  data <- data %>%
    group_by(across(all_of(categories))) %>%
    summarise(
      mean = mean(.data[[dataType]], na.rm = TRUE),
      sd = sd(.data[[dataType]], na.rm = TRUE),
      cv = sd(.data[[dataType]], na.rm = TRUE) / mean(.data[[dataType]], na.rm = TRUE),
      .groups = "drop"
    ) 
  
  if (avg_feet) {
    data <- data %>%
      group_by(across(all_of(setdiff(categories, "heelStrikes.foot")))) %>%
      summarise(
        mean = mean(mean, na.rm = TRUE),
        sd = mean(sd, na.rm = TRUE),
        cv = mean(cv, na.rm = TRUE),
        .groups = "drop"
      )
  }

  return(data)
}

# Average across feet and also calculate diff between left and right foot and add it to mu
average_over_feet <- function(data, types, categories, add_diff = FALSE) {
  # Get summaries averaging over feet
  categories_no_foot <- setdiff(categories, "heelStrikes.foot")
  mu_avg <- lapply(types, get_summ_by_foot, categories_no_foot, data, avg_feet = TRUE)
  mu_avg <- setNames(mu_avg, types)

  if (add_diff) {
    # Keep heelStrikes.foot in categories for per-foot summaries
    categories_with_foot <- categories
    
    # Step 1: Get per-foot summaries without averaging over feet
    mu_per_foot <- lapply(types, get_summ_by_foot, categories_with_foot, data, avg_feet = FALSE)
    mu_per_foot <- setNames(mu_per_foot, types)

    # Step 2: Calculate the differences between left and right foot measurements
    mu_diff <- lapply(mu_per_foot, function(df) {
      # Pivot the data wider to have separate columns for left and right foot measurements
      df_wide <- df %>%
        pivot_wider(names_from = heelStrikes.foot, values_from = c(mean, sd, cv))
      
      # Ensure both left and right foot data are available
      #df_wide <- df_wide %>%
      #  filter(!is.na(mean_Left) & !is.na(mean_Right))
      
      # Calculate the difference between left and right foot measurements
      df_wide <- df_wide %>%
        mutate(
          diffFeet_mean = mean_Left - mean_Right,
          diffFeet_sd = sd_Left - sd_Right,
          diffFeet_cv = cv_Left - cv_Right
        ) %>%
        select(-starts_with("mean_"), -starts_with("sd_"), -starts_with("cv_"))  # Remove per-foot columns if not needed
      
      return(df_wide)
    })
    mu_diff <- setNames(mu_diff, types)

    # Step 3: Add diff columns to mu with averaged feet
    mu <- mapply(function(avg_df, diff_df) {
      # Merge the averaged data with the diff data on categories_no_foot
      merged_df <- avg_df %>%
        left_join(diff_df, by = categories_no_foot)
      return(merged_df)
    }, mu_avg, mu_diff, SIMPLIFY = FALSE)
    mu <- setNames(mu, types)

    return(mu)
  }
  else {
    return(mu_avg)
  }
}

# This table is huge (like 160 columns)
summarize_table <- function(data, allQResults, categories, avg_feet = TRUE, add_diff = FALSE) { # note: add diff only works if also averaging over feet
  if (avg_feet){
    categories <- c(categories,"heelStrikes.foot")
  }

  dataTypes <- setdiff(getTypes(data), categories)

  # Define the list of columns to remove - We add these later, but remove them here so they are not considered for the summary table
  data <- data %>% select(-all_of(columns_to_not_summarize)) # Remove the specified columns from the data
  types <- setdiff(dataTypes, columns_to_not_summarize) # Also remove them from our types list

  # Initialize the list 'mu' to store the final data frames
  mu <- list()

  if (avg_feet) {
    mu <- average_over_feet(data, types, categories, add_diff = add_diff)
    categories <- setdiff(categories, "heelStrikes.foot") # remove heelStrikes.foot from category list
  } else {
    # If not averaging over feet, compute mu normally
    mu <- lapply(types, get_summ_by_foot, categories, data, avg_feet = FALSE)
    mu <- setNames(mu, types)
  }

  # Combine the list of data frames into one data frame
  mu_long <- bind_rows(mu, .id = "dataType") %>%
    pivot_longer(
      cols = -c(all_of(categories), dataType),
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
  mu_full <- add_category_columns(mu_full)

  # Reorder columns
  mu_full <- mu_full %>%
    select(all_of(categories), all_of(columns_to_not_summarize), everything())

  return(mu_full)
}


get_full_mu <- function(allGaitParams, allTargetParams, allQResults, categories, avg_feet = TRUE, add_diff = FALSE) { # I could not get the optional feet averaging to work without having to pass it all the way down (would be nice to have some post-processing function that averages afterwards, optionally, but in the end went with this cumbersome road)
  targetColumnsToAdd <- c("score", "targetDist", "rel_x", "rel_z")

  # Join the columns
  muGait <- summarize_table(allGaitParams, allQResults, categories, avg_feet, add_diff) ##### Note that we add heelStrikes.foot here as a category, to make sure we summarize each foot individually
  muTarget <- summarize_table(allTargetParams, allQResults, categories, FALSE, FALSE)

  # Find columns that partially match the names listed in targetColumnsToAdd
  matched_columns <- unlist(lapply(targetColumnsToAdd, function(x) grep(x, names(muTarget), value = TRUE)))

  # Select only the columns we are interested in
  matchByList <- c("participant", "trialNum")
  muTarget <- muTarget %>% select(all_of(matchByList), all_of(matched_columns))

  if (length(matched_columns) > 0) {
    muTarget <- muTarget %>% select(all_of(matchByList), all_of(matched_columns))
    
    # Rename matched columns with the prefix "target."
    muTarget <- muTarget %>%
      rename_with(~ paste0("target.", .), all_of(matched_columns))
  } else {
    # If no matched columns, select only matchByList columns to avoid errors
    muTarget <- muTarget %>% select(all_of(matchByList))
  }

  # Combine the data frames
  combined_mu <- merge(muGait, muTarget, by = matchByList, all = TRUE)

  return(combined_mu)
}

### SUMMARIZE AGAIN AND CALCULATE DIFF, SO WE CAN USE FOR CORRELATIONN PLOT

summarize_across_conditions <- function(data) {
  # Filter for the relevant trial numbers
  data <- data %>%
    dplyr::filter(trialNum %in% c(2, 3, 5, 6))

  # Group by participant and condition (trialNum)
  summarized_data <- data %>%
    mutate(condition = case_when(
      trialNum %in% c(2, 3) ~ "condition_1",
      trialNum %in% c(5, 6) ~ "condition_2"
    )) %>%
    group_by(participant, VFD, startedWithNoise, condition) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  return(summarized_data)
}

calculate_vfd_difference <- function(data) {
  # Split the data into VFD==TRUE and VFD==FALSE
  vfd_true <- data %>% dplyr::filter(VFD == TRUE)
  vfd_false <- data %>% dplyr::filter(VFD == FALSE)

  if (nrow(vfd_true) == 0 || nrow(vfd_false) == 0 || nrow(vfd_false) != nrow(vfd_true)) {
    print("Can't calculate VFD differences when filtering based on VFD!!!")
    return(data)
  }

  # Ensure the columns are aligned for subtraction
  vfd_true <- vfd_true %>% dplyr::select(-VFD)
  vfd_false <- vfd_false %>% dplyr::select(-VFD)

  # Calculate the difference between VFD==TRUE and VFD==FALSE for each numeric column
  difference_data <- vfd_true %>%
    mutate(across(where(is.numeric), ~ . - vfd_false[[cur_column()]], .names = "diff_{.col}"))

  # Calculate the mean of VFD==TRUE and VFD==FALSE for each numeric column
  mean_data <- vfd_true %>%
    mutate(across(where(is.numeric), ~ (. + vfd_false[[cur_column()]]) / 2, .names = "mean_{.col}"))

  # Combine the difference and mean data
  if ("condition" %in% colnames(difference_data) && "condition" %in% colnames(mean_data)) {
    final_data <- difference_data %>%
      left_join(mean_data %>% select(participant, condition, starts_with("mean_")), by = c("participant", "condition"))
    # Select relevant columns for the final output
    final_output <- final_data %>%
      select(participant, condition, starts_with("diff_"), starts_with("mean_"))
  } else {
    final_data <- difference_data %>%left_join(mean_data %>% select(participant, starts_with("mean_")), by = c("participant"))
    final_output <- final_data %>%
      select(participant, starts_with("diff_"), starts_with("mean_"))
  }

  return(final_output)
}
