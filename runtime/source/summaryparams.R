get_summ <- function(dataType, matchByList, data) {
  if (is.numeric(data[[dataType]])) {
    data %>%
      group_by(across(all_of(c(matchByList, "heelStrikes.foot")))) %>%
      summarise(
        mean = mean(.data[[dataType]], na.rm = TRUE),
        sd = sd(.data[[dataType]], na.rm = TRUE),
        cv = sd(.data[[dataType]], na.rm = TRUE) / mean(.data[[dataType]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      group_by(across(all_of(matchByList))) %>%
      summarise(
        mean = mean(mean, na.rm = TRUE),
        sd = mean(sd, na.rm = TRUE),
        cv = mean(cv, na.rm = TRUE),
        .groups = "drop"
      )
  }
}

get_mu <- function(data, types, matchByList) {
  mu <- lapply(types, get_summ, matchByList = matchByList, data = data)
  mu <- setNames(mu, types)
  return(mu)
}

summarize_table <- function(data, allQResults) {
  # Remove category columns that are not in matchByList
  columns_to_remove <- setdiff(categories, matchByList)
  data <- data %>% select(-all_of(columns_to_remove))

  dataTypes <- setdiff(names(data), c(matchByList, "heelStrikes.foot"))

  # Assuming mu is a list of data frames, each corresponding to a dataType
  mu <- get_mu(data, dataTypes, matchByList)

  # Convert list to a single data frame
  mu_long <- bind_rows(mu, .id = "dataType") %>%
    pivot_longer(
      cols = -c(all_of(matchByList), dataType),
      names_to = "statistic",
      values_to = "value"
    ) # %>%    filter(!is.na(value))  # Remove NA values

  # Create new column names and pivot to a wider format
  mu_wide <- mu_long %>%
    unite("new_col_name", dataType, statistic, sep = ".") %>%
    pivot_wider(
      names_from = new_col_name,
      values_from = value
    )

  mu_full <- merge(allQResults, mu_wide, by = matchByList, all = TRUE)

  # Get the original column names
  original_cols <- names(mu_full)

  # Re-add category columns using add_p_results
  for (i in 1:nrow(mu_full)) {
    mu_full[i, ] <- add_p_results(mu_full[i, ], mu_full$participant[i], mu_full$trialNum[i])
  }

  # Keep only the original columns
  mu_full <- mu_full[, original_cols]

  return(mu_full)
}

get_full_mu <- function(allGaitParams, allQResults) {
  # Join the columns
  muGait <- summarize_table(allGaitParams, allQResults)

  return(muGait)
}

summarize_across_conditions <- function(data) {
  return(data)
  # Filter for the relevant trial numbers
  data <- data %>%
    dplyr::filter(trialNum %in% c(2, 3, 5, 6))

  # Group by participant and condition (trialNum)
  summarized_data <- data %>%
    mutate(condition = case_when(
      trialNum %in% c(2, 3) ~ "condition_1",
      trialNum %in% c(5, 6) ~ "condition_2"
    )) %>%
    group_by(categories) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  return(summarized_data)
}
