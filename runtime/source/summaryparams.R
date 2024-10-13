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

get_summ_by_foot <- function(dataType, categories, data) {
  data %>%
    group_by(across(all_of(categories))) %>%
    summarise(
      mean_foot = mean(.data[[dataType]], na.rm = TRUE),
      sd_foot = sd(.data[[dataType]], na.rm = TRUE),
      cv_foot = sd(.data[[dataType]], na.rm = TRUE) / mean(.data[[dataType]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(across(all_of(setdiff(categories, "heelStrikes.foot")))) %>%
    summarise(
      mean = mean(mean_foot, na.rm = TRUE),
      sd = mean(sd_foot, na.rm = TRUE),
      cv = mean(cv_foot, na.rm = TRUE),
      .groups = "drop"
    )
}

get_mu <- function(data, types, categories) {
  mu <- lapply(types, get_summ_by_foot, categories = categories, data = data)
  mu <- setNames(mu, types)

  return(mu)
}

# This table is huge (like 160 columns)
summarize_table <- function(data, allQResults, categories) {
  dataTypes <- setdiff(getTypes(data), categories)

  # Assuming mu is a list of data frames, each corresponding to a dataType
  mu <- get_mu(data, dataTypes, categories)

  # Convert list to a single data frame
  mu_long <- bind_rows(mu, .id = "dataType") %>%
    pivot_longer(
      cols = -c(categories, dataType),
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

  mu_full <- merge(allQResults, mu_wide, by = matchByList, all = TRUE)

  mu_full <- mu_full %>%
    select(all_of(categories), everything())

  return(mu_full)
}

get_full_mu <- function(allGaitParams, allQResults, categories) {
  # Join the columns
  print("HERE?")
  muGait <- summarize_table(allGaitParams, allQResults, c(categories, "heelStrikes.foot"))

  # Summarize across feet
  muGait_avg <- muGait %>%
    select(-heelStrikes.foot) %>%
    group_by(across(all_of(c(categories)))) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

  return(muGait_avg)
}
