### Questionnaire results calculation
compute_scores <- function(participant, combined, qWeights, trialNum) {
  # Get max and min score for mirroring
  max_score <- qWeights[qWeights$category == "max_score", "weight"]
  min_score <- qWeights[qWeights$category == "min_score", "weight"]
  do_average <- qWeights[qWeights$category == "do_average", "weight"]

  # Mirror the scores if needed
  combined$Answer <- ifelse(combined$mirror,
    max_score + min_score - combined$Answer,
    combined$Answer
  )

  # Find all columns that contain the word 'category'
  category_columns <- grep("category", colnames(combined), value = TRUE)

  # Create a long format dataframe to handle multiple categories
  combined_long <- combined %>%
    tidyr::pivot_longer(cols = all_of(category_columns), names_to = "category_type", values_to = "category") %>%
    dplyr::filter(!is.na(category) & category != "")

  # Compute the scores for each category
  if (do_average == 0) {
    # Compute the weighted scores for each category
    combined_long <- combined_long %>%
      dplyr::mutate(
        weight = dplyr::case_when(
          category %in% qWeights$category ~ qWeights$weight[match(category, qWeights$category)],
          TRUE ~ 1
        ),
        weighted_Answer = Answer * weight
      )

    scores <- tapply(combined_long$weighted_Answer, combined_long$category, sum, na.rm = TRUE)
    # Compute the total score (sum of unweighted scores, multiplied by total weight)
    total <- tapply(combined_long$Answer, combined_long$category, sum, na.rm = TRUE)

    # Compute the total weighted score
    # Check if total weight is found, if not assign a value of 1
    if ("total" %in% qWeights$category) {
      total_weight <- qWeights[qWeights$category == "total", "weight"]
    } else {
      total_weight <- 1
    }

    scores["total"] <- sum(total, na.rm = TRUE) * total_weight
  } else {
    scores <- tapply(combined_long$Answer, combined_long$category, mean, na.rm = TRUE)

    # Compute the total score
    scores["total"] <- mean(scores, na.rm = TRUE)
  }

  return(scores)
}


calculate_all_scores <- function(qType) {
  # Initialize an empty data frame to hold the results
  dfScores <- data.frame()

  # Iterate over the participants
  for (participant in participants) {
    # Get the questionnaire data for this participant
    qData <- get_q_data(participant, qType)
    qInfo <- get_question_info(qType)
    combined <- merge(qData, qInfo, by = "QuestionID")
    qWeights <- get_question_weights(qType)
    # Get the trial numbers from the questionnaire data
    trialNums <- unique(qData$trialNum)

    # Iterate over each trial
    for (trialNum in trialNums) {
      # Compute the scores for this trial
      scores <- compute_scores(participant, combined, qWeights, trialNum)

      # Transform the scores into a data frame with a single row
      scoresRow <- cbind(
        participant = participant,
        trialNum = trialNum,
        as.data.frame(t(scores))
      )

      # Add the scores to the data frame
      dfScores <- rbind(dfScores, scoresRow)
    }
  }

  return(dfScores)
}

get_all_questionnaire_results <- function() {
  # Initialize an empty list to store results for each questionnaire
  all_q_results <- list()

  # Process each questionnaire
  for (q_type in questionnaireList) {
    # Calculate scores for current questionnaire
    q_data <- calculate_all_scores(q_type)

    # Ensure all required columns for merging are present
    for (col in matchByList) {
      if (!(col %in% colnames(q_data))) {
        q_data[[col]] <- NA # Add missing column with NA values
      }
    }

    # Rename columns with the questionnaire prefix
    colnames(q_data) <- ifelse(
      colnames(q_data) %in% c(matchByList, "unique_id"),
      colnames(q_data),
      paste0(q_type, ".", colnames(q_data))
    )

    # Add to the list
    all_q_results[[q_type]] <- q_data
  }

  # Merge all questionnaire results
  merged_results <- Reduce(function(x, y) merge(x, y, by = matchByList, all = TRUE), all_q_results)

  # Remove the temporary unique_id column
  merged_results$unique_id <- NULL

  # # Add category columns for each row
  # merged_results <- merged_results %>%
  #   rowwise() %>%
  #   mutate(across(everything(), ~ {
  #     row <- add_category_columns(cur_data(), participant, trialNum)
  #     if (is.data.frame(row) && ncol(row) == 1) row[[1]] else row
  #   })) %>%
  #   ungroup()

  return(merged_results)
}

filter_questionnaire_results <- function(allQResults, qType) { # qType= "IMI", "VEQ", "SSQ"
  # Get the columns that belong to the specified questionnaire
  columns_to_keep <- grep(paste0("^", qType, "\\."), colnames(allQResults), value = TRUE)
  columns_to_keep <- c(categories, columns_to_keep)

  # Filter the data frame to keep only the relevant columns
  filtered_results <- allQResults[, columns_to_keep, drop = FALSE]

  return(filtered_results)
}
