### Questionnaire results calculation
compute_scores <- function(pnum, qType) {
  qdata <- get_q_data(pnum, qType)
  
  qinfo <- get_question_info(qType)
  combined <- merge(qdata, qinfo, by = "QuestionID")
  
  # Retrieve the weights and score info
  qweights <- get_question_weights(qType)
  
  # Get max and min score for mirroring
  max_score <- qweights[qweights$category == "max_score", "weight"]
  min_score <- qweights[qweights$category == "min_score", "weight"]
  do_average <- qweights[qweights$category == "do_average", "weight"]
  
  # Mirror the scores if needed
  combined$Answer_Participant__condition_Base <- ifelse(combined$mirror, 
                                                        max_score + min_score - combined$Answer_Participant__condition_Base, 
                                                        combined$Answer_Participant__condition_Base)
  combined$Answer_Participant__condition_Noise <- ifelse(combined$mirror, 
                                                         max_score + min_score - combined$Answer_Participant__condition_Noise, 
                                                         combined$Answer_Participant__condition_Noise)
  
  # Find all columns that contain the word 'category'
  category_columns <- grep("category", colnames(combined), value = TRUE)
  
  # Create a long format dataframe to handle multiple categories
  combined_long <- combined %>%
    tidyr::pivot_longer(cols = all_of(category_columns), names_to = "category_type", values_to = "category") %>%
    dplyr::filter(!is.na(category))
  combined_long <- combined_long[combined_long$category!="",]
  #print(head(combined_long, 10), width = Inf) # to check if this worked
  
  # Compute the scores for each category
  if (do_average==0) {
    # Compute the weighted scores for each category
    combined_long <- combined_long %>%
      dplyr::mutate(
        weight = dplyr::case_when(
          category %in% qweights$category ~ qweights$weight[match(category, qweights$category)],
          TRUE ~ 1
        ),
        weighted_Answer_Participant__condition_Base = Answer_Participant__condition_Base * weight,
        weighted_Answer_Participant__condition_Noise = Answer_Participant__condition_Noise * weight
      )
    
    scoresBase <- tapply(combined_long$weighted_Answer_Participant__condition_Base, combined_long$category, sum, na.rm = TRUE)
    scoresNoise <- tapply(combined_long$weighted_Answer_Participant__condition_Noise, combined_long$category, sum, na.rm = TRUE)
    # Compute the total score for each condition (sum of unweighted scores, multiplied by total weight)
    totalBase <- tapply(combined_long$Answer_Participant__condition_Base, combined_long$category, sum, na.rm = TRUE)
    totalNoise <- tapply(combined_long$Answer_Participant__condition_Noise, combined_long$category, sum, na.rm = TRUE)
    
    # Compute the total weighted score for each condition
    # Check if total weight is found, if not assign a value of 1
    if ("total" %in% qweights$category) {
      total_weight <- qweights[qweights$category == "total", "weight"]
    } else {
      total_weight <- 1
    }
    
    scoresBase["total"] <- sum(totalBase, na.rm = TRUE) * total_weight
    scoresNoise["total"] <- sum(totalNoise, na.rm = TRUE) * total_weight
  } else {
    scoresBase <- tapply(combined_long$Answer_Participant__condition_Base, combined_long$category, mean, na.rm = TRUE)
    scoresNoise <- tapply(combined_long$Answer_Participant__condition_Noise, combined_long$category, mean, na.rm = TRUE)
    
    # Compute the total score for each condition
    scoresBase["total"] <- mean(scoresBase, na.rm = TRUE)
    scoresNoise["total"] <- mean(scoresNoise, na.rm = TRUE)
  }
  
  return(list(base = scoresBase, noise = scoresNoise))
}


calculate_all_scores <- function(qType) {
  # Initialize an empty dataframe to hold all results
  allScores <- data.frame()
  
  # Iterate over the participants
  for (participant in participants) {
    # Compute the scores
    scores <- compute_scores(participant, qType)
    
    # Assuming scores is a list with 10 elements, one for each trial
    for (trialNum in 1:10) {
      # Transform the scores into a data frame with a single row
      scoreRow <- cbind(
        participant = participant,
        trialNum = trialNum,
        as.data.frame(t(scores[[trialNum]]))
      )
      
      # Add the scores to the main dataframe
      allScores <- rbind(allScores, scoreRow)
    }
  }
  
  # Ensure column names are appropriate
  colnames(allScores) <- make.names(colnames(allScores))
  
  # Add row names
  rownames(allScores) <- NULL
  
  return(allScores)
}

get_all_questionnaire_results <- function() {
  # Function to rename columns with the questionnaire prefix
  rename_columns <- function(data, prefix) {
    colnames(data) <- ifelse(colnames(data) %in% matchByIdentifiers, colnames(data), paste0(prefix, ".", colnames(data)))
    return(data)
  }
  
  # Initialize allQResults with the first questionnaire to establish a base for merging
  initialData <- calculate_all_scores(allQs[1])
  initialData <- rename_columns(initialData, allQs[1])
  allQResults <- initialData
  
  # Loop through the remaining questionnaires
  for (currQ in allQs[-1]) {
    # Calculate scores for current questionnaire
    qData <- calculate_all_scores(currQ)
    qData <- rename_columns(qData, currQ)
    
    # Merge with allQResults based on matchByIdentifiers
    allQResults <- merge(allQResults, qData, by = matchByIdentifiers, all = TRUE)
  }
  
  # Add a column indicating if the participant started with noise
  allQResults$startedWithNoise <- sapply(allQResults$participant, started_with_noise)
  allQResults$noticed <- sapply(allQResults$participant, noticed_vfd)
  
  return(allQResults)
}

filter_questionnaire_results <- function(allQResults, qType) { # qType= "IMI", "VEQ", "SSQ"
  # Get the columns that belong to the specified questionnaire
  columns_to_keep <- grep(paste0("^", qType, "\\."), colnames(allQResults), value = TRUE)
  columns_to_keep <- c(matchByIdentifiers, columns_to_keep)
  
  # Filter the data frame to keep only the relevant columns
  filtered_results <- allQResults[, columns_to_keep, drop = FALSE]
  
  return(filtered_results)
}