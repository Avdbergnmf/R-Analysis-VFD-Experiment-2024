# Filter data based on inputs

filteredParams <- reactive({
  # Force dependency on `refresh_trigger()`
  refresh_trigger()

  data <- allGaitParams
  included <- data[["participant"]] %in% input$filterParticipants
  # Trial based
  included <- included & data[["VFD"]] %in% input$filterVFD
  included <- included & data[["trialNum"]] %in% input$filterTrials
  included <- included & data[["conditionNumber"]] %in% input$filterConditionNumber
  included <- included & data[["trialNumWithinCondition"]] %in% input$filterTrialsWithinCondition
  included <- included & data[["trialNumWithoutPractice"]] %in% input$filterTrialsWithoutPractice
  included <- included & data[["practice"]] %in% input$filterPractice
  # participant group based
  included <- included & data[["startedWithNoise"]] %in% input$filterStartCondition
  included <- included & data[["noticed"]] %in% input$filterNoticed
  # Step based
  included <- included & data[["heelStrikes.incorrectDetection"]] %in% input$filterImpossible
  included <- included & data[["heelStrikes.targetIgnoreSteps"]] %in% input$filterTargets
  included <- included & data[["heelStrikes.outlierSteps"]] %in% input$filterOutliers

  included <- included & data[["heelStrikes.foot"]] %in% input$filterSide

  return(data[included, ])
})

filteredQResults_new <- reactive({
  data <- allQResults
  included <- data[["participant"]] %in% input$filterParticipants
  included <- included & data[["VFD"]] %in% input$filterVFD
  # participant group based
  included <- included & data[["startedWithNoise"]] %in% input$filterStartCondition
  included <- included & data[["noticed"]] %in% input$filterNoticed

  return(data[included, ])
})

filteredTargetParams <- reactive({
  data <- allTargetParams
  included <- data[["participant"]] %in% input$filterParticipants
  # Trial based
  included <- included & data[["VFD"]] %in% input$filterVFD
  included <- included & data[["trialNum"]] %in% input$filterTrials
  included <- included & data[["conditionNumber"]] %in% input$filterConditionNumber
  included <- included & data[["trialNumWithinCondition"]] %in% input$filterTrialsWithinCondition
  included <- included & data[["trialNumWithoutPractice"]] %in% input$filterTrialsWithoutPractice
  included <- included & data[["practice"]] %in% input$filterPractice
  # participant group based
  included <- included & data[["startedWithNoise"]] %in% input$filterStartCondition
  included <- included & data[["noticed"]] %in% input$filterNoticed

  included <- included & data[["foot"]] %in% input$filterSide

  return(data[included, ])
})

get_mu_dyn_long <- reactive({
  return(get_full_mu(filteredParams(), filteredTargetParams(), allQResults, categories, input$avg_feet))
})
