# Filter data based on inputs

filteredParams <- reactive({
  data <- allGaitParams
  included <- data[["participant"]] %in% input$filterParticipants
  included <- included & data[["VFD"]] %in% input$filterVFD
  included <- included & data[["trialNum"]] %in% input$filterTrials
  included <- included & data[["trialNumWithinCondition"]] %in% input$filterTrialsWithinCondition
  included <- included & data[["practice"]] %in% input$filterPractice
  included <- included & data[["startedWithNoise"]] %in% input$filterStartCondition
  included <- included & data[["noticed"]] %in% input$filterNoticed
  # Step based
  included <- included & data[["heelStrikes.incorrectDetection"]] %in% input$filterImpossible
  included <- included & data[["heelStrikes.targetIgnoreSteps"]] %in% input$filterTargets
  included <- included & data[["heelStrikes.outlierSteps"]] %in% input$filterOutliers
  
  return(data[included, ])
})

filteredQResults_new <- reactive({
  included <- allQResults[["participant"]] %in% input$filterParticipants
  included <- included & allQResults[["VFD"]] %in% input$filterVFD
  included <- included & allQResults[["startedWithNoise"]] %in% input$filterStartCondition
  return(allQResults[included, ])
})

filteredTargetParams <- reactive({
  data <- allTargetParams
  included <- data[["participant"]] %in% input$filterParticipants
  included <- included & data[["VFD"]] %in% input$filterVFD
  included <- included & data[["trialNum"]] %in% input$filterTrials
  included <- included & data[["trialNumWithinCondition"]] %in% input$filterTrialsWithinCondition
  included <- included & data[["practice"]] %in% input$filterPractice
  included <- included & data[["startedWithNoise"]] %in% input$filterStartCondition
  included <- included & data[["noticed"]] %in% input$filterNoticed
  
  return(data[included, ])
})

get_mu_dyn_long <- reactive({
  return(get_full_mu(filteredParams(), filteredTargetParams(), allQResults, categories))
})
