# Filter data based on inputs

filteredParams <- reactive({
  # Force dependency on `refresh_trigger()`
  refresh_trigger()

  data <- allGaitParams
  included <- data[["participant"]] %in% input$filterParticipants
  # Trial based
  included <- included & data[["trialNum"]] %in% input$filterTrials

  # Step based
  included <- included & data[["heelStrikes.incorrectDetection"]] %in% input$filterImpossible
  included <- included & data[["heelStrikes.outlierSteps"]] %in% input$filterOutliers

  included <- included & data[["heelStrikes.foot"]] %in% input$filterSide

  return(data[included, ])
})

filteredQResults_new <- reactive({
  data <- allQResults
  included <- data[["participant"]] %in% input$filterParticipants
  included <- included & data[["trialNum"]] %in% input$filterTrials

  return(data[included, ])
})

get_mu_dyn_long <- reactive({
  return(get_full_mu(filteredParams(), allQResults))
})
