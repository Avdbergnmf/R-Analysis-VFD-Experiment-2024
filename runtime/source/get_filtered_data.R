# Initialize reactiveVal to store the filtered parameters
filteredParams <- reactiveVal()

filter_data <- function(allGaitParams, filterParticipants, filterVFD, filterTrials, filterTargets, filterOutliers, filterPractice, filterStartCondition, additionalArg) {
  included <- allGaitParams[["participant"]] %in% filterParticipants
  included <- included & allGaitParams[["VFD"]] %in% filterVFD
  included <- included & allGaitParams[["trialNum"]] %in% filterTrials
  included <- included & allGaitParams[["heelStrikes.targetIgnoreSteps"]] %in% filterTargets
  included <- included & allGaitParams[["heelStrikes.outlierSteps"]] %in% filterOutliers
  included <- included & allGaitParams[["practice"]] %in% filterPractice
  included <- included & allGaitParams[["startedWithNoise"]] %in% filterStartCondition
  return(allGaitParams[included, ])
}

# Update the filtered parameters whenever any input changes
observeEvent({
  list(input$filterParticipants, input$filterVFD, input$filterTrials,
       input$filterTargets, input$filterOutliers, input$filterPractice, input$filterStartCondition, input$additionalArg)
}, {
  filtered_data <- filter_data(
    allGaitParams, 
    input$filterParticipants, 
    input$filterVFD, 
    input$filterTrials, 
    input$filterTargets, 
    input$filterOutliers, 
    input$filterPractice,
    input$filterStartCondition,
    input$additionalArg  # The additional argument
  )
  filteredParams(filtered_data)
})

filteredTargetParams <- reactive({
  included <- allTargetParams[["participant"]] %in% input$filterParticipants
  included <- included & allTargetParams[["VFD"]] %in% input$filterVFD
  included <- included & allTargetParams[["trialNum"]] %in% input$filterTrials
  included <- included & allTargetParams[["practice"]] %in% input$filterPractice
  included <- included & allTargetParams[["startedWithNoise"]] %in% input$filterStartCondition
  
  return(allTargetParams[included, ])
})

get_mu_dyn_long <- reactive({
  return(get_full_mu(filteredParams(), filteredTargetParams(), allQResults, categories))
})
