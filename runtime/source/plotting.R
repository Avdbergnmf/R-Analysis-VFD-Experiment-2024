plot_steps <- function(filteredGaitParams, participant, trialNum, start=1, end=500, x_axis = "time", y_axis = "pos_z", doFilter = FALSE, show_legend = TRUE, extraTitle="", baseSize = 5) { # start=first step to plot, end=last step to plot
  filteredGaitParams <- filteredGaitParams[filteredGaitParams$participant == participant & filteredGaitParams$trialNum == trialNum, ]
  
  preprocessedData <- preprocess_data(participant, trialNum) 
  rightData <- preprocessedData$rightFoot
  leftData <- preprocessedData$leftFoot
  
  # Filter out a subset of the steps if needed
  if (start > 1) {start <- start - 1} # make sure we get the step before
  steps <- start:end
  
  if (length(steps) > 1) {
    filteredGaitParams <- filteredGaitParams[filteredGaitParams$heelStrikes.step %in% steps,]
  }
  
  timeMin <- min(filteredGaitParams$heelStrikes.time)
  timeMax <- max(filteredGaitParams$heelStrikes.time)
  
  rightData <- rightData %>% dplyr::filter(time > timeMin & time < timeMax)
  leftData <- leftData %>% dplyr::filter(time > timeMin & time < timeMax)
  
  if (doFilter) {
    numeric_columns <- sapply(rightData, is.numeric)  # Identify numeric columns
    
    # Apply the filter to all numeric columns
    poly_order <- 4  # Order of the polynomial (savgoy=3,butter=4)
    frame_size <- 5  # (for savgoy Must be odd)
    rightData[numeric_columns] <- lapply(rightData[numeric_columns], function(column) { apply_padding_and_filter(column, poly_order, frame_size, 90) } ) 
    leftData[numeric_columns] <- lapply(leftData[numeric_columns], function(column) { apply_padding_and_filter(column, poly_order, frame_size, 90) } )
  }
  
  # Add a new column to both dataframes to identify the foot
  rightData$foot <- "Right"
  leftData$foot <- "Left"
  # Combine the dataframes
  both <- rbind(rightData, leftData)
  both <- both[order(both$time), ] # Order by time
  
  
  rParams <- filteredGaitParams[filteredGaitParams$heelStrikes.foot == "Right", ]
  lParams <- filteredGaitParams[filteredGaitParams$heelStrikes.foot == "Left", ]
  rTargets <- rParams[rParams$heelStrikes.target, ]
  lTargets <- lParams[lParams$heelStrikes.target, ]
  
  # Create the plot
  targetSize <- 5
  footEventSize <- 2
  p <- ggplot(both, aes(x = .data[[x_axis]], y = .data[[y_axis]], color = .data$foot)) +
    geom_path() +
    # toeOffs
    geom_point(data = rParams, aes(x = .data[[paste0("toeOffs.", x_axis)]], y = .data[[paste0("toeOffs.", y_axis)]]), shape = 24, color = "red", size = footEventSize) +
    geom_point(data = lParams, aes(x = .data[[paste0("toeOffs.", x_axis)]], y = .data[[paste0("toeOffs.", y_axis)]]), shape = 24, color = "blue", size = footEventSize) + # 12=empty square
    # heelstrikes
    geom_point(data = rParams, aes(x = .data[[paste0("heelStrikes.", x_axis)]], y = .data[[paste0("heelStrikes.", y_axis)]]), shape = 25, color = "red", size = footEventSize) + # 16=ball
    geom_point(data = lParams, aes(x = .data[[paste0("heelStrikes.", x_axis)]], y = .data[[paste0("heelStrikes.", y_axis)]]), shape = 25, color = "blue", size = footEventSize) +
    # targets
    geom_point(data = rTargets, aes(x = .data[[paste0("heelStrikes.", x_axis)]], y = .data[[paste0("heelStrikes.", y_axis)]]), shape = 10, color = "red", size = targetSize) +
    geom_point(data = lTargets, aes(x = .data[[paste0("heelStrikes.", x_axis)]], y = .data[[paste0("heelStrikes.", y_axis)]]), shape = 10, color = "blue", size = targetSize) + # 10=target
    scale_color_manual(values = c("Right" = "black", "Left" = "grey")) + 
    theme_minimal(base_size = baseSize) + ggtitle(extraTitle)
  
  if (x_axis != "time" && y_axis != "time") {
    p <- p + coord_equal()
  }
  
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  else {
    p <- p + theme(legend.position = "inside", legend.position.inside = c(0.95, 0.15))
  }
  
  return(p)
}

add_lines <- function(p, footEvents, rightData, leftData, start, end, x_axis = "time", y_axis = "pos_z") { # start=first step to plot, end=last step to plot
  heelStrikes <- footEvents$heelStrikes
  
  # Filter out a subset of the steps if needed
  if (start > 1) {
    start <- start - 1
  } # make sure we get the step before
  steps <- start:end
  
  if (length(steps) > 1) {
    heelStrikes <- subset(heelStrikes, step %in% steps)
    # Filter out the steps we want to plot
    rHeelStrikes <- subset(heelStrikes, .data$foot == "Right")
    timeMin <- min(rHeelStrikes$time)
    timeMax <- max(rHeelStrikes$time)
    rightData <- subset(rightData, time > timeMin & time < timeMax)
    leftData <- subset(leftData, time > timeMin & time < timeMax)
  }
  
  # Add a new column to both dataframes to identify the foot
  rightData$foot <- "Right"
  leftData$foot <- "Left"
  
  # Create the plot
  p <- p + geom_path(data = rightData, aes(x = .data[[x_axis]], y = .data[[y_axis]]), color = "pink") +
    geom_path(data = leftData, aes(x = .data[[x_axis]], y = .data[[y_axis]]), color = "lightblue")
  if (x_axis != "time" && y_axis != "time") {
    p <- p + coord_equal()
  }
  return(p)
}

plot_steps_with_overlay <- function(data, selected_participant, selected_trialNum, axis_to_plot, doFilter, alpha = 0.15, show_legend = FALSE, extraTitle = "") {
  p <- plot_steps(data, selected_participant, selected_trialNum, 0, 0, "time", axis_to_plot, doFilter, show_legend, extraTitle)
  
  VFD <- get_p_results(selected_participant, "noise_enabled", selected_trialNum)
  if (VFD) {
    color <- "green"
  } else {
    color <- "yellow"
  }
  
  # Add a semi-transparent square (replace xmin, xmax, ymin, ymax with your desired coordinates)
  p <- p + annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = color, alpha = alpha)
  
  return(p)
}

plot_2d <- function(xtracker, ytracker, participant, trialNum, startTime, endTime, x_axis = "time", y_axis = "pos_z", plotlines = TRUE, plotpoints = FALSE) { 
  xData <- get_t_data(participant, xtracker, trialNum)
  yData <- get_t_data(participant, ytracker, trialNum)
  startTime <- get_p_results(participant,"start_time",trialNum)
  xData <- adjust_times(xData, startTime)
  yData <- adjust_times(yData, startTime)
  
  # Filter time subset if needed
  if (endTime > startTime && endTime != 0) {
    xData <- xData %>% dplyr::filter(time > startTime & time < endTime)
    yData <- yData %>% dplyr::filter(time > startTime & time < endTime)
  }    
  
  # Combine the dataframes
  both <- data.frame(
    x = xData[[x_axis]], 
    y = yData[[y_axis]]
  )
  
  p <- ggplot(xData, aes(.data[[x_axis]], .data[[y_axis]])) +  
    labs(title = "Time Series Plot", x = x_axis, y = y_axis)
  
  if (plotlines) { 
    p <- p + geom_path()
  }
  
  if (plotpoints) { 
    p <- p + geom_point()
  }
  
  if (x_axis != "time" && y_axis != "time") {
    p <- p + coord_equal()
  }
  
  return(p)
}


plot_questionnaire_data <- function(qType, participants, cols_to_include) {
  # Get the data
  data <- calculate_all_scores(qType)
  data <- data[data$participant %in% participants, ]
  
  # Only keep the columns to include in the plot
  if (length(cols_to_include) == 0) {
    cols_to_include <- setdiff(colnames(data), c("participant", "VFD")) # just take all of them, except for participant and VFD (condition)
  }
  data <- data[, c("participant", "VFD", cols_to_include), drop = FALSE]
  
  # Reshape the data to long format for ggplot
  data_long <- reshape2::melt(data, id.vars = c("participant", "VFD"))
  #data_long$trialNum <- as.factor(data_long$trialNum)
  
  # Create the plot for each column to include
  p <- ggpaired(
    data = data_long,
    x = "VFD",
    y = "value",
    id = "participant",
    color = "VFD",
    line.color = "gray",
    line.size = 0.4
  ) +
    facet_wrap(~variable, scales = "free", ncol = length(cols_to_include)) +
    labs(x = "VFD", y = "Score") +
    ggtitle(paste0("Boxplots of ", qType, " Scores")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylim(0, 7) # Set y-axis limits
  
  return(p)
}

plot_variability_data <- function(mu_dyn, participants, datatype, metric, baseSize = 10) {
  # Get the data
  data <- mu_dyn[[datatype]]
  data <- data[, c("participant", "VFD", "trialNum", metric), drop = FALSE]
  # Identify the min and max trial numbers for each condition and participant
  #data <- data %>%
  #  group_by(participant, VFD) %>%
  #  mutate(trial_pair = ifelse(trialNum == min(trialNum), 1, 2))
  
  # Reshape the data to long format for ggplot
  data_long <- melt(data, id.vars = c("participant", "VFD", "trialNum")) # "trial_pair"
  data_long <- data_long[data_long$participant %in% participants, ]
  data_long$trialNum <- as.factor(data_long$trialNum)
  #data_long$trial_pair <- as.factor(data_long$trial_pair)
  
  # Create the plot
  p <- ggpaired(
    data = data_long,
    x = "VFD",
    y = "value",
    id = "participant",
    color = "trialNum", # VFD, trialNum, trial_pair
    line.color = "grey",
    point.color = "color",
    line.size = 0.0
  ) +
    labs(x = "VFD", y = metric) +
    ggtitle(paste0("Boxplots of ", datatype, " (", metric, ") Scores")) +
    theme(plot.title = element_text(hjust = 0.5)) + coord_cartesian(ylim = c(0,0.3)) + 
    theme_minimal(base_size = baseSize)
    #coord_cartesian(ylim = range(data_long$value, na.rm = TRUE)) # Set y limits to the range of y values in data
    #scale_color_identity(name = "Trial Pair", labels = names(color_map), aesthetics = "point.color") + # Use the colors as they are
    
  
  return(p)
}

make_pie_chart <- function(data, titleExtra = "", show_legend = TRUE, baseSize = 5){
  #data <- filteredParams()
  targetIgnoreSteps <- length(data[data$heelStrikes.targetIgnoreSteps==TRUE & data$heelStrikes.outlierSteps == FALSE,]$VFD)
  outlierSteps <- length(data[data$heelStrikes.targetIgnoreSteps==FALSE & data$heelStrikes.outlierSteps == TRUE,]$VFD)
  bothSteps <- length(data[data$heelStrikes.targetIgnoreSteps==TRUE & data$heelStrikes.outlierSteps == TRUE,]$VFD)
  included <- length(data[data$heelStrikes.targetIgnoreSteps==FALSE & data$heelStrikes.outlierSteps == FALSE,]$VFD) # non filtered out
  total_steps <- length(data$VFD)
  
  # Create a data frame for ggplot
  df_filtered <- data.frame(
    StepType = factor(c("Both", "Target Ignore", "Outlier", "Included")),
    TotalCount = c(bothSteps, targetIgnoreSteps, outlierSteps, included)
  )
  
  ### LABELING LOOKS HELLA OFF, HARDCODE FIX FOR NOW
  # Order the data frame by StepType for consistent plotting
  #df_filtered <- df_filtered[order(df_filtered$StepType, decreasing = TRUE), ]
  
  # Calculate label positions for the pie chart
  #df_filtered$midpoint <- cumsum(df_filtered$TotalCount) - df_filtered$TotalCount / 2
  #df_filtered$label_pos <- df_filtered$midpoint / sum(df_filtered$TotalCount) * 2 * pi
  
  # Calculate label positions for the pie chart
  df_filtered$label_pos <- cumsum(df_filtered$TotalCount) - df_filtered$TotalCount/2
  
  # Generate the pie chart
  p <- ggplot(df_filtered, aes(x = "", y = TotalCount, fill = StepType)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y", start = 0) +
    theme_void() +
    scale_fill_brewer(palette = "Pastel1") +
    geom_text(aes(label = TotalCount, y = label_pos), color = "black") +
    ggtitle(paste0(titleExtra, "Total steps = ", total_steps)) +
    theme_minimal(base_size = baseSize)
  
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}


#### Target plots

circleFun <- function(center = c(0,0),r = 1, npoints = 100){
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

make_target_steps_plot <- function(targetData){
  #filteredTargetParams()
  circle = circleFun(c(0,0),0.3,100)
  axesLims <- 0.3
  p <- ggplot() +
    geom_path(data = circle, aes(x = x, y = y), color = "black") +
    geom_point(data = targetData, aes(x = rel_x, y = rel_z,col=VFD), fill=rgb(0,0,0,0.2),shape = 21,size=5) + 
    xlim(-axesLims,axesLims) +
    ylim(-axesLims,axesLims)
  
  p <- p + coord_equal() +
    labs(title = "Center Foot Relative to Target Center", x = "x", y = "z")
  
  return(p)
}


### Scatter plots
make_scatter_plot_steps <- function(data, group, xplot, yplot, show_legend = FALSE, baseSize = 10){
  if (group == "None") {
    aes <- aes_string(x = xplot, y = yplot)
  }
  else{
    aes <- aes_string(x = xplot, y = yplot, col = group)
  }
  
  p <- ggplot(data, aes) +
    geom_point(alpha = 0.5) + # Set the alpha to make overlapping points more visible
    theme_minimal(base_size = baseSize)
  
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  #else {
  #  p <- p + theme(legend.position = "inside", legend.position.inside = c(0.95, 0.15))
  #}
  
  both_contain_pos <- grepl("pos", xplot, ignore.case = TRUE) && grepl("pos", yplot, ignore.case = TRUE)
  if (both_contain_pos) {
    p <- p + coord_equal()
  }
  
  # Add marginal density plots
  p <- ggMarginal(p, type = "density", margins = "both", groupColour = TRUE, groupFill = TRUE)
  
  return(p)
}

###### Extra

# Function to save plot as an image
save_plot <- function(plot, filename) {
  ggsave(filename, plot, device = "png", width = 8, height = 4)
}

save_plot_as_pdf <- function(plot, filename) {
  pdf(filename, width = 8, height = 4)
  print(plot)
  dev.off()
}