### Layout helpers
get_sized_theme <- function(baseSize) {
  return(theme(
    axis.title = element_text(size = baseSize * 2),
    axis.text = element_text(size = baseSize),
    legend.title = element_text(size = baseSize * 1.5),
    legend.text = element_text(size = baseSize * 1.5),
    plot.title = element_text(size = baseSize * 2.5),
    strip.text = element_text(size = baseSize * 1.5)
  ))
}

get_proper_legend <- function(show_legend, position = "inside") {
  if (!show_legend) {
    return(theme(legend.position = "none"))
  } else {
    if (position == "inside") {
      return(theme(legend.position = position, legend.position.inside = c(0.95, 0.15)))
    } else {
      theme(legend.position = position)
    }
  }
}

plot_steps <- function(filteredGaitParams, participant, trialNum, x_axis = "time", y_axis = "pos_z", doFilter = FALSE, show_legend = TRUE, extraTitle = "", baseSize = 5, xlim = NULL, ylim = NULL, preprocessedData = NULL) { # start=first step to plot, end=last step to plot
  filteredGaitParams <- filteredGaitParams[filteredGaitParams$participant == participant & filteredGaitParams$trialNum == trialNum, ]

  if (is.null(preprocessedData)) {
    preprocessedData <- preprocess_data(participant, trialNum)
  }

  rightData <- preprocessedData$rightFoot
  leftData <- preprocessedData$leftFoot

  if (doFilter) {
    numeric_columns <- sapply(rightData, is.numeric) # Identify numeric columns
    rightData[numeric_columns] <- lapply(rightData[numeric_columns], function(column) {
      apply_padding_and_filter(column, 4, 90)
    })
    leftData[numeric_columns] <- lapply(leftData[numeric_columns], function(column) {
      apply_padding_and_filter(column, 4, 90)
    })
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

  # Separate outliers from non-outliers
  rOutliers <- rParams[rParams$heelStrikes.outlierSteps, ]
  lOutliers <- lParams[lParams$heelStrikes.outlierSteps, ]
  rParams <- rParams[!rParams$heelStrikes.outlierSteps, ]
  lParams <- lParams[!lParams$heelStrikes.outlierSteps, ]

  # Create the plot
  targetSize <- round(baseSize / 2)
  footEventSize <- round(baseSize / 4)
  outlierSize <- round(baseSize / 2) # Use a larger size for outliers

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
    # outlier steps (with filled icons and larger size)
    geom_point(data = rOutliers, aes(x = .data[[paste0("heelStrikes.", x_axis)]], y = .data[[paste0("heelStrikes.", y_axis)]]), shape = 25, fill = "red", alpha = 0.5, color = "red", size = outlierSize) +
    geom_point(data = lOutliers, aes(x = .data[[paste0("heelStrikes.", x_axis)]], y = .data[[paste0("heelStrikes.", y_axis)]]), shape = 25, fill = "blue", alpha = 0.5, color = "blue", size = outlierSize) + # 21 = filled circle
    geom_point(data = rOutliers, aes(x = .data[[paste0("toeOffs.", x_axis)]], y = .data[[paste0("toeOffs.", y_axis)]]), shape = 10, fill = "red", alpha = 0.5, color = "red", size = outlierSize) +
    geom_point(data = lOutliers, aes(x = .data[[paste0("toeOffs.", x_axis)]], y = .data[[paste0("toeOffs.", y_axis)]]), shape = 10, fill = "blue", alpha = 0.5, color = "blue", size = outlierSize) +
    scale_color_manual(values = c("Right" = "black", "Left" = "grey")) +
    ggtitle(extraTitle) +
    theme_minimal(base_size = baseSize) # get_sized_theme(baseSize)

  if (x_axis != "time" && y_axis != "time") {
    p <- p + coord_equal()
  }

  xlimValid <- xlim[1] < xlim[2] && length(xlim) == 2 && sum(is.na(xlim)) == 0
  ylimValid <- ylim[1] < ylim[2] && length(ylim) == 2 && sum(is.na(ylim)) == 0
  if (!is.null(xlim) && xlimValid) {
    p <- p + xlim(xlim)
  }
  if (!is.null(ylim) && ylimValid) {
    p <- p + ylim(ylim)
  }

  p <- p + get_proper_legend(show_legend)

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

plot_steps_with_overlay <- function(data, selected_participant, selected_trialNum, axis_to_plot, doFilter, alpha = 0.15, show_legend = FALSE, extraTitle = "", baseSize = 10) {
  p <- plot_steps(data, selected_participant, selected_trialNum, "time", axis_to_plot, doFilter, show_legend, extraTitle, baseSize)

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

plot_2d <- function(xtracker, ytracker, participant, trialNum, startTime, endTime, x_axis = "time", y_axis = "pos_z", plotlines = TRUE, plotpoints = FALSE, extraTitle = "", override_ylims = c(), baseSize = 10) {
  xData <- get_t_data(participant, xtracker, trialNum)
  yData <- get_t_data(participant, ytracker, trialNum)
  startTime <- get_p_results(participant, "start_time", trialNum)
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
    labs(x = x_axis, y = y_axis) +
    theme_minimal(base_size = baseSize) +
    ggtitle(paste(extraTitle, ",", x_axis, "vs.", y_axis))

  if (length(override_ylims) == 2) {
    p <- p + coord_cartesian(ylim = override_ylims) # ylim(override_ylims)#
  }

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


plot_questionnaire_data <- function(data, qType, cols_to_include = c(), baseSize = 10) {
  data <- filter_questionnaire_results(data, qType)
  # Only keep the columns to include in the plot
  if (length(cols_to_include) == 0) {
    cols_to_include <- setdiff(colnames(data), c("participant", "VFD", "none")) # just take all of them, except for participant and VFD (condition) - Also removing the none scale because in that case we are just interested in the total.
  }
  data <- data[, c("participant", "VFD", cols_to_include), drop = FALSE]

  # Reshape the data to long format for ggplot
  data_long <- reshape2::melt(data, id.vars = c("participant", "VFD"))

  qweights <- get_question_weights(qType)
  min_plot <- if ("min_plot" %in% qweights$category) qweights[qweights$category == "min_plot", "weight"] else NULL
  max_plot <- if ("max_plot" %in% qweights$category) qweights[qweights$category == "max_plot", "weight"] else NULL

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
    ggtitle(paste0(qType, " Scores")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    get_sized_theme(baseSize) # theme_minimal(base_size = baseSize)

  # Conditionally add y-axis limits if min_plot and max_plot are provided
  if (!is.null(min_plot) && !is.null(max_plot)) {
    p <- p + ylim(min_plot, max_plot)
  }

  return(p)
}

make_histogram <- function(data, mu_data, showMeans, group, split, xinput, binwidth, position, baseSize) {
  aes <- aes_string(x = xinput)
  a <- 1
  fill <- "grey"
  if (group != "None") {
    fill <- "white"
    aes <- modifyList(aes, aes_string(col = group))
    if (position == "identity") {
      a <- 1 / 2
    }
  }

  p <- ggplot(data, aes) +
    geom_histogram(binwidth = binwidth, fill = fill, alpha = a, position = position) +
    theme_minimal(base_size = baseSize)

  # if (showMeans && split != "None") {
  #  p <- p + geom_vline(mu_data[[xinput]], mapping = aes_string(xintercept = "mean", col = split), linetype = "dashed")
  # }
  # if (showMeans && split == "None" && group != "None") {
  #  p <- p + geom_vline(mu_data[[xinput]], mapping = aes_string(xintercept = "mean", col = group), linetype = "dashed")
  # }

  if (split != "None") {
    p <- p + facet_grid(sym(split))
  }

  return(p)
}

plot_boxplots <- function(mu, participants, datatype, xaxis = c("VFD"), baseSize = 10) {
  ylims <- c()
  if (grepl(".sd", datatype)) {
    # ylims <- c(0, 0.05)
  } else if (grepl(".cv", datatype)) {
    # ylims <- c(0, 0.3)
  }

  # Filter data for the specified columns and participants
  data_long <- mu %>%
    select(c("participant", "trialNum", all_of(xaxis), !!datatype))
  data_long <- data_long[data_long$participant %in% participants, ]

  # Reshape data to long format for ggplot
  data_long <- data_long %>%
    pivot_longer(
      cols = !!datatype,
      names_to = "variable",
      values_to = "value"
    )

  # Set trialNum as factor so we can use it to color our datapoints
  data_long$trialNum <- factor(data_long$trialNum, levels = c(2, 3, 5, 6), labels = c(1, 2, 3, 4))

  # Create a combined x-axis variable
  data_long$xaxis_combined <- apply(data_long[, xaxis], 1, paste, collapse = "_")

  # Define shapes and colors
  shapes <- c(15, 15, 16, 16) # Square for 1, 2 and Circle for 3, 4
  colors <- c("darkred", "pink", "darkblue", "lightblue") # Dark/light for 1, 2 and 3, 4

  # Create the plot
  p <- ggplot(data_long, aes(x = xaxis_combined, y = value)) +
    geom_boxplot() +
    geom_jitter(aes(color = trialNum, shape = trialNum), width = 0.2, size = baseSize / 4, alpha = 0.7) +
    labs(x = paste(xaxis, collapse = " + "), y = datatype) +
    ggtitle(datatype) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(name = "Trial Number", values = colors) +
    scale_shape_manual(name = "Trial Number", values = shapes) +
    get_sized_theme(baseSize)

  if (length(ylims) == 2) {
    p <- p + coord_cartesian(ylim = ylims)
  }
  return(p)
}



make_pie_chart <- function(data, extraTitle = "", show_legend = TRUE, baseSize = 10) {
  # data <- filteredParams()
  # incorrectDetectionSteps   <- length(data[data$heelStrikes.incorrectDetection==TRUE  & data$heelStrikes.targetIgnoreSteps==FALSE & data$heelStrikes.outlierSteps == TRUE, ]$VFD)
  # targetIgnoreSteps         <- length(data[data$heelStrikes.incorrectDetection==FALSE & data$heelStrikes.targetIgnoreSteps==TRUE  & data$heelStrikes.outlierSteps == TRUE, ]$VFD)
  # outlierSteps              <- length(data[data$heelStrikes.incorrectDetection==FALSE & data$heelStrikes.targetIgnoreSteps==FALSE & data$heelStrikes.outlierSteps == TRUE, ]$VFD)
  # bothSteps                 <- length(data[data$heelStrikes.incorrectDetection==FALSE & data$heelStrikes.targetIgnoreSteps==TRUE  & data$heelStrikes.outlierSteps == TRUE, ]$VFD)
  # new marking
  incorrectDetectionSteps <- length(data[data$heelStrikes.incorrectDetection == TRUE, ]$VFD)
  targetIgnoreSteps <- length(data[data$heelStrikes.targetIgnoreSteps == TRUE, ]$VFD)
  outlierSteps <- length(data[data$heelStrikes.outlierSteps == TRUE, ]$VFD)
  included <- length(data[data$heelStrikes.incorrectDetection == FALSE & data$heelStrikes.targetIgnoreSteps == FALSE & data$heelStrikes.outlierSteps == FALSE, ]$VFD) # non filtered out
  total_steps <- length(data$VFD)

  # Create a data frame for ggplot
  df_filtered <- data.frame(
    StepType = factor(c("Impossible", "Target Ignore", "Outlier", "Included")),
    TotalCount = c(incorrectDetectionSteps, targetIgnoreSteps, outlierSteps, included)
  )

  # Calculate label positions for the pie chart
  df_filtered$label_pos <- cumsum(df_filtered$TotalCount) - df_filtered$TotalCount / 2

  # Generate the pie chart
  p <- ggplot(df_filtered, aes(x = "", y = TotalCount, fill = StepType)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y", start = 0) +
    theme_void() +
    scale_fill_brewer(palette = "Pastel1") +
    geom_text(aes(label = TotalCount, y = label_pos), color = "black", size = round(baseSize / 2)) +
    ggtitle(paste0(extraTitle, "Total steps = ", total_steps)) +
    theme_minimal(base_size = baseSize) # get_sized_theme(baseSize)# theme_minimal(base_size = baseSize)

  p <- p + get_proper_legend(show_legend, "right")

  return(p)
}


#### Target plots

make_target_histogram <- function(data, group, split, xinput, binwidth, position, baseSize = 10) {
  aes <- aes_string(x = xinput)
  a <- 1
  fill <- "grey"
  if (group != "None") {
    fill <- "white"
    aes <- modifyList(aes, aes_string(col = group))
    if (position == "identity") {
      a <- 1 / 2
    }
  }

  p <- ggplot(data, aes) +
    geom_histogram(binwidth = binwidth, fill = fill, alpha = a, position = position) +
    theme_minimal(base_size = baseSize)

  if (split != "None") {
    p <- p + facet_grid(sym(split))
  }

  return(p)
}

circleFun <- function(center = c(0, 0), r = 1, npoints = 100) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

make_target_steps_plot <- function(targetData, show_legend = TRUE, baseSize = 10) {
  circle <- circleFun(c(0, 0), 0.3, 100)
  axesLims <- 0.3
  p <- ggplot() +
    geom_path(data = circle, aes(x = x, y = y), color = "black") +
    geom_point(data = targetData, aes(x = rel_x, y = rel_z, col = VFD), fill = rgb(0, 0, 0, 0.2), shape = 21, size = 5) +
    xlim(-axesLims, axesLims) +
    ylim(-axesLims, axesLims) +
    theme_minimal(base_size = baseSize)
  p <- p + get_proper_legend(show_legend)

  p <- p + coord_equal() +
    labs(title = "Center Foot Relative to Target Center", x = "x", y = "z")

  # Add marginal density plots
  p <- ggExtra::ggMarginal(p, type = "density", margins = "both", groupColour = TRUE, groupFill = TRUE)

  return(p)
}


### Scatter plots
make_scatter_plot_steps <- function(data, group, xplot, yplot, show_legend = FALSE, baseSize = 10) {
  if (group == "None") {
    aes <- aes_string(x = xplot, y = yplot)
  } else {
    aes <- aes_string(x = xplot, y = yplot, col = group)
  }

  p <- ggplot(data, aes) +
    geom_point(alpha = 0.5) + # Set the alpha to make overlapping points more visible
    theme_minimal(base_size = baseSize)

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  # else {
  #  p <- p + theme(legend.position = "inside", legend.position.inside = c(0.95, 0.15))
  # }

  both_contain_pos <- grepl("pos", xplot, ignore.case = TRUE) && grepl("pos", yplot, ignore.case = TRUE)
  if (both_contain_pos) {
    p <- p + coord_equal()
  }

  # Add marginal density plots
  p <- ggMarginal(p, type = "density", margins = "both", groupColour = TRUE, groupFill = TRUE)

  return(p)
}

make_scatter_plot_mu <- function(data, xinput, yinput, group, baseSize = 10) {
  if (group == "None") {
    aes <- aes_string(x = xinput, y = yinput)
  } else {
    aes <- aes_string(x = xinput, y = yinput, col = group)
  }

  p <- ggplot(data, aes) +
    geom_point(alpha = 0.5, size = round(baseSize / 5)) + # Set the alpha to make overlapping points more visible
    theme_minimal(base_size = baseSize)

  return(p)
}

###### Extra

# Function to save plot as an image
save_plot <- function(plot, filename, width = 8, height = 4, pdf = FALSE) {
  if (!pdf) {
    ggsave(filename, plot, device = "png", width = width, height = height)
  } else {
    pdf(filename, width = width, height = height)
    print(plot)
    dev.off()
  }
}
