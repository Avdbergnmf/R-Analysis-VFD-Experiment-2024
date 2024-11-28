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
  rTargets <- rParams[rParams$heelStrikes.targetIgnoreSteps, ]
  lTargets <- lParams[lParams$heelStrikes.targetIgnoreSteps, ]

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
    # geom_point(data = rParams, aes(x = .data[[paste0("toeOffs.", x_axis)]], y = .data[[paste0("toeOffs.", y_axis)]]), shape = 24, color = "red", size = footEventSize) +
    # geom_point(data = lParams, aes(x = .data[[paste0("toeOffs.", x_axis)]], y = .data[[paste0("toeOffs.", y_axis)]]), shape = 24, color = "blue", size = footEventSize) + # 12=empty square
    # heelstrikes
    geom_point(data = rParams, aes(x = .data[[paste0("heelStrikes.", x_axis)]], y = .data[[paste0("heelStrikes.", y_axis)]]), shape = 25, color = "red", size = footEventSize) + # 16=ball
    geom_point(data = lParams, aes(x = .data[[paste0("heelStrikes.", x_axis)]], y = .data[[paste0("heelStrikes.", y_axis)]]), shape = 25, color = "blue", size = footEventSize) +
    # targets
    geom_point(data = rTargets, aes(x = .data[[paste0("heelStrikes.", x_axis)]], y = .data[[paste0("heelStrikes.", y_axis)]]), shape = 10, color = "red", size = targetSize) +
    geom_point(data = lTargets, aes(x = .data[[paste0("heelStrikes.", x_axis)]], y = .data[[paste0("heelStrikes.", y_axis)]]), shape = 10, color = "blue", size = targetSize) + # 10=target
    # outlier steps (with filled icons and larger size)
    geom_point(data = rOutliers, aes(x = .data[[paste0("heelStrikes.", x_axis)]], y = .data[[paste0("heelStrikes.", y_axis)]]), shape = 25, fill = "red", alpha = 0.5, color = "red", size = outlierSize) +
    geom_point(data = lOutliers, aes(x = .data[[paste0("heelStrikes.", x_axis)]], y = .data[[paste0("heelStrikes.", y_axis)]]), shape = 25, fill = "blue", alpha = 0.5, color = "blue", size = outlierSize) + # 21 = filled circle
    # geom_point(data = rOutliers, aes(x = .data[[paste0("toeOffs.", x_axis)]], y = .data[[paste0("toeOffs.", y_axis)]]), shape = 24, fill = "red", alpha = 0.5, color = "red", size = outlierSize) +
    # geom_point(data = lOutliers, aes(x = .data[[paste0("toeOffs.", x_axis)]], y = .data[[paste0("toeOffs.", y_axis)]]), shape = 24, fill = "blue", alpha = 0.5, color = "blue", size = outlierSize) +
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

plot_2d <- function(xtracker, ytracker, participant, trialNum, x_axis = "time", y_axis = "pos_z", plotlines = TRUE, plotpoints = FALSE, extraTitle = "", baseSize = 10) {
  xData <- get_t_data(participant, xtracker, trialNum)
  yData <- get_t_data(participant, ytracker, trialNum)
  startTime <- get_p_results(participant, "start_time", trialNum)
  maxTime <- ifelse(get_p_results(participant, "practice", trialNum) == "True", 120, 180)
  xData <- adjust_times(xData, startTime, maxTime)
  yData <- adjust_times(yData, startTime, maxTime)

  # Combine the dataframes
  both <- data.frame(
    x = xData[[x_axis]],
    y = yData[[y_axis]]
  )

  p <- ggplot(xData, aes(.data[[x_axis]], .data[[y_axis]])) +
    labs(x = x_axis, y = y_axis) +
    theme_minimal(base_size = baseSize) +
    ggtitle(paste(extraTitle, ",", x_axis, "vs.", y_axis))

  # p <- p + coord_cartesian(ylim = override_ylims)

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

plot_boxplots <- function(mu, datatype, xaxis = c("VFD"), color_var = NULL, shape_var = NULL, baseSize = 10) {
  # Reshape data to long format for ggplot
  data_long <- mu %>%
    pivot_longer(
      cols = !!datatype,
      names_to = "variable",
      values_to = "value"
    )

  # Create a combined x-axis variable
  data_long$xaxis_combined <- apply(data_long[, xaxis], 1, paste, collapse = "_")

  if (is.null(color_var) || color_var == "None") { #
    aesString <- aes_string()
  } else {
    aesString <- aes_string(color = color_var)
  }

  # Dynamically add shape if shape_var is provided
  if (!(is.null(shape_var) || shape_var == "None")) {
    aesString <- modifyList(aesString, aes_string(shape = shape_var))
  }

  # Create the plot
  p <- ggplot(data_long, aes(x = xaxis_combined, y = value)) +
    geom_boxplot() +
    geom_jitter(aesString, width = 0.2, size = baseSize / 4, alpha = 0.7) + # << make this point?
    labs(x = paste(xaxis, collapse = " + "), y = datatype, title = datatype) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_viridis_d(option = "turbo") + # Choose "viridis", "plasma", "magma", etc.
    # scale_shape_manual(values = c(1:25)) +
    get_sized_theme(baseSize)

  if (color_var == "trialNum") { # special case for trialnum, because we used this in our paper and we wanted this layout there
    shapes <- c(15, 15, 16, 16) # Square for 1, 2 and Circle for 3, 4
    colors <- c("darkred", "pink", "darkblue", "lightblue") # Dark/light for 1, 2 and 3, 4

    p <- p +
      scale_color_manual(name = "Trial Number", values = colors) +
      scale_shape_manual(name = "Trial Number", values = shapes)
  }

  return(p)
}

plot_paired <- function(mu, datatype, xPaired, xaxis = NULL, color_var = NULL, shape_var = NULL, baseSize = 10) {
  # Reshape data to long format for ggplot
  data_long <- mu %>%
    pivot_longer(
      cols = !!datatype,
      names_to = "variable",
      values_to = "value"
    )

  if (is.null(color_var) || color_var == "None") { #
    aesString <- aes_string()
  } else {
    aesString <- aes_string(color = color_var)
  }

  # Dynamically add shape if shape_var is provided
  if (!(is.null(shape_var) || shape_var == "None")) {
    aesString <- modifyList(aesString, aes_string(shape = shape_var))
  }

  # Create the plot using ggplot
  p <- ggplot(data_long, aes_string(x = xPaired, y = "value", group = "participant")) +
    geom_line(aes(group = participant), color = "gray", size = 0.4) +
    geom_point(aesString, size = baseSize / 4) +
    labs(x = xPaired, y = datatype, title = datatype) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_viridis_d(option = "turbo") + # Choose "viridis", "plasma", "magma", etc.
    # scale_shape_manual(values = c(1:25)) +
    get_sized_theme(baseSize) #+ scale_color_jco()

  # Add facets if split_vars are provided
  if (!is.null(xaxis) && length(xaxis) > 0) {
    # Create facet formula
    facet_formula <- paste("~", paste(xaxis, collapse = " + "))
    p <- p + facet_wrap(as.formula(facet_formula))
  }

  return(p)
}



make_pie_chart <- function(data, extraTitle = "", show_legend = TRUE, baseSize = 10) {
  # new marking
  targetIgnoreSteps <- length(data[data$heelStrikes.targetIgnoreSteps == TRUE, ]$VFD)
  outlierSteps <- length(data[data$heelStrikes.outlierSteps == TRUE, ]$VFD)
  included <- length(data[data$heelStrikes.targetIgnoreSteps == FALSE & data$heelStrikes.outlierSteps == FALSE, ]$VFD) # non filtered out
  total_steps <- length(data$VFD)

  # Create a data frame for ggplot
  df_filtered <- data.frame(
    StepType = factor(c("Target Ignore", "Outlier", "Included")),
    TotalCount = c(targetIgnoreSteps, outlierSteps, included)
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

plot_correlation_stats <- function(data, x_var_name, y_var_name, type = "parametric", base_size = 12, do_heatmap = FALSE, heatmap_bins = 30) {
  x_var <- sym(x_var_name)
  y_var <- sym(y_var_name)
  # Create the base plot with ggscatterstats or a heatmap based on plot_type
  if (!do_heatmap) {
    # Scatter plot with statistical summary
    p <- ggscatterstats(
      data = data,
      x = !!x_var,
      y = !!y_var,
      type = type
    ) +
      theme_minimal(base_size = base_size)
  } else {
    # Heatmap to visualize density of points
    p <- ggplot(data, aes(x = !!x_var, y = !!y_var)) +
      stat_bin2d(bins = heatmap_bins) +
      scale_fill_gradient(low = "blue", high = "red") +
      theme_minimal(base_size = base_size) +
      labs(
        title = paste("Heatmap of", x_var_name, "and", y_var_name),
        x = x_var_name,
        y = y_var_name,
        fill = "Count"
      )
  }

  return(p)
}
