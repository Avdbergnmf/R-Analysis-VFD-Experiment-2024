# setup.R
knitr::opts_chunk$set(echo = FALSE)
library(data.table)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(readxl)
library(rstatix)
library(plotrix)
library(car)
library(Rmisc)

# data manipulation
library(plyr)
library(dplyr)
library(purrr)

# plotting
library(tidyr)
library(devtools)
library(ggpattern) # To differentiate when printing in black and white

# LMER stuff
library(lme4)
library(DT) # for visual tables and stuff
library(lmerTest) # to get p-values from lmer
library(emmeans)
library(MuMIn)

# saving figures
library(svglite)

# File importing / reading
library(jsonlite) # for json files

# Reshaping data for questionnaires
library(reshape2)

library(pwr)

# Filtering of foot signal for event detection
library(signal) # butterworth filter
library(zoo) # For rolling statistics

# For correlation plots
library(ggstatsplot)
library(ggside)
# scatterplot with distribution on the sides
library(ggExtra)

# For checking parametric assumptions
library(lmtest)

# Parallel processing
# Load necessary packages
library(foreach)
library(doParallel)

# To identify local outliers
# library(Rlof) # for lof filtering
library(dbscan) # also for lof filtering ?
library(DT)
library(plotly)

library(shinyBS) # For tooltips

library(viridis)