# Analysis Code
The code used for the analysis of kinematic data to investigate the effect of visual feedback distortions (VFD) on gait variability while walking on a treadmill.

## How to use
To run this code with your local R-Studio installation:
1. Within the R instance, navigate to `./runtime/`, and open the `index.Rmd` file.
2. Click `Run Document`. If it asks you to install any packages, click yes. You can find the list of packages used in `./runtime/source/setup.R`.
> Initial calculation of the datasets may take some time (~5 minutes). After the first run, the results should be saved to tables in `results/` and startup should be significantly faster.
3. Browse the tabs and interact with the data

## Contents
- `index.Rmd` - The main Rmd code to run that loads all the source code, calculates or loads the results, and renders all the pages + sidebar. --> Run this code to start the interface.
    - The first block in this file (setup block) loads all the source code and calculates the heelstrikes and results. The first time this is done, it creates Rds files in the results folder, which will be loaded instead of calculated in subsequent runs. To recalculate the datasets, simply delete the old ones, and re-run this first block (or the whole file).
    - Then a series of pages are loaded. For an overview of each page, see below.

- `data/` - Contains all data of all participants.
- `pages/` - Contains the Rmd code for rendering the shiny pages + sidebar (shown below).
- `source/` - Contains the source code for the calculation of all the parameters etc. Note: also contains a `README.md`.
- `questionnaires/` - Contains information about the questionnaire questions (how to calculate the results, categories within the questionnaire, etc.).
- `results/` - Folder in which the resulting tables are stored (calculated gait parameters, target parameters, questionnaire results).

## Pages
Click through the different tabs to get a preview and a brief explanation of what can be found there.

- [Page 0: `sidebar_dynamicDataFiltering.Rmd`](#page-0-sidebar_dynamicDataFilteringrmd)
- [Page 1: `page1_feetTrajectories.Rmd`](#page-1-page1_feettrajectoriesrmd)
- [Page 2: `page2_removedSteps.Rmd`](#page-2-page2_removedstepsrmd)
- [Page 3: `page3_rawTrackerData.Rmd`](#page-3-page3_rawtrackerdatarmd)
- [Page 4: `page4_targetSteps.Rmd`](#page-4-page4_targetstepsrmd)
- [Page 5: `page5_histograms.Rmd`](#page-5-page5_histogramsrmd)
- [Page 6: `page6_scatterplots.Rmd`](#page-6-page6_scatterplotsrmd)
- [Page 7: `page7_trialScatterplots.Rmd`](#page-7-page7_trialscatterplotsrmd)
- [Page 8: `page8_questionnaires.Rmd`](#page-8-page8_questionnairesrmd)
- [Page 9: `page9_boxplots.Rmd`](#page-9-page9_boxplotsrmd)
- [Page 10: `page10_statistics.Rmd`](#page-10-page10_statisticsrmd)
- [Page 11: `page11_correlations.Rmd`](#page-11-page11_correlationsrmd)
- [Page 12: `page12_participantSummary.Rmd`](#page-12-page12_participantsummaryrmd)
- [Page 13: `page13_table.Rmd`](#page-13-page13_tablermd)
- [Page 14: `page14_dataCorrection.Rmd`](#page-14-page14_datacorrectionrmd)
- [Page 15: `page15_dataFiltering.Rmd`](#page-15-page15_datafilteringrmd)
- [Page 16: `page16_manualOutlierFiltering.Rmd`](#page-16-page16_manualoutlierfilteringrmd)


---

### Page 0: `sidebar_dynamicDataFiltering.Rmd`

some general info:

- `sidebar_dynamicDataFiltering.Rmd`
The first "page" is the sidebar. This contains some settings that are shared across all pages. Most critical of this are the filtering settings, allowing you to filter out steps / trials / participants. There are also settings that change the figure sizes. (Sidebar is shown below in the screenshot of page1). Note that the filtered data is used for all plots and statistics calculations, and so changing these settings affects all the plots.

- Categories explanation:
    Some pages, or filters in the sidebar have selectors for categories (e.g., the histograms), which allow you to split the data for that visualization. These categories are:
    - **participant**: split data per participant
    - **trialNum**: split data by trial number (1=practice, 2, 3, 4=practice, 5, or 6)
    - **practice**: categorize data based on whether it's a practice trial (trials 1 and 4) or not (the rest) (TRUE or FALSE)
    - **startedWithNoise**: separate data based on whether the participant started with VFD (noise) condition or not (TRUE or FALSE)
    - **conditionNumber**: categorize data by condition number (first or second)
    - **trialNumWithoutPractice**: split data by trial number, excluding practice trials (1, 2, 3, or 4)
    - **trialNumWithinCondition**: categorize data by trial number within each condition (1 or 2, or 0 for practise)
    - **noticed**: split data based on whether the participant noticed the VFD during the experiment (asked each participant after their experiment, and noted this in the data). (TRUE or FALSE)
- Some pages allow additional options:
    - Average data across conditions - take average of the two trials of each participant and each condition.
    - Calculate difference + means per participant - Calculate the difference from baseline to VFD condition, as well as mean of both conditions.

---

### Page 1: `page1_feetTrajectories.Rmd`

Plot the trajectory of the feet, with the heelstrike positions overlaid.

![page1](./readme_figures/main.png)

---

### Page 2: `page2_removedSteps.Rmd`

Pie-chart to show which steps have been removed.

![page2](./readme_figures/p2.png)

---

### Page 3: `page3_rawTrackerData.Rmd`

2D plot of raw tracker data of all the different trackers.

![page3](./readme_figures/p3.png)

---

### Page 4: `page4_targetSteps.Rmd`

Histograms of target step results.

![page4](./readme_figures/p4.png)

---

### Page 5: `page5_histograms.Rmd`

Histograms of step parameters (heelstrike locations, step width, step length, etc).

![page5](./readme_figures/p5.png)

---

### Page 6: `page6_scatterplots.Rmd`

Scatter plots to analyze relationships between step parameters.

![page6](./readme_figures/p6.png)

---

### Page 7: `page7_trialScatterplots.Rmd`

Scatter plots by trial to analyze relationships between summarized results (mean, SD, CV) of step parameters, and/or questionnaire data.

![page7](./readme_figures/p7.png)

---

### Page 8: `page8_questionnaires.Rmd`

Boxplots of questionnaire results (subcategories + total score).

![page8](./readme_figures/p8.png)

---

### Page 9: `page9_boxplots.Rmd`

Box plots for step parameters.

![page9](./readme_figures/p9.png)

---

### Page 10: `page10_statistics.Rmd`

Statistical analysis of step data using LMM. Outputs tables with results and performs post-hoc analysis if required. Scroll down to visually evaluate model assumptions (QQ-plots, etc).

![page10](./readme_figures/p10.png)

---

### Page 11: `page11_correlations.Rmd`

Correlation plots showing relationships between different step parameters. Scroll down to evaluate test assumptions. If violated, you can set data to be non-parametric/Bayesian/robust types to perform different (linear) correlation tests.

![page11](./readme_figures/p11.png)

---

### Page 12: `page12_participantSummary.Rmd`

Shows information about participant groups (including demographics and category data). Allows generation of PDF reports per participant, which create all plots of this interface for all trials of a particular participant, and allows you to download the PDF.

![page12](./readme_figures/p12.png)

---

### Page 13: `page13_table.Rmd`

Data table with all (filtered) step data. Enable the "Summarize" checkbox to see aggregated results per trial.

![page13](./readme_figures/p13.png)

---

### Page 14: `page14_dataCorrection.Rmd`

Functions to correct step data by rotating the whole dataset around the y-axis (to correctly align the treadmill direction with the data z-axis). Save rotations into a table, shown next to the table.

![page14](./readme_figures/p14.png)

---

### Page 15: `page15_dataFiltering.Rmd`

Tools for automatically filtering out unwanted data based on local outlier filtering (note: this was not used in the end and may be buggy).

![page15](./readme_figures/p15.png)

---

### Page 16: `page16_manualOutlierFiltering.Rmd`

Interface to manually filter out outliers in the data. Save outliers to CSV & import later to continue. Once finished, click "Overwrite Outliers" to mark the selected steps as outliers. Click "Save to Current RDS" to re-save the gait parameters table to the RDS file with the current outliers flagged. Also includes auto-saving functionality (advised to use, as software may crash randomly sometimes).

![page16](./readme_figures/p16.png)
