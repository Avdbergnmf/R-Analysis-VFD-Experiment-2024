# Analysis Code
The code used for the analysis of kinematic data to investigate the effect of visual feedback distortions (VFD) on gait variability while walking on a treadmill.

## How to use
To run this code with your local R-Studio installation:
1. Within the R instance, navigate to `./runtime/`, and open the `index.Rmd` file.
2. Click `Run Document`. If it asks you to install any packages, click yes. You can find the list of packages used in `./runtime/source/source.R`.
> Initial calculation of the datasets may take some time (~5 minutes). After the first run, the results should be saved to tables in `results/` and startup should be significantly faster.
3. Browse the tabs and interact with the data

## Contents
- `index.Rmd` - The main Rmd code to run that loads all the source code, calculates or loads the results, and renders all the pages + sidebar. --> Run this code to start the interface.

- `data/` - Contains all data of all participants.
- `pages/` - Contains the Rmd code for rendering the shiny pages + sidebar.
- `source/` - Contains the source code for the calculation of all the parameters etc. Note: also contains a `README.md`.
- `questionnaires/` - Contains information about the questionnaire questions (how to calculate the results, categories within the questionnaire, etc.).
- `results/` - Folder in which the resulting tables are stored (calculated gait parameters, target parameters, questionnaire results).