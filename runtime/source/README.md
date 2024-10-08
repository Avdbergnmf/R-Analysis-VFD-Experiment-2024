This folder contains all the source code to calculate the gait parameters from the participant data.

- `setup.R` - Loads all the libraries needed to run the code.
- `dataloading.R` - Contains all the functions used to load data, or specific pieces of information from all the datasets.
- `find_foot_events.R` - Contains the algorithms used to identify heelstrike & toe-off events, and put them into a table.
- `calc_all_gait_params.R` - Runs the methods defined in `find_foot_events.R` for all participants and all trials to get all the results into one big table.
- `plotting.R` - Contains all the code that generates the plots in the different tabs of the interface.
- `get_filtered_data.R` - Filter the data based on the inputs selected in the sidebar.
- `get_questionnaire_results.R` - Extract the questionnaire results from the datasets in `../data/` using the information in `../questionnaires/`.
- `pre_processing.R` - Contains the preprocessing of all the data, such as filters, rotations, and the combining of datasets into dataframes.
- `summaryparams.R` - The methods used to retrieve the aggregated results, used for the statistics etc.