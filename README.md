# Analysis Code - Rocker
The code used for the analysis of kinematic data to investigate the effect of visual feedback distortions (VFD) on gait variability while walking on a treadmill.

With the files in this repository, you are able to run the code from any device that is able to install and run Docker.

## How to use
### Use with Docker
1. build and start the docker container defined in docker-compose.yml: `docker-compose up --build -d`
2. Navigate to http://localhost:8787
3. Within the R instance, navigate to `/home/rstudio/workspace/`, and open the `index.Rmd` file.
3. Click `Run Document` and allow the popup window to appear.
> Initial calculation of the datasets may take some time (~5 minutes). After the first run, the results should be saved to tables in `runtime/results/` and startup should be significantly faster.
4. Browse the tabs and interact with the data

### Without Docker
You can also just use the code with your local R-Studio installation:
1. Within the R instance, navigate to `./runtime/`, and open the `index.Rmd` file.
2. Click `Run Document`. If it asks you to install any packages, click yes. You can find the list of packages used in `./runtime/source/source.R`.
3. Browse the tabs and interact with the data

## Contents
- `.rstudio.env`
- `docker-compose.yml`
- `Dockerfile.rocker` - The dockerfile used to build the docker container - based on [rocker/rstudio:4.4.0](https://rocker-project.org).

- `runtime/` - contains all the code that is ran in the docker container. Note: also contains a `README.md` explaining its contents.