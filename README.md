# Analysis Code - Rocker
The code used for the analysis of kinematic data to investigate the effect of visual feedback distortions (VFD) on gait variability while walking on a treadmill.

With the files in this repository, you are able to run the code from any device that is able to install and run [Docker](https://docs.docker.com/desktop/install/windows-install/#install-interactively).

Links:
- [Zenodo data upload](https://zenodo.org/record/14017075)
> This repository already contains the calculated step parameters datasets (`./runtime/results/*.rds`). If you want to recalculate step parameters, delete these files, and put participant data ([found on Zenodo](https://zenodo.org/record/14017075)) folders in `./runtime/data/` (in the same folder as the `index.Rmd`).
- [GitHub Repo](https://github.com/Avdbergnmf/R-Analysis-VFD-Experiment-2024)
- [GitLab Repo](https://gitlab.tudelft.nl/mln-lab-public/r-analysis-alex-van-den-berg-2024-vfd-experiment)

## How to use

### Use with Docker
1. build and start the docker container defined in docker-compose.yml: `docker-compose up --build -d`
2. Navigate to http://localhost:8787
3. Within the R instance, navigate to `/home/rstudio/workspace/`, and open the `index.Rmd` file.
3. Click `Run Document` and allow the popup window to appear.
> Notes:
> 1. Initial calculation of the datasets may take some time (~5 minutes). After the first run, the results should be saved to tables in `runtime/results/` and startup should be significantly faster. If you need to recalculate these tabled for any reason, just delete them and they will be recreated with the current contents of the data folder.\
> 2. If you notice that the output of the *Render* window in R Studio get stuck, and the interface does not load, stop the code, *Clear Prerendered Output* and try again.
4. Browse the tabs and interact with the data. See [runtime readme](./runtime) for more info.

### Without Docker
You can also just use the code with your local R-Studio installation:
1. Within the R instance, navigate to `./runtime/`, and open the `index.Rmd` file.
2. Click `Run Document`. If it asks you to install any packages, click yes. You can find the list of packages used in `./runtime/source/setup.R`.
3. Browse the tabs and interact with the data

## Contents
- `.rstudio.env` - Contains the password for the rocker container.
- `docker-compose.yml` - The description of how to create the docker container. This also sets the port and shared volume.
- `Dockerfile.rocker` - The dockerfile used to build the docker container - based on [rocker/rstudio:4.4.0](https://rocker-project.org).

- `runtime/` - contains all the code that is ran in the docker container. Note: also contains a [`README.md`](./runtime) explaining its contents.