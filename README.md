# readme for Braziunas et al. in review. Less fuel for the next fire

## purpose

This readme gives an overview of directory structure, files, and steps for recreating
outputs and analyses associated with Braziunas et al. in review.

**Manuscript citation:** Braziunas, K.H., N.G. Kiel, and M.G. Turner. In Review. Less fuel for the next fire? Warmer-drier climate amplifies effects of short-interval fire on forest recovery. Ecology.

## platforms

- Operating systems and software used for development and implementation
  - OS: Windows 10
  - R version: 4.1.3
  - ArcGIS Desktop 10.6

## directory overview

Directory structure and files:

- `analysis/`: Results from data analysis.
- `data/`: Raw data.
- `processed_data/`: Any data altered from raw form. Data derived during study design, plot selection, and spatial data processing. Includes summary tables and intermediate products created during data analysis.
- `scripts/`: R scripts.

## scripts

Scripts are named in order (`step01_`, etc.). `.Rmd` scripts rely on external data inputs not included in this deposit (e.g., raw climate downloads, fire perimeters from MTBS). `.R` scripts can be rerun with data included in deposit.

Script descriptions:

- `step01_fire_selection.Rmd`: Identifies short- and long-interval fire perimeters from multiple data sources (MTBS, Yellowstone and Grand Teton National Parks). Creates shapefiles for final selected fires for 2021 field sampling. Outputs in `processed_data/fire_selection/`.
- `step02_plot_selection.Rmd`: Implements random selection of potential short-interval plots at pre-identified sampling sites where short- and long-interval severe fire occurred in close proximity. Identifies nearby long-interval plots that share similar topographic characteristics.
- `step03_field_data_cleaning_prep.R`: Cleans and organizes field data into plot-level totals prior to fuels calculations and data analysis. Inputs in `data/raw_data/` and outputs in `data/cleaned_data/` and `processed_data/plot_selection/` (final coordinates of plots sampled in the field).
- `step04_biomass_fuels_calcs.R`: Calculates aboveground live and dead biomass and fuels prior to data analysis. Inputs in `data/` and outputs in `processed_data/biomass_fuels.csv`.
- `step05_climate_data_prep.Rmd`: Prepares predictor climate variables relevant for tree regeneration. Outputs in `processed_data/climate/`.
- `step06_data_analysis.R`: Data analysis to answer questions in this study: (1) How do short-interval fire, climate, and other factors (topography, distance to live edge) interact to affect post-fire forest recovery? (2) How do forest biomass and fuels vary following short- versus long-interval severe fires? Inputs in `data/` and `processed_data/`, outputs in `analysis/`.
