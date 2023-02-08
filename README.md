# Quantifying the impact of habitat modifications on species behavior and mortality: case-study on floating objects and tropical tuna

<!--
[![License](https://img.shields.io/github/license/adupaix/Quantif_impact_FAD)](https://github.com/adupaix/Quantif_impact_FAD/blob/master/LICENSE)
[![DOI](https://zenodo.org/badge/338344443.svg)](https://zenodo.org/badge/latestdoi/338344443)
[![Latest Release](https://img.shields.io/github/release/adupaix/Quantif_impact_FAD)](https://github.com/adupaix/Quantif_impact_FAD/releases)
-->

---

Scripts used to generate the results and figures of the following paper:

Dupaix A., Dagorn L., Deneubourg J.-L., Capello M. _(in prep)_. Quantifying the impact of habitat modifications on species behavior and mortality: case-study on floating objects and tropical tuna.

Should you have any question, please contact me: amael.dupaix@ens-lyon.fr

## Running the analysis

Scripts run with the __R 3.6.3__ statistical sofware.
The [conda](https://docs.conda.io/projects/conda/en/latest/) environment to run the model is provided. To create, type : `conda env create -f Quantif_impact_FAD.yml`

To run the analysis:
- prepare a configuration file (template provided in `cfg/ex_cfg.R`)
- fill in the `config_name` and `BASE_DIR` variables in `launch.R`
- run `launch.R`

## Datasets

Simulated Continuous Absence Times (read in folder `SIM_OUTPUT_PATH`) were obtained using [FAT albaCoRaW v1.4](https://doi.org/10.5281/zenodo.5834056)

The following IOTC (Indian Ocean Tuna Commission) datasets were used:
- [Instrumented buoy data (Jan 2020 - May 2021)](https://iotc.org/WGFAD/02/Data/04-BU) (read in `IOTC_BUOY_DATA_FILE`)
- [FAD activity data (2013-2020)](https://www.iotc.org/WGFAD/02/Data/01-FA) (read in `IOTC_SETS_3FA_FILE`)
- [Catch and Effort Data - Surface Fisheries](https://iotc.org/WPTT/24DP/Data/05-CESurface) (read in `IOTC_SETS_3CE_FILE`)
- [Code list for CWP grids](https://iotc.org/WGFAD/02/Data/00-CWP) (codes for 1° cells used, read in `IOTC_CELLREF_FILE`)

If `NLOG_ONLY` is set to `TRUE`, two other datasets are also needed, obtained through a datacall to the [Ob7 - Observatoire des Écosystèmes Pélagiques Tropicaux exploités](https://www.ob7.ird.fr/en/pages/datacall.html). This data is obtained from observers onboard french purse seine vessels:
- One containing all the operation on floating objects (in `OBSERVERS_FOBFILE`)
- One containing all the vessel activities (operations on FOBs but also sets, etc. in `OBSERVERS_ACTIVITYFILE`)

## References

Data from onboard observers (ObServe): Source IRD/Ob7, co-funded by IRD and the EUMAP program for the collection of fisheries data (DCF). https://www.ob7.ird.fr/pages/datacall.html

Data from onboard observers (ObServe) as part of the OCUP program: Source Orthongel, data processing by IRD/Ob7, co-financed by Orthongel and France Filière Pêche (FFP). https://www.ob7.ird.fr/pages/datacall.html

Dupaix, Amaël, Pérez, Géraldine, & Capello, Manuela. (2022). FAT albaCoRaW (v1.4). Zenodo. https://doi.org/10.5281/zenodo.5834056

IOTC. (2022). Catch and effort data—Surface fisheries [IOTC 24th Working Party on Tropical Tuna (WPTT24)]. https://iotc.org/WPTT/24DP/Data/05-CESurface

IOTC. (2021a). FAD activity data (2013-2020) IOTC ad hoc Working Group on FADs (WGFAD). Indian Ocean Tuna Commission. https://www.iotc.org/WGFAD/02/Data/01-FA

IOTC. (2021b). Instrumented buoy data (Jan 2020—May 2021) IOTC ad hoc Working Group on FADs (WGFAD2). https://iotc.org/WGFAD/02/Data/04-BU

R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
