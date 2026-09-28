# LandRCBM_NWT

Scripts simulating and analysing forest landscape dynamics and carbon
stocks in the Northwest Territories (NWT) by coupling the **LandR**
vegetation dynamics modules with the **Carbon Budget Model (CBM)**, run
through the SpaDES modelling framework. The code reproduces results and figures
for the paper [LandRCBM: Bridging Forest Dynamics and Carbon Accounting to Advance
Forest Carbon Predictions](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=6963643).

The study area is the Taiga Plains ecozone within the Northwest
Territories, Canada. Two simulation case studies are provided:
one driven by historical disturbance records (wildfire from CanLaD,
2000-2024) and one driven by a stochastic fire model (`scfm`) projected
forward to 2520.

## Repository contents

| Path | Description |
|---|---|
| `globalHistorical.R` | Sets up and runs the simulation using observed historical wildfire disturbances (2000-2024). |
| `globalSCFM.R` | Sets up and runs the simulation using the `scfm` stochastic fire model, projected from 2020 to 2520. |
| `scripts/` | Post-processing and figure/table generation scripts used to produce the outputs and figures reported in the associated manuscript (e.g. study area profiling, figures 1 and 3-6, appendix figure, shared plotting themes and utility functions). |
| `appFigures/` | Supplementary/appendix figures. |
| `pubFigures/` | Publication figures. |
| `pubTables/` | Publication tables. |

## Requirements

* Internet access to fetch pinned versions of the required SpaDES
  modules from GitHub, and to download study-area boundary and ecozone
  shapefiles.
* A Google account, since some inputs are retrieved from Google Drive
  and R will prompt for Google Drive authentication.
* Access to permanent sample plot (PSP) data used by some modules is
  restricted; running the full workflow requires separately requesting
  permission to use that data.
* Access to a high-computing machine to run the simulations.

## Running a simulation

Each `global*.R` script is self-contained: it installs/loads
`SpaDES.project`, defines the study area (Taiga Plains ecozone clipped
to the NWT boundary), pins the exact module versions to use, sets model
parameters, and then calls `simInit2()`/`spades()` to run the
simulation.

Historical-disturbance run (2000-2024, CanLaD wildfire history):

```r
source("globalHistorical.R")
```

Stochastic-fire run (2020-2520, `scfm`):

```r
source("globalSCFM.R")
```

Outputs are written under `outputs/historicalDisturbances/` and
`outputs/SCFM/` respectively (created on first run), including yield
tables, species/carbon summaries, the raster-to-match, and disturbance
event records.

## Reproducing figures and tables

After a simulation run has produced its outputs, the numbered scripts in
`scripts/` reproduce the study-area profiling and the figures/tables
used in the associated manuscript:

```r
source("scripts/01_studyAreaProfiling.R")
source("scripts/02_figure1.R")
# ... etc.
```

`scripts/utils.R` and `scripts/themes.R` hold shared helper functions
and ggplot themes used across these scripts.
