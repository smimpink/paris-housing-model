# Paris apartment price model

A hedonic price model for Paris apartments, built entirely from French open data, with a script that prices a flat at any Paris address. It regresses the log sale price of every single-apartment sale in Paris from 2021 to 2025 on the flat's surface, rooms, energy label, floor, ceiling height, window area, the building's construction period and heritage protections, and the street type, with cadastral-section and sale-quarter fixed effects. Given an address and a few facts about a flat, it returns an expected price, a median price and an 80% prediction band.

Everything needed to run it is in the repository: the code (R), the sales sample and the building attributes. The raw pulls from each source are included too, so the sample can be rebuilt.

## Quick start

You need R (4.1 or later) with the packages `data.table`, `jsonlite`, `httr`, `stringi` and `R.utils`, and an internet connection for two geocoding calls.

```bash
git clone https://github.com/smimpink/paris-housing-model.git
cd paris-housing-model
Rscript -e 'install.packages(c("data.table", "jsonlite", "httr", "stringi", "R.utils"))'
```

Describe the flat in `target.json` (see the next section), then run the regression from the project root:

```bash
Rscript code/04_regression.R              # all of Paris: about 74,000 sales, a few minutes
SCOPE=5e Rscript code/04_regression.R     # 5e arrondissement only: about 2,200 sales, seconds
```

The script prints the coefficients and the prediction and writes them to `output/regression_coefs_<scope>.csv` and `output/target_prediction_<scope>.csv`. To keep a flat of your own out of version control, put it in a file matching `target_*.json` (ignored by git) and point the `TARGET` variable at it:

```bash
TARGET=target_myflat.json Rscript code/04_regression.R
```

## Describing the flat: `target.json`

The example file prices a hypothetical two-room flat at 13 rue Poliveau in the 5e:

```json
{
  "address": "13 rue Poliveau 75005 Paris",
  "surf": 45,
  "rooms": 2,
  "deps": 1,
  "label": "E",
  "ground": 0,
  "ceiling": 2.7,
  "terrace": 0,
  "terrace_value": 0,
  "win_area": null,
  "quarter": "2025Q4"
}
```

| Field | Meaning |
|---|---|
| `address` | Street number, street and postcode. Geocoded with the Base Adresse Nationale (BAN); the cadastral parcel is the one containing the geocoded point (IGN cadastre API), or failing that the parcel of past sales at the same address. |
| `surf` | Habitable surface in m², measured (a Carrez or DPE surface). Excludes balconies, terraces and cellars. |
| `rooms` | Main rooms in the French sense (living room and bedrooms; kitchen, bathrooms and corridors do not count). |
| `deps` | Dependencies sold with the flat: cellar, parking space, storeroom. |
| `label` | DPE energy label, A to G. A, B and C are pooled in the model. |
| `ground` | 1 if the flat is on the ground floor, 0 otherwise. The model does not distinguish upper floors. |
| `ceiling` | Ceiling height in metres. |
| `terrace` | Outdoor area in m² (balcony or terrace), 0 if none. Optional. |
| `terrace_value` | Euros added to the interior price for the outdoor space. Optional, default 0. No public source records usable outdoor space, so the model cannot price it; the interior is priced and this amount is added afterwards. |
| `win_area` | Glazed area in m² (sum of the windows listed in the flat's DPE). Optional; `null` uses the median of similar flats in the sample. |
| `quarter` | Sale quarter the prediction is for, from `2021Q3` to `2025Q4`. For `2025Q4` the script also rolls the prediction forward to 2026Q2 with the INSEE-Notaires Paris apartment index. |
| `parcel` | Cadastral parcel id (14 characters, e.g. `75105000AR0028`). Optional; given, it skips the automatic lookup. The id is shown on [cadastre.gouv.fr](https://www.cadastre.gouv.fr). |

The construction period and the heritage protections of the building are looked up from the parcel in `data/raw/building_attributes_paris.csv.gz`, so you do not enter them.

## What you get

`output/target_prediction_<scope>.csv` has one row per basis:

| Column | Meaning |
|---|---|
| `mean` | Expected price in euros (Duan smearing of the log-scale prediction), terrace value included. |
| `median` | Median price, exp(xb), terrace value included. |
| `pi80_lo`, `pi80_hi` | 80% prediction band: the 10th and 90th percentiles of the cross-validated errors of sales similar to the flat (same ground/upper floor, rooms within 1, surface within 20 m²), applied to the median. |
| `mean_interior`, `median_interior` | The same without the terrace value. |
| `mean_ppm` | Expected price per m² of `surf`. |
| `period`, `pvp`, `mh`, `psmv_grade`, `styp`, `win_area_used`, `n_similar` | The building attributes, street type, window area and number of similar sales used. |
| `basis` | `model` for the target quarter; `model 2025Q4 x Paris apartment index` for the roll-forward. |

For the example flat (45 m², two rooms, one cellar, label E, upper floor, 2.7 m ceilings, in a building of 1851–1914 with no protection):

| Scope | Sales | Expected price | Median | 80% band | €/m² |
|---|---|---|---|---|---|
| Paris | 73,794 | 471,000 | 463,000 | 381,000 to 556,000 | 10,475 |
| 5e only | 2,177 | 468,000 | 459,000 | 372,000 to 551,000 | 10,394 |

`output/regression_coefs_<scope>.csv` has the coefficients with standard errors clustered by cadastral section. From the Paris estimates: the surface elasticity is 0.92; labels D, E, F and G sell for 2.7%, 5.0%, 7.6% and 8.2% less than A to C; a ground-floor flat for 4.7% less; a flat in a listed monument historique for 7.2% more; and 10% more window area adds about 0.6%. The Paris model fits with R² 0.933 and a residual standard deviation of 0.19 log points.

## How the model works

```
log(price) = b log(surface) + rooms + dependencies + energy label + ground floor + ceiling height
           + construction period + heritage protections + street type + log(window area)
           + cadastral-section FE + sale-quarter FE + e
```

- **Sample.** Every DVF sale of exactly one apartment (with or without dependencies, no house or commercial lot) in Paris from January 2021 to December 2025: 149,719 sales. Sales with a price per m² outside Tukey's far-out fences on the log scale within their arrondissement (Q1 − 3 IQR, Q3 + 3 IQR) are dropped: luxury outliers above, symbolic transfers, family sales and life-annuity down payments below. 142,545 sales remain.
- **DPE match.** Each sale is matched to a DPE (energy certificate) at the same BAN address with a habitable surface within 1.5 m² or 5% of the Carrez surface (or 2.5 m² or 8% of the cadastral surface), issued between three years before and 15 days after the sale. 82,986 sales match. The DPE gives the measured surface, the label, the floor, the ceiling height, and through the BDNB the glazed area.
- **Estimation.** Sales with a window area and a dated building: 73,794 sales in 1,099 sections. The section fixed effects are absorbed by the within transformation; standard errors are clustered by section. Reference categories: one room, no dependency, label A to C, built before 1800, a `rue`.
- **Prediction.** The target flat's row is built from `target.json` and its parcel's building attributes; its section effect is the estimated one (or, for a section with no sale in the sample, the mean of its arrondissement's sections). The 80% band comes from the out-of-fold errors of 10-fold cross-validation, restricted to sales similar to the flat; the errors are heavy-tailed, so a normal band would be too wide in the middle and too narrow in the tails.
- **Roll-forward.** DVF ends in December 2025. For a 2025Q4 target the prediction is also rolled to 2026Q2 with the INSEE-Notaires index of Paris apartment prices (quarterly changes in `04_regression.R`; update them when a later index release is used).

## Data

All files are gzipped CSVs; `fread` reads them directly. The `_75105_` files are the 5e-only versions used with `SCOPE=5e`.

| File | Content | Source | Licence |
|---|---|---|---|
| `data/raw/dvf_paris_rows.csv.gz` | 420,058 DVF lines (a sale spans several lines), 2021–2025 | [DVF](https://app.dvf.etalab.gouv.fr) via the Etalab DVF app API, pulled October 2026 | Licence Ouverte 2.0 |
| `data/raw/dpe_paris_appartements.csv.gz` | 829,496 apartment DPEs issued since July 2021 | [ADEME, DPE logements existants](https://data.ademe.fr/datasets/dpe03existant) | Licence Ouverte 2.0 |
| `data/raw/bdnb_dpe_logement_75.csv.gz` | Window areas by orientation, wall and glazing details for 1,047,240 DPEs | [BDNB](https://bdnb.io) (CSTB), millésime 2026-02-a, table `dpe_logement`, département 75 | Licence Ouverte 2.0 |
| `data/raw/building_attributes_paris.csv.gz` | For 78,154 parcels: construction period and year of the largest dated building, urban fabric, Ville de Paris protection, monument historique, Marais/7e heritage-plan grade | [APUR Emprise bâtie](https://opendata.apur.org) (ODbL), [Ville de Paris PLU protections](https://opendata.paris.fr/explore/dataset/plub_surf_pvp) (ODbL), [Mérimée](https://www.pop.culture.gouv.fr) (Licence Ouverte), PSMV from the [Géoportail de l'urbanisme](https://www.geoportail-urbanisme.gouv.fr), [cadastre Etalab](https://cadastre.data.gouv.fr) (Licence Ouverte 2.0) | ODbL |
| `data/derived/sales_paris_single_apt.csv.gz` | 149,719 single-apartment sales with their matched DPE, building attributes and window area; built by `03_build_sample.R` | the above | ODbL |

Geocoding at prediction time uses the [BAN geocoder](https://adresse.data.gouv.fr) and the [IGN cadastre API](https://apicarto.ign.fr); nothing from them is stored.

The sales file contains the addresses of real sales. They are published as such by the French state in DVF.

### Rebuilding the data

`Rscript code/run_all.R` runs the whole pipeline (`SCOPE=5e` for the 5e only). The steps:

| Script | What it does | Needs |
|---|---|---|
| `01_pull_dvf.R` | Pulls every DVF mutation, one request per cadastral section | about an hour for Paris (1,100 sections with a pause between requests) |
| `02_pull_dpe.R` | Pulls apartment DPEs by postcode from the ADEME API | minutes |
| `02b_pull_bdnb.R` | Downloads the BDNB extract for département 75 (263 MB zip) and keeps the window and wall columns | `unzip`; set `BDNB_ZIP` to a local copy to skip the download |
| `02c_pull_buildings.R` | Downloads the cadastre parcels, APUR footprints, Paris protections, Mérimée and PSMV files and joins them spatially | the [DuckDB](https://duckdb.org) command-line tool with its spatial extension; set `BUILDINGS_CACHE` to keep the downloads |
| `03_build_sample.R` | Builds the single-apartment sales sample and matches DPEs, building attributes and window areas | minutes |
| `04_regression.R` | Estimates the model and prices the target flat | |

The pulls are deterministic given the sources, but the sources move: DVF adds and corrects mutations, and the ADEME register grows daily. The files here are the October 2026 pulls.

## Repository layout

```
code/      00_setup.R (settings, target loader, estimators), 01 to 04 (pipeline), run_all.R
data/raw/      source pulls
data/derived/  sales sample
output/    regression_coefs_<scope>.csv (committed); target_prediction_<scope>.csv (written by 04, not committed)
target.json    the flat to price
```

## Limitations

- The floor enters only as ground versus upper; views, orientation, elevator, condition and renovation state are not observed.
- Outdoor space cannot be priced from any public source; `terrace_value` is your own assumption.
- Only sales with a matched DPE and a window area enter the estimation (about half of the sales in the fences); flats without a recent DPE are under-represented.
- The prediction band describes the dispersion of similar sales around the model, not the uncertainty in the coefficients, which is small at this sample size.
- This is a statistical description of past sales, not an appraisal.

## Licence

The code is released under the MIT licence (see `LICENSE`). The data files are redistributed from the sources listed above under their own licences. The building-attributes file and the derived sales sample contain content from the APUR and the Ville de Paris under the Open Database License, and are therefore made available under the ODbL.

Stephen Impink, HEC Paris, 2026. If you use the model or the data, please cite this repository.
