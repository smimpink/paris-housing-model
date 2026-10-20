# 02b: Pull window, orientation and wall details per DPE from the BDNB (Base de donnees nationale des batiments,
# CSTB) open-data extract for departement 75, table dpe_logement (one row per DPE, keyed by identifiant_dpe =
# ADEME numero_dpe). The ADEME open dataset has no window areas; the BDNB re-processes the full DPE files.
# Source: https://bdnb.io (millesime 2026-02-a, open licence). The zip (~263 MB) is downloaded to a temporary
# file unless BDNB_ZIP points to a local copy; only the columns below are kept.
# Output: data/raw/bdnb_dpe_logement_75.csv.gz
source("code/00_setup.R")

BDNB_URL <- "https://open-data.s3.fr-par.scw.cloud/bdnb_millesime_2026-02-a/millesime_2026-02-a_dep75/open_data_millesime_2026-02-a_dep75_csv.zip"
zip <- Sys.getenv("BDNB_ZIP", "")
if (!nzchar(zip) || !file.exists(zip)) {
  zip <- tempfile(fileext = ".zip")
  message("downloading BDNB departement 75 extract")
  if (download.file(BDNB_URL, zip, mode = "wb", quiet = TRUE) != 0) stop("BDNB download failed")
}

keep <- c("identifiant_dpe", "date_etablissement_dpe", "surface_habitable_logement",
          "surface_vitree_nord", "surface_vitree_sud", "surface_vitree_est", "surface_vitree_ouest",
          "surface_vitree_horizontal", "l_orientation_baie_vitree", "pourcentage_surface_baie_vitree_exterieur",
          "surface_mur_exterieur", "traversant", "plusieurs_facade_exposee", "presence_balcon",
          "materiaux_structure_mur_exterieur", "type_vitrage")
b <- fread(cmd = sprintf("unzip -p '%s' csv/dpe_logement.csv", zip), sep = ";", select = keep, encoding = "UTF-8")
fwrite(b, file.path(RAW, "bdnb_dpe_logement_75.csv.gz"), compress = "gzip")
message(nrow(b), " BDNB DPE records saved; with any window area: ",
        b[, sum(!is.na(surface_vitree_nord) | !is.na(surface_vitree_sud) | !is.na(surface_vitree_est) |
                !is.na(surface_vitree_ouest) | !is.na(surface_vitree_horizontal))])
