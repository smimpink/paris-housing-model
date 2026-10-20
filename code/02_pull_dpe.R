# 02: Pull ADEME DPE records for apartments with a postcode in scope (one query per postcode).
# Dataset: "DPE Logements existants (depuis juillet 2021)", id dpe03existant, data-fair API.
# Covers only DPEs issued from 1 July 2021; older DPEs (previous method) are in the dpe-france dataset.
# Notes: filter with code_postal_ban_in= (a bare code_postal_ban= is silently ignored);
#        paginate by following the 'next' URL; rate limit is about 10 requests/second/IP.
# Output: data/raw/dpe_<TAG>_appartements.csv.gz
source("code/00_setup.R")

fields <- c("numero_dpe", "date_etablissement_dpe", "date_fin_validite_dpe", "numero_dpe_remplace",
            "etiquette_dpe", "etiquette_ges", "conso_5_usages_par_m2_ep", "type_batiment",
            "annee_construction", "periode_construction", "nombre_niveau_immeuble", "nombre_appartement",
            "surface_habitable_logement", "identifiant_ban", "adresse_ban", "score_ban", "id_rnb",
            "numero_etage_appartement", "position_logement_dans_immeuble", "complement_adresse_logement",
            "hauteur_sous_plafond", "logement_traversant", "type_installation_chauffage",
            "type_energie_principale_chauffage")

pull_postcode <- function(pc) {
  url <- paste0(DPE_API, "?size=2500&code_postal_ban_in=", pc,
                "&type_batiment_in=appartement&select=", paste(fields, collapse = ","))
  pages <- list(); i <- 0
  while (!is.null(url) && i < 500) {
    r <- get_json(url); i <- i + 1
    pages[[i]] <- as.data.table(r$results)
    url <- r[["next"]]
  }
  if (!is.null(url)) stop("Page cap reached for ", pc)
  out <- rbindlist(pages, fill = TRUE)
  message(pc, ": ", i, " pages, ", nrow(out), " records")
  out
}
dpe <- unique(rbindlist(lapply(POSTCODES, pull_postcode), fill = TRUE), by = "numero_dpe")
fwrite(dpe, file.path(RAW, sprintf("dpe_%s_appartements.csv.gz", TAG)), compress = "gzip")
message(nrow(dpe), " DPE records saved")
