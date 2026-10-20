# 03: Build the single-apartment sales sample and attach a matched DPE to each sale.
# Output: data/derived/sales_<TAG>_single_apt.csv.gz  (one row per sale)
source("code/00_setup.R")
num <- function(x) suppressWarnings(as.numeric(x))

# Blank fields must read as NA (otherwise an empty address suffix becomes "_" in the BAN key)
dv <- fread(file.path(RAW, sprintf("dvf_%s_rows.csv.gz", TAG)), colClasses = "character", encoding = "UTF-8",
            na.strings = c("", "NA"))
dv[, `:=`(valeur_fonciere = num(valeur_fonciere), surface_reelle_bati = num(surface_reelle_bati),
          nombre_pieces_principales = num(nombre_pieces_principales), lot1_surface_carrez = num(lot1_surface_carrez))]
dv <- dv[nature_mutation == "Vente"]

# BAN address key, e.g. 75105_2169_00008 or 75105_8705_00012_bis (matches DPE identifiant_ban)
sfx <- c(B = "bis", T = "ter", Q = "quater")
dv[, numero := suppressWarnings(as.integer(num(adresse_numero)))]
dv[, ban_key := fifelse(is.na(adresse_code_voie) | is.na(numero), NA_character_,
     paste0(code_commune, "_", tolower(stri_pad_left(adresse_code_voie, 4, "0")), "_", sprintf("%05d", numero),
            fifelse(is.na(adresse_suffixe), "",
                    paste0("_", fifelse(adresse_suffixe %in% names(sfx), sfx[adresse_suffixe], tolower(adresse_suffixe))))))]

# DVF repeats a local once per lot/parcel combination; keep distinct locals within each mutation
dv[, local_key := paste(type_local, surface_reelle_bati, nombre_pieces_principales, lot1_numero, sep = "|")]
# Type codes: 1 house, 2 apartment, 3 dependency (cellar, parking, storeroom), 4 commercial. Match on codes,
# not the accented labels, which fail to compare under a non-UTF-8 locale (e.g. Rscript from a bare shell).
loc  <- unique(dv[!is.na(code_type_local)], by = c("id_mutation", "local_key"))
summ <- loc[, .(n_apt = sum(code_type_local == "2"), deps = sum(code_type_local == "3"),
                n_other = sum(!code_type_local %in% c("2", "3"))), by = id_mutation]
apt  <- loc[code_type_local == "2", .(surf = surface_reelle_bati[1], rooms = nombre_pieces_principales[1],
                                            carrez = lot1_surface_carrez[1]), by = id_mutation]
keys <- dv[!is.na(ban_key), .(ban_keys = paste(unique(ban_key), collapse = ";")), by = id_mutation]
first <- dv[, .(date = as.IDate(date_mutation[1]), price = valeur_fonciere[1], id_parcelle = id_parcelle[1],
                commune = code_commune[1], section = section[1],
                street_type = sub(" .*$", "", adresse_nom_voie[1]),   # RUE, AV, BD, PL, QUAI, PAS, VLA ...
                address = paste(adresse_numero[1], fcoalesce(adresse_suffixe[1], ""),
                adresse_nom_voie[1])), by = id_mutation]

s <- merge(summ[n_apt == 1 & n_other == 0, .(id_mutation, deps)], apt, by = "id_mutation")
s <- merge(merge(s, first, by = "id_mutation"), keys, by = "id_mutation", all.x = TRUE)
s[, `:=`(year = year(date), quarter = paste0(year(date), "Q", quarter(date)), ppm = price / surf,
         sec_id = paste0(commune, "_", section))]   # section codes repeat across arrondissements

# Valid records: positive price, known rooms, surface >= 9 m2 (legal minimum for a dwelling, decret 2002-120).
# Outliers: Tukey's far-out fences on log price per m2, Q1 - 3*IQR and Q3 + 3*IQR, over all valid
# single-apartment sales in each arrondissement (pooled over 2021-2025), since price levels differ across
# Paris. Percentile trimming is not used because symbolic transfers (EUR 1 to a few thousand) make up
# about 4% of sales, more than a 1% tail.
TUKEY_K <- 3
s[, valid := !is.na(price) & price > 0 & !is.na(surf) & surf >= 9 & !is.na(rooms)]
fences <- s[valid == TRUE, {q <- quantile(log(ppm), c(0.25, 0.75))
                           .(fence_lo = exp(q[[1]] - TUKEY_K * diff(q)), fence_hi = exp(q[[2]] + TUKEY_K * diff(q)))}, by = commune]
s <- merge(s, fences, by = "commune", all.x = TRUE)
s[, in_sample := valid & ppm >= fence_lo & ppm <= fence_hi]
message(nrow(s), " single-apartment sales; ", sum(s$valid), " valid; ", sum(s$in_sample),
        " inside Tukey fences (k = ", TUKEY_K, ") on log price per m2 by arrondissement")
print(fences[order(commune), .(commune, fence_lo = round(fence_lo), fence_hi = round(fence_hi))])

# ---- DPE matching ---------------------------------------------------------------
# Rules: (1) DPE at the same BAN address as the sale; (2) habitable surface within
# max(1.5 m2, 5%) of DVF Carrez or within max(2.5 m2, 8%) of cadastral surface;
# (3) DPE issued between 3 years before and 15 days after the sale; pick the closest
# surface, then the most recent. Tier 'high' = DPE within 12 months before the sale and
# no other unit at the address of similar size (+/-1.5 m2) with a different label in that window.
dpe <- fread(file.path(RAW, sprintf("dpe_%s_appartements.csv.gz", TAG)), encoding = "UTF-8")
dpe[, `:=`(ban = tolower(identifiant_ban), dpe_date = as.IDate(substr(date_etablissement_dpe, 1, 10)))]

parse_floor <- function(txt) {
  t <- tolower(stri_trans_general(fcoalesce(as.character(txt), ""), "Latin-ASCII"))
  v <- as.integer(fcoalesce(
    stri_match_first_regex(t, "(\\d{1,2})\\s*(e|er|ere|eme|em|ieme)?\\s*etage")[, 2],
    stri_match_first_regex(t, "etage\\s*:?\\s*n?o?\\s*(\\d{1,2})")[, 2],
    stri_match_first_regex(t, "\\b(\\d{1,2})\\s*(eme|ieme|er|ere|em)\\b")[, 2]))
  v[!is.na(v) & v > 12] <- NA
  v[grepl("\\b(rdc|rez[- ]?de[- ]?chaussee|rez de ch|rez-de)", t)] <- 0L
  v
}
# The register's floor field is almost always 0, so fall back on the free-text address complement
dpe[, dpe_floor := fifelse(!is.na(numero_etage_appartement) & numero_etage_appartement > 0,
                           as.integer(numero_etage_appartement), parse_floor(complement_adresse_logement))]

long <- s[!is.na(ban_keys), .(ban = unlist(strsplit(ban_keys, ";"))), by = id_mutation]
allc <- unique(merge(long, dpe, by = "ban", allow.cartesian = TRUE), by = c("id_mutation", "numero_dpe"))
allc <- merge(allc, s[, .(id_mutation, sale_date = date, surf, carrez)], by = "id_mutation")
allc[, dt := as.numeric(dpe_date - sale_date)]
allc[, `:=`(dS = abs(surface_habitable_logement - surf), dC = abs(surface_habitable_logement - carrez))]
cand <- allc[!is.na(surface_habitable_logement) &
             ((!is.na(carrez) & dC <= pmax(1.5, 0.05 * carrez)) | dS <= pmax(2.5, 0.08 * surf))]
cand[, rel := pmin(fifelse(is.na(carrez), 9, dC / carrez), dS / surf)]

pre <- cand[dt <= 15 & dt >= -3 * 365.25]
setorder(pre, id_mutation, rel, -dt)
best <- pre[, .SD[1], by = id_mutation]
bb  <- best[, .(id_mutation, b_num = numero_dpe, b_lab = etiquette_dpe, b_s = surface_habitable_logement)]
riv <- merge(allc, bb, by = "id_mutation")[numero_dpe != b_num & etiquette_dpe != b_lab &
         abs(surface_habitable_logement - b_s) <= 1.5 & abs(surface_habitable_logement - b_s) > 0.2 &
         dt <= 15 & dt >= -365, .(n_rivals = .N), by = id_mutation]
best <- merge(best, riv, by = "id_mutation", all.x = TRUE)
best[, dpe_tier := fifelse(dt >= -365 & is.na(n_rivals), "high", "medium")]
post_only <- setdiff(cand[dt > 15, unique(id_mutation)], best$id_mutation)

# Attach the matched DPE to each sale; unmatched sales get tier 'post_only' (only a DPE issued after
# the sale matches) or 'none'
dcols <- c("id_mutation", "numero_dpe", "dpe_date", "dt", "etiquette_dpe", "etiquette_ges",
           "conso_5_usages_par_m2_ep", "annee_construction", "periode_construction", "dpe_floor",
           "hauteur_sous_plafond", "logement_traversant", "type_energie_principale_chauffage",
           "surface_habitable_logement", "dpe_tier")
s <- merge(s, best[, ..dcols], by = "id_mutation", all.x = TRUE)
s[is.na(dpe_tier), dpe_tier := fifelse(id_mutation %in% post_only, "post_only", "none")]

# Building attributes by parcel (02c: construction period, heritage protections) and the matched DPE's glazed
# area from the BDNB (02b): sum over orientations, NA only when the BDNB gives no window area at all
s <- merge(s, fread(file.path(RAW, "building_attributes_paris.csv.gz")), by = "id_parcelle", all.x = TRUE)
vit <- c("surface_vitree_nord", "surface_vitree_sud", "surface_vitree_est", "surface_vitree_ouest", "surface_vitree_horizontal")
bw <- fread(file.path(RAW, "bdnb_dpe_logement_75.csv.gz"), select = c("identifiant_dpe", vit))
bw[, win_area := fifelse(rowSums(!is.na(.SD)) > 0, rowSums(.SD, na.rm = TRUE), NA_real_), .SDcols = vit]
s <- merge(s, unique(bw[, .(numero_dpe = identifiant_dpe, win_area)], by = "numero_dpe"), by = "numero_dpe", all.x = TRUE)
s[is.na(numero_dpe), win_area := NA_real_]

fwrite(s, file.path(DER, sprintf("sales_%s_single_apt.csv.gz", TAG)), compress = "gzip")
message(nrow(s), " sales saved; DPE match among in-sample sales: ",
        paste(capture.output(print(s[in_sample == TRUE, table(dpe_tier)])), collapse = " "))
