# 04: Hedonic price regression on single-apartment sales in scope (all of Paris, or the 5e; see SCOPE in
# 00_setup.R), 2021-2025, with DPE, building and window controls, and a price prediction for the target apartment
# described in target.json (see 00_setup.R): at the target quarter from the model and, when that is the last
# quarter in the data (2025Q4), rolled forward to 2026Q2 with the INSEE-Notaires Paris apartment index.
#   log(price) = b*log(surface) + rooms + dependencies + energy label + ground floor + ceiling height
#                + construction period + heritage protections + street type + log(window area)
#                + section FE + sale-quarter FE + e
# Sample: sales with a matched DPE, a dated building and a window area, inside Tukey's far-out fences on log price
# per m2 (03, by arrondissement): sales above them (luxury outliers) and below them (symbolic transfers, family sales,
# life-annuity down payments) are dropped. Surface is the DPE habitable
# surface (measured). Floor enters only as a ground-floor indicator (1 if the DPE puts the flat on floor 0,
# else 0). Ceiling height in metres.
# Construction period: APUR class of the parcel's largest dated building (02c); reference before 1800; sales
# whose parcel has no dated building are dropped. Heritage: Ville de Paris protected building (pvp), monument
# historique (mh), Marais/7e heritage-plan grade (psmv_A = MH or type A, psmv_B = type B). Street type from the
# DVF street name; reference rue. Window area: glazed area of the matched DPE (BDNB, 02b); sales without it
# are dropped. Glazing-to-floor ratios outside Tukey's far-out fences are winsorized to the nearest fence.
# Section FE are cadastral sections within arrondissements, absorbed by the within transformation; SEs
# clustered by section. Section and quarter fixed effects are not reported.
# 80% prediction band: 10th and 90th percentiles of 10-fold cross-validated residuals of sales similar to
# the target (same ground/upper floor, rooms within 1, surface within 20 m2). Residuals are heavy-tailed,
# so a normal band would cover more than 80% of sales.
# Input:  data/derived/sales_<TAG>_single_apt.csv.gz, data/raw/building_attributes_paris.csv.gz (target lookup),
#         target.json (or the file named by TARGET)
# Output: output/regression_coefs_<TAG>.csv, output/target_prediction_<TAG>.csv
source("code/00_setup.R")

sales <- fread(file.path(DER, sprintf("sales_%s_single_apt.csv.gz", TAG)), colClasses = list(character = "commune"))
d <- sales[in_sample == TRUE & dpe_tier %in% c("high", "medium")]
TARGET <- load_target(sales)

periods <- c("<1800", "1801-1850", "1851-1914", "1915-1939", "1940-1967", "1968-1975", "1976+", "undated")
period_of <- function(code) factor(fcase(code == 1, "<1800", code == 2, "1801-1850", code == 3, "1851-1914",
                                         code == 5, "1915-1939", code == 6, "1940-1967", code == 7, "1968-1975",
                                         code %in% 8:12, "1976+", default = "undated"), levels = periods)
styps <- c("rue", "boulevard/avenue", "quai", "place", "villa/passage/cite")
styp_of <- function(st) factor(fcase(st %in% c("BD", "AV", "AVE"), "boulevard/avenue", st == "QUAI", "quai", st == "PL", "place",
                                     st %in% c("VLA", "VIL", "CITE", "IMP", "PAS", "SQ", "ALL", "COUR"), "villa/passage/cite",
                                     default = "rue"), levels = styps)

# Rooms top-coded at 5+ and dependencies at 2+; labels A-C pooled (few A and B sales).
# References: 1 room, 0 dependencies, label A-C, built before 1800, rue.
d[, `:=`(rooms_f = factor(fifelse(rooms >= 5, "5+", as.character(rooms)), levels = c("1", "0", "2", "3", "4", "5+")),
         deps_f  = factor(fifelse(deps >= 2, "2+", as.character(deps)), levels = c("0", "1", "2+")),
         label   = factor(fifelse(etiquette_dpe %in% c("A", "B", "C"), "A-C", etiquette_dpe), levels = c("A-C", "D", "E", "F", "G")),
         ground  = as.integer(dpe_floor %in% 0L),
         # Heights above 10 are unit errors in the DPE register (cm or dm): 262 -> 2.62 m, 28 -> 2.8 m
         ceiling = fcase(hauteur_sous_plafond > 100, hauteur_sous_plafond / 100,
                         hauteur_sous_plafond > 10, hauteur_sous_plafond / 10, rep(TRUE, .N), hauteur_sous_plafond),
         dpe_surf = surface_habitable_logement,
         period  = period_of(apur_period),
         pvp     = fcoalesce(as.integer(pvp), 0L), mh = fcoalesce(as.integer(mh), 0L),
         psmv_A  = as.integer(psmv_grade %in% 1:2), psmv_B = as.integer(psmv_grade %in% 3),
         styp    = styp_of(street_type),
         quarter = factor(quarter))]

# Window area: Tukey far-out fences on log(glazed m2 per floor m2), over all matched sales with a window area;
# ratios outside them are winsorized to the fence (window area = fence ratio x floor surface)
lr <- d[win_area > 0, log(win_area / dpe_surf)]
win_fence <- exp(quantile(lr, c(0.25, 0.75)) + c(-3, 3) * diff(quantile(lr, c(0.25, 0.75))))
d[, win_ok := !is.na(win_area)]
n_matched <- nrow(d)
n_drop <- d[, .(no_window = sum(!win_ok), undated = sum(period == "undated"), both = sum(!win_ok & period == "undated"))]
d <- d[win_ok == TRUE & period != "undated"]
d[, win_ratio := win_area / dpe_surf]
n_wins <- d[, .(low = sum(win_ratio < win_fence[1]), high = sum(win_ratio > win_fence[2]))]
d[, `:=`(win_area = dpe_surf * pmin(pmax(win_ratio, win_fence[1]), win_fence[2]),
         period = droplevels(period), quarter = droplevels(quarter))]   # 2021Q2 has no sale left
d[, lwin := log(win_area)]
if (!TARGET$quarter %in% levels(d$quarter))
  stop("target quarter must be a sale quarter in the sample: ", paste(levels(d$quarter), collapse = " "))

f   <- log(price) ~ log(dpe_surf) + rooms_f + deps_f + label + ground + ceiling + period + pvp + mh + psmv_A + psmv_B +
  styp + lwin + quarter
fit <- ols_fe(f, d, fe = "sec_id", cluster = "sec_id")

# Prediction band from the cross-validated errors of similar sales
e_cv    <- cv_resid_fe(f, d, fe = "sec_id", fallback = "commune")
similar <- d$ground == TARGET$ground & abs(d$rooms - TARGET$rooms) <= 1 & abs(d$dpe_surf - TARGET$surf) <= 20
if (sum(similar) < 30) warning("only ", sum(similar), " sales similar to the target: the prediction band is unreliable")
band    <- quantile(e_cv[similar], c(0.1, 0.9))

# Target: building attributes from its parcel; unknown window area -> median of similar flats with window data
ba <- fread(file.path(RAW, "building_attributes_paris.csv.gz"))[id_parcelle == TARGET$parcel]
if (nrow(ba) != 1) stop("Target parcel not found in building_attributes_paris.csv.gz: ", TARGET$parcel)
t_win <- if (is.na(TARGET$win_area)) d[similar, exp(median(log(win_area)))] else TARGET$win_area
# A section with no sale in the estimation sample takes the mean effect of its arrondissement's sections (as in the
# cross-validation)
sec_id <- paste0(TARGET$commune, "_", TARGET$section)
if (!sec_id %in% names(fit$alpha)) {
  arr <- grep(paste0("^", TARGET$commune, "_"), names(fit$alpha), value = TRUE)
  warning("no sale in section ", sec_id, " in the estimation sample; using the mean effect of the arrondissement's ",
          length(arr), " sections")
  fit$alpha[sec_id] <- mean(fit$alpha[arr])
}
# Outdoor space: DVF, the Carrez area and the DPE surface all exclude balconies and terraces, and no source records
# usable outdoor space, so the regression cannot price it. The model prices the interior alone (surf); the target's
# outdoor space, if any, is added afterwards as the fixed terrace_value in euros given in the target file.
TERRACE_VALUE <- if (TARGET$terrace > 0) TARGET$terrace_value else 0
t_surf <- TARGET$surf
tgt  <- newrow(d, list(dpe_surf = t_surf, rooms_f = as.character(TARGET$rooms), deps_f = as.character(TARGET$deps),
                       label = TARGET$label, ground = TARGET$ground, ceiling = TARGET$ceiling,
                       period = as.character(period_of(ba$apur_period)), pvp = ba$pvp, mh = ba$mh,
                       psmv_A = as.integer(ba$psmv_grade %in% 1:2), psmv_B = as.integer(ba$psmv_grade %in% 3),
                       styp = as.character(styp_of(TARGET$street_type)), lwin = log(t_win),
                       quarter = TARGET$quarter, sec_id = sec_id))
pred <- cbind(as.data.table(TARGET[c("address", "parcel", "surf", "terrace", "rooms", "deps", "label", "ground", "ceiling", "quarter")]),
              surf_priced = t_surf, terrace_value = TERRACE_VALUE,
              period = as.character(tgt$period), pvp = ba$pvp, mh = ba$mh, psmv_grade = ba$psmv_grade,
              styp = as.character(tgt$styp), win_area_used = t_win, predict_fe(fit, tgt))
pred[, `:=`(basis = "model", pi80_lo = median * exp(band[[1]]), pi80_hi = median * exp(band[[2]]), n_similar = sum(similar))]

# INSEE-Notaires index, Paris apartments, seasonally adjusted quarterly changes after the last sale quarter in DVF
# (2026Q2 provisional); update when a later index or DVF release is used. INSEE Informations rapides, "Au deuxieme
# trimestre 2026, les prix des logements anciens reculent de 1,0 %", https://www.insee.fr/fr/statistiques/9040119
PARIS_APT_QCHG <- c("2026Q1" = 0.001, "2026Q2" = -0.004)
if (TARGET$quarter == max(levels(d$quarter))) {
  roll <- prod(1 + PARIS_APT_QCHG)
  pred <- rbind(pred, copy(pred)[, `:=`(quarter = tail(names(PARIS_APT_QCHG), 1),
    basis = sprintf("model %s x Paris apartment index", TARGET$quarter),
    mean = mean * roll, median = median * roll, pi80_lo = pi80_lo * roll, pi80_hi = pi80_hi * roll)])
}
# Interior-only figures are kept; the terrace value is then added to the price and the band in euros (not indexed)
pred[, `:=`(mean_interior = mean, median_interior = median)]
pred[, `:=`(mean = mean + TERRACE_VALUE, median = median + TERRACE_VALUE,
            pi80_lo = pi80_lo + TERRACE_VALUE, pi80_hi = pi80_hi + TERRACE_VALUE)]
pred[, `:=`(mean_ppm = mean / TARGET$surf, mean_ppm_interior = mean_interior / TARGET$surf)]

coefs <- coef_table(fit, "surf|rooms_f|deps_f|label|ground|ceiling|period|pvp|mh|psmv|styp|lwin")
fwrite(coefs, file.path(OUT, sprintf("regression_coefs_%s.csv", TAG)))
fwrite(pred, file.path(OUT, sprintf("target_prediction_%s.csv", TAG)))

print(coefs, nrows = 100)
with(fit, message("n = ", n, ", sections = ", length(alpha), ", clusters = ", G, ", R2 = ", round(r2, 3),
                  ", residual SD = ", round(sqrt(sigma2), 3), ", CV RMSE = ", round(sqrt(mean(e_cv^2)), 4),
                  ", smearing = ", round(smear, 3)))
message("dropped from ", n_matched, " DPE-matched sales: ", n_drop$no_window, " without window area, ", n_drop$undated,
        " undated (", n_drop$both, " both); winsorized to the fences (", round(win_fence[1], 3), "-", round(win_fence[2], 3),
        " glazed m2 per floor m2): ", n_wins$low, " low, ", n_wins$high, " high; target window area used: ", round(t_win, 1), " m2")
message("similar sales: ", sum(similar), "; 80% band on log scale: ", round(band[[1]], 3), " to ", round(band[[2]], 3))
print(pred[, .(quarter, basis, mean_interior = round(mean_interior, -3), terrace_value, mean = round(mean, -3),
               median = round(median, -3), pi80_lo = round(pi80_lo, -3), pi80_hi = round(pi80_hi, -3),
               mean_ppm = round(mean_ppm), mean_ppm_interior = round(mean_ppm_interior))])
