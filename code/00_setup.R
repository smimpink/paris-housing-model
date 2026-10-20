# Shared settings and helpers. Run every script from the project root, e.g.
#   Rscript code/run_all.R              (all of Paris)
#   SCOPE=5e Rscript code/run_all.R     (Paris 5e only)
# Requires R >= 4.1 with data.table, jsonlite, httr, stringi and R.utils (fread needs it to read the .csv.gz data files).
suppressPackageStartupMessages({
  library(data.table); library(jsonlite); library(httr); library(stringi)
})
if (!requireNamespace("R.utils", quietly = TRUE)) stop("Install the R.utils package: fread needs it to read the .csv.gz data files")

ROOT <- getwd()
if (!dir.exists(file.path(ROOT, "code"))) stop("Run from the project root (folder containing code/).")
RAW <- file.path(ROOT, "data", "raw")
DER <- file.path(ROOT, "data", "derived")
OUT <- file.path(ROOT, "output")
for (d in c(RAW, DER, OUT)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Scope: all 20 arrondissements ("paris", default) or the 5e alone ("5e"). TAG names the data and output files.
SCOPE <- Sys.getenv("SCOPE", "paris")
if (!SCOPE %in% c("paris", "5e")) stop("SCOPE must be 'paris' or '5e'")
COMMUNES  <- if (SCOPE == "5e") "75105" else sprintf("751%02d", 1:20)                 # INSEE codes
POSTCODES <- if (SCOPE == "5e") "75005" else c(sprintf("750%02d", 1:20), "75116")     # 16e has two
TAG <- if (SCOPE == "5e") "75105" else "paris"
DVF_API      <- "https://app.dvf.etalab.gouv.fr/api/mutations3"
SECTIONS     <- "https://cadastre.data.gouv.fr/bundler/cadastre-etalab/communes/%s/geojson/sections"
DPE_API      <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/lines"
BAN_API      <- "https://data.geopf.fr/geocodage/search"          # Base Adresse Nationale geocoder
CADASTRE_API <- "https://apicarto.ign.fr/api/cadastre/parcelle"   # IGN: cadastral parcel containing a point

# GET a JSON endpoint with retries (data.ademe.fr is sometimes slow or drops requests)
get_json <- function(url, tries = 6, wait = 3) {
  for (t in seq_len(tries)) {
    r <- tryCatch(GET(url, timeout(180)), error = function(e) NULL)
    if (!is.null(r) && status_code(r) == 200)
      return(fromJSON(content(r, as = "text", encoding = "UTF-8"), simplifyVector = TRUE))
    message("  retry ", t, " for ", substr(url, 1, 90))
    Sys.sleep(wait * t)
  }
  stop("Failed after retries: ", url)
}

# ---- Target apartment -----------------------------------------------------------------------------------------
# The flat to price is described in a JSON file: target.json at the project root, or the file named by the TARGET
# environment variable. Fields (see README):
#   address        street number, street and postcode, e.g. "13 rue Poliveau 75005 Paris"
#   surf           habitable surface in m2 (measured, comparable to the DPE surface; excludes balconies and terraces)
#   rooms          main rooms (pieces principales: living room and bedrooms; kitchen and bathrooms do not count)
#   deps           dependencies sold with the flat (cellar, parking space, storeroom)
#   label          DPE energy label, A to G
#   ground         1 if the flat is on the ground floor, else 0
#   ceiling        ceiling height in metres
#   terrace        outdoor area in m2 (balcony or terrace), 0 if none                 [optional, default 0]
#   terrace_value  euros added to the interior price for the outdoor space             [optional, default 0]
#   win_area       glazed (window) area in m2 from the flat's DPE; null if unknown      [optional, default null]
#   quarter        sale quarter the prediction is for, e.g. "2025Q4" (a quarter with sales in the sample)
#   parcel         cadastral parcel id, e.g. "75105000AR0028"                          [optional: found from the address]
# The address is geocoded with the BAN. The parcel is the one containing the geocoded point (IGN cadastre API) or,
# failing that, the parcel of past DVF sales at the same address. The street type (rue, boulevard, ...) comes from
# the BAN street name, coded as in DVF.
street_code <- function(street) {
  w <- toupper(stri_trans_general(sub(" .*$", "", street), "Latin-ASCII"))
  codes <- c(RUE = "RUE", BOULEVARD = "BD", AVENUE = "AV", PLACE = "PL", QUAI = "QUAI", VILLA = "VLA", PASSAGE = "PAS",
             CITE = "CITE", IMPASSE = "IMP", SQUARE = "SQ", ALLEE = "ALL", COUR = "COUR")
  if (w %in% names(codes)) codes[[w]] else "RUE"
}

load_target <- function(sales, path = Sys.getenv("TARGET", file.path(ROOT, "target.json"))) {
  if (!file.exists(path)) stop("Target file not found: ", path, " (describe the flat in target.json, see README)")
  t <- fromJSON(path)
  need <- c("address", "surf", "rooms", "deps", "label", "ground", "ceiling", "quarter")
  miss <- setdiff(need, names(t))
  if (length(miss)) stop("Target file is missing: ", paste(miss, collapse = ", "))
  for (v in c("terrace", "terrace_value")) if (is.null(t[[v]])) t[[v]] <- 0
  if (is.null(t$win_area)) t$win_area <- NA_real_
  if (!t$label %in% LETTERS[1:7]) stop("label must be a DPE letter A to G")
  t$ground <- as.integer(t$ground)

  g <- get_json(paste0(BAN_API, "?limit=1&type=housenumber&q=", URLencode(t$address, reserved = TRUE)))
  if (length(g$features) == 0) stop("Address not found by the BAN geocoder: ", t$address)
  p <- g$features$properties
  if (p$score[1] < 0.5) warning("Low geocoding score for '", t$address, "': matched ", p$label[1])
  if (!p$citycode[1] %in% COMMUNES) stop("'", p$label[1], "' is outside the scope (SCOPE=", SCOPE, "): commune ", p$citycode[1])
  t$ban_label <- p$label[1]; t$ban_id <- p$id[1]; t$commune <- p$citycode[1]; t$street_type <- street_code(p$street[1])
  xy <- g$features$geometry$coordinates
  ll <- if (is.matrix(xy)) xy[1, ] else unlist(xy[[1]])

  if (is.null(t$parcel)) {
    pt <- sprintf('{"type":"Point","coordinates":[%.6f,%.6f]}', ll[1], ll[2])
    cd <- get_json(paste0(CADASTRE_API, "?geom=", URLencode(pt, reserved = TRUE)))
    if (length(cd$features) > 0) {
      t$parcel <- cd$features$properties$idu[1]; t$parcel_source <- "cadastre"
    } else {
      past <- sales[grepl(paste0("(^|;)", t$ban_id, "(;|$)"), ban_keys), .N, by = id_parcelle][order(-N)]
      if (nrow(past) == 0) stop("No cadastral parcel found for ", t$ban_label, "; give \"parcel\" in the target file")
      t$parcel <- past$id_parcelle[1]; t$parcel_source <- "DVF sales at the address"
    }
  } else t$parcel_source <- "target file"
  if (!grepl("^75[0-9]{3}[0-9]{3}[0-9A-Z]{2}[0-9]{4}$", t$parcel)) stop("parcel must be a 14-character id such as 75105000AR0028")
  t$section <- substr(t$parcel, 9, 10)
  message("target: ", t$ban_label, " (BAN ", t$ban_id, "), parcel ", t$parcel, " from ", t$parcel_source,
          ", section ", t$commune, "_", t$section, ", street type ", t$street_type)
  t
}

# ---- Estimation helpers ---------------------------------------------------------------------------------------
# OLS with cluster-robust variance (same small-sample factor as Stata/statsmodels/fixest:
# G/(G-1) * (N-1)/(N-K)). Data must have no missing values in model variables.
ols_cl <- function(formula, data, cluster) {
  m  <- lm(formula, data = data)
  ok <- !is.na(coef(m))
  X  <- model.matrix(m)[, ok, drop = FALSE]
  e  <- resid(m); n <- nrow(X); k <- ncol(X)
  XtXi <- solve(crossprod(X))
  S <- rowsum(X * e, data[[cluster]])
  G <- nrow(S)
  V <- XtXi %*% crossprod(S) %*% XtXi * (G / (G - 1)) * ((n - 1) / (n - k))
  list(m = m, coef = coef(m)[ok], V = V, sigma2 = sum(e^2) / (n - k),
       smear = mean(exp(e)), n = n, G = G, r2 = summary(m)$r.squared)
}

# Prediction in levels: Duan smearing for the mean, 80% prediction interval on the log scale
predict_cl <- function(fit, newdata) {
  tt <- delete.response(terms(fit$m))
  X  <- model.matrix(tt, model.frame(tt, newdata, xlev = fit$m$xlevels))
  X  <- X[, names(fit$coef), drop = FALSE]
  xb <- drop(X %*% fit$coef)
  se_fit <- sqrt(rowSums((X %*% fit$V) * X))   # diagonal of X V X', so newdata can have several rows
  se_p <- sqrt(se_fit^2 + fit$sigma2); z <- qnorm(0.9)
  data.table(mean = exp(xb) * fit$smear, pi80_lo = exp(xb - z * se_p), pi80_hi = exp(xb + z * se_p))
}

coef_table <- function(fit, pattern) {
  b <- fit$coef; se <- sqrt(diag(fit$V))[names(b)]
  keep <- grepl(pattern, names(b))
  data.table(term = names(b)[keep], coef = round(b[keep], 4), se = round(se[keep], 4))
}

# One-row newdata whose factor columns carry the estimation sample's levels
newrow <- function(d, vals) {
  nd <- as.data.table(vals)
  for (v in names(nd)) if (is.factor(d[[v]])) nd[[v]] <- factor(nd[[v]], levels = levels(d[[v]]))
  nd
}

# OLS with one absorbed fixed effect (within transformation) and cluster-robust variance. The fixed effect
# must be nested in the clusters (section FE, section clusters); small-sample factor as fixest with nested FE:
# G/(G-1) * (n-1)/(n-k), k = non-absorbed coefficients. Same slopes as dummy-variable OLS (Frisch-Waugh-Lovell),
# without building one column per fixed effect. Data must have no missing values in model variables.
ols_fe <- function(formula, data, fe, cluster) {
  mf <- model.frame(formula, data); y <- model.response(mf)
  X  <- model.matrix(formula, mf)[, -1, drop = FALSE]          # intercept absorbed by the fixed effect
  gi <- match(data[[fe]], unique(data[[fe]])); ng <- tabulate(gi)
  dm <- function(M) M - (rowsum(M, gi, reorder = FALSE) / ng)[gi, , drop = FALSE]
  Xd <- dm(X); yd <- drop(dm(matrix(y)))
  b  <- lm.fit(Xd, yd)$coefficients; b <- b[!is.na(b)]
  Xd <- Xd[, names(b), drop = FALSE]
  e  <- yd - drop(Xd %*% b)
  alpha <- tapply(y - drop(X[, names(b), drop = FALSE] %*% b), data[[fe]], mean)
  n <- length(y); k <- length(b)
  XtXi <- solve(crossprod(Xd))
  S <- rowsum(Xd * e, data[[cluster]]); G <- nrow(S)
  V <- XtXi %*% crossprod(S) %*% XtXi * (G / (G - 1)) * ((n - 1) / (n - k))
  list(coef = b, V = V, alpha = alpha, fe = fe, terms = terms(mf), xlevels = .getXlevels(terms(mf), mf),
       sigma2 = sum(e^2) / (n - k - length(ng)), smear = mean(exp(e)), n = n, G = G,
       r2 = 1 - sum(e^2) / sum((y - mean(y))^2))
}

# Expected price (Duan smearing) and median price exp(xb) for new rows; newdata carries the fixed-effect column
predict_fe <- function(fit, newdata) {
  tt <- delete.response(fit$terms)
  X  <- model.matrix(tt, model.frame(tt, newdata, xlev = fit$xlevels))[, names(fit$coef), drop = FALSE]
  xb <- drop(X %*% fit$coef) + fit$alpha[as.character(newdata[[fit$fe]])]
  data.table(mean = exp(xb) * fit$smear, median = exp(xb))
}

# Out-of-fold residuals from k-fold cross-validation of ols_fe's model (folds stratified by the fixed effect),
# for prediction intervals built from the empirical error distribution. A test sale whose fixed-effect group
# has no training sales gets the mean training residual of its `fallback` group (e.g. its arrondissement).
cv_resid_fe <- function(formula, data, fe, fallback, k = 10, seed = 20261002) {
  mf <- model.frame(formula, data); y <- model.response(mf)
  X  <- model.matrix(formula, mf)[, -1, drop = FALSE]
  g  <- as.character(data[[fe]]); fb <- as.character(data[[fallback]])
  set.seed(seed)
  fold <- integer(length(y))
  for (ix in split(seq_along(y), g)) fold[ix] <- sample(rep(seq_len(k), length.out = length(ix)))
  e <- numeric(length(y))
  for (f in seq_len(k)) {
    tr <- which(fold != f); te <- which(fold == f)
    gi <- match(g[tr], unique(g[tr])); ng <- tabulate(gi)
    dm <- function(M) M - (rowsum(M, gi, reorder = FALSE) / ng)[gi, , drop = FALSE]
    b  <- lm.fit(dm(X[tr, , drop = FALSE]), drop(dm(matrix(y[tr]))))$coefficients; b[is.na(b)] <- 0
    r  <- y[tr] - drop(X[tr, , drop = FALSE] %*% b)
    a  <- tapply(r, g[tr], mean); a_fb <- tapply(r, fb[tr], mean)
    at <- a[g[te]]; at[is.na(at)] <- a_fb[fb[te][is.na(at)]]
    e[te] <- y[te] - drop(X[te, , drop = FALSE] %*% b) - at
  }
  e
}
