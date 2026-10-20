# 01: Pull every DVF transaction in scope (all cadastral sections of each arrondissement) from the Etalab
# DVF app API. Endpoint documented in github.com/etalab/DVF-app (app.py): /api/mutations3/<commune>/<prefix+section>
# As of Oct 2026 the app holds mutations dated 2021-01-01 to 2025-12-31.
# Output: data/raw/dvf_<TAG>_rows.csv.gz  (one row per DVF line; a sale spans several lines)
source("code/00_setup.R")

rows <- rbindlist(lapply(COMMUNES, function(cm) {
  props <- as.data.table(get_json(sprintf(SECTIONS, cm))$features$properties)
  message(cm, ": ", nrow(props), " cadastral sections")
  rbindlist(lapply(seq_len(nrow(props)), function(i) {
    d <- get_json(sprintf("%s/%s/%s", DVF_API, cm, paste0(props$prefixe[i], props$code[i])))
    Sys.sleep(0.2)
    if (length(d$mutations) == 0) return(NULL)
    dt <- as.data.table(d$mutations)
    dt[, section := props$code[i]]
    dt
  }), fill = TRUE)
}), fill = TRUE)

# The API returns every value as a string; Python-style missing markers become NA
for (j in names(rows)) set(rows, which(rows[[j]] %in% c("None", "nan", "NaT", "")), j, NA)

fwrite(rows, file.path(RAW, sprintf("dvf_%s_rows.csv.gz", TAG)), compress = "gzip")
message(nrow(rows), " DVF lines, ", uniqueN(rows$id_mutation), " mutations, dates ",
        min(rows$date_mutation), " to ", max(rows$date_mutation))
