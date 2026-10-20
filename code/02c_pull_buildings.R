# 02c: Building attributes for every Paris cadastral parcel: construction period, heritage protections.
#   - Construction period: APUR "Emprise batie Paris" building footprints (c_perconst class, an_const year,
#     c_tissu urban fabric). A parcel takes the period of its largest dated footprint (point-on-surface in parcel).
#   - Protection Ville de Paris (PLU bioclimatique, opendata.paris.fr plub_surf_pvp): protected-building
#     polygons, each carrying its parcel reference c_asp ('18-AS-0207' -> 75118000AS0207).
#   - Monuments historiques (Base Merimee, Ministere de la Culture): entries located by their WGS84 point.
#   - Marais and 7e heritage plans (PSMV, Geoportail de l'urbanisme WFS wfs_du:prescription_surf): building
#     grades 1 = monument historique, 2 = type A, 3 = type B, 4 = may be kept or replaced, 5 = demolition may be
#     imposed; a parcel takes the grade covering most of its area.
#   - Parcels: Etalab cadastre (departement 75).
# Spatial joins run in DuckDB with its spatial extension (duckdb command-line tool must be installed).
# Sources are cached in BUILDINGS_CACHE (default: a temporary folder); existing files are not downloaded again.
# Output: data/raw/building_attributes_paris.csv.gz (one row per parcel)
source("code/00_setup.R")

cache <- Sys.getenv("BUILDINGS_CACHE", file.path(tempdir(), "buildings"))
dir.create(cache, recursive = TRUE, showWarnings = FALSE)
cf <- function(x) file.path(cache, x)
fetch <- function(url, dest) if (!file.exists(dest)) {
  message("downloading ", basename(dest))
  if (download.file(url, dest, mode = "wb", quiet = TRUE) != 0) stop("Download failed: ", url)
}

# Parcels
fetch("https://cadastre.data.gouv.fr/data/etalab-cadastre/latest/geojson/departements/75/cadastre-75-parcelles.json.gz",
      cf("cadastre-75-parcelles.json.gz"))
if (!file.exists(cf("cadastre-75-parcelles.json"))) system2("gunzip", c("-k", shQuote(cf("cadastre-75-parcelles.json.gz"))))

# APUR footprints: ArcGIS REST query, 3,000 features per page, one GeoJSON file per page
if (!file.exists(cf("emprise_batie_paris.geojson")) && !file.exists(cf("apur_eb_000.geojson"))) {
  base <- "https://carto2.apur.org/apur/rest/services/OPENDATA/EMPRISE_BATIE_PARIS/MapServer/0/query"
  off <- 0
  repeat {
    q <- list(where = "1=1", outFields = "OBJECTID,c_perconst,an_const,c_tissu,Shape_Area", returnGeometry = "true",
              outSR = "4326", geometryPrecision = "7", orderByFields = "OBJECTID", resultOffset = off,
              resultRecordCount = 3000, f = "geojson")
    txt <- NULL
    for (t in 1:4) {
      r <- tryCatch(GET(base, query = q, timeout(180)), error = function(e) NULL)
      if (!is.null(r) && status_code(r) == 200) { txt <- content(r, as = "text", encoding = "UTF-8"); break }
      Sys.sleep(5 * t)
    }
    if (is.null(txt)) stop("APUR query failed at offset ", off)
    n <- length(fromJSON(txt, simplifyVector = FALSE)$features)
    writeLines(txt, cf(sprintf("apur_eb_%03d.geojson", off / 3000)))
    message("APUR offset ", off, ": ", n, " footprints")
    if (n < 3000) break
    off <- off + 3000
  }
}
apur_files <- if (file.exists(cf("emprise_batie_paris.geojson"))) cf("emprise_batie_paris.geojson") else
  sort(list.files(cache, "^apur_eb_\\d+\\.geojson$", full.names = TRUE))

# Ville de Paris protected buildings, Merimee (national file, filtered to Paris in SQL)
fetch("https://opendata.paris.fr/api/explore/v2.1/catalog/datasets/plub_surf_pvp/exports/csv", cf("plub_surf_pvp.csv"))
fetch("https://ministere-culture.s3.sbg.io.cloud.ovh.net/POP/merimee.csv", cf("merimee.csv"))

# PSMV prescriptions (Marais = partition A, 7e = partition B), paged WFS
for (part in c("A", "B")) {
  dest <- cf(sprintf("PSMV_75056_%s_prescription_surf.geojson", part))
  if (file.exists(dest)) next
  feats <- list(); start <- 0
  repeat {
    url <- paste0("https://data.geopf.fr/wfs/ows?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature",
                  "&TYPENAMES=wfs_du:prescription_surf&OUTPUTFORMAT=application/json&COUNT=5000&STARTINDEX=", start,
                  "&CQL_FILTER=", URLencode(sprintf("partition='PSMV_75056_%s'", part), reserved = TRUE))
    pg <- get_json(url)
    if (length(pg$features) == 0) break
    feats[[length(feats) + 1]] <- toJSON(pg$features, auto_unbox = TRUE, digits = NA)
    if (nrow(as.data.frame(pg$features$properties)) < 5000) break
    start <- start + 5000
  }
  writeLines(paste0('{"type":"FeatureCollection","features":[',
                    paste(gsub("^\\[|\\]$", "", unlist(feats)), collapse = ","), "]}"), dest)
}

sq <- function(x) gsub("'", "''", x)
eb_union <- paste(sprintf("SELECT c_perconst, an_const, c_tissu, Shape_Area, geom FROM ST_Read('%s')", sq(apur_files)),
                  collapse = " UNION ALL ")
out <- file.path(RAW, "building_attributes_paris.csv.gz")
sql <- sprintf("
INSTALL spatial; LOAD spatial;
CREATE TABLE parc AS SELECT id AS id_parcelle, geom FROM ST_Read('%s');
CREATE TABLE eb AS SELECT c_perconst::INT AS c_perconst, an_const::INT AS an_const, c_tissu, Shape_Area::DOUBLE AS area,
  ST_PointOnSurface(geom) AS pt FROM (%s) WHERE c_perconst IS NOT NULL AND c_perconst::INT <> 99;
CREATE TABLE per AS SELECT p.id_parcelle, arg_max(e.c_perconst, e.area) AS apur_period, arg_max(e.an_const, e.area) AS apur_year,
  arg_max(e.c_tissu, e.area) AS apur_tissu FROM parc p JOIN eb e ON ST_Within(e.pt, p.geom) GROUP BY 1;
CREATE TABLE pvp AS SELECT DISTINCT '751' || substr(c_asp, 1, 2) || '000' || substr(c_asp, 4, 2) || substr(c_asp, 7, 4) AS id_parcelle
  FROM read_csv('%s', delim = ';', all_varchar = true) WHERE c_asp IS NOT NULL;
CREATE TABLE mh AS SELECT Typologie_de_la_protection AS typ,
  ST_Point(CAST(trim(split_part(coordonnees_au_format_WGS84, ',', 2)) AS DOUBLE), CAST(trim(split_part(coordonnees_au_format_WGS84, ',', 1)) AS DOUBLE)) AS pt
  FROM read_csv('%s', delim = '|', all_varchar = true, quote = '\"')
  WHERE Departement_format_numerique = '75' AND coordonnees_au_format_WGS84 LIKE '%%,%%';
CREATE TABLE mhp AS SELECT p.id_parcelle, max(CASE WHEN m.typ ILIKE '%%class%%' THEN 1 ELSE 0 END) AS mh_classe
  FROM parc p JOIN mh m ON ST_Within(m.pt, p.geom) GROUP BY 1;
CREATE TABLE psmv AS SELECT CASE
    WHEN libelle = 'Immeubles protégés en totalité au titre des monuments historiques' THEN 1
    WHEN libelle LIKE '%%type A' THEN 2 WHEN libelle LIKE '%%type B' THEN 3
    WHEN libelle LIKE '%%pouvant être conservé, amélioré ou remplacé' THEN 4
    WHEN libelle LIKE '%%démolition%%imposée%%' THEN 5 END AS grade, geom
  FROM (SELECT libelle, typepsc, geom FROM ST_Read('%s') UNION ALL SELECT libelle, typepsc, geom FROM ST_Read('%s'))
  WHERE typepsc IN ('07', '99', '03') AND libelle LIKE 'Immeuble%%';
CREATE TABLE psmvp AS SELECT p.id_parcelle, arg_max(s.grade, ST_Area(s.geom)) AS psmv_grade
  FROM parc p JOIN psmv s ON ST_Within(ST_PointOnSurface(s.geom), p.geom) WHERE s.grade IS NOT NULL GROUP BY 1;
COPY (SELECT p.id_parcelle, per.apur_period, per.apur_year, per.apur_tissu,
        CASE WHEN pvp.id_parcelle IS NULL THEN 0 ELSE 1 END AS pvp,
        CASE WHEN mhp.id_parcelle IS NULL THEN 0 ELSE 1 END AS mh, coalesce(mhp.mh_classe, 0) AS mh_classe,
        psmvp.psmv_grade
      FROM parc p LEFT JOIN per USING (id_parcelle) LEFT JOIN pvp USING (id_parcelle)
      LEFT JOIN mhp USING (id_parcelle) LEFT JOIN psmvp USING (id_parcelle)) TO '%s' (HEADER, COMPRESSION gzip);
", sq(cf("cadastre-75-parcelles.json")), eb_union, sq(cf("plub_surf_pvp.csv")), sq(cf("merimee.csv")),
   sq(cf("PSMV_75056_A_prescription_surf.geojson")), sq(cf("PSMV_75056_B_prescription_surf.geojson")), sq(out))
sql_file <- tempfile(fileext = ".sql"); writeLines(sql, sql_file)
res <- system2("duckdb", stdin = sql_file, stdout = TRUE, stderr = TRUE)
if (!is.null(attr(res, "status")) && attr(res, "status") != 0) stop("DuckDB failed:\n", paste(res, collapse = "\n"))

b <- fread(out)
message(nrow(b), " parcels; dated: ", sum(!is.na(b$apur_period)), "; Ville de Paris protected: ", sum(b$pvp),
        "; monument historique: ", sum(b$mh), "; Marais/7e graded: ", sum(!is.na(b$psmv_grade)))
