# Run the whole pipeline from the project root. Scope from the SCOPE environment variable (see 00_setup.R).
for (f in c("01_pull_dvf.R", "02_pull_dpe.R", "02b_pull_bdnb.R", "02c_pull_buildings.R", "03_build_sample.R",
            "04_regression.R")) {
  message("== ", f)
  source(file.path("code", f), local = new.env())
}
