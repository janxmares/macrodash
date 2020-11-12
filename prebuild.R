library(readr)
library(czso)


# Pre-build (almost) static data for mortality chart ----------------------

vek <- czso::czso_get_table("130142")
write_rds(vek, "vek.rds")

vek_ciselnik <- czso::czso_get_codelist("cis7700")
write_rds(vek_ciselnik, "vek_ciselnik.rds")