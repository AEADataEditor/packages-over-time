# Getting the historical supplements migrated from the AEA website to openICPSR
# Data from https://github.com/AEADataEditor/aea-supplement-migration
#

source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)

# filenames in config.R

for ( f in migration.files ) {
  dest <- file.path(migrationloc,basename(f))
  if ( download_migration == TRUE | !file.exists(dest) ) {
    message(paste0("Downloading ",f))
    download.file(paste(migration.base,f,sep="/"),destfile=dest,mode="wb")
  } else {
    warning(paste("Re-using file:",dest))
  }
}

# First batch (October 2019): article DOI -> openICPSR DOI
mapping <- read_csv(file.path(migrationloc,"table.aea.icpsr.mapping.csv")) %>%
  select(doi,icpsr_doi)
nrow(mapping)

# Final batch (September 2021): large supplements migrated later; no openICPSR DOI recorded
final.batch <- read_csv(file.path(migrationloc,"aea_icpsr_final_batch_20210930.csv")) %>%
  transmute(doi=aa_DOI,icpsr_doi=NA_character_)
nrow(final.batch)

migration <- bind_rows(mutate(mapping,batch="2019-10"),
                       mutate(final.batch,batch="2021-09")) %>%
  filter(!is.na(doi)) %>%
  mutate(doi=tolower(doi),icpsr_doi=tolower(icpsr_doi)) %>%
  distinct(doi,.keep_all=TRUE)

nrow(migration)
saveRDS(migration,file=file.path(migrationloc,"migration_packages.Rds"))
