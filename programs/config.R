# ###########################
# CONFIG: define  and filenames for later reference
# ###########################

# AEA DOI prefix

doi_prefix <- "10.1257"

## These control whether the external data is downloaded.
download_crossref  <- TRUE
download_migration <- TRUE
download_datacite  <- TRUE

## Time frame: Crossref download starts here; figures cover firstyear through lastyear
firstpubday <- "2000-01-01"
firstyear   <- 2000
# last complete year; the current (partial) year is excluded from figures
lastyear    <- as.integer(format(Sys.Date(),"%Y")) - 1

# Crossref-related filenames

issns.file <- file.path(crossrefloc,paste0("issns.Rds"))

doi.file <- file.path(crossrefloc,"crossref_dois")
doi.file.Rds <- paste(doi.file,"Rds",sep=".")
doi.file.csv <- paste(doi.file,"csv",sep=".")
crossref.raw.Rds <- file.path(interwrk,"crossref_raw.Rds")
# references to openICPSR deposits, extracted from the Crossref reference lists
icpsr.refs.Rds   <- file.path(crossrefloc,"crossref_icpsr_references.Rds")

# Migration data (historical supplements migrated to openICPSR)
# from https://github.com/AEADataEditor/aea-supplement-migration

migration.base <- "https://raw.githubusercontent.com/AEADataEditor/aea-supplement-migration/master/data"
migration.files <- c("generated/table.aea.icpsr.mapping.csv",
                     "acquired/aea_icpsr_final_batch_20210930.csv")

# DataCite (openICPSR deposits linked to AEA articles)

datacite.api     <- "https://api.datacite.org/dois"
datacite.client  <- "gesis.icpsr"
datacite.raw.Rds <- file.path(dataciteloc,"datacite_aea_raw.Rds")
datacite.csv     <- file.path(dataciteloc,"datacite_aea_links.csv")
datacite.titles.raw.Rds <- file.path(dataciteloc,"datacite_titles_raw.Rds")
datacite.titles.csv     <- file.path(dataciteloc,"datacite_aea_titles.csv")
# title templates used for AEA replication packages at openICPSR
datacite.title.templates <- c("Data and Code for","Replication data for","Replication package for",
                              "Replication files for","Code for")

# Outputs

packages.Rds      <- file.path(Outputs,"packages_by_article.Rds")
packages.csv      <- file.path(Outputs,"packages_by_article.csv")
by_year.csv       <- file.path(Outputs,"packages_by_year.csv")
by_journal.csv    <- file.path(Outputs,"packages_by_journal_year.csv")
# P&P articles printed in the May AER (to 2017) attributed to the P&P
by_journal_pp.csv <- file.path(Outputs,"packages_by_journal_year_pp.csv")
