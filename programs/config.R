# ###########################
# CONFIG: define  and filenames for later reference
# ###########################

# AEA DOI prefix

doi_prefix <- "10.1257"

## These control whether the external data is downloaded.
download_crossref  <- TRUE
download_migration <- TRUE
download_datacite  <- TRUE

## Time frame: DOIs were introduced in 1997 and Crossref has AEA records from 1999 (AER, JEL) on,
## but some print dates are wrong (e.g., 1999 articles dated 1996). Download everything Crossref
## has (from firstpubday) and restrict to firstyear after correcting the year from the volume.
## Figures end at the last complete year.
firstpubday <- "1985-01-01"
firstyear   <- 1999
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
# article-level database, with policy flags
article.db.Rds    <- file.path(Outputs,"articles_policy.Rds")
article.db.csv    <- file.path(Outputs,"articles_policy.csv")
# by year and policy period
by_period.csv     <- file.path(Outputs,"packages_by_period.csv")

# Data (and code) availability policy
# The AER data policy took effect in 2005 (announced in 2004). The AEJs had the policy from
# their creation (2009). AER:Insights started in 2019 under the policy. No other AEA journal
# (including the P&P, whether printed in the May AER or not) had a mandate before the
# 2019 AEA-wide policy. Journal is the journal an article is attributed to,
# after moving the P&P articles printed in the May AER to AEA P&P.
policy.year <- function(Journal) {
  dplyr::case_when(Journal=="AER"                  ~ 2005L,
                   stringr::str_starts(Journal,"AEJ:") ~ 2009L,
                   TRUE                             ~ 2019L)
}

# Submission-to-publication delay (06_delay.R)
# DOIs based on manuscript numbers (10.1257/aer.20151234: submitted 2015, manuscript 1234).
# The P&P (numbered by conference program), JEL and JEP (commissioned; counter not reset each year)
# are excluded.
delay.prefixes <- c("aer"="AER","aeri"="AER:Insights","app"="AEJ:Applied Economics",
                    "pol"="AEJ:Economic Policy","mac"="AEJ:Macro","mic"="AEJ:Micro")
# policy that is compared: published in or after this year (AEA-wide policy, Data Editor review)
delay.policy.year <- 2019L
# alternative cutoff, as a robustness check (papers accepted after mid-2019 are published from 2020 on)
delay.policy.year.alt <- 2020L
# A submission year (cohort) is mapped to quarters only if at least this share of its eventual
# publications is observed with a manuscript-number DOI, and it has at least delay.min.cohort articles
delay.min.coverage <- 0.8
delay.min.cohort   <- 20L
# lags (publication year - submission year) of delay.max.lag or more are pooled
delay.max.lag      <- 7L
# A publication year is analyzed only if at least this share of its articles is from mapped cohorts
delay.min.pubyear  <- 0.9
# significance level for a shift (Wilcoxon rank-sum, Holm-adjusted across journals)
delay.alpha        <- 0.05
# Outputs
delay.articles.csv <- file.path(Outputs,"delay_articles.csv")
delay.cohorts.csv  <- file.path(Outputs,"delay_cohorts.csv")
delay.pubyears.csv <- file.path(Outputs,"delay_pubyears.csv")
delay.tests.csv    <- file.path(Outputs,"delay_tests.csv")
delay.by.year.csv  <- file.path(Outputs,"delay_by_year.csv")
