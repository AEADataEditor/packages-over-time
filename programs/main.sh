#!/bin/bash

R CMD BATCH 01_get_crossref.R
R CMD BATCH 02_get_migration.R
R CMD BATCH 03_get_datacite.R
R CMD BATCH 04_combine.R
R CMD BATCH 05_figures.R
R CMD BATCH 06_delay.R
