# Custom image for the AEA packages-over-time analysis
# Adds the R packages used by programs/*.R
# Build with: ./build.sh (tag)

FROM rocker/tidyverse:4.4.2

# R packages: run the project's own package initialization
# The CRAN repo is the dated snapshot configured by the rocker base image
COPY global-libraries.R /tmp/rsetup/
COPY programs/libraries.R /tmp/rsetup/programs/
RUN cd /tmp/rsetup \
    && Rscript -e 'source("global-libraries.R"); source("programs/libraries.R")' \
    && rm -rf /tmp/rsetup /tmp/downloaded_packages
