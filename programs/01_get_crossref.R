# Getting all articles published in AEA journals
# Code derived from aea-cumulative-summary/programs/11_get_crossref.R
# NOTE: THIS REQUIRES R 4.2.0 and newer packages!
#

source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)

library(rcrossref)

# filenames in config.R


# Each journal has a ISSN
if (!file.exists(issns.file)) {
  issns <- data.frame(matrix(ncol=3,nrow=18))
  names(issns) <- c("journal","issn","lastdate")
  tmp.date <- substr(firstpubday,1,7)
# Both print and online ISSNs are needed: individual works carry only one of them,
# and which one varies by journal and year. Duplicates are removed by DOI later.
issns[1,] <- c("American Economic Journal: Applied Economics","1945-7790",tmp.date)
issns[2,] <- c("American Economic Journal: Applied Economics","1945-7782",tmp.date)
issns[3,] <- c("American Economic Journal: Economic Policy","1945-774X",tmp.date)
issns[4,] <- c("American Economic Journal: Economic Policy","1945-7731",tmp.date)
issns[5,] <- c("American Economic Journal: Macroeconomics", "1945-7715",tmp.date)
issns[6,] <- c("American Economic Journal: Macroeconomics", "1945-7707",tmp.date)
issns[7,] <- c("American Economic Journal: Microeconomics", "1945-7685",tmp.date)
issns[8,] <- c("American Economic Journal: Microeconomics", "1945-7669",tmp.date)
issns[9,] <- c("The American Economic Review","1944-7981",tmp.date)
issns[10,] <- c("The American Economic Review","0002-8282",tmp.date)  # print ISSN is needed!
issns[11,] <- c("Journal of Economic Literature","2328-8175",tmp.date)
issns[12,] <- c("Journal of Economic Literature","0022-0515",tmp.date)
issns[13,] <- c("Journal of Economic Perspectives","1944-7965",tmp.date)
issns[14,] <- c("Journal of Economic Perspectives","0895-3309",tmp.date)
issns[15,] <- c("American Economic Review: Insights","2640-2068",tmp.date)
issns[16,] <- c("American Economic Review: Insights","2640-205X",tmp.date)
issns[17,] <- c("AEA Papers and Proceedings","2574-0768",tmp.date)  # since 2018; earlier P&P are AER May issues
issns[18,] <- c("AEA Papers and Proceedings","2574-0776",tmp.date)

  saveRDS(issns %>% filter(!is.na(journal)), file= issns.file)
} else {
  warning(paste("Reusing file: ",issns.file))
}

issns <- readRDS(file = issns.file)

if ( download_crossref == TRUE ) {
  warning("Deleting existing raw Crossref file to force re-download.")
  if ( file.exists(crossref.raw.Rds) ) {
    file.remove(crossref.raw.Rds)
  }
}


if (file.exists(crossref.raw.Rds) ) {
  warning(paste("Re-using file:",crossref.raw.Rds))
} else{
  new.df <- NA
  for ( x in 1:nrow(issns) ) {
    message(paste0("Processing ",issns[x,"journal"]," (",issns[x,"issn"],")"))
    new <- cr_journals(issn=issns[x,"issn"], works=TRUE,
                       filter=c(from_pub_date=issns[x,"lastdate"]),
                       select=c("DOI","title","published-print","issued","volume","issue","page",
                                "container-title","author","type","reference"),
                       .progress="text",
                       cursor = "*",
                       # default cursor_max is 5000, which truncates AER
                       cursor_max = 100000)
    if ( x == 1 ) {
      new.df <- as.data.frame(new$data)
      new.df$issn = issns[x,"issn"]
    } else {
      tmp.df <- as.data.frame(new$data)
      if ( nrow(tmp.df) > 0 ) {
        tmp.df$issn = issns[x,"issn"]
        new.df <- bind_rows(new.df,tmp.df)
      } else {
        warning(paste0("Did not find records for ISSN=",issns[x,"issn"]))
      }
      rm(tmp.df)
    }
  }
  # AEA articles cite their own replication package in the references.
  # Keep only references to openICPSR deposits (10.3886/E...), then drop the
  # (large) reference column.
  icpsr.refs <- new.df %>%
    select(doi,reference) %>%
    distinct(doi,.keep_all=TRUE) %>%
    mutate(reference = map(reference, function(r) {
      if ( !is.data.frame(r) || nrow(r)==0 ) return(NULL)
      tibble(ref_doi          = if ("DOI" %in% names(r)) r$DOI else NA_character_,
             ref_unstructured = if ("unstructured" %in% names(r)) r$unstructured else NA_character_)
    })) %>%
    unnest(reference) %>%
    mutate(icpsr_doi = tolower(coalesce(str_extract(ref_doi,regex("10\\.3886/e\\d+[^\\s]*",ignore_case=TRUE)),
                                        str_extract(ref_unstructured,regex("10\\.3886/e\\d+[^\\s]*",ignore_case=TRUE))))) %>%
    filter(!is.na(icpsr_doi)) %>%
    mutate(doi = tolower(doi))
  nrow(icpsr.refs)
  saveRDS(icpsr.refs, file= icpsr.refs.Rds)

  new.df <- new.df %>% select(-reference)
  saveRDS(new.df, file= crossref.raw.Rds)
  rm(new)
}
