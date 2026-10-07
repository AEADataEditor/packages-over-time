# Getting openICPSR deposits that are AEA replication packages, from DataCite
# Two complementary queries:
#  1. deposits with a relatedIdentifier pointing to an AEA article DOI
#     (set for the migrated supplements and for deposits up to about 2020)
#  2. deposits whose title follows the AEA template ("Data and Code for: <article title>",
#     "Replication data for: ...", ...). Recent deposits often lack the link in (1);
#     they are matched to articles by title in 04_combine.R
#

source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)

# filenames in config.R

# cursor-based pagination through all matching records
fetch_datacite <- function(query) {
  url <- modify_url(datacite.api,
                    query=list(`client-id`=datacite.client,
                               query=query,
                               `page[size]`=1000,
                               `page[cursor]`=1,
                               `fields[dois]`="doi,titles,publicationYear,registered,relatedIdentifiers"))
  pages <- list()
  while ( !is.null(url) ) {
    message(paste0("Fetching page ",length(pages)+1))
    resp <- RETRY("GET",url,times=5)
    stop_for_status(resp)
    js <- fromJSON(content(resp,as="text",encoding="UTF-8"),simplifyVector=FALSE)
    if ( length(js$data) == 0 ) break
    pages[[length(pages)+1]] <- js$data
    url <- js$links[["next"]]
  }
  records <- unlist(pages,recursive=FALSE)
  message(paste0("Retrieved ",length(records)," records"))
  records
}

if ( download_datacite == TRUE ) {
  warning("Deleting existing DataCite files to force re-download.")
  for ( f in c(datacite.raw.Rds,datacite.titles.raw.Rds) ) {
    if ( file.exists(f) ) file.remove(f)
  }
}

if (file.exists(datacite.raw.Rds) ) {
  warning(paste("Re-using file:",datacite.raw.Rds))
} else {
  records <- fetch_datacite(paste0("relatedIdentifiers.relatedIdentifier:",doi_prefix,"*"))
  saveRDS(records,file=datacite.raw.Rds)
}

if (file.exists(datacite.titles.raw.Rds) ) {
  warning(paste("Re-using file:",datacite.titles.raw.Rds))
} else {
  records <- fetch_datacite(paste(paste0('titles.title:"',datacite.title.templates,'"'),collapse=" OR "))
  saveRDS(records,file=datacite.titles.raw.Rds)
}

# Deposits are registered at several levels: the project (10.3886/E123456),
# its versions (10.3886/E123456V1, V2, ...), and sometimes individual files
# (10.3886/E123456V1-98765). A package is the project, regardless of version.
add_level <- function(df) {
  df %>%
    mutate(level = case_when(str_detect(datacite_doi,"^10\\.3886/e\\d+$")        ~ "project",
                             str_detect(datacite_doi,"^10\\.3886/e\\d+v\\d+$")   ~ "version",
                             TRUE                                                ~ "other"),
           icpsr_project = str_extract(datacite_doi,"^10\\.3886/e\\d+"))
}

# --- (1) one row per (deposit DOI, related AEA DOI)

records <- readRDS(datacite.raw.Rds)

datacite.links <- map_dfr(records, function(r) {
  rel <- r$attributes$relatedIdentifiers
  if ( length(rel) == 0 ) return(NULL)
  tibble(datacite_doi      = tolower(r$attributes$doi),
         publicationYear   = as.integer(r$attributes$publicationYear %||% NA),
         registered        = r$attributes$registered %||% NA_character_,
         relationType      = map_chr(rel, ~ .x$relationType %||% NA_character_),
         relatedIdentifier = map_chr(rel, ~ .x$relatedIdentifier %||% NA_character_))
}) %>%
  mutate(relatedIdentifier = tolower(str_remove(relatedIdentifier,"^https?://(dx\\.)?doi\\.org/"))) %>%
  filter(str_starts(relatedIdentifier,paste0(doi_prefix,"/"))) %>%
  add_level()

# what relations are there? IsCitedBy are ICPSR studies (10.3886/ICPSR...)
# used as data by AEA articles, not replication packages
datacite.links %>% count(relationType,sort=TRUE)
datacite.links %>% count(level)

write_csv(datacite.links,file=datacite.csv)

# --- (2) one row per deposit DOI with an AEA-template title

records <- readRDS(datacite.titles.raw.Rds)

datacite.titles <- map_dfr(records, function(r) {
  tibble(datacite_doi    = tolower(r$attributes$doi),
         publicationYear = as.integer(r$attributes$publicationYear %||% NA),
         title           = r$attributes$titles[[1]]$title %||% NA_character_)
}) %>%
  add_level() %>%
  filter(level != "other") %>%
  # the article title is whatever follows the template prefix
  mutate(article_title = str_remove(title,regex(paste0("^.*?(",paste(datacite.title.templates,collapse="|"),")\\s*:?\\s*"),
                                                ignore_case=TRUE)))

datacite.titles %>% count(level)

write_csv(datacite.titles,file=datacite.titles.csv)
