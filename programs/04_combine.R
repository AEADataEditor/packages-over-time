# Combine articles (Crossref) with replication packages (migration + DataCite)
#

source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)

# filenames in config.R

# ===========================
# Articles: filter Crossref to research articles
# ===========================

new.df <- readRDS(crossref.raw.Rds)
nrow(new.df)

# Front matter and editorial/administrative content that is not a research article.
# Most of these have no authors; the title patterns catch the ones that do.
editorial.titles <- c("^front matter$",
                      "^book reviews?$",
                      "^foreword$",
                      "^editors?['’]?s?['’]? (introduction|note)",
                      "^report (of|by) ",
                      ": report of ",
                      "\\(aeastat\\).*annual report",
                      "^minutes of ",
                      "^recommendations for further reading$",
                      "universal academic questionnaire",
                      "^general information on the association",
                      "^list of online reports",
                      "^annotated listing",
                      "^jel classification",
                      "independent auditor",
                      "^(american economic association )?committee on ",
                      "volume \\d+,? number \\d+",
                      "forthcoming",
                      "corrigendum",
                      "^erratum",
                      "^correction to ",
                      "^abstracts$",
                      "^certificate of incorporation",
                      "data and code availability policy",
                      "^note from the aea")

new.df %>%
  distinct(doi,.keep_all=TRUE) %>%
  mutate(doi = tolower(doi),
         title.clean = str_squish(str_remove_all(title,"<[^>]+>")),
         no.author   = map_lgl(author, ~ is.null(.x) || length(.x)==0 ||
                                         (is.data.frame(.x) && nrow(.x)==0)),
         editorial   = str_detect(title.clean,
                                  regex(paste(editorial.titles,collapse="|"),ignore_case=TRUE)),
         # JEL book reviews have DOIs ending in .r1, .r2, ...
         bookreview  = str_detect(doi,"\\.r\\d+$"),
         # frontmatter DOIs end in "i"
         frontmatter = str_sub(doi, start= -1)=="i") -> crossref.df

# How many are dropped by each criterion (not mutually exclusive)?
crossref.df %>%
  summarize(total=n(),
            not_article=sum(type!="journal-article"),
            frontmatter=sum(frontmatter),
            no.author=sum(no.author),
            editorial=sum(editorial),
            bookreview=sum(bookreview))

crossref.df %>%
  filter(type=="journal-article") %>%
  filter(!frontmatter) %>%
  filter(!no.author) %>%
  filter(!editorial) %>%
  filter(!bookreview) %>%
  filter(!is.na(title)) %>%
  mutate(year = as.integer(substr(published.print,1,4)),
         # replace full journal name by short name
         Journal = str_replace_all(container.title,
                                   c("^The "=""," +"=" ",
                                     "American Economic Journal: ?"="AEJ:",
                                     "American Economic Review: ?Insights"="AER:Insights",
                                     "American Economic Review"="AER",
                                     "Journal of Economic Literature"="JEL",
                                     "Journal of Economic Perspectives"="JEP",
                                     "AEA Papers and Proceedings"="AEA P&P")),
         Journal = if_else(Journal=="AEJ:Macroeconomics","AEJ:Macro",Journal),
         Journal = if_else(Journal=="AEJ:Microeconomics","AEJ:Micro",Journal)) %>%
  filter(year >= firstyear) %>%
  select(doi,title,year,published.print,Journal,container.title,volume,issue,page) -> articles

nrow(articles)
articles %>% count(Journal)

saveRDS(articles, file= doi.file.Rds)
write_csv(articles, file= doi.file.csv)

# ===========================
# Packages
# ===========================

# Historical supplements migrated to openICPSR
migration <- readRDS(file.path(migrationloc,"migration_packages.Rds")) %>%
  transmute(doi,
            package = coalesce(str_extract(icpsr_doi,"^10\\.3886/e\\d+"),paste0("migration:",doi)),
            source  = "migration")

# Deposits at openICPSR linked to AEA articles (includes most of the migrated ones)
datacite <- read_csv(datacite.csv) %>%
  filter(relationType %in% c("IsSupplementTo","IsDocumentedBy")) %>%
  filter(!is.na(icpsr_project)) %>%
  transmute(doi=relatedIdentifier, package=icpsr_project, source="datacite") %>%
  distinct()

# Normalized titles, for matching
normalize_title <- function(x) {
  x %>%
    str_remove_all("<[^>]+>") %>%
    iconv(to="ASCII//TRANSLIT") %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]+"," ") %>%
    str_squish()
}

# Article titles that are long enough and unique enough to be matched on
articles.titles <- articles %>%
  mutate(title.norm = normalize_title(title)) %>%
  add_count(title.norm,name="n.title") %>%
  filter(n.title == 1, nchar(title.norm) >= 20) %>%
  select(doi,year,title.norm)

# Articles cite their own replication package in their references.
# Keep references to openICPSR deposits whose text contains the article's own title
# (articles may also cite other deposits, e.g., data from another paper).
references <- readRDS(icpsr.refs.Rds) %>%
  inner_join(articles.titles,by="doi") %>%
  mutate(ref.norm = normalize_title(ref_unstructured)) %>%
  filter(str_detect(ref.norm,fixed(str_sub(title.norm,1,60)))) %>%
  transmute(doi, package=str_extract(icpsr_doi,"^10\\.3886/e\\d+"), source="reference") %>%
  distinct()

# Deposits titled "Data and Code for: <article title>" (and similar), matched on
# the normalized title, and deposited no earlier than 3 years before publication
titles <- read_csv(datacite.titles.csv) %>%
  mutate(title.norm = normalize_title(article_title)) %>%
  distinct(icpsr_project,title.norm,publicationYear) %>%
  inner_join(articles.titles,by="title.norm",relationship="many-to-many") %>%
  filter(is.na(publicationYear) | between(publicationYear-year,-3,2)) %>%
  transmute(doi, package=icpsr_project, source="title") %>%
  distinct()

all.packages <- bind_rows(migration,datacite,references,titles) %>%
  group_by(doi,package) %>%
  summarize(source = paste(sort(unique(source)),collapse="+"), .groups="drop")

all.packages %>% count(source,sort=TRUE)

# Attach article information. A package linked to several articles (rare)
# is counted once, with its earliest article.
packages <- all.packages %>%
  inner_join(articles %>% select(doi,year,Journal),by="doi") %>%
  arrange(package,year,doi) %>%
  distinct(package,.keep_all=TRUE)

# How many package-article links do not match a research article in scope?
all.packages %>% anti_join(articles,by="doi") %>% nrow()
nrow(packages)

saveRDS(packages,file=packages.Rds)
write_csv(packages,file=packages.csv)

# ===========================
# Tabulate by year and by journal x year
# ===========================

articles.flagged <- articles %>%
  left_join(packages %>% count(doi,name="n_packages"),by="doi") %>%
  mutate(n_packages = coalesce(n_packages,0L))

tabulate <- function(df, ...) {
  df %>%
    group_by(...) %>%
    summarize(articles              = n(),
              articles_with_package = sum(n_packages>0),
              packages              = sum(n_packages),   # distinct openICPSR projects
              .groups = "drop") %>%
    mutate(pct_with_package = round(100*articles_with_package/articles,1))
}

by.year    <- tabulate(articles.flagged, year)
by.journal <- tabulate(articles.flagged, Journal, year)

by.year %>% print(n=Inf)

write_csv(by.year,   file=by_year.csv)
write_csv(by.journal,file=by_journal.csv)
