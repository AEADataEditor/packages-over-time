# Pre-render step for the Quarto site: reads the pipeline outputs (../outputs) and writes the
# small files the pages read into data/. Runs on GitHub Actions; the analysis is not re-run here.
suppressMessages({library(readr); library(dplyr)})

outputs <- file.path("..","outputs")
dir.create("data", showWarnings=FALSE)
lastyear <- as.integer(format(Sys.Date(),"%Y")) - 1

articles <- read_csv(file.path(outputs,"articles_policy.csv"), show_col_types=FALSE) %>%
  filter(year <= lastyear)

# share with a package by journal (P&P re-attributed) and year, plus all journals
share <- bind_rows(articles, articles %>% mutate(Journal="All journals", policy_year=NA)) %>%
  group_by(Journal, year) %>%
  summarize(articles=n(), with_package=sum(has_package), share=mean(has_package),
            policy_year=first(policy_year), .groups="drop")
# for d3.autoType in the pages: missing values as empty cells, logicals as true/false
write_site <- function(df, f) {
  df %>% mutate(across(where(is.logical), ~ ifelse(is.na(.x), NA, tolower(as.character(.x))))) %>%
    write_csv(file.path("data", f), na="")
}

write_site(share, "share_by_journal_year.csv")

for (f in c("packages_by_period.csv","time_to_pub_articles.csv","time_to_pub_by_year.csv","time_to_pub_tests.csv",
            "time_to_pub_by_package.csv","time_to_pub_ks.csv","regressions_packages.csv","regressions_time_to_pub.csv")) {
  write_site(read_csv(file.path(outputs,f), show_col_types=FALSE), f)
}

# Article-level dataset for the Data page: every article, its flags, and the DOIs of its replication packages
pkgs <- read_csv(file.path(outputs,"packages_by_article.csv"), show_col_types=FALSE) %>%
  group_by(doi) %>%
  summarize(package_dois=paste(package, collapse=" "),
            package_sources=paste(unique(unlist(strsplit(source, "+", fixed=TRUE))), collapse="+"), .groups="drop")
ttp <- read_csv(file.path(outputs,"time_to_pub_articles.csv"), show_col_types=FALSE) %>%
  transmute(doi, pub_month, sub_year, sub_quarter, months_to_pub, in_ttp_sample=TRUE)
regfirst <- min(read_csv(file.path(outputs,"regressions_packages.csv"), show_col_types=FALSE)$first_year)
read_csv(file.path(outputs,"articles_policy.csv"), show_col_types=FALSE) %>%
  left_join(pkgs, by="doi") %>%
  left_join(ttp, by="doi") %>%
  mutate(complete_year   = year <= lastyear,
         in_ttp_sample   = coalesce(in_ttp_sample, FALSE),
         in_pkg_regressions = complete_year & year >= regfirst) %>%
  select(doi, title, Journal, Journal_published=Journal.published, year, volume, issue, page, pp_in_aer,
         policy_year, under_policy, has_package, n_packages, package_dois, package_sources,
         complete_year, in_pkg_regressions, in_ttp_sample, pub_month, sub_year, sub_quarter, months_to_pub) %>%
  arrange(Journal, year, doi) %>%
  write_site("articles.csv")
