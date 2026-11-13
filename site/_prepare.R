# Pre-render step for the Quarto site: reads the pipeline outputs (../outputs) and writes the
# small files the pages read into data/. Runs on GitHub Actions; the analysis is not re-run here.
suppressMessages({library(readr); library(dplyr)})

outputs <- file.path("..","outputs")
dir.create("data", showWarnings=FALSE)
lastyear <- as.integer(format(Sys.Date(),"%Y")) - 1

articles <- read_csv(file.path(outputs,"articles_policy.csv"), show_col_types=FALSE) %>%
  filter(year <= lastyear)

# Main analysis: AER, AER:Insights, AEJs. The P&P, JEL and JEP are on the Appendix page.
# (Same lists as main.journals / main.label in programs/config.R.)
main.journals     <- c("AER","AER:Insights","AEJ:Applied Economics","AEJ:Economic Policy","AEJ:Macro","AEJ:Micro")
appendix.journals <- c("AEA P&P","JEL","JEP")
main.label        <- "Main journals"

# share with a package by journal (P&P re-attributed) and year, plus the main journals pooled
share <- bind_rows(articles,
                   articles %>% filter(Journal %in% main.journals) %>% mutate(Journal=main.label, policy_year=NA)) %>%
  group_by(Journal, year) %>%
  summarize(articles=n(), with_package=sum(has_package), share=mean(has_package),
            policy_year=first(policy_year), .groups="drop")

# share by policy period, for each journal and the main journals pooled
period <- function(y) cut(y, breaks=c(-Inf,2004,2008,2018,Inf),
                          labels=c("1999-2004","2005-2008","2009-2018","2019 and later"))
by.period <- bind_rows(articles,
                       articles %>% filter(Journal %in% main.journals) %>% mutate(Journal=main.label)) %>%
  mutate(period=period(year)) %>%
  group_by(group=Journal, period) %>%
  summarize(first_year=min(year), last_year=max(year), articles=n(),
            articles_with_package=sum(has_package), share=mean(has_package), .groups="drop") %>%
  mutate(appendix = group %in% appendix.journals)
# for d3.autoType in the pages: missing values as empty cells, logicals as true/false
write_site <- function(df, f) {
  df %>% mutate(across(where(is.logical), ~ ifelse(is.na(.x), NA, tolower(as.character(.x))))) %>%
    write_csv(file.path("data", f), na="")
}

write_site(share, "share_by_journal_year.csv")
write_site(by.period, "packages_by_period_site.csv")

for (f in c("time_to_pub_articles.csv","time_to_pub_by_year.csv","time_to_pub_tests.csv",
            "time_to_pub_by_package.csv","time_to_pub_ks.csv","regressions_packages.csv","regressions_time_to_pub.csv","event_study.csv")) {
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
         main_journal    = Journal %in% main.journals,
         # pooled main analysis (main journals); the P&P, JEL and JEP enter the appendix regressions
         in_pkg_regressions = complete_year & year >= regfirst) %>%
  select(doi, title, Journal, main_journal, Journal_published=Journal.published, year, volume, issue, page, pp_in_aer,
         policy_year, under_policy, has_package, n_packages, package_dois, package_sources,
         complete_year, in_pkg_regressions, in_ttp_sample, pub_month, sub_year, sub_quarter, months_to_pub) %>%
  # most recent first
  arrange(desc(year), desc(issue), Journal, doi) %>%
  write_site("articles.csv")

# Static figures from the pipeline shown on the pages
file.copy(file.path(outputs,"figures","figure_share_aer.png"), "data", overwrite=TRUE)
