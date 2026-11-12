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

for (f in c("packages_by_period.csv","delay_articles.csv","delay_by_year.csv","delay_tests.csv",
            "delay_by_package.csv","delay_ks.csv","regressions_packages.csv","regressions_delay.csv")) {
  write_site(read_csv(file.path(outputs,f), show_col_types=FALSE), f)
}
