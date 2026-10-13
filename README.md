# Replication packages in AEA journals over time

Counts, by year of print publication, the research articles in AEA journals and how many have a replication package in the [AEA Data and Code Repository](https://www.openicpsr.org/openicpsr/aea) at openICPSR. This shows how replication-package coverage has grown over time, overall and by journal.

## How it works

`programs/main.sh` runs, in order:

1. `01_get_crossref.R`: all AEA journal works since 1999 from Crossref (DOIs were introduced in 1997; Crossref has AER and JEL from 1999) (the denominator), plus citations of openICPSR deposits in reference lists.
2. `02_get_migration.R`: the mapping of historical supplements to openICPSR.
3. `03_get_datacite.R`: openICPSR deposits in DataCite that link to AEA articles or follow the AEA title template.
4. `04_combine.R`: restricts Crossref to research articles (dropping front matter, book reviews, and editorial/administrative items), matches packages to articles by migration mapping, DataCite link, reference list, or title, and tabulates by year and journal.
5. `05_figures.R`: the figures.
6. `06_delay.R`: the delay between submission and publication, from manuscript-number DOIs (see below).

Settings (first and last year, whether to re-download) are in `programs/config.R`.

## Research articles (denominator)

From the Crossref records, the following are removed:

- records that are not of type `journal-article` (issue records)
- front matter (DOIs ending in `i`)
- records without authors (front matter, book review sections, committee reports, journal mastheads, ...)
- JEL book reviews (DOIs ending in `.r1`, `.r2`, ...)
- editorial and administrative content with authors, identified by title: forewords, editors' introductions and notes, reports of the editors, committees, treasurer, secretary, and Data Editor, minutes, auditor reports, corrigenda and errata, "Recommendations for Further Reading", and similar (the full list of patterns is in `04_combine.R`)

Comments and replies are kept. Before 2018, the Papers and Proceedings were the May issue of the AER, and are counted as AER; since 2018, they are counted as AEA P&P.

## Replication packages (numerator)

A package is an openICPSR project (all versions and file-level DOIs of `10.3886/E123456` count as one). A package is matched to an article by any of:

- `migration`: the migration of historical supplements (from `aea-supplement-migration`)
- `datacite`: the DataCite record of the deposit lists the article DOI as `IsSupplementTo` or `IsDocumentedBy`. ICPSR studies that AEA articles merely cite (`IsCitedBy`) are not counted. This link is missing for most deposits after about 2020.
- `reference`: the article's Crossref reference list cites the deposit, and the citation text contains the article's own title (the AEA requires that articles cite their replication package; articles may also cite other deposits as data sources)
- `title`: the deposit's title is "Data and Code for: *article title*" (or a similar template), the normalized title matches exactly one AEA research article, and the deposit was published no more than 3 years before or 2 years after the article

The `source` column in `outputs/packages_by_article.csv` records which methods matched each package.

`articles_with_package` is what the figures plot. `packages` counts distinct openICPSR projects and can be slightly higher, because a few articles have several (for example, a comment and its reply, or a supplement re-deposited later). Figures cover `firstyear` through `lastyear` (the last complete year); the CSV files include the current, partial year.

## Outputs

- `outputs/figures/`: articles with a replication package by year, against all research articles; and AER/AEA P&P and the other journals in separate figures.
- `outputs/figures/figure_packages_aer_pp.*`, `figure_packages_other_journals.*`: the same, with the AEA Papers & Proceedings articles that were printed in the May AER (through 2017) attributed to the P&P (hatched). `figure_packages_aer_pp_asis.*` shows AER and P&P without the re-attribution (the May issue stays in the AER). AER and P&P are in one figure, all other journals in another; panels within a figure share one vertical scale, with a red reference line (50 and 30 articles).
- `outputs/packages_by_journal_year_pp.csv`: counts by journal (P&P re-attributed), year, and `pp_in_aer`.
- `outputs/articles_policy.csv`: one row per article, with the journal (as published and with the P&P re-attributed), whether it has a package, the year the journal's data policy started (`policy_year`: AER 2005, AEJs 2009, all others 2019), `under_policy` (published in or after that year), and the policy `period`.
- `outputs/figures/figure_share_{aer,aej,others,pp}.*`: share of articles with a package by year, 1999 on, for the AER (without the May P&P), the AEJs, AER:Insights/JEL/JEP, and the P&P. Dashed lines mark the policy changes (2005 AER data policy, 2009 AEJs launched, 2019 AEA-wide policy); red lines are the pooled share in each of the four periods (`outputs/packages_by_period.csv`). The same dashed lines are on the count figures.
- `outputs/packages_by_year.csv`, `outputs/packages_by_journal_year.csv`: counts of articles, articles with a package, and packages.
- `outputs/packages_by_article.csv`: one row per package, with the matching method(s) in `source`.

## Delay from submission to publication

Since 2015 (AER, AEJs; AER:Insights from its start in 2019), DOIs are based on the manuscript number: `10.1257/aer.20151234` was submitted in 2015, as manuscript 1234 of that year (the counter restarts each year). The number has no date, but increases with submissions: within a submission year, the quartiles of the numbers of all eventually published manuscripts define the submission quarter, dated at its middle. Delay runs from there to the middle of the month of the print issue, so it is precise to about plus or minus 1.5 months.

The quartiles are only meaningful when (nearly) all published manuscripts of a submission year are observed. Submission years that also published before the switch to manuscript-number DOIs, or that are still being published, are not mapped: `outputs/delay_cohorts.csv` gives each submission year's estimated coverage (from the distribution of publication year minus submission year), and years with at least 80% are used. Only publication years with at least 90% of their articles from mapped submission years are analyzed (`outputs/delay_pubyears.csv`), so that long delays are not cut off. The AEA P&P (numbered by conference program), JEL, and JEP (commissioned; numbers not restarted each year) are excluded. Thresholds are in `programs/config.R`.

Before and after the 2019 AEA-wide policy is defined by year of publication. `outputs/delay_tests.csv` has, for each journal, the medians and means, the Hodges-Lehmann shift with its 95% confidence interval, the difference in means with a Welch 95% confidence interval, and Wilcoxon rank-sum and Kolmogorov-Smirnov p-values (raw and Holm-adjusted across journals), for 2019 and, as a robustness check, 2020 (papers accepted after the Data Editor started in mid-2019 are mostly published from 2020 on). AER:Insights has no articles before the policy. For the AEJs, delays fall on a grid of 3 months (quarter midpoints, quarterly issues), so the Hodges-Lehmann shift and its interval are multiples of 3 months and need not be centered; the difference in means is a continuous alternative, and the figures show both. Article-level delays are in `outputs/delay_articles.csv`.

- `outputs/figures/figure_delay_by_journal.*`: the delay distribution for each journal (points: articles; line: kernel density).
- `outputs/figures/figure_delay_policy.*`: before and after 2019, by journal. Blue: before; orange: after. Journals with no significant shift (Holm-adjusted Wilcoxon, 5%) are drawn in muted tints.
- `outputs/figures/figure_delay_by_pubyear.*`, `figure_delay_by_subyear.*`: diagnostic, mean delay (95% CI) by year of publication and by year of submission, each journal against the others in grey (`outputs/delay_by_year.csv`). Recent submission years miss their slowest papers (hollow points).
- `outputs/figures/figure_delay_{aer,aer_insights,aej_*}.*`: one figure per journal, the journal in color against all other journals in grey, before (top) and after (bottom) the policy.

## Running

```bash
bash build.sh 2026-10-07
DOCKEREXTRA="--user $(id -u):$(id -g) -e HOME=/tmp" bash run_docker.sh 2026-10-07 -c \
  'cd /home/rstudio/packages-over-time/programs && bash main.sh'
```

## Prior work

Builds on earlier work by Lars Vilhuber in [aea-cumulative-summary](https://github.com/AEADataEditor/aea-cumulative-summary) and [aea-supplement-migration](https://github.com/AEADataEditor/aea-supplement-migration).
