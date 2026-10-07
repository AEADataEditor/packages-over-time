# Replication packages in AEA journals over time

Counts, by year of print publication, the research articles published in AEA journals and the number of those articles that have a replication package in the [AEA Data and Code Repository](https://www.openicpsr.org/openicpsr/aea) at openICPSR. Figures show the article counts with a replication package, with the total number of research articles as a faint benchmark, overall and by journal.

Code patterns are derived from [aea-cumulative-summary](https://github.com/AEADataEditor/aea-cumulative-summary); historical supplement data from [aea-supplement-migration](https://github.com/AEADataEditor/aea-supplement-migration).

## Outputs

| File | Content |
|------|---------|
| `outputs/figures/figure_packages_by_year.{png,pdf}` | Articles with a replication package, by year, against all research articles |
| `outputs/figures/figure_packages_by_journal.{png,pdf}` | Same, by journal (vertical scales differ across panels) |
| `outputs/packages_by_year.csv` | `year`, `articles`, `articles_with_package`, `packages`, `pct_with_package` |
| `outputs/packages_by_journal_year.csv` | Same, by `Journal` and `year` |
| `outputs/packages_by_article.csv` | One row per package: article `doi`, openICPSR `package`, `source`, `year`, `Journal` |
| `data/crossref/crossref_dois.csv` | The research articles (the denominator) |

`articles_with_package` is what the figures plot. `packages` counts distinct openICPSR projects and can be slightly higher, because a few articles have several (for example, a comment and its reply, or a supplement re-deposited later). Figures cover `firstyear` through `lastyear` (the last complete year), set in `programs/config.R`; the CSV files include the current, partial year.

## Steps

All programs are in `programs/` and are run in order by `programs/main.sh`.

1. `01_get_crossref.R`: downloads all works since 2000 from Crossref for each journal, using both print and online ISSNs (individual works carry only one of them). Also extracts, from each article's reference list, citations of openICPSR deposits (`10.3886/E...`).
2. `02_get_migration.R`: downloads the mapping of historical supplements (formerly ZIP files on the AEA website) to openICPSR, from `aea-supplement-migration` (October 2019 batch and September 2021 final batch).
3. `03_get_datacite.R`: queries DataCite for openICPSR deposits (a) with a related identifier pointing to an AEA DOI and (b) with a title following the AEA template ("Data and Code for: ...", "Replication data for: ...").
4. `04_combine.R`: filters Crossref to research articles and matches packages to articles (see below); tabulates by year and by journal and year.
5. `05_figures.R`: creates the figures.

### Research articles (denominator)

From the Crossref records, the following are removed:

- records that are not of type `journal-article` (issue records)
- front matter (DOIs ending in `i`)
- records without authors (front matter, book review sections, committee reports, journal mastheads, ...)
- JEL book reviews (DOIs ending in `.r1`, `.r2`, ...)
- editorial and administrative content with authors, identified by title: forewords, editors' introductions and notes, reports of the editors, committees, treasurer, secretary, and Data Editor, minutes, auditor reports, corrigenda and errata, "Recommendations for Further Reading", and similar (the full list of patterns is in `04_combine.R`)

Comments and replies are kept. Before 2018, the Papers and Proceedings were the May issue of the AER, and are counted as AER; since 2018, they are counted as AEA P&P.

### Replication packages (numerator)

A package is an openICPSR project (all versions and file-level DOIs of `10.3886/E123456` count as one). A package is matched to an article by any of:

- `migration`: the migration of historical supplements (from `aea-supplement-migration`)
- `datacite`: the DataCite record of the deposit lists the article DOI as `IsSupplementTo` or `IsDocumentedBy`. ICPSR studies that AEA articles merely cite (`IsCitedBy`) are not counted. This link is missing for most deposits after about 2020.
- `reference`: the article's Crossref reference list cites the deposit, and the citation text contains the article's own title (the AEA requires that articles cite their replication package; articles may also cite other deposits as data sources)
- `title`: the deposit's title is "Data and Code for: *article title*" (or a similar template), the normalized title matches exactly one AEA research article, and the deposit was published no more than 3 years before or 2 years after the article

The `source` column in `outputs/packages_by_article.csv` records which methods matched each package.

## Running in Docker

The `Dockerfile` extends `rocker/tidyverse` and runs the project's own package initialization (`global-libraries.R`, `programs/libraries.R`). The image and tag are set in `.myconfig.sh`.

```bash
bash build.sh 2026-10-07       # build larsvilhuber/packages-over-time:2026-10-07
DOCKEREXTRA="--user $(id -u):$(id -g) -e HOME=/tmp" bash run_docker.sh 2026-10-07 -c \
  'cd /home/rstudio/packages-over-time/programs && bash main.sh'
```

Set `download_crossref`, `download_migration`, and `download_datacite` in `programs/config.R` to `FALSE` to reuse previously downloaded data. A full run takes about 10 minutes, mostly for the Crossref download.
