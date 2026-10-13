# AI notes: submission-to-publication delay

Notes from a Claude Code session (October 2026) that added `programs/06_delay.R`. They record the design choices, the results, and the open questions. The README describes how to run things and what each output is.

## Question

Does the delay between submission and publication shift after the 2019 AEA-wide data and code availability policy (Data Editor review)?

## Data and method

### Submission date from the DOI

- Since 2015 (AER and the four AEJs), and from its start in 2019 (AER:Insights), DOIs are based on the manuscript number. For example, `10.1257/aer.20151234` was submitted in 2015 as manuscript 1234 of that year.
- For these six journals the counter restarts every year. AER numbers run up to about 1,700–2,100 a year, AEJ numbers to about 350–900, and AER:Insights numbers to about 550–830.
- The sequence number has no date. Within each journal and submission year, the quartiles of the sequence numbers of all published manuscripts define the submission quarter, which is dated at its midpoint.
- Publication date: the month of the print issue. Crossref print dates are always the first of the month. The delay is precise to about ±1.5 months.

### Excluded journals

- **AEA P&P** (`pandp.2018NNNN`): numbered by conference program (1000, 1001, …). Delay is essentially zero.
- **JEL and JEP**: the counter does not restart each year (JEL numbers run 680–1,830 across years and overlap between years). Papers are commissioned.
- **Malformed DOIs** such as `aer.15000001`, and the few AER articles after 2015 that still have volume-based DOIs.

### Which years can be used

The quartile mapping needs (nearly) all published manuscripts of a submission year. A submission year is incomplete in two cases:

- **Left side:** some of its papers were published before 2015 with volume-based DOIs, so they are invisible.
- **Right side:** its slow papers are not published yet.

`06_delay.R` handles this in three steps:

1. **Lag distribution.** It estimates the distribution of publication year minus submission year (pooled at 7+ years) from submission years observed for all lags.
2. **Coverage.** Each submission year's coverage is the probability mass of the lags at which its papers can be observed. Submission years with coverage of at least 80% and at least 20 articles are mapped (`delay_cohorts.csv`).
3. **Publication years.** A publication year is analysed only if at least 90% of its articles come from mapped submission years (`delay_pubyears.csv`). Without this filter, early publication years would contain only fast papers.

This leaves:

| Journal | Publication years analysed |
|---|---|
| AER | 2018–2023 |
| AEJ:Applied | 2017–2024 |
| AEJ:Policy | 2016–2024 |
| AEJ:Macro | 2016–2023 |
| AEJ:Micro | 2016–2024 |
| AER:Insights | 2019–2024 |

Only complete years are used, so nothing from 2026 is included.

### Before and after

- **Definition:** by year of publication, before 2019 vs. 2019 and later, matching the 2019 line used elsewhere in the repo. A 2020 cutoff is the robustness check, since papers accepted after the Data Editor started in mid-2019 are mostly published from 2020 on.
- **Tests:**
  - Wilcoxon rank-sum test with the Hodges–Lehmann shift and its 95% CI.
  - Kolmogorov–Smirnov test.
  - p-values are Holm-adjusted across journals.
  - Difference in means with a Welch 95% CI.
- **Why the means are reported:** AEJ delays fall on a 3-month grid (quarter midpoints and quarterly issues). The Hodges–Lehmann shift and its CI are therefore multiples of 3 months and can look off-center, e.g. a shift of +6 with CI [3, 6]. The difference in means is a continuous alternative, and the figure labels show both.

### Figures

- **Color choice:** using color to show significance only works if the hue keeps meaning the period. Before is always blue (`#2F6DB5`) and after is always orange (`#C2611F`); this pair passes the colorblind and contrast checks of the dataviz palette validator. When the shift is not significant, both periods are drawn in muted tints, and every panel also states the result in text.
- **Densities:** all use one bandwidth (4 months), so the curves are comparable. AER:Insights was spiky with its own default bandwidth.
- **Per-journal figures:** each journal in color, all other journals in grey, with before on top and after below. A dashed line repeats the journal's pre-2019 median in both panels.

## Findings

### Before and after (`delay_tests.csv`)

| Journal | Before (n) | After (n) | Median shift (months) | Mean difference (months) [95% CI] | Wilcoxon p (Holm) |
|---|---|---|---|---|---|
| AER | 2018 (106) | 2019–23 (552) | −1 | −0.3 [−2.8, 2.2] | 0.48 |
| AEJ:Applied | 2017–18 (85) | 2019–24 (301) | +6 | +5.8 [4.0, 7.5] | <0.001 |
| AEJ:Policy | 2016–18 (136) | 2019–24 (348) | +3 | +3.8 [2.0, 5.5] | <0.001 |
| AEJ:Macro | 2016–18 (91) | 2019–23 (230) | +6 | +6.9 [4.4, 9.3] | <0.001 |
| AEJ:Micro | 2016–18 (125) | 2019–24 (322) | +6 | +6.2 [4.5, 7.8] | <0.001 |
| AER:Insights | none | 2019–24 (186) | — | — | — |

- **With a 2020 cutoff:** the AEJ mean differences are +4.3 to +7.2 months, and the AER shows no change.
- **Typical delays:** median delay is about 2.1–2.4 years for the AER and AEJs, and 1.3 years for AER:Insights.

### Mean delay by year (`delay_by_year.csv`)

The by-year diagnostic changes how the before/after comparison should be read.

- **By publication year:** AEJ means rise gradually from 2016–17 and peak in 2021–22, with a partial decline in 2023. There is no step at 2019. AEJ:Applied (1.6 to 2.0 years) and AEJ:Micro (1.9 to 2.3 years) already jump between 2017 and 2018, before the policy.
- **By submission year:**
  - **AEJ:Applied and AEJ:Micro:** a jump of about 6–9 months between the 2016 and 2017 submissions, flat afterwards. Papers submitted in 2017 were mostly accepted in 2018–19, which straddles the Data Editor's start.
  - **AEJ:Macro:** a gradual rise starting with the 2014 submissions.
  - **AEJ:Economic Policy:** flat at about 2.5 years for every submission year, even though its publication-year mean rises.
  - **AER:** flat or slightly falling. **AER:Insights:** flat at about 1.3–1.4 years.
- **Publication volume:** AEJ:Micro published 82 papers in 2022, against 33–56 before, and AEJ:Applied 58. A backlog cleared in 2021–22 would raise publication-year delays without the handling process getting slower. COVID falls in the same window.

**Reading:** AEJ delays rose over 2017–2022. That fits several explanations: Data Editor review, a backlog in publication, COVID, and changes at individual journals (editors, process). These data cannot tell them apart. The submission-year view puts the AEJ:Applied and AEJ:Micro jump at the 2017 submissions, earlier than a pure 2019 effect would predict. The AER, which had a data policy since 2005, shows no change.

## Caveats and open items

- **Residual truncation in the "before" years.** These are the earliest years that pass the 90% filter. In them, 2–10% of papers come from unmapped submission years, and those papers are the slowest. This biases the "before" means down and the shift up, probably by a month or two, not the whole 4–7 months. Not yet done: rerun with `delay.min.pubyear` at 0.97 or higher.
- **Print date vs. online date.** Print dates include the queue for an issue slot. Crossref may have online or accepted dates; these are not downloaded by `01_get_crossref.R`. Submission-to-acceptance would isolate the Data Editor stage better.
- **Quarter mapping assumes steady submissions.** It assumes submissions, and the share eventually published, are roughly constant within a year.
- **AER before-period.** It is only one year (2018), because earlier AER submission years cannot be mapped.
- **Not causal.** These are before/after comparisons without a control group. AER:Insights has no before-period. JEL, JEP and the P&P cannot be measured this way.

## Possible next steps

- **Website.** An interactive selector by journal, as originally suggested. The per-journal static figures already follow that layout (one journal in color, the others in grey).
- **Stricter coverage threshold.** Rerun with a stricter publication-year threshold (see caveats).
- **Delay by package.** Split the delay by whether the paper has a replication package (`has_package` is in `delay_articles.csv`).
- **Online and acceptance dates.** Download online or acceptance dates from Crossref, if available, to separate review time from production time.
