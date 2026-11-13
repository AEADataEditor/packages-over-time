# Regressions: did the 2019 AEA-wide data and code policy change
#   (a) the presence of replication packages (article level, linear probability model),
#   (b) the time from submission to publication (months, articles from 06_time_to_pub.R)?
#
# Main specification: event studies (year dummies relative to es.ref, with no trend, linear or quadratic
# pre-period journal trends, or rolling local trends; bootstrap over issues), written to es.csv.
# Robustness: the static specifications below, including the actual cutoff on a matched window.
#
# post is defined by year of print publication. Linear trends are in publication quarter
# (from the Crossref print date), centered on the first quarter of the cutoff year.
#
# Specifications (OLS; standard errors clustered by journal x publication year, CR1, t with G-1 df).
# For (a), each specification is also estimated as a probit; the reported effect is the average, over
# articles with the term = 1, of Phi(xb) - Phi(xb - b_term), with a cluster-robust delta-method SE.
#   its_fe     interrupted time series: y ~ post + journal fixed effects
#   its_trend  interrupted time series: y ~ post + journal fixed effects + journal-specific linear trends
#   did        (a) only. Difference in differences: journals without a data policy before 2019
#              (P&P, JEL, JEP) against the AER and AEJs, which had one since 2005 and 2009:
#              y ~ post x newly covered + journal fixed effects + year fixed effects
#   did_trend  (a) only. As did, with separate linear trends for the two groups
#   pkg        (b) only. y ~ post x has_package + journal fixed effects: does the change in time to publication
#              differ for articles with a replication package?
# Each specification is estimated for all journals pooled, and (its_fe, its_trend) for each journal.
#
# Placebo implementation years: reg.placebo.pre (estimated on years before reg.policy.year only) and
# reg.placebo.post (estimated on years from reg.policy.year on only). The actual policy break is never
# inside a placebo sample. Trends are centered on the cutoff, so that post is the level shift at the cutoff.

source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)

# ===========================
# OLS with cluster-robust (CR1) standard errors
# ===========================

ols_cluster <- function(formula, data, term, cluster) {
  fit <- lm(formula, data=data)
  b <- coef(fit)
  if (!term %in% names(b) || is.na(b[[term]])) return(NULL)
  X <- model.matrix(fit)[, !is.na(b), drop=FALSE]
  b <- b[!is.na(b)]
  u <- residuals(fit)
  cl <- data[[cluster]][as.integer(rownames(X))]
  G <- length(unique(cl)); n <- nrow(X); k <- ncol(X)
  if (G < 3) return(NULL)
  bread <- solve(crossprod(X))
  S <- rowsum(X*u, cl)
  V <- (G/(G-1))*((n-1)/(n-k)) * bread %*% crossprod(S) %*% bread
  se <- sqrt(V[term,term])
  tq <- qt(0.975, G-1)
  tibble(estimate=b[[term]], se=se, lo=b[[term]]-tq*se, hi=b[[term]]+tq*se,
         p=2*pt(-abs(b[[term]]/se), G-1), n=n, n_clusters=G)
}

# Probit with cluster-robust standard errors. Effect of term: average over articles with term = 1 of
# Phi(xb) - Phi(xb - b_term) (average effect on the treated, in probability units). Journals whose
# outcome does not vary in the sample are dropped (perfect prediction); returns NULL if the fit fails.
probit_cluster <- function(formula, data, term, cluster) {
  vary <- data %>% group_by(Journal) %>% filter(n_distinct(y) > 1) %>% ungroup()
  if (n_distinct(vary$Journal) == 1 && any(tapply(vary$y, vary$post, function(v) length(unique(v))) < 2))
    return(NULL)                        # single journal: outcome constant before or after
  if (nrow(vary) == 0) return(NULL)
  fit <- tryCatch(suppressWarnings(glm(formula, family=binomial(link="probit"), data=vary)),
                  error=function(e) NULL)
  if (is.null(fit) || !fit$converged) return(NULL)
  b <- coef(fit)
  if (!term %in% names(b) || is.na(b[[term]])) return(NULL)
  X <- model.matrix(fit)[, !is.na(b), drop=FALSE]
  b <- b[!is.na(b)]
  xb <- drop(X %*% b); p <- pnorm(xb); y <- fit$y
  cl <- vary[[cluster]][as.integer(rownames(X))]
  G <- length(unique(cl)); n <- nrow(X)
  if (G < 3) return(NULL)
  sc <- X * ((y - p) * dnorm(xb) / pmax(p*(1-p), 1e-12))
  bread <- vcov(fit)
  V <- (G/(G-1)) * bread %*% crossprod(rowsum(sc, cl)) %*% bread
  on <- X[, term] == 1
  X0 <- X[on, , drop=FALSE]; X0[, term] <- 0
  xb1 <- xb[on]; xb0 <- drop(X0 %*% b)
  est <- mean(pnorm(xb1) - pnorm(xb0))
  g <- colMeans(dnorm(xb1) * X[on, , drop=FALSE] - dnorm(xb0) * X0)
  se <- sqrt(drop(t(g) %*% V %*% g))
  tq <- qt(0.975, G-1)
  tibble(estimate=est, se=se, lo=est-tq*se, hi=est+tq*se,
         p=2*pt(-abs(est/se), G-1), n=n, n_clusters=G)
}

# Samples: the actual cutoff on all years; placebo cutoffs on one side of the actual cutoff only
cutoffs <- bind_rows(
  tibble(cutoff=reg.policy.year, type="actual"),
  tibble(cutoff=reg.policy.year, type="actual (matched window)"),
  tibble(cutoff=reg.placebo.pre, type="placebo (before)"),
  tibble(cutoff=reg.placebo.post,type="placebo (after)"))

sample_for <- function(df, type) {
  switch(type,
         "actual"           = df,
         # the same number of years on each side of the cutoff (robustness for pooled pre/post averages)
         "actual (matched window)" = {
           m <- min(reg.matched.max, reg.policy.year - min(df$year), max(df$year) - reg.policy.year + 1L)
           df %>% filter(between(year, reg.policy.year - m, reg.policy.year + m - 1L)) },
         "placebo (before)" = df %>% filter(year <  reg.policy.year),
         "placebo (after)"  = df %>% filter(year >= reg.policy.year))
}

# Estimate all specifications at all cutoffs. specs: named list of (formula, term) for pooled;
# by.journal: named list for single journals.
run_all <- function(df, specs, by.journal, outcome, estimator=ols_cluster, method="OLS") {
  out <- list()
  for (i in seq_len(nrow(cutoffs))) {
    c0 <- cutoffs$cutoff[i]; ty <- cutoffs$type[i]
    d <- sample_for(df, ty) %>%
      # trend in publication quarters, 0 = first quarter of the cutoff year
      mutate(post=as.integer(year >= c0), t=4L*(year - c0) + (pub_quarter - 1L),
             cl=paste(Journal,year))
    yr <- range(d$year)
    pre.mean <- function(dd) mean(dd$y[dd$post==0])
    for (s in names(specs)) {
      dd <- if (!is.null(specs[[s]]$filter)) specs[[s]]$filter(d) else d
      r <- estimator(specs[[s]]$formula, dd, specs[[s]]$term, "cl")
      if (!is.null(r)) out[[length(out)+1]] <- r %>%
        mutate(Journal=if (is.null(specs[[s]]$label)) main.label else specs[[s]]$label, spec=s, cutoff=c0, type=ty,
               first_year=min(dd$year), last_year=max(dd$year), mean_pre=pre.mean(dd))
    }
    for (j in sort(unique(d$Journal))) {
      dj <- d %>% filter(Journal==j)
      if (length(unique(dj$post)) < 2) next
      for (s in names(by.journal)) {
        r <- estimator(by.journal[[s]]$formula, dj, by.journal[[s]]$term, "cl")
        if (!is.null(r)) out[[length(out)+1]] <- r %>%
          mutate(Journal=j, spec=s, cutoff=c0, type=ty,
                 first_year=min(dj$year), last_year=max(dj$year), mean_pre=pre.mean(dj))
      }
    }
  }
  bind_rows(out) %>%
    mutate(outcome=outcome, method=method) %>%
    select(outcome,method,Journal,spec,type,cutoff,first_year,last_year,n,n_clusters,mean_pre,
           estimate,se,lo,hi,p) %>%
    arrange(outcome,method,Journal,spec,cutoff)
}

its.journal <- list(
  its_fe    = list(formula=y ~ post,     term="post"),
  its_trend = list(formula=y ~ post + t, term="post"))

# ===========================
# (a) Presence of a replication package
# ===========================

# publication quarter from the Crossref print date (the year is the corrected year from 04_combine.R)
pub.quarters <- read_csv(doi.file.csv,show_col_types=FALSE) %>%
  distinct(doi,.keep_all=TRUE) %>%
  transmute(doi, pub_quarter=(as.integer(substr(published.print,6,7)) - 1L) %/% 3L + 1L)

articles <- read_csv(article.db.csv,show_col_types=FALSE) %>%
  filter(between(year,reg.pkg.firstyear,lastyear)) %>%
  left_join(pub.quarters,by="doi") %>%
  mutate(y=as.numeric(has_package),
         newly=as.integer(Journal %in% reg.pkg.newly))

articles %>% count(Journal,newly)
# articles without a print month: quarter unknown, dropped from the regressions
articles %>% count(Journal,missing_quarter=is.na(pub_quarter)) %>% print(n=Inf)
articles <- articles %>% filter(!is.na(pub_quarter))

no.insights <- function(d) d %>% filter(!Journal %in% reg.pkg.excluded)
main.only   <- function(d) d %>% filter(Journal %in% main.journals)
did.label   <- "DiD: P&P, JEL, JEP vs AER, AEJs"
pkg.specs <- list(
  its_fe    = list(formula=y ~ post + factor(Journal),                    term="post", filter=main.only),
  its_trend = list(formula=y ~ post + factor(Journal) + factor(Journal):t, term="post", filter=main.only),
  did       = list(formula=y ~ post:newly + factor(Journal) + factor(year), term="post:newly",
                   filter=no.insights, label=did.label),
  did_trend = list(formula=y ~ post:newly + factor(Journal) + factor(year) + newly:t, term="post:newly",
                   filter=no.insights, label=did.label))

reg.pkg <- bind_rows(
  run_all(articles, pkg.specs, its.journal, "has_package", ols_cluster,    "Linear probability"),
  run_all(articles, pkg.specs, its.journal, "has_package", probit_cluster, "Probit"))
reg.pkg %>% filter(Journal %in% c(main.label,did.label)) %>% print(n=Inf,width=Inf)
write_csv(reg.pkg, reg.packages.csv)

# ===========================
# (b) Time from submission to publication (months)
# ===========================

delay <- read_csv(ttp.articles.csv,show_col_types=FALSE) %>%
  filter(!is.na(pub_month)) %>%
  mutate(y=months_to_pub, has_package=as.integer(has_package), pub_quarter=(pub_month - 1L) %/% 3L + 1L)

ttp.specs <- list(
  its_fe    = list(formula=y ~ post + factor(Journal),                     term="post"),
  its_trend = list(formula=y ~ post + factor(Journal) + factor(Journal):t, term="post"),
  # differential change for articles with a package (main effect of has_package included)
  pkg       = list(formula=y ~ post*has_package + factor(Journal),          term="post:has_package"))

reg.delay <- run_all(delay, ttp.specs, its.journal, "months_to_pub")
reg.delay %>% filter(Journal==main.label) %>% print(n=Inf,width=Inf)
write_csv(reg.delay, reg.ttp.csv)

# ===========================
# Event studies (main specification)
# ===========================
# y = a_j + f_j(t) + sum_{k != es.ref} b_k 1(year = k) + e, main journals with years before the policy.
# Calendar-year effects cannot be separated from b_k (all main journals are treated in 2019), so:
#   none       f = 0: b_k is the journal-adjusted mean in year k minus that in es.ref
#   linear     f_j linear in publication quarter, fitted on es.firstyear..es.ref and extrapolated;
#              b_k = mean deviation from the fitted trend in year k minus that in es.ref
#   quadratic  as linear, with a quadratic in publication quarter
#   rolling    b_k = mean deviation in year k from the linear journal trends fitted on the es.roll
#              years before k (a local jump; no reference year)
# A journal enters the trend variants only with at least es.min.pre.years years in the fit window.
# Confidence intervals: percentile bootstrap, resampling issues within journal x year.

es_fit <- function(d, trend) {
  fe   <- if (n_distinct(d$Journal) > 1) "factor(Journal)" else "1"
  tr   <- if (n_distinct(d$Journal) > 1) "factor(Journal):t" else "t"
  yrs  <- sort(unique(d$year)); dum <- setdiff(yrs, es.ref)
  pre_ok <- function(dd, win) dd %>% group_by(Journal) %>%
    filter(n_distinct(year[year %in% win]) >= es.min.pre.years) %>% ungroup()
  if (trend == "none") {
    X <- model.matrix(as.formula(paste("~", fe)), d)
    for (k in dum) X <- cbind(X, as.numeric(d$year == k))
    b <- lm.fit(X, d$y)$coefficients
    return(tibble(year=c(es.ref, dum), estimate=c(0, tail(b, length(dum)))))
  }
  if (trend %in% c("linear","quadratic")) {
    win <- es.firstyear:es.ref
    d <- pre_ok(d, win); if (nrow(d) == 0) return(NULL)
    f <- paste("y ~", fe, "+", tr, if (trend == "quadratic") paste0("+ ", sub(":t", ":I(t^2)", tr), "") else "")
    if (trend == "quadratic" && tr == "t") f <- "y ~ t + I(t^2)"
    fit <- lm(as.formula(f), data=d %>% filter(year %in% win))
    r <- d$y - predict(fit, newdata=d)
    m <- tapply(r, d$year, mean)
    return(tibble(year=as.integer(names(m)), estimate=as.numeric(m - m[as.character(es.ref)])))
  }
  # rolling local trend
  out <- list()
  for (k in yrs) {
    win <- (k - es.roll):(k - 1L)
    dk <- d %>% filter(year %in% c(win, k)) %>% pre_ok(win) %>%
      group_by(Journal) %>% filter(any(year == k)) %>% ungroup()
    if (nrow(dk) == 0 || !any(dk$year %in% win)) next
    fit <- lm(as.formula(paste("y ~", fe, "+", tr)), data=dk %>% filter(year %in% win))
    nd <- dk %>% filter(year == k)
    out[[length(out)+1]] <- tibble(year=k, estimate=mean(nd$y - predict(fit, newdata=nd)))
  }
  bind_rows(out)
}

es_run <- function(df, outcome, firstyear) {
  df <- df %>% filter(Journal %in% setdiff(main.journals, reg.pkg.excluded), year <= lastyear) %>%
    mutate(t=4L*(year - reg.policy.year) + (pub_quarter - 1L), strat=paste(Journal, year))
  samples <- c(setNames(list(df), main.label), split(df, df$Journal))
  RNGkind("L'Ecuyer-CMRG"); set.seed(20190101)
  res <- list()
  for (j in names(samples)) for (tr in c("none","linear","quadratic","rolling")) {
    dj <- samples[[j]]
    # the rolling trend needs es.roll years before es.firstyear; others start at the window
    d0 <- if (tr == "rolling") dj else dj %>% filter(year >= firstyear)
    est <- es_fit(d0, tr); if (is.null(est) || nrow(est) == 0) next
    if (tr == "rolling") est <- est %>% filter(year >= firstyear)
    # bootstrap: resample issues within journal x year
    idx <- split(seq_len(nrow(d0)), d0$strat)
    iss <- lapply(idx, function(i) split(i, d0$issue_id[i]))
    boots <- parallel::mclapply(seq_len(es.boot), function(r) {
      rows <- unlist(lapply(iss, function(g) unlist(g[sample.int(length(g), length(g), replace=TRUE)], use.names=FALSE)),
                     use.names=FALSE)
      b <- es_fit(d0[rows, ], tr)
      b$estimate[match(est$year, b$year)]
    }, mc.cores=es.cores, mc.set.seed=TRUE)
    boots <- matrix(unlist(boots), nrow=nrow(est))
    nyr <- d0 %>% count(year)
    res[[length(res)+1]] <- est %>%
      mutate(Journal=j, trend=tr,
             se=apply(boots, 1, sd, na.rm=TRUE),
             lo=apply(boots, 1, quantile, 0.025, na.rm=TRUE),
             hi=apply(boots, 1, quantile, 0.975, na.rm=TRUE),
             p=pmin(1, 2*pmin(rowMeans(boots <= 0, na.rm=TRUE), rowMeans(boots >= 0, na.rm=TRUE))),
             mean_ref=mean(d0$y[d0$year == es.ref]),
             journals=n_distinct(d0$Journal)) %>%
      left_join(nyr %>% rename(n_year=n), by="year") %>%
      mutate(across(c(se, lo, hi, p), ~ if_else(trend != "rolling" & year == es.ref, NA_real_, .x)))
  }
  bind_rows(res) %>% mutate(outcome=outcome) %>%
    select(outcome, Journal, trend, year, n_year, journals, mean_ref, estimate, se, lo, hi, p) %>%
    arrange(outcome, Journal, trend, year)
}

# tidylog would print a message for every dplyr call in every bootstrap draw
old.tl <- options(tidylog.display=list())
issues <- read_csv(article.db.csv, show_col_types=FALSE) %>%
  transmute(doi, issue_id=paste(Journal, volume, issue))
es <- bind_rows(
  es_run(articles %>% left_join(issues %>% select(-any_of("Journal")), by="doi"), "has_package", es.firstyear),
  es_run(delay %>% left_join(issues, by="doi") %>% mutate(issue_id=coalesce(issue_id, paste(Journal, year, pub_month))),
         "months_to_pub", es.firstyear))
es %>% filter(Journal == main.label) %>% print(n=Inf, width=Inf)
write_csv(es, es.csv)
options(old.tl)

# ===========================
# Figure: estimates at the actual and the placebo implementation years (pooled)
# ===========================

spec.labels <- c(its_fe="Journal FE",its_trend="Journal FE + journal quarterly trends",
                 did="DiD: P&P, JEL, JEP vs AER, AEJs (year FE)",did_trend="DiD with group quarterly trends",
                 pkg="With vs without a package (post x package)")
col.actual  <- "#C2611F"
col.placebo <- "#2F6DB5"

fig_placebo <- function(r, title, ylab, file, scale=1) {
  df <- r %>% filter(Journal %in% c(main.label,did.label), method %in% c("OLS","Linear probability"), type != "actual (matched window)") %>%
    mutate(spec=factor(spec.labels[spec],levels=spec.labels),
           kind=if_else(startsWith(type,"actual"),"Actual (published 2019 and later)","Placebo"),
           across(c(estimate,lo,hi),~ .x*scale))
  p <- ggplot(df,aes(cutoff,estimate,colour=kind)) +
    geom_hline(yintercept=0,colour="grey50",linewidth=0.3) +
    geom_vline(xintercept=reg.policy.year-0.5,colour="grey40",linetype="dashed",linewidth=0.4) +
    geom_linerange(aes(ymin=lo,ymax=hi),linewidth=0.7) +
    geom_point(size=2.2) +
    facet_wrap(~ spec,ncol=2,drop=TRUE) +
    scale_colour_manual(values=c("Actual (published 2019 and later)"=col.actual,"Placebo"=col.placebo)) +
    scale_x_continuous(breaks=c(reg.placebo.pre,reg.policy.year,reg.placebo.post)) +
    labs(title=title,
         subtitle=paste0("Pooled: AER, AER:Insights, AEJs (DiD: with P&P, JEL, JEP as treated). OLS estimate and 95% CI (clustered by journal x year). Placebos at ",
                         paste(reg.placebo.pre,collapse=", ")," use years before ",reg.policy.year," only;\n",
                         "placebos at ",paste(reg.placebo.post,collapse=", ")," use years from ",reg.policy.year," on only."),
         x="Cutoff year c: post = 1 if published in year c or later (actual c = 2019; others placebos)",y=ylab) +
    theme_classic() +
    theme(legend.position="top",legend.justification="left",legend.title=element_blank(),
          strip.background=element_blank(),strip.text=element_text(face="bold",hjust=0),
          panel.grid.major.y=element_line(colour="grey92",linewidth=0.3),
          plot.subtitle=element_text(colour="grey30",size=8.5),plot.title.position="plot")
  ggsave(file.path(Figures,paste0(file,".png")),p,width=9,height=6.5,dpi=300)
  ggsave(file.path(Figures,paste0(file,".pdf")),p,width=9,height=6.5)
  p
}

fig_placebo(reg.pkg,"Estimated change in articles with replication packages (linear probability model)",
            "Change in share with a package (percentage points)","figure_regressions_packages",scale=100)
fig_placebo(reg.delay,"Estimated change in the time from submission to publication",
            "Change in time to publication (months)","figure_regressions_time_to_pub")
