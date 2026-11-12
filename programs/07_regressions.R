# Regressions: did the 2019 AEA-wide data and code policy change
#   (a) the presence of replication packages (article level, linear probability model),
#   (b) the time from submission to publication (months, articles from 06_time_to_pub.R)?
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
  tibble(cutoff=reg.placebo.pre, type="placebo (before)"),
  tibble(cutoff=reg.placebo.post,type="placebo (after)"))

sample_for <- function(df, type) {
  switch(type,
         "actual"           = df,
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
        mutate(Journal="All journals", spec=s, cutoff=c0, type=ty,
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
pkg.specs <- list(
  its_fe    = list(formula=y ~ post + factor(Journal),                    term="post"),
  its_trend = list(formula=y ~ post + factor(Journal) + factor(Journal):t, term="post"),
  did       = list(formula=y ~ post:newly + factor(Journal) + factor(year), term="post:newly",
                   filter=no.insights),
  did_trend = list(formula=y ~ post:newly + factor(Journal) + factor(year) + newly:t, term="post:newly",
                   filter=no.insights))

reg.pkg <- bind_rows(
  run_all(articles, pkg.specs, its.journal, "has_package", ols_cluster,    "Linear probability"),
  run_all(articles, pkg.specs, its.journal, "has_package", probit_cluster, "Probit"))
reg.pkg %>% filter(Journal=="All journals") %>% print(n=Inf,width=Inf)
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
reg.delay %>% filter(Journal=="All journals") %>% print(n=Inf,width=Inf)
write_csv(reg.delay, reg.ttp.csv)

# ===========================
# Figure: estimates at the actual and the placebo implementation years (pooled)
# ===========================

spec.labels <- c(its_fe="Journal FE",its_trend="Journal FE + journal quarterly trends",
                 did="DiD: P&P, JEL, JEP vs AER, AEJs (year FE)",did_trend="DiD with group quarterly trends",
                 pkg="With vs without a package (post x package)")
col.actual  <- "#C2611F"
col.placebo <- "#2F6DB5"

fig_placebo <- function(r, title, ylab, file, scale=1) {
  df <- r %>% filter(Journal=="All journals", method %in% c("OLS","Linear probability")) %>%
    mutate(spec=factor(spec.labels[spec],levels=spec.labels),
           kind=if_else(type=="actual","Actual (published 2019 and later)","Placebo"),
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
         subtitle=paste0("All journals pooled. OLS estimate and 95% CI (clustered by journal x year). Placebos at ",
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
