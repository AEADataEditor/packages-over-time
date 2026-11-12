# Regressions: did the 2019 AEA-wide data and code policy change
#   (a) the presence of replication packages (article level, linear probability model),
#   (b) the delay from submission to publication (months, articles from 06_delay.R)?
#
# Specifications (OLS; standard errors clustered by journal x publication year, CR1, t with G-1 df):
#   its_fe     interrupted time series: y ~ post + journal fixed effects
#   its_trend  interrupted time series: y ~ post + journal fixed effects + journal-specific linear trends
#   did        (a) only. Difference in differences: journals without a data policy before 2019
#              (P&P, JEL, JEP) against the AER and AEJs, which had one since 2005 and 2009:
#              y ~ post x newly covered + journal fixed effects + year fixed effects
#   did_trend  (a) only. As did, with separate linear trends for the two groups
#   pkg        (b) only. y ~ post x has_package + journal fixed effects: does the change in delay
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
run_all <- function(df, specs, by.journal, outcome) {
  out <- list()
  for (i in seq_len(nrow(cutoffs))) {
    c0 <- cutoffs$cutoff[i]; ty <- cutoffs$type[i]
    d <- sample_for(df, ty) %>%
      mutate(post=as.integer(year >= c0), t=year - c0,
             cl=paste(Journal,year))
    yr <- range(d$year)
    pre.mean <- function(dd) mean(dd$y[dd$post==0])
    for (s in names(specs)) {
      dd <- if (!is.null(specs[[s]]$filter)) specs[[s]]$filter(d) else d
      r <- ols_cluster(specs[[s]]$formula, dd, specs[[s]]$term, "cl")
      if (!is.null(r)) out[[length(out)+1]] <- r %>%
        mutate(Journal="All journals", spec=s, cutoff=c0, type=ty,
               first_year=min(dd$year), last_year=max(dd$year), mean_pre=pre.mean(dd))
    }
    for (j in sort(unique(d$Journal))) {
      dj <- d %>% filter(Journal==j)
      if (length(unique(dj$post)) < 2) next
      for (s in names(by.journal)) {
        r <- ols_cluster(by.journal[[s]]$formula, dj, by.journal[[s]]$term, "cl")
        if (!is.null(r)) out[[length(out)+1]] <- r %>%
          mutate(Journal=j, spec=s, cutoff=c0, type=ty,
                 first_year=min(dj$year), last_year=max(dj$year), mean_pre=pre.mean(dj))
      }
    }
  }
  bind_rows(out) %>%
    mutate(outcome=outcome) %>%
    select(outcome,Journal,spec,type,cutoff,first_year,last_year,n,n_clusters,mean_pre,
           estimate,se,lo,hi,p) %>%
    arrange(outcome,Journal,spec,cutoff)
}

its.journal <- list(
  its_fe    = list(formula=y ~ post,     term="post"),
  its_trend = list(formula=y ~ post + t, term="post"))

# ===========================
# (a) Presence of a replication package
# ===========================

articles <- read_csv(article.db.csv,show_col_types=FALSE) %>%
  filter(between(year,reg.pkg.firstyear,lastyear)) %>%
  mutate(y=as.numeric(has_package),
         newly=as.integer(Journal %in% reg.pkg.newly))

articles %>% count(Journal,newly)

no.insights <- function(d) d %>% filter(!Journal %in% reg.pkg.excluded)
pkg.specs <- list(
  its_fe    = list(formula=y ~ post + factor(Journal),                    term="post"),
  its_trend = list(formula=y ~ post + factor(Journal) + factor(Journal):t, term="post"),
  did       = list(formula=y ~ post:newly + factor(Journal) + factor(year), term="post:newly",
                   filter=no.insights),
  did_trend = list(formula=y ~ post:newly + factor(Journal) + factor(year) + newly:t, term="post:newly",
                   filter=no.insights))

reg.pkg <- run_all(articles, pkg.specs, its.journal, "has_package")
reg.pkg %>% filter(Journal=="All journals") %>% print(n=Inf,width=Inf)
write_csv(reg.pkg, reg.packages.csv)

# ===========================
# (b) Delay from submission to publication (months)
# ===========================

delay <- read_csv(delay.articles.csv,show_col_types=FALSE) %>%
  mutate(y=delay_months, has_package=as.integer(has_package))

delay.specs <- list(
  its_fe    = list(formula=y ~ post + factor(Journal),                     term="post"),
  its_trend = list(formula=y ~ post + factor(Journal) + factor(Journal):t, term="post"),
  # differential change for articles with a package (main effect of has_package included)
  pkg       = list(formula=y ~ post*has_package + factor(Journal),          term="post:has_package"))

reg.delay <- run_all(delay, delay.specs, its.journal, "delay_months")
reg.delay %>% filter(Journal=="All journals") %>% print(n=Inf,width=Inf)
write_csv(reg.delay, reg.delay.csv)

# ===========================
# Figure: estimates at the actual and the placebo implementation years (pooled)
# ===========================

spec.labels <- c(its_fe="Journal FE",its_trend="Journal FE + journal trends",
                 did="DiD: P&P, JEL, JEP vs AER, AEJs (year FE)",did_trend="DiD with group trends",
                 pkg="With vs without a package (post x package)")
col.actual  <- "#C2611F"
col.placebo <- "#2F6DB5"

fig_placebo <- function(r, title, ylab, file, scale=1) {
  df <- r %>% filter(Journal=="All journals") %>%
    mutate(spec=factor(spec.labels[spec],levels=spec.labels),
           kind=if_else(type=="actual","Actual (2019)","Placebo"),
           across(c(estimate,lo,hi),~ .x*scale))
  p <- ggplot(df,aes(cutoff,estimate,colour=kind)) +
    geom_hline(yintercept=0,colour="grey50",linewidth=0.3) +
    geom_vline(xintercept=reg.policy.year-0.5,colour="grey40",linetype="dashed",linewidth=0.4) +
    geom_linerange(aes(ymin=lo,ymax=hi),linewidth=0.7) +
    geom_point(size=2.2) +
    facet_wrap(~ spec,ncol=2,drop=TRUE) +
    scale_colour_manual(values=c("Actual (2019)"=col.actual,"Placebo"=col.placebo)) +
    scale_x_continuous(breaks=c(reg.placebo.pre,reg.policy.year,reg.placebo.post)) +
    labs(title=title,
         subtitle=paste0("All journals pooled. OLS estimate and 95% CI (clustered by journal x year). Placebos at ",
                         paste(reg.placebo.pre,collapse=", ")," use years before ",reg.policy.year," only;\n",
                         "placebos at ",paste(reg.placebo.post,collapse=", ")," use years from ",reg.policy.year," on only."),
         x="Implementation year (actual or placebo)",y=ylab) +
    theme_classic() +
    theme(legend.position="top",legend.justification="left",legend.title=element_blank(),
          strip.background=element_blank(),strip.text=element_text(face="bold",hjust=0),
          panel.grid.major.y=element_line(colour="grey92",linewidth=0.3),
          plot.subtitle=element_text(colour="grey30",size=8.5),plot.title.position="plot")
  ggsave(file.path(Figures,paste0(file,".png")),p,width=9,height=6.5,dpi=300)
  ggsave(file.path(Figures,paste0(file,".pdf")),p,width=9,height=6.5)
  p
}

fig_placebo(reg.pkg,"Replication packages: effect of the 2019 policy, actual and placebo years",
            "Change in share with a package (percentage points)","figure_regressions_packages",scale=100)
fig_placebo(reg.delay,"Delay to publication: effect of the 2019 policy, actual and placebo years",
            "Change in delay (months)","figure_regressions_delay")
