# Delay between submission and publication, from manuscript-number DOIs
#
# Since about 2015, AEA DOIs are based on the manuscript number: 10.1257/aer.20151234 was submitted
# in 2015, as manuscript 1234 of that year. The sequence number has no date, but it increases with
# submissions. Within a submission year (cohort), the quartiles of the sequence numbers of all
# eventually published manuscripts define the submission quarter. This is only possible for cohorts
# whose published manuscripts are (nearly) all observed: cohorts that also published before the
# switch to manuscript-number DOIs (left), or that are still being published (right), are not mapped.
#
# Delay: from the middle of the submission quarter to the middle of the month of print publication.
# Pre/post: published before or from ttp.policy.year (2019 AEA-wide policy, Data Editor review).

source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)

journal.order <- unname(ttp.prefixes)

# ===========================
# Articles with manuscript-number DOIs
# ===========================

pub.dates <- read_csv(doi.file.csv,show_col_types=FALSE) %>%
  distinct(doi,.keep_all=TRUE) %>%
  select(doi,published.print)

articles <- read_csv(article.db.csv,show_col_types=FALSE) %>%
  filter(year <= lastyear) %>%
  left_join(pub.dates,by="doi") %>%
  mutate(m      = str_match(doi,"^10\\.1257/([a-z]+)\\.(\\d{4})(\\d{4})$"),
         prefix = m[,2],
         sub_year = as.integer(m[,3]),
         seq    = as.integer(m[,4]),
         # print month (Crossref print dates are the first of the month); year corrected in 04_combine.R
         pub_month = as.integer(substr(published.print,6,7))) %>%
  select(-m)

# Journals with manuscript-number DOIs, by journal: the first publication year with them.
# Earlier articles of the same journal have volume-based DOIs (left truncation of cohorts),
# except for journals that started with manuscript-number DOIs (AER:Insights).
ms <- articles %>%
  filter(prefix %in% names(ttp.prefixes), Journal %in% ttp.prefixes,
         # a few malformed DOIs (e.g., aer.15000001) are not manuscript numbers
         between(sub_year,2000,year))

first.ms <- ms %>% group_by(Journal) %>%
  summarize(first_ms_year=min(year),.groups="drop") %>%
  left_join(articles %>% filter(Journal %in% ttp.prefixes) %>%
              group_by(Journal) %>% summarize(first_year=min(year),.groups="drop"),
            by="Journal") %>%
  # left truncation only if the journal published (with volume DOIs) before
  mutate(left_bound=if_else(first_year < first_ms_year,first_ms_year,NA_integer_))
first.ms

# ===========================
# Coverage of submission cohorts
# Lag distribution (publication year - submission year), from cohorts observed for all lags
# 0 to ttp.max.lag (lags beyond are pooled); a cohort's coverage is the probability mass of
# the lags at which its articles are observed.
# ===========================

ms <- ms %>% left_join(first.ms %>% select(Journal,left_bound),by="Journal") %>%
  mutate(lag=pmin(year-sub_year,ttp.max.lag))

lag.dist <- ms %>%
  filter(is.na(left_bound) | sub_year >= left_bound,
         sub_year + ttp.max.lag <= lastyear) %>%
  count(Journal,lag) %>%
  group_by(Journal) %>% mutate(p=n/sum(n)) %>% ungroup() %>%
  complete(Journal,lag=0:ttp.max.lag,fill=list(n=0L,p=0))

cohorts <- ms %>% count(Journal,sub_year,name="articles") %>%
  left_join(first.ms %>% select(Journal,left_bound),by="Journal") %>%
  rowwise() %>%
  mutate(coverage = {
           lo <- if (is.na(left_bound)) 0 else max(0L,left_bound - sub_year)
           hi <- lastyear - sub_year
           lags <- lag.dist[lag.dist$Journal==Journal,]
           # the pooled lag ttp.max.lag+ counts only if the cohort is observed that long
           sum(lags$p[lags$lag >= lo & lags$lag <= min(hi,ttp.max.lag)])
         }) %>%
  ungroup() %>%
  mutate(mapped = coverage >= ttp.min.coverage & articles >= ttp.min.cohort)

cohorts %>% print(n=Inf)
write_csv(cohorts,ttp.cohorts.csv)

# ===========================
# Submission quarter and delay
# ===========================

ms <- ms %>%
  left_join(cohorts %>% select(Journal,sub_year,mapped),by=c("Journal","sub_year")) %>%
  group_by(Journal,sub_year) %>%
  mutate(sub_quarter=if_else(mapped,ntile(seq,4),NA_integer_)) %>%
  ungroup() %>%
  mutate(sub_time   = sub_year + (sub_quarter-0.5)/4,
         pub_time   = year + (pub_month-0.5)/12,
         months_to_pub = 12*(pub_time - sub_time))

# Publication years: analyzed only if (nearly) all of their articles come from mapped cohorts
pubyears <- ms %>% group_by(Journal,year) %>%
  summarize(articles=n(),mapped=sum(mapped),share_mapped=mapped/articles,.groups="drop") %>%
  mutate(analyzed = share_mapped >= ttp.min.pubyear)
pubyears %>% print(n=Inf)
write_csv(pubyears,ttp.pubyears.csv)

delay <- ms %>%
  semi_join(pubyears %>% filter(analyzed),by=c("Journal","year")) %>%
  filter(mapped) %>%
  mutate(Journal = factor(Journal,levels=journal.order),
         post    = year >= ttp.policy.year,
         period  = factor(if_else(post,paste0("Published ",ttp.policy.year," and later"),paste0("Published before ",ttp.policy.year)),
                          levels=c(paste0("Published before ",ttp.policy.year),paste0("Published ",ttp.policy.year," and later"))))

delay %>%
  select(doi,title,Journal,year,pub_month,sub_year,seq,sub_quarter,months_to_pub,period,has_package) %>%
  mutate(months_to_pub=round(months_to_pub,1)) %>%
  write_csv(ttp.articles.csv)

# ===========================
# Tests: is there a shift after the policy?
# Wilcoxon rank-sum (Hodges-Lehmann shift and 95% CI), Holm-adjusted across journals;
# Kolmogorov-Smirnov for any difference in distribution.
# Delays fall on a lattice (quarter midpoints; quarterly AEJ issues), so the Hodges-Lehmann shift and
# its CI are multiples of 3 months for the AEJs. The difference in means, with a Welch 95% CI, is reported
# alongside, as a continuous estimate.
# ===========================

shift_test <- function(df, cutoff) {
  pre  <- df$months_to_pub[df$year <  cutoff]
  post <- df$months_to_pub[df$year >= cutoff]
  if (length(pre) < 10 || length(post) < 10) {
    return(tibble(n_pre=length(pre),n_post=length(post),
                  median_pre=if (length(pre)) median(pre) else NA_real_,median_post=median(post)))
  }
  w <- suppressWarnings(wilcox.test(post,pre,conf.int=TRUE))
  k <- suppressWarnings(ks.test(post,pre))
  t <- t.test(post,pre)
  tibble(n_pre=length(pre),n_post=length(post),
         first_year=min(df$year),last_year=max(df$year),
         median_pre=median(pre),median_post=median(post),
         shift=unname(w$estimate),shift_lo=w$conf.int[1],shift_hi=w$conf.int[2],
         mean_pre=mean(pre),mean_post=mean(post),
         mean_diff=mean(post)-mean(pre),mean_diff_lo=t$conf.int[1],mean_diff_hi=t$conf.int[2],
         p_wilcox=w$p.value,p_ks=k$p.value)
}

tests <- bind_rows(
  delay %>% group_by(Journal) %>% group_modify(~ shift_test(.x,ttp.policy.year)) %>%
    ungroup() %>% mutate(cutoff=ttp.policy.year),
  delay %>% group_by(Journal) %>% group_modify(~ shift_test(.x,ttp.policy.year.alt)) %>%
    ungroup() %>% mutate(cutoff=ttp.policy.year.alt)) %>%
  group_by(cutoff) %>%
  mutate(p_wilcox_holm=p.adjust(p_wilcox,method="holm"),
         p_ks_holm=p.adjust(p_ks,method="holm"),
         significant=!is.na(p_wilcox_holm) & p_wilcox_holm < ttp.alpha) %>%
  ungroup() %>%
  relocate(cutoff,.after=Journal)

tests %>% print(n=Inf,width=Inf)
write_csv(tests,ttp.tests.csv)

main.tests <- tests %>% filter(cutoff==ttp.policy.year)

# ===========================
# Delay by whether the article has a replication package
# For each journal (and all journals pooled), articles with a package against articles without, over
# all analyzed years and separately before and after the policy. Same statistics as above
# (with package minus without). Holm adjustment across journals, within each period.
# Articles without a package after 2019 are mostly exempt (no data or code, e.g., theory) or not matched.
# ===========================

package_test <- function(df) {
  with.p <- df$months_to_pub[df$has_package]
  no.p   <- df$months_to_pub[!df$has_package]
  out <- tibble(n_package=length(with.p),n_no_package=length(no.p),
                median_package=if (length(with.p)) median(with.p) else NA_real_,
                median_no_package=if (length(no.p)) median(no.p) else NA_real_,
                mean_package=if (length(with.p)) mean(with.p) else NA_real_,
                mean_no_package=if (length(no.p)) mean(no.p) else NA_real_)
  if (length(with.p) < 5 || length(no.p) < 5) return(out)
  w <- suppressWarnings(wilcox.test(with.p,no.p,conf.int=TRUE))
  t <- t.test(with.p,no.p)
  out %>% mutate(shift=unname(w$estimate),shift_lo=w$conf.int[1],shift_hi=w$conf.int[2],
                 mean_diff=mean(with.p)-mean(no.p),mean_diff_lo=t$conf.int[1],mean_diff_hi=t$conf.int[2],
                 p_wilcox=w$p.value)
}

ttp.pkg.data <- bind_rows(delay %>% mutate(Journal=as.character(Journal)),
                            delay %>% mutate(Journal="All journals"))
by.package <- bind_rows(
  ttp.pkg.data %>% mutate(period="All years"),
  ttp.pkg.data %>% mutate(period=as.character(period))) %>%
  group_by(Journal,period) %>%
  group_modify(~ package_test(.x)) %>%
  ungroup() %>%
  group_by(period,pooled=Journal=="All journals") %>%
  mutate(p_wilcox_holm=if (first(pooled)) p_wilcox else p.adjust(p_wilcox,method="holm")) %>%
  ungroup() %>% select(-pooled) %>%
  mutate(significant=!is.na(p_wilcox_holm) & p_wilcox_holm < ttp.alpha,
         Journal=factor(Journal,levels=c(journal.order,"All journals")),
         period=factor(period,levels=c("All years",levels(delay$period)))) %>%
  arrange(Journal,period)

by.package %>% print(n=Inf,width=Inf)
write_csv(by.package,ttp.package.csv)

# ===========================
# Kolmogorov-Smirnov tests: are the two distributions different?
# The two-sample K-S statistic D compares the empirical CDFs, which are shares, so groups of different
# sizes are comparable; the sizes enter through the effective sample size n1*n2/(n1+n2), which sets
# the critical value (5%: 1.358/sqrt(n_eff)). Delays fall on a lattice (quarter midpoints, monthly or
# quarterly issues), so there are many ties, and the asymptotic p-value is conservative. The permutation
# p-value (group labels reshuffled, sizes kept) is exact for the observed sizes and ties.
# Comparisons: before vs after 2019; with vs without a package (all years, before, after).
# Holm adjustment across journals, within each comparison and period.
# ===========================

ks_perm <- function(x, y, B=ttp.ks.permutations) {
  n1 <- length(x); n2 <- length(y)
  out <- tibble(n1=n1, n2=n2)
  if (n1 < 5 || n2 < 5) return(out)
  D <- unname(suppressWarnings(ks.test(x,y,exact=FALSE))$statistic)
  p.asy <- suppressWarnings(ks.test(x,y,exact=FALSE))$p.value
  z <- c(x,y); grid <- sort(unique(z))
  Dperm <- replicate(B, {
    i <- sample.int(n1+n2, n1)
    max(abs(ecdf(z[i])(grid) - ecdf(z[-i])(grid)))
  })
  n.eff <- n1*n2/(n1+n2)
  out %>% mutate(n_eff=n.eff, D=D, D_crit_5=1.358/sqrt(n.eff),
                 p_asymptotic=p.asy, p_permutation=(1+sum(Dperm >= D - 1e-12))/(B+1))
}

set.seed(20191002)
ks.data <- ttp.pkg.data %>% mutate(period=as.character(period))
ks.tests <- bind_rows(
  ks.data %>% group_by(Journal) %>%
    group_modify(~ ks_perm(.x$months_to_pub[.x$year < ttp.policy.year],
                           .x$months_to_pub[.x$year >= ttp.policy.year])) %>%
    ungroup() %>% mutate(comparison=paste0("Published before vs. from ",ttp.policy.year),period="All years",
                         group1=paste0("Published before ",ttp.policy.year),group2=paste0("Published ",ttp.policy.year," and later")),
  bind_rows(ks.data %>% mutate(period="All years"), ks.data) %>%
    group_by(Journal,period) %>%
    group_modify(~ ks_perm(.x$months_to_pub[.x$has_package], .x$months_to_pub[!.x$has_package])) %>%
    ungroup() %>% mutate(comparison="With vs without a package",
                         group1="With a package",group2="Without")) %>%
  group_by(comparison,period,pooled=Journal=="All journals") %>%
  mutate(p_permutation_holm=if (first(pooled)) p_permutation else p.adjust(p_permutation,method="holm")) %>%
  ungroup() %>% select(-pooled) %>%
  mutate(significant=!is.na(p_permutation_holm) & p_permutation_holm < ttp.alpha,
         Journal=factor(Journal,levels=c(journal.order,"All journals"))) %>%
  arrange(comparison,Journal,period) %>%
  relocate(comparison,Journal,period,group1,group2)

ks.tests %>% print(n=Inf,width=Inf)
write_csv(ks.tests,ttp.ks.csv)

# ===========================
# Figures
# ===========================

col.pre    <- "#2F6DB5"   # before the policy
col.post   <- "#C2611F"   # after the policy, significant shift
col.grey   <- "grey72"    # other journals
col.single <- "#44546A"   # all periods pooled
# not significant: both periods are shown in muted tints of their colors
col.pre.ns  <- "#93B1D6"
col.post.ns <- "#E2B08E"

theme_delay <- function() {
  theme_classic() +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.x = element_line(colour="grey92",linewidth=0.3),
          legend.position = "top",
          legend.justification = "left",
          legend.title = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(face="bold",hjust=0),
          plot.subtitle = element_text(colour="grey30",size=8.5),
          plot.caption = element_text(hjust=0,colour="grey40",size=7),
          plot.title.position = "plot",
          plot.caption.position = "plot")
}

xmax <- ceiling(max(delay$months_to_pub)/12)*12
# one bandwidth (months) for all densities, so that they are comparable and smooth
# (delays fall on a lattice: quarterly submission dates, monthly or quarterly issues)
bw.months <- 4
x.scale <- scale_x_continuous(breaks=seq(0,xmax,12),limits=c(-6,xmax),
                              labels=function(x) paste0(x/12," yr"),expand=expansion(add=c(0,4)))

# Densities as data, so that they can be scaled and drawn as faint lines;
# points are jittered in a band below the density (one row per group).
dens_of <- function(df, ...) {
  df %>% group_by(...) %>%
    group_modify(~ { if (nrow(.x) < 5) return(tibble())
                     d <- density(.x$months_to_pub,bw=bw.months,from=-6,to=xmax,n=256)
                     tibble(x=d$x,y=d$y) }) %>%
    ungroup()
}
ymax.dens <- max(dens_of(delay,Journal,period)$y,dens_of(delay,Journal)$y)
band <- ymax.dens*0.35     # height of a row of points
set.seed(20191001)
delay <- delay %>% mutate(jit=runif(n(),0.15,0.85))

caption.delay <- paste0(
  "Submission: year and manuscript number from the DOI (e.g., 10.1257/aer.20151234). Within a submission year, the quartiles of the manuscript numbers\n",
  "of all published articles define the submission quarter (dated at its middle). Only submission years with at least ",percent(ttp.min.coverage),
  " of their published articles observed\n",
  "with such a DOI are used, and only publication years with at least ",percent(ttp.min.pubyear)," of articles from those submission years. ",
  "Publication: month of the print issue.\n",
  "Times fall on a grid of 3 months for the AEJs (quarterly issues), so the median shift is a multiple of 3 months.\n",
  "Points: articles (jittered vertically). Lines: kernel density (bandwidth ",bw.months," months). Excluded: AEA P&P, JEL, JEP (no submission-based numbering).")

n.label <- function(df) df %>% summarize(n=n(),med=median(months_to_pub),.groups="drop")

# ---------------------------
# 1. Distribution for every journal, all periods
# ---------------------------

d.all <- dens_of(delay,Journal)
fig.all <- ggplot() +
  geom_point(data=delay,aes(months_to_pub,-jit*band),colour=col.single,alpha=0.45,size=0.8,stroke=0) +
  geom_line(data=d.all,aes(x,y),colour=col.single,alpha=0.5,linewidth=0.6) +
  geom_vline(data=n.label(delay %>% group_by(Journal)),aes(xintercept=med),colour=col.single,linewidth=0.4) +
  geom_text(data=n.label(delay %>% group_by(Journal)),
            aes(x=xmax,y=ymax.dens,label=paste0("n = ",n,"\nmedian ",round(med/12,1)," yr")),
            hjust=1,vjust=1,size=2.8,colour="grey30") +
  facet_wrap(~ Journal,ncol=2) +
  x.scale +
  scale_y_continuous(limits=c(-band,ymax.dens*1.15),expand=expansion(add=0)) +
  labs(title="Time from submission to publication, AEA journals",
       subtitle=paste0("Articles published ",min(delay$year),"-",max(delay$year),
                       " (years depend on the journal; see time_to_pub_pubyears.csv). Vertical line: median."),
       x="Time from submission to print publication",y=element_blank(),caption=caption.delay) +
  theme_delay()
ggsave(file.path(Figures,"figure_time_to_pub_by_journal.png"),fig.all,width=9,height=8,dpi=300)
ggsave(file.path(Figures,"figure_time_to_pub_by_journal.pdf"),fig.all,width=9,height=8)

# ---------------------------
# 2. Before and after the policy, every journal
# Colors identify the period. If the shift is not significant (Holm-adjusted Wilcoxon), both
# periods are drawn in muted tints.
# ---------------------------

period.levels <- levels(delay$period)
sig.of <- function(j) {
  s <- main.tests$significant[main.tests$Journal==j]
  length(s) == 1 && isTRUE(s)
}
col.key <- function(period, sig) paste0(if_else(period==period.levels[1],"pre","post"),if_else(sig,"",".ns"))
col.values <- c("pre"=col.pre,"post"=col.post,"pre.ns"=col.pre.ns,"post.ns"=col.post.ns)

delay <- delay %>%
  mutate(sig=vapply(as.character(Journal),sig.of,logical(1)),
         key=col.key(period,sig),
         row=as.integer(period))       # row of points: 1 = before, 2 = after
d.per <- dens_of(delay,Journal,period) %>%
  left_join(delay %>% distinct(Journal,period,key),by=c("Journal","period"))
med.per <- delay %>% group_by(Journal,period,key) %>% summarize(med=median(months_to_pub),.groups="drop")

period.years <- function(a,b) if (a==b) as.character(a) else paste0(a,"-",b)
stat.label <- function(t) {
  if (is.na(t$p_wilcox)) return(paste0("No articles before the policy (n = ",t$n_post,")"))
  p <- if (t$p_wilcox_holm < 0.001) "p < 0.001" else paste0("p = ",sprintf("%.2f",t$p_wilcox_holm))
  paste0("Before: ",period.years(t$first_year,ttp.policy.year-1)," (n = ",t$n_pre,"); after: ",
         period.years(ttp.policy.year,t$last_year)," (n = ",t$n_post,")\n",
         "Median shift ",sprintf("%+.0f",t$shift)," months; mean ",sprintf("%+.1f",t$mean_diff),
         " [",sprintf("%.1f",t$mean_diff_lo),", ",sprintf("%.1f",t$mean_diff_hi),"]\n",
         "Wilcoxon ",p," (Holm)",
         if (t$significant) "" else "\nNo significant shift")
}
labels.per <- main.tests %>% mutate(Journal=factor(Journal,levels=journal.order)) %>%
  rowwise() %>% mutate(label=stat.label(pick(everything()))) %>% ungroup()

legend.labels <- c("pre"=period.levels[1],"post"=period.levels[2],
                   "pre.ns"=paste0(period.levels[1]," (no significant shift)"),
                   "post.ns"=paste0(period.levels[2]," (no significant shift)"))

fig.policy <- ggplot() +
  geom_point(data=delay,aes(months_to_pub,-(row-1+jit)*band/2,colour=key),alpha=0.55,size=0.8,stroke=0) +
  geom_line(data=d.per,aes(x,y,colour=key,group=period),alpha=0.7,linewidth=0.6) +
  geom_segment(data=med.per,aes(x=med,xend=med,y=0,yend=ymax.dens*0.9,colour=key),linewidth=0.5) +
  geom_text(data=labels.per,aes(x=xmax,y=ymax.dens,label=label),hjust=1,vjust=1,size=2.6,colour="grey30",lineheight=0.95) +
  facet_wrap(~ Journal,ncol=2) +
  scale_colour_manual(values=col.values,labels=legend.labels,breaks=names(col.values)) +
  guides(colour=guide_legend(nrow=2,override.aes=list(alpha=1,size=2.5,linewidth=1))) +
  x.scale +
  scale_y_continuous(limits=c(-band,ymax.dens*1.15),expand=expansion(add=0)) +
  labs(title=paste0("Time from submission to publication, before and after the ",ttp.policy.year," AEA data policy"),
       subtitle=paste0("By year of publication. Vertical lines: medians. Median shift: Hodges-Lehmann estimate (after minus before); mean: difference in means, Welch 95% CI.\n",
                       "Muted colors: no significant shift (Wilcoxon rank-sum, Holm-adjusted across journals, ",percent(ttp.alpha)," level). ",
                       "Points: upper row before, lower row after."),
       x="Time from submission to print publication",y=element_blank(),caption=caption.delay) +
  theme_delay()
ggsave(file.path(Figures,"figure_time_to_pub_policy.png"),fig.policy,width=9,height=9,dpi=300)
ggsave(file.path(Figures,"figure_time_to_pub_policy.pdf"),fig.policy,width=9,height=9)

# ---------------------------
# 3. One figure per journal: the journal highlighted, all other journals in grey
# Two panels: before and after the policy. Dashed: the journal's median before the policy.
# ---------------------------

fig_highlight <- function(j) {
  hl    <- delay %>% filter(Journal==j)
  sig   <- sig.of(j)
  t     <- labels.per %>% filter(Journal==j)
  d.oth <- d.per %>% filter(Journal!=j)
  d.hl  <- d.per %>% filter(Journal==j)
  med   <- med.per %>% filter(Journal==j)
  med.pre <- med %>% filter(period==period.levels[1])
  # the journal's colors: after the policy in the accent color only if the shift is significant
  hl.cols <- c("pre"=col.pre,"pre.ns"=col.pre,"post"=col.post,"post.ns"=col.pre.ns)
  p <- ggplot() +
    geom_line(data=d.oth,aes(x,y,group=Journal),colour=col.grey,linewidth=0.4) +
    geom_point(data=hl,aes(months_to_pub,-jit*band,colour=key),alpha=0.6,size=1.1,stroke=0) +
    geom_line(data=d.hl,aes(x,y,colour=key),alpha=0.8,linewidth=0.8) +
    geom_segment(data=med,aes(x=med,xend=med,y=0,yend=ymax.dens*0.9,colour=key),linewidth=0.6) +
    { if (nrow(med.pre)) geom_vline(xintercept=med.pre$med,colour=col.pre,linetype="dashed",linewidth=0.4) } +
    facet_wrap(~ period,ncol=1,drop=FALSE) +
    scale_colour_manual(values=hl.cols,guide="none") +
    x.scale +
    scale_y_continuous(limits=c(-band,ymax.dens*1.15),expand=expansion(add=0)) +
    labs(title=paste0(j,": time from submission to publication"),
         subtitle=paste0(if (nrow(t)) gsub("\n","; ",t$label) else "","\n",
                         "Colored: ",j," (points: articles; line: density; solid vertical: median; dashed: median before ",ttp.policy.year,"). ",
                         "Grey: other AEA journals."),
         x="Time from submission to print publication",y=element_blank(),caption=caption.delay) +
    theme_delay()
  file <- paste0("figure_time_to_pub_",str_to_lower(str_replace_all(j,"[^A-Za-z]+","_")))
  ggsave(file.path(Figures,paste0(file,".png")),p,width=8,height=6.5,dpi=300)
  ggsave(file.path(Figures,paste0(file,".pdf")),p,width=8,height=6.5)
  p
}

fig.highlight <- lapply(journal.order,fig_highlight)

# ---------------------------
# 4. Diagnostic: mean delay by year, by journal
# By publication year (the years analyzed above), and by submission year (all mapped cohorts).
# Submission years still being published miss their slowest papers, so their means are too low:
# those with coverage below 95% are drawn hollow. Grey: the other journals.
# ---------------------------

by_year <- function(df, yvar) {
  df %>% group_by(Journal,year={{ yvar }}) %>%
    summarize(n=n(),mean=mean(months_to_pub),median=median(months_to_pub),
              se=sd(months_to_pub)/sqrt(n),.groups="drop") %>%
    mutate(lo=mean-qt(0.975,pmax(n-1,1))*se,hi=mean+qt(0.975,pmax(n-1,1))*se)
}

ttp.pub <- by_year(delay %>% mutate(Journal=as.character(Journal)),year) %>%
  mutate(view="publication year",complete=TRUE)
ttp.sub <- by_year(ms %>% filter(mapped),sub_year) %>%
  left_join(cohorts %>% select(Journal,year=sub_year,coverage),by=c("Journal","year")) %>%
  mutate(view="submission year",complete=coverage >= 0.95) %>%
  select(-coverage)

bind_rows(ttp.pub,ttp.sub) %>%
  select(view,Journal,year,n,mean,lo,hi,median,complete) %>%
  mutate(across(c(mean,lo,hi,median),~ round(.x,1))) %>%
  write_csv(ttp.by.year.csv)

# the method part of the caption (no points, densities, or median shifts in these figures)
caption.method <- paste0(sub("\nTimes fall on a grid.*$","",caption.delay),
                         "\nExcluded: AEA P&P, JEL, JEP (no submission-based numbering).")

fig_by_year <- function(df, xlab, title, subtitle, note, file, policy.line=TRUE) {
  df <- df %>% mutate(Journal=factor(Journal,levels=journal.order))
  # every journal's series, drawn in grey in all other panels
  others <- tidyr::crossing(panel=factor(journal.order,levels=journal.order),df %>% rename(series=Journal)) %>%
    filter(panel!=series) %>% rename(Journal=panel)
  p <- ggplot(df,aes(year,mean)) +
    { if (policy.line) geom_vline(xintercept=ttp.policy.year-0.5,colour="grey40",linetype="dashed",linewidth=0.4) } +
    geom_line(data=others,aes(group=series),colour="grey82",linewidth=0.4) +
    geom_linerange(aes(ymin=lo,ymax=hi),colour=col.single,linewidth=0.5,alpha=0.6) +
    geom_line(colour=col.single,linewidth=0.6) +
    geom_point(aes(shape=complete),colour=col.single,fill="white",size=2) +
    geom_text(aes(y=0,label=n),size=2.3,colour="grey45",vjust=0) +
    scale_shape_manual(values=c("TRUE"=16,"FALSE"=21),guide="none") +
    facet_wrap(~ Journal,ncol=3) +
    scale_x_continuous(breaks=seq(2010,2030,2)) +
    scale_y_continuous(limits=c(0,NA),breaks=seq(0,96,12),labels=function(x) paste0(x/12," yr"),
                       expand=expansion(mult=c(0,0.05))) +
    labs(title=title,subtitle=subtitle,x=xlab,y="Mean time to publication",
         caption=paste0(note,caption.method)) +
    theme_delay() +
    theme(axis.text.y=element_text(),axis.line.y=element_line(),axis.ticks.y=element_line(),
          panel.grid.major.y=element_line(colour="grey92",linewidth=0.3),
          panel.grid.major.x=element_blank())
  ggsave(file.path(Figures,paste0(file,".png")),p,width=10,height=6.5,dpi=300)
  ggsave(file.path(Figures,paste0(file,".pdf")),p,width=10,height=6.5)
  p
}

fig.pubyear <- fig_by_year(ttp.pub,"Year of publication",
  "Mean time from submission to publication, by year of publication",
  paste0("Mean and 95% CI; numbers: articles. Dashed line: ",ttp.policy.year," AEA-wide policy. Grey: other journals.\n",
         "Only publication years with at least ",percent(ttp.min.pubyear)," of articles from mapped submission years."),
  "",
  "figure_time_to_pub_by_pubyear")

fig.subyear <- fig_by_year(ttp.sub,"Year of submission",
  "Mean time from submission to publication, by year of submission",
  paste0("Mean and 95% CI; numbers: articles. Grey: other journals. Hollow: submission years with less than 95% of their\n",
         "published articles observed; recent years miss their slowest papers, so their means are too low."),
  paste0("Papers accepted after mid-2019 were reviewed by the Data Editor; given the times to publication, that includes many papers submitted from about 2016 on.\n"),
  "figure_time_to_pub_by_subyear",policy.line=FALSE)

# ---------------------------
# 5. Delay by whether the article has a replication package, by journal and period
# Columns: before and after the policy; rows: journals. Blue: with a package; grey: without.
# ---------------------------

col.pkg   <- "#1B4F8A"   # with a package (as in 05_figures.R)
col.nopkg <- "grey50"    # without a package
pkg.levels <- c("With a replication package","Without")

ttp.pkg <- delay %>%
  mutate(pkg=factor(if_else(has_package,pkg.levels[1],pkg.levels[2]),levels=pkg.levels),
         row=as.integer(pkg))
d.pkg <- dens_of(ttp.pkg,Journal,period,pkg)
med.pkg <- ttp.pkg %>% group_by(Journal,period,pkg) %>%
  summarize(med=median(months_to_pub),.groups="drop")
labels.pkg <- by.package %>%
  filter(Journal %in% journal.order, period!="All years") %>%
  mutate(Journal=factor(as.character(Journal),levels=journal.order),
         period=factor(as.character(period),levels=period.levels),
         label=paste0("n = ",n_package," / ",n_no_package,
                      if_else(is.na(mean_diff),"",
                              paste0("\nmean ",sprintf("%+.1f",mean_diff)," [",sprintf("%.1f",mean_diff_lo),", ",
                                     sprintf("%.1f",mean_diff_hi),"]",
                                     if_else(significant,"","\nn.s.")))))

fig.pkg <- ggplot() +
  geom_point(data=ttp.pkg,aes(months_to_pub,-(row-1+jit)*band/2,colour=pkg),alpha=0.5,size=0.7,stroke=0) +
  geom_line(data=d.pkg,aes(x,y,colour=pkg),alpha=0.8,linewidth=0.6) +
  geom_segment(data=med.pkg,aes(x=med,xend=med,y=0,yend=ymax.dens*0.9,colour=pkg),linewidth=0.5) +
  geom_text(data=labels.pkg,aes(x=xmax,y=ymax.dens,label=label),hjust=1,vjust=1,size=2.4,colour="grey30",lineheight=0.95) +
  facet_grid(Journal ~ period) +
  scale_colour_manual(values=setNames(c(col.pkg,col.nopkg),pkg.levels)) +
  guides(colour=guide_legend(override.aes=list(alpha=1,size=2.5,linewidth=1))) +
  x.scale +
  scale_y_continuous(limits=c(-band,ymax.dens*1.15),expand=expansion(add=0)) +
  labs(title="Time from submission to publication, by whether the article has a replication package",
       subtitle=paste0("Vertical lines: medians. n: articles with / without a package. Mean: difference in means (with minus without), Welch 95% CI;\n",
                       "n.s.: no significant difference (Wilcoxon rank-sum, Holm-adjusted across journals, ",percent(ttp.alpha),
                       " level). Points: upper row with a package, lower row without."),
       x="Time from submission to print publication",y=element_blank(),caption=caption.delay) +
  theme_delay() +
  theme(strip.text.y=element_text(angle=0,hjust=0))
ggsave(file.path(Figures,"figure_time_to_pub_by_package.png"),fig.pkg,width=10,height=11,dpi=300)
ggsave(file.path(Figures,"figure_time_to_pub_by_package.pdf"),fig.pkg,width=10,height=11)
