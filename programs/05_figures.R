# Figures: replication packages over time, overall and by journal
# Total research articles are shown as a faint benchmark
#

source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)

# filenames in config.R

by.year    <- read_csv(by_year.csv)    %>% filter(between(year,firstyear,lastyear))

journal.order <- c("AER","AER:Insights","AEA P&P",
                   "AEJ:Applied Economics","AEJ:Economic Policy","AEJ:Macro","AEJ:Micro",
                   "JEL","JEP")

# palette: packages in deep blue against a light grey benchmark; years under a data policy
# have a soft green background; policy changes in slate; period averages in muted red
col.packages  <- "#1B4F8A"
col.benchmark <- "grey78"
col.policy    <- "#E1EEE3"
col.vline     <- "#44546A"
col.average   <- "#C8372D"

# Policy changes: AER data policy (2005, announced 2004), launch of the AEJs (2009), AEA-wide policy (2019)
policy.lines <- c("2005: AER data policy"=2005,"2009: AEJs launched"=2009,"2019: AEA-wide policy"=2019)
vlines <- function() geom_vline(xintercept=unname(policy.lines)-0.5,colour=col.vline,linetype="dashed",linewidth=0.4)

# Background for the years in which a journal was covered by a data policy (one rectangle per panel)
policy_shade <- function(journals) {
  geom_rect(data=tibble(Journal=factor(journals,levels=journal.order),
                        xmin=policy.year(journals)-0.5, xmax=lastyear+0.5),
            aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf),
            inherit.aes=FALSE,fill=col.policy)
}

shade.note <- "Green background: years in which the journal was covered by a data and code availability policy.\n"

caption.text <- paste0("Dashed vertical lines: 2005 (AER data policy), 2009 (AEJs launched), 2019 (AEA-wide policy).\n","Research articles: Crossref, excluding front matter, book reviews, and editorial/administrative notes.\n",
                       "Replication packages: AEA Data and Code Repository at openICPSR (DataCite links and titles, article references, migrated supplements).\n",
                       "By year of print publication, ",firstyear,"-",lastyear,".")

theme_packages <- function() {
  theme_classic() +
    theme(axis.line = element_line(linewidth=0),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_line(colour="grey92",linewidth=0.3),
          legend.position = "top",
          legend.justification = "left",
          legend.title = element_blank(),
          plot.caption = element_text(hjust=0,colour="grey40",size=7),
          plot.title.position = "plot",
          plot.caption.position = "plot")
}

# long format, so that the legend identifies both series
to_long <- function(df) {
  df %>%
    select(any_of("Journal"),year,articles,articles_with_package) %>%
    pivot_longer(c(articles,articles_with_package),names_to="series",values_to="n") %>%
    mutate(series=factor(series,levels=c("articles","articles_with_package"),
                         labels=c("All research articles","Articles with a replication package")))
}

fill.scale <- scale_fill_manual(values=c("All research articles"=col.benchmark,
                                         "Articles with a replication package"=col.packages))
alpha.scale <- scale_alpha_manual(values=c("All research articles"=0.45,
                                           "Articles with a replication package"=1),
                                  guide="none")

# ===========================
# Overall
# ===========================

by.year %>%
  to_long() %>%
  ggplot(aes(year,n,fill=series,alpha=series)) +
    # benchmark drawn first and wider, packages in front
    geom_col(data=~ filter(.x,series=="All research articles"),width=0.85) +
    geom_col(data=~ filter(.x,series!="All research articles"),width=0.55) +
    vlines() +
    fill.scale + alpha.scale +
    scale_x_continuous(breaks=seq(2000,lastyear,5),expand=expansion(add=0.6)) +
    scale_y_continuous(labels=label_comma(),expand=expansion(mult=c(0,0.05))) +
    labs(title="Replication packages in AEA journals, by year",
         x=element_blank(),y=element_blank(),caption=caption.text) +
    theme_packages() -> fig.packages.year

ggsave(file.path(Figures,"figure_packages_by_year.png"),fig.packages.year,width=8,height=5,dpi=300)
ggsave(file.path(Figures,"figure_packages_by_year.pdf"),fig.packages.year,width=8,height=5)

# ===========================
# By journal, with the P&P re-attributed
# Until 2017, the P&P was the May issue of the AER. These articles are shown under
# AEA P&P (hatched), and removed from the AER panel.
# Two figures: AER and AEA P&P; all other journals. Panels within a figure share
# one vertical scale, repeated on the right, with a red reference line.
# ===========================

by.journal.pp <- read_csv(by_journal_pp.csv) %>%
  filter(between(year,firstyear,lastyear)) %>%
  mutate(Journal=factor(Journal,levels=journal.order))

pp.note <- paste0("Note: The AEA Papers & Proceedings was the May issue of the AER through volume 107 (2017). Those articles are attributed to AEA P&P here,\n",
                  "and are not counted in the AER panel; they are shown hatched. Since 2018, the P&P is published separately.")

by.journal.pp %>%
  select(Journal,year,pp_in_aer,articles,articles_with_package) %>%
  pivot_longer(c(articles,articles_with_package),names_to="series",values_to="n") %>%
  mutate(series=factor(series,levels=c("articles","articles_with_package"),
                       labels=c("All research articles","Articles with a replication package")),
         pattern=if_else(pp_in_aer,"stripe","none")) -> long.pp

# one figure: journals, common y-axis, red reference line, scale repeated on the right
fig_pp <- function(data, journals, ymax, refline, ncol, title, subtitle, note, file, width, height) {
  df <- data %>% filter(Journal %in% journals) %>% mutate(Journal=droplevels(Journal))
  bar <- function(w) list(width=w, pattern_fill="white", pattern_colour="white",
                          pattern_density=0.35, pattern_spacing=0.025, pattern_angle=45, colour=NA)
  p <- ggplot(df,aes(year,n,fill=series,alpha=series)) +
    policy_shade(levels(df$Journal)) +
    do.call(geom_col_pattern,c(list(data=~ filter(.x,series=="All research articles"),
                                    mapping=aes(pattern=pattern)),bar(0.85))) +
    do.call(geom_col_pattern,c(list(data=~ filter(.x,series!="All research articles"),
                                    mapping=aes(pattern=pattern)),bar(0.55))) +
    vlines() +
    geom_hline(yintercept=refline,colour=col.average,linewidth=0.5) +
    facet_wrap(~ Journal,ncol=ncol) +
    fill.scale + alpha.scale +
    scale_pattern_manual(values=c("none"="none","stripe"="stripe"),guide="none") +
    guides(fill=guide_legend(override.aes=list(pattern="none"))) +
    scale_x_continuous(limits=c(firstyear-0.5,lastyear+0.5),breaks=seq(2000,lastyear,5),
                       expand=expansion(add=0.2)) +
    scale_y_continuous(labels=label_comma(),limits=c(0,ymax),expand=expansion(mult=c(0,0.02)),
                       sec.axis=dup_axis()) +
    labs(title=title,subtitle=subtitle,x=element_blank(),y=element_blank(),
         caption=paste0(note,shade.note,caption.text)) +
    theme_packages() +
    theme(strip.background = element_blank(),
          strip.text = element_text(face="bold",hjust=0),
          plot.subtitle = element_text(colour="grey30",size=8.5),
          panel.spacing.x = unit(1.5,"lines"))
  ggsave(file.path(Figures,paste0(file,".png")),p,width=width,height=height,dpi=300)
  ggsave(file.path(Figures,paste0(file,".pdf")),p,width=width,height=height)
  p
}

fig_pp(long.pp, c("AER","AEA P&P"), ymax=150, refline=50, ncol=2,
       title="AER and AEA Papers & Proceedings, by year",
       subtitle="Both panels share one vertical scale; red line at 50 articles. Hatched bars: P&P articles printed in the May issue of the AER, 1999-2017.",
       note=paste0(pp.note,"\n"),
       file="figure_packages_aer_pp", width=10, height=5) -> fig.aer.pp

fig_pp(long.pp, setdiff(journal.order,c("AER","AEA P&P")), ymax=90, refline=30, ncol=3,
       title="Other AEA journals, by year",
       subtitle="All panels share one vertical scale; red line at 30 articles. AER and AEA P&P are shown separately.",
       note="",
       file="figure_packages_other_journals", width=10, height=8) -> fig.other

# AER and P&P as originally attributed by Crossref (no re-allocation):
# the May issue of the AER, through 2017, stays in the AER; the P&P starts in 2018.
read_csv(by_journal.csv) %>%
  filter(between(year,firstyear,lastyear)) %>%
  mutate(Journal=factor(Journal,levels=journal.order)) %>%
  select(Journal,year,articles,articles_with_package) %>%
  pivot_longer(c(articles,articles_with_package),names_to="series",values_to="n") %>%
  mutate(series=factor(series,levels=c("articles","articles_with_package"),
                       labels=c("All research articles","Articles with a replication package")),
         pattern="none") -> long.orig

fig_pp(long.orig, c("AER","AEA P&P"), ymax=260, refline=50, ncol=2,
       title="AER and AEA Papers & Proceedings, by year, as published",
       subtitle="Both panels share one vertical scale; red line at 50 articles.",
       note=paste0("Note: Articles are attributed to the journal in which they were published. Through 2017, the AEA Papers & Proceedings was the May issue\n",
                   "of the AER and is counted in the AER panel; the P&P panel starts in 2018, when it became a separate journal.\n"),
       file="figure_packages_aer_pp_asis", width=10, height=5) -> fig.aer.pp.asis

# ===========================
# Share of articles with a replication package, with policy periods
# Vertical lines: AER data policy (2005; announced 2004), AEA-wide policy (2019).
# Horizontal lines: share of articles with a package, pooled over each period.
# ===========================

article.db <- read_csv(article.db.csv,show_col_types=FALSE) %>%
  filter(between(year,firstyear,lastyear))

# df: articles; lines: named years where a policy took effect (dashed vertical lines).
# Red horizontal lines: share with a package, pooled within the periods the lines define.
fig_share <- function(df, policy.from, title, subtitle, note, file, lines=policy.lines) {
  by.yr <- df %>% group_by(year) %>% summarize(share=mean(has_package),.groups="drop")
  by.per <- df %>%
    mutate(period=cut(year,breaks=c(-Inf,unname(lines)-1,Inf),labels=FALSE)) %>%
    group_by(period) %>%
    summarize(xmin=min(year)-0.5,xmax=max(year)+0.5,share=mean(has_package),.groups="drop")
  p <- ggplot(by.yr,aes(year,share)) +
    annotate("rect",xmin=policy.from-0.5,xmax=lastyear+0.5,ymin=-Inf,ymax=Inf,fill=col.policy) +
    geom_vline(xintercept=unname(lines)-0.5,colour=col.vline,linetype="dashed") +
    # labels alternate sides, so that the closely spaced 2005 and 2009 lines do not collide
    annotate("text",x=unname(lines)-0.5,y=c(1.06,1.06,0.96)[seq_along(lines)],size=3,colour=col.vline,label=names(lines),
             hjust=rep(c(1.05,-0.05),length.out=length(lines))) +
    geom_line(colour=col.packages,linewidth=0.5) +
    geom_point(colour=col.packages,size=1.6) +
    geom_segment(data=by.per,aes(x=xmin,xend=xmax,y=share,yend=share),
                 inherit.aes=FALSE,colour=col.average,linewidth=0.9) +
    geom_label(data=by.per,aes(x=(xmin+xmax)/2,y=share,label=percent(share,accuracy=1)),
               inherit.aes=FALSE,colour=col.average,fill="white",label.size=0,size=3,vjust=-0.4) +
    scale_x_continuous(breaks=seq(2000,lastyear,5),limits=c(firstyear-0.5,lastyear+0.5),
                       expand=expansion(add=0.2)) +
    scale_y_continuous(labels=percent_format(accuracy=1),limits=c(0,1.08),breaks=seq(0,1,0.25),
                       expand=expansion(mult=c(0,0))) +
    labs(title=title,subtitle=subtitle,x=element_blank(),y=element_blank(),
         caption=paste0(note,shade.note,caption.text)) +
    theme_packages()
  ggsave(file.path(Figures,paste0(file,".png")),p,width=8,height=5,dpi=300)
  ggsave(file.path(Figures,paste0(file,".pdf")),p,width=8,height=5)
  p
}

red.note <- paste0("Red lines: share of articles with a package, pooled over ",firstyear,"-2004, 2005-2008, 2009-2018, and 2019-",lastyear,".\n")

# AER (without the P&P printed in the May issue): policy in 2005, AEA-wide policy in 2019
fig_share(article.db %>% filter(Journal=="AER"), policy.from=policy.year("AER"),
          title="Share of AER articles with a replication package, 1999-2025",
          subtitle="AER excluding the P&P printed in the May issue (through 2017).",
          note=red.note,
          file="figure_share_aer") -> fig.share.aer

# AEJs (Applied, Policy, Macro, Micro): data policy since their creation in 2009
fig_share(article.db %>% filter(str_starts(Journal,"AEJ:")), policy.from=policy.year("AEJ:Macro"),
          title="Share of AEJ articles with a replication package, 2009-2025",
          subtitle="AEJ: Applied, Policy, Macro, and Micro. Data policy in force since their creation in 2009.",
          note=red.note,
          file="figure_share_aej") -> fig.share.aej

# Other AEA journals (AER:Insights, JEL, JEP): no mandate before 2019
fig_share(article.db %>% filter(Journal %in% c("AER:Insights","JEL","JEP")), policy.from=policy.year("JEL"),
          title="Share of articles with a replication package: AER:Insights, JEL, and JEP",
          subtitle="Journals other than the AER, AEJs, and P&P. No data policy before 2019 (AER:Insights began in 2019).",
          note=red.note,
          file="figure_share_others") -> fig.share.others

# P&P, including the issues printed as the May AER through 2017. Not covered by a policy before 2019.
fig_share(article.db %>% filter(Journal=="AEA P&P"), policy.from=policy.year("AEA P&P"),
          title="Share of AEA Papers & Proceedings articles with a replication package, 1999-2025",
          subtitle="Includes the P&P printed as the May AER through 2017. Not covered by a data policy before 2019.",
          note=red.note,
          file="figure_share_pp") -> fig.share.pp
