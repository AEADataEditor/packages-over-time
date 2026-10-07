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

col.packages  <- "#2F6BAD"
col.benchmark <- "grey75"

caption.text <- paste0("Research articles: Crossref, excluding front matter, book reviews, and editorial/administrative notes.\n",
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
    fill.scale + alpha.scale +
    scale_x_continuous(breaks=seq(firstyear,lastyear,5),expand=expansion(add=0.6)) +
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
    do.call(geom_col_pattern,c(list(data=~ filter(.x,series=="All research articles"),
                                    mapping=aes(pattern=pattern)),bar(0.85))) +
    do.call(geom_col_pattern,c(list(data=~ filter(.x,series!="All research articles"),
                                    mapping=aes(pattern=pattern)),bar(0.55))) +
    geom_hline(yintercept=refline,colour="red",linewidth=0.5) +
    facet_wrap(~ Journal,ncol=ncol) +
    fill.scale + alpha.scale +
    scale_pattern_manual(values=c("none"="none","stripe"="stripe"),guide="none") +
    guides(fill=guide_legend(override.aes=list(pattern="none"))) +
    scale_x_continuous(limits=c(firstyear-0.5,lastyear+0.5),breaks=seq(firstyear,lastyear,5),
                       expand=expansion(add=0.2)) +
    scale_y_continuous(labels=label_comma(),limits=c(0,ymax),expand=expansion(mult=c(0,0.02)),
                       sec.axis=dup_axis()) +
    labs(title=title,subtitle=subtitle,x=element_blank(),y=element_blank(),
         caption=paste0(note,caption.text)) +
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
       subtitle="Both panels share one vertical scale; red line at 50 articles. Hatched bars: P&P articles printed in the May issue of the AER, 2000-2017.",
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
