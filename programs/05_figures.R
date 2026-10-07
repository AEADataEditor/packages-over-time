# Figures: replication packages over time, overall and by journal
# Total research articles are shown as a faint benchmark
#

source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)

# filenames in config.R

by.year    <- read_csv(by_year.csv)    %>% filter(between(year,firstyear,lastyear))
by.journal <- read_csv(by_journal.csv) %>% filter(between(year,firstyear,lastyear))

journal.order <- c("AER","AER:Insights","AEA P&P",
                   "AEJ:Applied Economics","AEJ:Economic Policy","AEJ:Macro","AEJ:Micro",
                   "JEL","JEP")
by.journal <- by.journal %>% mutate(Journal=factor(Journal,levels=journal.order))

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
# By journal
# ===========================

by.journal %>%
  to_long() %>%
  ggplot(aes(year,n,fill=series,alpha=series)) +
    geom_col(data=~ filter(.x,series=="All research articles"),width=0.85) +
    geom_col(data=~ filter(.x,series!="All research articles"),width=0.55) +
    facet_wrap(~ Journal,ncol=3,scales="free_y") +
    fill.scale + alpha.scale +
    scale_x_continuous(limits=c(firstyear-0.5,lastyear+0.5),breaks=seq(firstyear,lastyear,10),
                       expand=expansion(add=0.2)) +
    scale_y_continuous(labels=label_comma(),expand=expansion(mult=c(0,0.05))) +
    labs(title="Replication packages in AEA journals, by journal and year",
         subtitle="Vertical scales differ across panels. AEA P&P is published separately since 2018; earlier, it was the May issue of the AER.",
         x=element_blank(),y=element_blank(),caption=caption.text) +
    theme_packages() +
    theme(strip.background = element_blank(),
          strip.text = element_text(face="bold",hjust=0),
          plot.subtitle = element_text(colour="grey30",size=8.5),
          panel.spacing.x = unit(1,"lines")) -> fig.packages.journal

ggsave(file.path(Figures,"figure_packages_by_journal.png"),fig.packages.journal,width=10,height=8,dpi=300)
ggsave(file.path(Figures,"figure_packages_by_journal.pdf"),fig.packages.journal,width=10,height=8)
