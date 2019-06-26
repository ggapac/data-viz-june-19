# Libraries --------------------------------------------------------------------
library(RColorBrewer)
library(maptools)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggpubr)


# Functions --------------------------------------------------------------------

rename_countries <- function(dat) {
  # Rename countries according to wrld_simpl data
  
  dat$Country <- as.character(dat$Country)
  
  dat$Country[dat$Country == "Libya"] <- "Libyan Arab Jamahiriya"
  dat$Country[dat$Country == "Ivory Coast"] <- "Cote d'Ivoire"
  dat$Country[dat$Country == "South Korea"] <- "Korea, Republic of"
  dat$Country[dat$Country == "Moldova"] <- "Republic of Moldova"
  dat$Country[dat$Country == "Vietnam"] <- "Viet Nam"
  dat$Country[dat$Country == "Macedonia"] <- "The former Yugoslav Republic of Macedonia"
  dat$Country[dat$Country == "Laos"] <- "Lao People's Democratic Republic"
  dat$Country[dat$Country == "Iran"] <- "Iran (Islamic Republic of)"
  dat$Country[dat$Country == "Myanmar"] <- "Burma"
  dat$Country[dat$Country == "Tanzania"] <- "United Republic of Tanzania"
  dat$Country[dat$Country == "Syria"] <- "Syrian Arab Republic"
  dat$Country[dat$Country == "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
  dat$Country[dat$Country == "Congo (Brazzaville)"] <- "Congo"
  dat$Country[dat$Country == "Palestinian Territories"] <- "Palestine"
  dat$Country[dat$Country == "Taiwan Province of China"] <- "Taiwan"
  dat$Country[dat$Country == "Hong Kong S.A.R., China"] <- "Hong Kong"
  
  dat <- dat[!(dat$Country %in% c("Kosovo", "North Cyprus",
                                  "South Sudan", "Somaliland Region",
                                  "Somaliland region")), ]
  
  dat$Country <- as.factor(dat$Country)
  dat
}

world_plot_per_year <- function(dat, wrld, wrld_data, world_map, year) {

  gg <- world_map +
    geom_map(data=dat, map=wrld, aes(map_id=Country, fill=Happiness.Score),
             color="white", size=0.25)
  pal <- colorRampPalette(brewer.pal(7, 'YlGn'))(100)
  palSz <- 100
  gg <- gg + scale_fill_gradient2(low = pal[1],
                                  mid = pal[palSz/2],
                                  high = pal[palSz],
                                  midpoint = (max(dat$Happiness.Score) +
                                                min(dat$Happiness.Score)) / 2,
                                  name = "Happiness Score") +
    ggtitle(year) + coord_fixed() +
    theme(legend.position = "bottom")
  ggsave(paste0("./Plots/Happiness_score_", year, ".png"), gg,
         width = 9, height = 6, units = "in")
}

std <- function(x) sd(x)/sqrt(length(x))

factors_per_region <- function(dat, year) {

  dat <- dat[dat$Year == year, ] %>% group_by(Region, Factor) %>%
    summarise(meanValue = mean(perc), se = std(perc))
  
  p <- ggplot(dat, aes(x = Region, y = meanValue, fill = Region)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = meanValue - se, ymax = meanValue + se),
                  width = .5) +
    facet_wrap(.~Factor, nrow = 1) + theme_light() + labs(y = "%") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = "bottom") +
    scale_fill_manual(values = colorRampPalette(brewer.pal(10, 'Paired'))(10),
                      name="Region") + ggtitle(year)
  
  ggsave(paste0("./Plots/Factors_per_region_", year, ".png"), p,
         width = 10, height = 4, units = "in")
}

plot_country_data <- function(dat, country) {
  
  dat <- dat[dat$Country == country, ]
  region <- unique(dat$Region)
  
  # Create a quick info table.
  hs <- c()
  ranks <- c()
  
  for(year in unique(dat$Year)) {
    hs <- c(hs, round(unique(dat$HappinessScore[dat$Year == year]), 2))
    ranks <- c(ranks, unique(dat$Rank[dat$Year == year]))
  }
  
  info_table <- data.frame("HappinessScore" = hs,
                           "Rank" = ranks)
  rownames(info_table) <- unique(dat$Year)
  
  dat <- dat %>% group_by(Year) %>% mutate(perc = Value / sum(Value)) %>%
    arrange(Factor) %>% mutate(perc_cum = cumsum(perc) - 0.5*perc) %>% ungroup
  
  # Tile plot showing importance of each of the factors per year.
  p1 <- ggplot(dat, aes(x = perc_cum, y = Year, fill = Factor)) +
    geom_tile(aes(width = perc), colour = "white") +
    geom_text(aes(label = sprintf("%1.1f", 100 * perc)), size = 3,
              colour = "white") +
    theme_minimal() + guides(fill = guide_legend(nrow = 1)) + labs(x = "%") +
    scale_fill_manual(values = colorRampPalette(brewer.pal(10, 'Paired'))(9),
                      name="Factor") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom") +
    scale_y_discrete(limits = rev(levels(dat$Year)))
  
  vals <- colorRampPalette(brewer.pal(10, 'Paired'))(9)
  dat$Color <- vals[1]
  for(i in 2:length(unique(dat$Factor))) {
    dat$Color[dat$Factor == unique(dat$Factor)[i]] <- vals[i]
  }
  
  # Histogram showing factors' values per year.
  p2 <- ggplot(dat, aes(x = Factor, y = Value, fill = Factor)) +
    geom_bar(stat="identity", position = "dodge", aes(alpha = Year)) +
    scale_alpha_discrete(range = c(0.4, 1)) +
    scale_fill_manual(values = unique(dat$Color)) + theme_light() +
    theme(axis.title.x = element_blank()) +
    guides(fill = FALSE)
  
  TSpecial <- ttheme_minimal(
    core = list(bg_params = list(fill = c("#D3D3D3", "#A9A9A9", "#808080"),
                                 col=NA),
              fg_params = list(fontface = 3)))
  
  p <- grid.arrange(tableGrob(info_table, theme = TSpecial), p1, p2,
                    heights = c(1, 2, 3), nrow = 3)
  annotate_figure(p, fig.lab = paste0(country, ", ", region))
  
  ggsave(paste0("./Plots/Country_summaries/", country, ".png"), width = 9,
         height = 8, units = "in")
}


# Script -----------------------------------------------------------------------

# Read data
dat2015 <- read.csv("2015.csv", header = T)
dat2016 <- read.csv("2016.csv", header = T)
dat2017 <- read.csv("2017.csv", header = T)


# Add Region to dat2017 
dat2017$Country <- as.character(dat2017$Country)
dat2016$Country <- as.character(dat2016$Country)
dat2016$Region <- as.character(dat2016$Region)

region17 <- vector(mode = "character", nrow(dat2017))
check <- c()

for(i in 1:nrow(dat2017)) {
  if(length(which(dat2016$Country == dat2017$Country[i])) > 0) {
    region17[i] <- dat2016$Region[dat2016$Country == dat2017$Country[i]]
  } else {
    check <- c(check, i)
  }
}

region17[33] <- "Eastern Asia" 
region17[71] <- "Eastern Asia"  
region17[113] <- "Sub-Saharan Africa"
region17[139] <- "Sub-Saharan Africa"
region17[155] <- "Sub-Saharan Africa"
dat2017$Region <- region17


# Rename countries according to wrld_simpl data 
dat2015 <- rename_countries(dat2015)
dat2016 <- rename_countries(dat2016)
dat2017 <- rename_countries(dat2017)


# Join data, rename columns
cols <- c("Country", "Region", "Happiness.Rank", "Happiness.Score",
          "Economy..GDP.per.Capita.", "Family", "Health..Life.Expectancy.",
          "Freedom", "Trust..Government.Corruption.", "Generosity",
          "Dystopia.Residual")
df <- dat2015[, colnames(dat2015) %in% cols] %>% mutate(Year = 2015)
df <- rbind(df, dat2016[, colnames(dat2016) %in% cols] %>% mutate(Year = 2016))
df <- rbind(df, dat2017[, colnames(dat2017) %in% cols] %>% mutate(Year = 2017))
colnames(df) <- c("Country", "Region", "Rank", "HappinessScore", "Economy",
                  "Family", "Health", "Freedom", "Trust", "Generosity",
                  "DystopiaResidual", "Year")


# World plot, plot happiness score per year
data("wrld_simpl")
wrld_data <- wrld_simpl@data
wrld_simpl@data$id <- wrld_simpl@data$NAME
wrld <- fortify(wrld_simpl, region="id")
wrld <- subset(wrld, id != "Antarctica")

world_map <- ggplot() +
  geom_map(aes(map_id = id, x = long, y = lat), data = wrld, map = wrld,
           fill = "white", color="#7f7f7f", size=0.25) + theme_void()

world_plot_per_year(dat2015, wrld, wrld_data, world_map, "2015")
world_plot_per_year(dat2016, wrld, wrld_data, world_map, "2016")
world_plot_per_year(dat2017, wrld, wrld_data, world_map, "2017")


# Plot factors per region per year
df_long <- gather(df, "Factor", "Value", Economy:DystopiaResidual, 
                  factor_key = T)
df_long$Year <- as.factor(df_long$Year)
df_long$perc <- 100*(df_long$Value / df_long$HappinessScore)

for(year in levels(df_long$Year)) {
  factors_per_region(df_long, year)
}


# Country summaries
vals <- colorRampPalette(brewer.pal(10, 'Paired'))(9)
df_long$Color <- vals[1]
for(i in 2:length(unique(df_long$Factor))) {
  df_long$Color[df_long$Factor == unique(df_long$Factor)[i]] <- vals[i]
}

for(country in unique(df_long$Country)) {
  print(country)
  plot_country_data(df_long, country)
}
