options(scipen=999) # to prevent scientific notation (I will explain it in detail later)
library(readxl) # to read excel files
library(dplyr)  # for data analysis purposes
library(tidyverse) # for data analysis purposes
library(ggplot2) # to make visualisations
library(viridis) # to make visualisations
library(sp)      # to read geospatial data


setwd("~/Desktop")
foreign_currency_accounts <- read_excel("~/Desktop/PivotGrid-2.xlsx")
tur_spatial <- readRDS("~/Desktop/gadm36_TUR_1_sp.rds")

head(foreign_currency_accounts)
colnames(foreign_currency_accounts)
colnames(foreign_currency_accounts)[2] <- "cities"
foreign_currency_accounts_2010_2020 <-  foreign_currency_accounts %>%
  dplyr::select("cities", "2010","2020") %>%
  pivot_longer(!"cities", names_to = "years", values_to = "account_numbers")

head(foreign_currency_accounts_2010_2020)
tur_spatial_fort <- fortify(tur_spatial)
head(tur_spatial_fort)
head(tur_spatial@data)


foreign_currency_accounts_2010_2020$cities <-
  tolower(foreign_currency_accounts_2010_2020$cities)

foreign_currency_accounts_2010_2020$cities <-
  str_to_title(foreign_currency_accounts_2010_2020$cities)

a <- unique(tur_spatial@data$NAME_1)
b <- unique(foreign_currency_accounts_2010_2020$cities)
setdiff(a,b)


to.plain <- function(s) {
  old1 <- "şŞığ"
  new1 <- "sSig"
  s1 <- chartr(old1, new1, s)
} 

s <- foreign_currency_accounts_2010_2020$cities
s1 <- to.plain(s)

s2 <- tur_spatial@data$NAME_1
s3 <- to.plain(s2)

foreign_currency_accounts_2010_2020$cities <- s1
tur_spatial@data$NAME_1 <- s3

a <- unique(tur_spatial@data$NAME_1)
b <- unique(foreign_currency_accounts_2010_2020$cities)

setdiff(a,b)

foreign_currency_accounts_2010_2020$cities[
  which(foreign_currency_accounts_2010_2020$cities == "Afyonkarahisar")] <- "Afyon"
foreign_currency_accounts_2010_2020$cities[
  which(foreign_currency_accounts_2010_2020$cities == "Kahramanmaras")] <- "K. Maras"
foreign_currency_accounts_2010_2020$cities[
  which(foreign_currency_accounts_2010_2020$cities == "Kirikkale")] <- "Kinkkale"
foreign_currency_accounts_2010_2020$cities[
  which(foreign_currency_accounts_2010_2020$cities == "Içel")] <- "Mersin"
foreign_currency_accounts_2010_2020$cities[
  which(foreign_currency_accounts_2010_2020$cities == "Zonguldak")] <- "Zinguldak"

a <- unique(tur_spatial@data$NAME_1)
b <- unique(foreign_currency_accounts_2010_2020$cities)

setdiff(a,b)


colnames(foreign_currency_accounts_2010_2020)[1] <- "NAME_1"

head(foreign_currency_accounts_2010_2020)

merged_raw <- data_frame(id = rownames(tur_spatial@data),
                         NAME_1 = tur_spatial@data$NAME_1) %>% 
  left_join(foreign_currency_accounts_2010_2020, by = "NAME_1")
merged_raw %>% head()

merged<- left_join(tur_spatial_fort, merged_raw, by = "id")
merged %>% head()

merged$periods <- merged$years
merged$periods <- as.factor(merged$periods)


quantile(merged$account_numbers, probs = seq(0, 1, 0.20), na.rm = FALSE)



p <- ggplot() +
  geom_polygon(data = merged, aes(fill = account_numbers, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(trans = "log", breaks=c(6187,97911,200608,412031,759047,9218687), name="Total Account Number", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  labs(
    title = "Number of Foreign Currency Accounts in Banks by Province in Turkey",
    subtitle = "2010 vs. 2020",
    caption = "Data: Banks Association of Turkey | Muhammet Ozkaraca"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 15, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text(size=9, color = "#4e4d47", margin = margin(b = 0.6, r=-1, unit = "mm") ),
    legend.position = c(0.22, -0.15)
  ) +
  coord_map() +
  facet_wrap(~ periods, nrow = 1)

p

