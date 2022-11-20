



library(tidyverse)
library(sf)
library(Cairo)
library(janitor)
library(ggrepel)
library(maps)
library(extrafont)
library(rnaturalearth) # for downloading dateline
library(rgdal)         # support for rnatural earth
library(rsdmx)
library(ISOcodes)
library(scales)
library(RColorBrewer)
library(glue)

#-----------------------land borders-------------------

# make two worlds together for drawing pacific-centred, adapted from
# https://stackoverflow.com/questions/34011100/plot-pacific-ocean-and-continents-with-ggplot2borders
mp1 <- fortify(maps::map(fill=TRUE, plot=FALSE)) |>
  as_tibble()
mp2 <- mp1 |>
  mutate(long = long + 360,
         group = group + max(mp1$group) + 1)
country_borders_tb <- rbind(mp1, mp2) |>
  filter(long > 90  & long <360 & lat <50 & lat > -60) 



#------------------international date line-----------------------------

if(!exists("international_date_line_sf")){
  international_date_line_sf <- ne_download(type = "geographic_lines", 
                        category = "physical", 
                        returnclass = "sf") |>
    st_shift_longitude() |>
    filter(name == "International Date Line")
}

#------------------------exclusive economic zones------------------------
# Note for future users - this is dated June 2022, so presumably it changes from time to time.
# Check out the Pacific Data Hub for later versions.

fn1 <- "global_ffa_spc_sla_pol_june2022_kml.zip"
fn2 <- gsub("_kml\\.zip", ".kml", fn1)

if(!file.exists(fn1)){
  url <- "https://pacificdata.org/data/dataset/a89d83bc-378d-4679-b1fb-7096e76f2e30/resource/4dad0629-4cf3-498e-9f56-75f5c49c2763/download/global_ffa_spc_sla_pol_june2022_kml.zip"
  download.file(url, destfile = fn1, mode = "wb")
}

if(!file.exists(fn2)){
  unzip(fn1)
}

if(!exists("eez")){
  eez <- st_read(fn2)
}

sf_use_s2(FALSE)
pac <- eez |>
  slice(c(67, 245:282)) |>
  clean_names() |>
  filter(!grepl("Joint", name)) |>
  filter(!grepl("Triangle between", name)) |>
  filter(!grepl("Australia", name)) |>
  filter(!grepl("New Zealand", name)) |>
  filter(!grepl("Howland and Baker", name)) |>
  filter(!grepl("Palmyra", name)) |>
  filter(!grepl("Wake Island", name)) |>
  filter(!grepl("Matthew and Hunter", name)) |>
  filter(!grepl("Jarvis", name)) |>
  filter(!grepl("Hawaii", name)) |>
  st_shift_longitude() |>
  mutate(name2 = gsub(" Exclusive Economic Zon.*", "", name)) |>
  mutate(name2 = gsub("n$", "", name2)) |>
  mutate(name2 = ifelse(name2 %in% c("Niuea", "Fijia", "Naurua", "Kiribatia", "Tuvalua"),
                        str_sub(name2, end = -2),
                        name2)) |>
  mutate(name2 = case_when(
    name2 == "Micronesia" ~ "Micronesia, Federated States of",
    name2 == "Northern Mariana" ~ "Northern Mariana Islands",
    name2 == "Pitcairn Islands" ~ "Pitcairn",
    TRUE ~ name2
  )) |>
  mutate(id = 1:n()) |>
  # next 3 lines are for combining the countries that were split by 180 degrees into one
  # eg Tuvalu
  group_by(id, name2) |>
  dplyr::summarise(across(geometry, ~ sf::st_union(., by_feature = TRUE))) |>
  ungroup() 

stopifnot(
  pac |>
    anti_join(ISO_3166_1, by = c("name2" = "Name")) |>
    nrow() == 0)

pac_c <- st_centroid(pac)




pac_map_sf <- cbind(pac, st_coordinates(pac_c)) |>
  left_join(select(ISO_3166_1, Name, geo_pict = Alpha_2, iso3 = Alpha_3), by = c("name2" = "Name")) 
  # note this is EEZ of whole country, so Kiribati has all in one number not split in 3,
  # and will have 3 reps of the same number
  
save(pac_map_sf, file = "pkg/data/pac_map_sf.rda")
save(country_borders_tb, file = "pkg/data/country_borders_tb.rda")
save(international_date_line_sf, file = "pkg/data/international_date_line_sf.rda")



#-------------------------combined map------------------------
ff <- "Roboto"

sf_use_s2(FALSE) # so reticules still drawn on right half of map
m1 <- pac |>
  ggplot() +
  geom_sf(aes(fill = dens_cat), colour = "grey70", alpha = 0.9) +
  geom_polygon(data = country_borders,
               aes(x = long, y = lat, group = group),
               fill = "white",
               alpha = 0.8) +
  geom_sf(data = international_date_line, colour = "steelblue", linetype = 1, alpha = 0.5) +
  annotate("text", x = 182, y = 38, label = "International date line", 
           colour = "steelblue", hjust = 0, family = ff, size = 3) +
  geom_text(aes(label = name2, x = X, y = Y),
            colour = "black", family = ff, size = 3, angle = 15) +
  theme_minimal(base_family = ff) +
  scale_fill_manual(values = brewer.pal(9, "Oranges")) +
  theme(legend.position = c(0.8, 0.7),
        panel.background = element_rect(fill = "lightsteelblue", colour = NA),
        panel.grid = element_blank(),
        plot.caption = element_text(colour = "grey50")) +
  coord_sf(xlim = c(120, 290),  ylim = c(-50, 50)) +
  labs(title = "Exclusive economic zones (EEZs) of Pacific Community island countries and territories",
       x = "",
       y = "",
       fill = "People per 1,000\nsquare km of EEZ",
       caption = "Source: http://freerangestats.info with data from the Pacific Data Hub")

