
## ---------------------------
##
## TidyTuesday 2021, Week 27: Animal Rescues in London  
##
## Author: Allison Koh
##
## Date Created: 2021-06-28
##
## Copyright (c) Allison Koh, 2021
## Email: koh@hertie-school.org
##
## ---------------------------

# setup / load dependencies -----------------------------------------------
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  tidytuesdayR,
  here,
  DataExplorer,
  sp,
  rgdal, 
  rgeos, 
  broom
)


# theme_void 4evr 
theme_set(theme_void())

# goals -------------------------------------------------------------------
#' make a spatial plot within 2 hours 
#' try out {gganimate} if time allows 
#' try catterplot!!! if time allows...

# notes and thoughts  -----------------------------------------------------
#' loading in maps is hard. SAD 
#' It took like 20 minutes to load in and plot the map. YES.
#' the syntax for catterplot is bad 
#' THE BROOM PACKAGE IS YOUR FRIEND. TIDY YOUR SPATIAL DATA BEFORE MERGING.

# import data -------------------------------------------------------------
tt_data <- tidytuesdayR::tt_load('2021-06-29')
rescues_raw <- tt_data$animal_rescues


# explore data ------------------------------------------------------------
plot_intro(rescues_raw)
plot_missing(rescues_raw)

glimpse(rescues_raw)

# import map --------------------------------------------------------------
ldn_spdf_import <- readOGR(
  dsn = here("London-wards-2018", "London-wards-2018_ESRI"), 
  layer = "London_Ward_CityMerged"
)

ldn_spdf_sp <- spTransform(ldn_spdf_import, CRS("+init=epsg:4326"))

# make it tidy 
ldn_spdf_tidy <- tidy(ldn_spdf_sp, region = "GSS_CODE")

# wrangle -----------------------------------------------------------------
rescues_tidy <- rescues_raw %>% 
  rename(
    lat = latitude, 
    long = longitude, 
    incident_national_cost = incident_notional_cost # fix typo
  ) %>% 
  mutate(
    incident_national_cost = as.numeric(incident_national_cost)
  ) %>% 
  drop_na()

# calls by year and ward 
by_cw <- rescues_tidy %>% 
  group_by(ward_code) %>% 
  summarize(cost = sum(incident_national_cost, na.rm = TRUE)) %>% 
  rename(id = ward_code)

# merge with map
ldn_spdf_tidy <- ldn_spdf_tidy %>% 
  left_join(., by_cw, by = c("id")) 

# calculate values to report in map ---------------------------------------
# total cost 
sum(rescues_tidy$incident_national_cost, na.rm = TRUE)
# most expensive ward 
rescues_tidy %>% group_by(ward) %>% summarize(cost = sum(incident_national_cost)) %>% top_n(1)

# map ---------------------------------------------------------------------
ldn_spdf_tidy %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = cost), color = "black") + 
  geom_polygon() + 
  coord_map() +
  scale_fill_viridis_c(option = "G", trans = "log10", na.value = "black") + 
  labs(
    title = "Total Cost of Animal Rescues in London (2009-2015)",
    fill = "Cost (£)",
    subtitle = "Between 2009 and 2015, the total cost of animal rescues by the\nLondon Fire Brigade amassed £1,302,915. Mile End was the most\nexpensive ward, at £10,735.",
    caption = "#TidyTuesday 2021, Week 27\nVisualization: Allison Koh (@allisonkoh_)\nData: London Fire Brigade"
  ) + 
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0.95),
    legend.position = "bottom"
  )


# save plot ---------------------------------------------------------------
ggsave("2021-w27-plt.png")

# code graveyard ----------------------------------------------------------
# rescue_map <- ggplot(by_yw_mapped) + 
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
#   coord_map()

# rescue_map <- ggplot(ldn_spdf1) + 
#   geom_polygon(aes(x = long, y = lat, group = group, fill = n))
# 
# rescue_map

# attempt to merge SEND HELP 
# by_yw_mapped <- merge(by_yw, ldn_spdf1)
# class(ldn_spdf1@data$n)
# 
# by_yw_2009 <- by_yw %>% filter(cal_year == 2009)
# 
# ldn_spdf1@data <- left_join(ldn_spdf1@data, by_yw, by = "GSS_CODE")
# 
# ldn_spdf1@data %>% View()
# ldn_spdf1 %>% View()
# class(by_yw_mapped)
# change district to gss cod

# subset for cats 
# cat_rescues_tidy <- rescues_tidy %>% filter(animal_group_parent == "Cat") %>% 
#   count(cal_year) %>% 
#   mutate(cal_year = as.numeric(cal_year), n = as.numeric(n))

# devtools::install_github("Gibbsdavidl/CatterPlots")

# CatterPlots::catplot(xs = cat_rescues_tidy$cal_year, ys = cat_rescues_tidy$n)
# by_yw_mapped <- sp::merge(ldn_spdf1, by_yw1, by = "GSS_CODE")

# nrow(ldn_spdf2)
# length(ldn_spdf1@data$GSS_CODE)

# point_map <- ggplot(ldn_spdf1) + 
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
#   # geom_point(data = rescues_tidy, aes(x = long, y = lat)) + 
#   coord_map()
# 
# point_map
# 
# 
# point_map <- ggplot() + 
#   geom_polygon(data = ldn_spdf1, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
#   geom_point(data = rescues_tidy_2015, aes(x = long, y = lat))  

# rescues_raw %>% count(animal_group_parent) %>% arrange(-n) %>% View()

# base_map <- ggplot(ldn_spdf_tidy) + 
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
#   coord_map()
# 
# base_map
# histogram of wards with highest cost 
# rescues_top10_cost <- rescues_tidy %>% 
#   group_by(ward) %>% 
#   summarize(cost = sum(incident_national_cost)) %>% 
#   arrange(-cost) %>% 
#   top_n(10)
# 
# rescues_top10_cost %>% View()
# 
# hist <- rescues_top10_cost %>% 
#   ggplot(aes(x = reorder(ward, cost), y = cost, fill = cost)) + 
#   geom_col(width = 0.5) +
#   geom_text(aes(label = cost, hjust = 1.2),  color = "navy", size = 1) +
#   theme_minimal() + 
#   coord_flip() + 
#   scale_fill_viridis_b(option = "G", begin = 0.6, trans = "log10") +
#   labs(
#     title = "",
#     x = "", 
#     y = "Cost (£)"
#   ) + 
#   theme(
#     legend.position = "none",
#     text = element_text(size = 5)
#   )

# put it together 
# map + annotation_custom(ggplotGrob(hist), xmin = 0.2, xmax = 0.4, ymin = 51.3, ymax = 51.4)

# map + 
  # expand_limits(x = 0.7, y = 51) + 
  # annotation_custom(ggplotGrob(hist), 
                    # xmin = 0.3,
                    # xmax = 0.7,
                    # ymin = 51,
                    # ymax = 51.4) 

# map + theme_minimal()

# plot_grid(map, hist, ncol = 1, rel_heights = c(2.3, 1), rel_widths = c(2.3,1))

# grid.arrange(tableGrob(rescues_top10_cost, rows = c())) 

# theme_1 <- ttheme_minimal(core = list(fg_params = list(hjust = 0, 
                                                       # x = 0.1)),
                          # colhead = list(fg_params = list(fontsize = 12, 
                                                          # fontface = "bold")))


# ward_tab <- tableGrob(rescues_top10_cost, rows=NULL, theme = ttheme_minimal())

# grid.arrange(ward_tab)
# format(df, digits = 1,
#        scientific=F,big.mark = ","),
# core.just="left",
# #core.just="right",
# #col.just="right",
# gpar.coretext=gpar(fontsize=8), 
# gpar.coltext=gpar(fontsize=9, fontface='bold'), 
# show.rownames = F,
# h.even.alpha = 0,
# gpar.rowtext = gpar(col="black", cex=0.7,
#                     equal.width = TRUE,
#                     show.vlines = TRUE, 
#                     show.hlines = TRUE,
#                     separator="grey")                     

# grid.arrange(ward_tab)

# tableGrob
# grid.arrange(
  # map,
  # tableGrob(rescues_top10_cost)
# )
# range(ldn_spdf_tidy$cost, na.rm = TRUE)

# rescues_tidy <- rescues_raw %>% 
#   drop_na() %>% 
#   mutate(
#     animal_group_parent = recode(
#       animal_group_parent, 
#       cat = "Cat", 
#       Budgie = "Bird",
#       Pidgeon = "Bird"
#     ),
#     incident_notional_cost = as.numeric(incident_notional_cost)
#   ) %>% 
#   rename(
#     lat = latitude, 
#     long = longitude, 
#     incident_national_cost = incident_notional_cost # fix typo
#   ) %>% 
#   filter(
#     lat != "NULL",
#     long != "NULL"
#   ) 

