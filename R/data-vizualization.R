###########################################################################
## PROJECT: L+I predictive modelimg
##
## SCRIPT PURPOSE: Data Visualzation
##    -
##    -
##    -
##
## DATE: 17 August 2017
## AUTHOR: Simon Kassel
###########################################################################

# functions + utilities
source("R/utilities.R")
source("R/functions.R")

# source('LNI-analysis-feature-engineering.R')
load("data/full_dataset_for_modeling.Rdata")

# source("R/feature_importance.R")
load('data/boruta.train.Rdata')

# source("R/model-selection.R")
load("data/prediction_results.Rdata")

# Additional datasets
# Failure-level dataset:
load("data/failure_level_dataset.Rdata")

# packages
packages(
  c(
    "h2o",
    "tidyverse",
    "sf",
    "ggjoy",
    "plotROC"
    )
)


# BAR CHART INDICATING LEVELS OF L+I DATA ---------------------------------

data_levels_bar <- df %>%
  select(apfailkey, apinspkey, casenumber, addresskey) %>%
  summarise_all(funs(n_distinct)) %>%
  gather %>%
  mutate(outline = ifelse(key == "apinspkey", 'y', 'n'))

vals <- (data_levels_bar$value / 1000) %>%
  round(1) %>%
  paste0('k')

data_levels_bar <- data_levels_bar %>%
  mutate(label = c(
    paste0(vals[1], " total\nviolations"),
    paste0("from ", vals[2], "\nbuilding inspections"),
    paste0("collected from\n", vals[3], " L+I cases"),
    paste0("at ", vals[4], "\nunique locations")
  ))


ggplot(data_levels_bar) + geom_bar(aes(x = reorder(key, value), y = value, color = outline), fill = cat_colors[1], size = 2, stat = 'identity') +
  geom_text(aes(x = key, y = value / 2, label = label), color = 'white') +
  scale_color_manual(values = c(cat_colors[5], cat_colors[3])) +
  ggtitle("Levels of L+I Inspection Data") +
  coord_flip() +
  theme_void() +
  theme(
    legend.position = 'none',
    plot.title = element_text(color = cat_colors[1], hjust = 0.1, vjust = -5)
  )

ggsave("output/plots/fig.x-Levels-of-data-bar-charts.png", device = "png")


# LOOK AT THE CASE LEVEL --------------------------------------------------

cases <- ds %>% group_by(i.casenumber) %>% 
  dplyr::summarise(
    total_fails = max(as.numeric(as.character(f.before))),
    long = first(l.long),
    lat = first(l.lat)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, agr = "constant") %>%
  st_transform(2272)

phila <- st_read("data/City_Limits.shp") %>%
  st_transform(2272)

phila_grid <- st_make_grid(phila, cellsize = c(1320, 1320)) %>%
  st_intersection(phila, .) %>%
  mutate(fid = c(1:nrow(.)))  

grids <- st_join(phila_grid, cases) %>%
  dplyr::select(fid, total_fails) %>%
  mutate(
    failed_once = ifelse(total_fails < 2, 1, 0),
    failed_more_than_once = ifelse(total_fails >= 2, 1, 0)
  ) %>%
  dplyr::select(-total_fails) %>%
  dplyr::group_by(fid) %>%
  dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  tidyr::gather(label, count, -fid, -geometry) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(label2 = ifelse(label == "failed_once", "One-time violations", "Repeat violations"))

# TODO: improve these aesthetics
ggplot(grids) + geom_sf(aes(fill = count), color = NA) + 
  facet_wrap(~label2) +
  scale_fill_distiller(type = "seq", palette = "PuOr") +
  theme_void()

ggsave("output/plots/fig.x-Map_of_fails_and_passes.png", device = "png")



# VARIABLE IMPORTANCE PLOT ------------------------------------------------

gbm_varImp <- h2o.varimp_plot(gbm_results$fit.gbm.tuned.h2o) %>%
  as.data.frame()

head(gbm_varImp)

# ROC  --------------------------------------------------------------------

res_roc <- results %>%
  select(o.failed.n, ends_with(".p1")) %>%
  gather(model, probability, -o.failed.n)

# TODO: improve aesthetics of this plot
ggplot(res_roc, aes(d = o.failed.n, m = probability, color = model)) + geom_roc()

ggsave("output/plots/fig.x-ROC_curves.png")


# CONFUSION MATRIX MAPS ---------------------------------------------------

# TODO: clean up this code
tidy_condusion_matrix <- function(dat, response_var, pred_var) {
  d <- dat %>%
    select(one_of(c(response_var, pred_var, "l.long", "l.lat")))
  names(d) <- c("response", "pred", "long", "lat")
  d <- d %>%
    mutate(
      true_positive = ifelse(response == 1 & pred == 1, 1, 0),
      true_negative = ifelse(response == 0 & pred == 0, 1, 0),
      false_positive = ifelse(response == 1 & pred == 0, 1, 0),
      false_negative = ifelse(response == 0 & pred == 1, 1, 0)
      )
  return(d)
}

test <- tidy_condusion_matrix(results, response_var = "o.failed.n", pred_var = "gbm.predict")

st_matrix <- st_as_sf(test, coords = c("long", "lat"), crs = 4326, agr = "constant") %>%
  st_transform(2272)

grid_mat <- st_join(phila_grid, st_matrix) %>%
  select(fid, true_positive, true_negative, false_positive, false_negative) %>%
  group_by(fid) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  tidyr::gather(label, count, -fid, -geometry) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(pred = ifelse(grepl("positive", label), "FAIL", "PASS"),
         result = ifelse(grepl("true", label), "CORRECT", "INCORRECT"))


# TODO: adjust these aesthetics
ggplot(grid_mat) + geom_sf(aes(fill = count)) +
  facet_grid(pred~result) +
  scale_fill_distiller(type = "seq", palette = "PuOr") +
  theme_void() 
ggsave("output/plots/fig.x-Confusion_matrix_maps.png")

# TODO: confusion matrix plt
conf_matrix <- grid_mat %>% 
  select(label, count) %>%
  group_by(label) %>%
  summarise(total = sum(count))
print(conf_matrix)

for (i in c(1:length(conf_matrix$total))) {
  print(
    paste0(
      conf_matrix$label[i],
      " rate: ",
      round(conf_matrix$total[i] / sum(conf_matrix$total), 3)
    )
  )
}


