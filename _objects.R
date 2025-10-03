# _objects.R
# This script creates all shared objects needed across multiple chapters
# Source this script at the beginning of chapters that need these objects

# Required packages
if (!require(dplyr)) library(dplyr)
if (!require(rnaturalearth)) library(rnaturalearth)
if (!require(sf)) library(sf)
if (!require(showtext)) library(showtext)

# 1. Load and prepare the main dataset
# This recreates the data processing from chapter 1
if (!exists("dat_vegetation")) {
  # Load raw data from folio package
  raw_dat_vegetation <- folio::vegetation
  
  # Process the data as done in chapter 1
  dat_vegetation <- raw_dat_vegetation |>
    mutate(
      country_label = case_when(
        country == "Zaire" ~ "Dem. Rep. of Congo",
        TRUE ~ country
      ),
      country = case_when(
        country == "Zaire" ~ "Democratic Republic of the Congo", 
        TRUE ~ country
      )
    ) |> 
    # html for subscript in C3, C4 (used in later chapters)
    mutate(type = case_when(
      type == "C3" ~ "C<sub>3</sub>",
      type == "C4" ~ "C<sub>4</sub>",
      TRUE ~ type
    ))
}

# 2. Create footnote text (from chapter 1)
folio_footnote <- "**Data source:** `vegetation` data from the `{folio}` package"

# 3. Store country labels as a named vector
country_labels <- sort(unique(dat_vegetation$country_label))
names(country_labels) <- sort(unique(dat_vegetation$country))

# 4. Get countries data and create sf objects
world_data <- ne_countries(scale = "medium", returnclass = "sf")

# Countries in the data
vegetation_countries <- unique(dat_vegetation$country)
vegetation_continents <- world_data[world_data$name_en %in% vegetation_countries, ]$continent

# Get their continents
vegetation_sf_continents <- world_data[world_data$continent %in% vegetation_continents, ]

# Subset the sf world data for countries of interest
vegetation_sf_countries <- world_data[world_data$name_en %in% vegetation_countries, ]

# Ensure sf objects have proper geometry columns (fix for publishing environments)
if (!is.null(vegetation_sf_continents) && "sf" %in% class(vegetation_sf_continents)) {
  sf::st_geometry(vegetation_sf_continents) <- "geometry"
}
if (!is.null(vegetation_sf_countries) && "sf" %in% class(vegetation_sf_countries)) {
  sf::st_geometry(vegetation_sf_countries) <- "geometry"
}

# 5. Create summary data objects
summarized_tbl_input <- dat_vegetation |>
  group_by(country, type) |>
  summarize(mean_d = mean(delta), .groups = "drop") |>
  arrange(country)

veg_summarized <- dat_vegetation |> 
  group_by(family, country, type) |>
  summarize(
    n_plants = n(),
    mean_d = mean(delta),
    sd_d = sd(delta),
  ) |> 
  ungroup()

# 6. Set up custom font (needed for plotting functions)
if (!exists("font_setup_done")) {
  font_add_google(name = "Open Sans", family = "open sans")
  showtext_auto()
  font_setup_done <- TRUE
}

# 7. Define plotting functions that depend on the objects above

# Function to plot countries (depends on vegetation_sf_countries, vegetation_sf_continents, country_labels)
PlotCountry <- function(target_country) {
  # Get coordinates of the continent where target country is located
  target_continent <- vegetation_sf_countries[vegetation_sf_countries$name_en == target_country, ]$continent
  # Country label will be used a plot's title
  country_label <- country_labels[names(country_labels) == target_country]

  p_cont <- ggplot() +
    # entire continent
    geom_sf(data = vegetation_sf_continents[vegetation_sf_continents$continent == target_continent, ], fill = "white") +
    # highlight target country
    geom_sf(data = vegetation_sf_countries[vegetation_sf_countries$name_en == target_country, ], fill = "#1F6E5EFF") +
    labs(title = country_label) +
    # removes all grids, axes, etc.
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0),
          plot.title = element_text(hjust = 0.5, family = "open sans", size = 18),
          plot.background = element_blank())
  
  return(p_cont)
}

# Function to create half violin plots (depends on dat_vegetation)
PlotHalfViolins <- function(data_to_plot) {
  # y-axis limits (calculated from full dataset)
  y_min <- (round(min(dat_vegetation$delta) %% 10, 0) * 10) * -1
  y_max <- (round(max(dat_vegetation$delta) %% 10, 0) * 10) * -1
  
  p_halfv <- ggplot(data_to_plot, aes(x = country, y = delta)) +
    geom_violinhalf(colour = "#005C55FF", fill = "#005C55FF", position = position_nudge(x = 0.15, y = 0)) +
    geom_jitter(aes(fill = type), colour = "#005C55FF", alpha = 0.8, width = 0.05) +
    scale_y_continuous(breaks = c(-10, -20, -30, -40)) +
    # set limits of the y axis
    coord_cartesian(ylim = c(y_min, y_max)) +
    theme_classic() +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_blank(),
      panel.background = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_line(colour = "black", linewidth = 1),
      axis.ticks.y = element_line(colour = "black", linewidth = 1),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(colour = "black", size = 16),
      axis.title = element_blank(),
      aspect.ratio = 2,
      legend.position = "none"
    )

  return(p_halfv)
}

# Clean up temporary objects
rm(world_data, vegetation_countries, vegetation_continents)
