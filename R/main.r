#############################################
# Visualizing tree cover loss
# Milos Popovic 2023/10/06
#############################################

libs <- c(
  "tidyverse", "terra",
  "sf", "exactextractr",
  "rgeoboundaries"
)

installed_libraries <- libs %in% rownames(
  installed.packages()
)

if (any(installed_libraries == F)) {
  install.packages(libs[!installed_libraries])
}

invisible(
  lapply(
    libs, library,
    character.only = T
  )
)

# 1. GET COUNTRY MAP
#-------------------

country_sf <- rgeoboundaries::gb_adm2(
  "SVN"
)

# 2. DOWNLOAD RASTER FILES
#-------------------------

urls <- c(
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2018/33T_20180101-20190101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/33T_20220101-20230101.tif"
)

for (url in urls) {
  download.file(
    url = url,
    destfile = basename(url),
    mode = "wb"
  )
}


# 3. ZONAL STATS
#---------------

raster_files <- list.files(
  path = getwd(),
  pattern = "tif",
  full.names = T
)

dd <- list()
for (raster in raster_files) {
  dd[[raster]] <- {
    rasters <- terra::rast(raster)
    lc <- exactextractr::exact_extract(
      rasters,
      country_sf,
      function(df) {
        df |>
          dplyr::group_by(
            shapeName
          ) |>
          dplyr::summarize(
            area_km2 = sum(
              coverage_area[value == 2] / 1e6
            )
          )
      },
      summarize_df = T,
      coverage_area = T,
      include_cols = "shapeName"
    )
    lc
  }
}

# 4. DATAFRAME
#-------------

output <- do.call(cbind, dd)
tree_df <- as.data.frame(output)
head(tree_df)

slovenia_tree_df <- tree_df |>
  dplyr::select(1:2, 4)

names(slovenia_tree_df) <- c(
  "shapeName", "tree_cover_2018", "tree_cover_2022"
)

head(slovenia_tree_df)

# 5. TREE COVER CHANGE
#---------------------

slovenia_tree_df <- slovenia_tree_df |>
  dplyr::mutate(
    tree_cover_change = (
      tree_cover_2022 - tree_cover_2018
      ) / tree_cover_2018 * 100
  )

summary(slovenia_tree_df$tree_cover_change)

slovenia_tree_cover_change <- cbind(
  country_sf, 
  slovenia_tree_df
)

# 6. MAP
#-------

map <- ggplot() +
  geom_sf(
    data = slovenia_tree_cover_change,
    aes(fill = tree_cover_change),
    color = "white",
    size = .15
  ) +
  scale_fill_gradient2(
    name = "%-change",
    midpoint = 0,
    mid = "#f7de7c",
    high = "#006f00",
    low = "#9e319d"
  ) +
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      barheight = unit(1.5, "mm"),
      barwidth = unit(20, "mm"),
      title.position = "top",
      label.position = "bottom",
      title.hjust = .5,
      label.hjust = .5,
      nrow = 1,
      byrow = T
    )
  ) +
  theme_void() +
  theme(
    legend.position = "top",
    plot.margin = unit(
      c(
        t = 0, b = 0,
        r = 0, l = 0
      ), "lines"
    )
  )
