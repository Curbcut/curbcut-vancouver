#### BUILD ALL CURBCUT DATA ####################################################


# Load libraries ----------------------------------------------------------
tictoc::tic()
library(sf)
library(cc.buildr)
invisible(lapply(list.files("dev/data_import", full.names = TRUE), source))


# Base of the study region and dictionaries -------------------------------

# All regions
all_tables <-
  list("CMA" = c("CSD", "CT", "DA", "building"),
       "city" = c("CSD", "CT", "DA", "building"),
       "cmhc" = c("cmhczone"))


# List all the regions geometries to create the master polygon
cancensus_cma_code <- 59933
all_regions <- list(CMA = list(CMA = cancensus_cma_code),
                    city = list(CSD = 5915022),
                    cmhc = get_cmhc_zones(list(CMA = cancensus_cma_code)))

base_polygons <- create_master_polygon(all_regions = all_regions)
crs <- base_polygons$crs

# Create the region dictionary
regions_dictionary <-
  regions_dictionary(
    all_tables = all_tables,
    region = c("CMA", "city", "cmhc"),
    name = c(CMA = "Metropolitan Area",
             city = "City of Vancouver",
             cmhc = "Canada Mortgage and Housing Corporation zones"),
    to_compare = c(CMA = "in the Vancouver region",
                   city = "in the City of Vancouver",
                   cmhc = "in the Vancouver region"),
    to_compare_determ = c(CMA = "the Vancouver region",
                          city = "the City of Vancouver",
                          cmhc = "the Vancouver region"),
    to_compare_short = c(CMA = "in the region",
                         city = "in the City",
                         cmhc = "in the region"),
    pickable = c(CMA = TRUE,
                 city = TRUE,
                 cmhc = FALSE))


# Build scales ------------------------------------------------------------

### Build census scales
census_scales <-
  build_census_scales(master_polygon = base_polygons$master_polygon,
                      regions = base_polygons$province_cancensus_code,
                      levels = c("CSD", "CT", "DA"),
                      crs = crs)

# Create the census scales dictionary
scales_dictionary <- census_scales_dictionary(census_scales)


### Build building scale
# # From MySQL
# building <- cc.data::db_read_long_table(table = "buildings",
#                                          DA_ID = census_scales$DA$ID)
# qs::qsave(building, file = "dev/data/built/building.qs")
# # From Local
# building <- qs::qread("dev/data/canada_buildings.qs")
# building <- building[building$DA_ID %in% census_scales$DA$ID, ]
# building <- qs::qsave(building, "dev/data/built/building.qs")
building <- qs::qread("dev/data/built/building.qs")

# Add building scale to the dictionary
scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "building",
                             sing = "building",
                             sing_with_article = "the building",
                             plur = "buildings",
                             slider_title = "Building",
                             place_heading = "{name}",
                             place_name = "{name}")

### Build CMHC scale
cmhczone <- get_cmhc_zones(list(CMA = cancensus_cma_code))
cmhczone <- additional_scale(additional_table = cmhczone,
                             DA_table = census_scales$DA,
                             ID_prefix = "cmhc",
                             name_2 = "CMHC zone",
                             crs = crs)
scales_dictionary <-
  append_scale_to_dictionary(scales_dictionary,
                             scale = "cmhczone",
                             sing = "CMHC zone",
                             sing_with_article = "the CMHC zone",
                             plur = "CMHC zones",
                             slider_title = "CMHC zone",
                             place_heading = "{name}",
                             place_name = "{name}")


# Consolidate scales ------------------------------------------------------

all_scales <- c(census_scales,
                list(building = building),
                list(cmhczone = cmhczone))

scales_consolidated <- consolidate_scales(all_tables = all_tables,
                                          all_scales = all_scales,
                                          regions = base_polygons$regions,
                                          crs = crs,
                                          match_with_centroids_regions = NULL)


# Verify conformity -------------------------------------------------------

verify_dictionaries(all_tables = all_tables,
                    regions_dictionary = regions_dictionary,
                    scales_dictionary = scales_dictionary)


# Create the modules and variables tables ---------------------------------

scales_variables_modules <-
  append_empty_variables_table(scales_consolidated = scales_consolidated)
scales_variables_modules <-
  append_empty_modules_table(scales = scales_variables_modules)


save.image("dev/data/built/empty_scales.RData")
load("dev/data/built/empty_scales.RData")
# Build the datasets ------------------------------------------------------

future::plan(future::multisession, workers = 4)
# future::plan(list(future::tweak(future::multisession,
#                                 workers = 2),
#                   future::tweak(future::multisession,
#                                 workers = length(cc.data::census_years))))

scales_variables_modules <-
  ba_census_data(scales_variables_modules = scales_variables_modules,
                 region_DA_IDs = census_scales$DA$ID,
                 crs = crs,
                 housing_module = TRUE)
census_variables <- get_census_vectors_details()

future::plan(future::multisession(), workers = 6)

scales_variables_modules <-
  ru_vac_rate(scales_variables_modules = scales_variables_modules,
              crs = crs, geo_uid = cancensus_cma_code,
              approximate_name_match = FALSE)
scales_variables_modules <-
  ru_alp(scales_variables_modules = scales_variables_modules,
         crs = crs,
         region_DA_IDs = census_scales$DA$ID)
scales_variables_modules <-
  ru_canbics(scales_variables_modules = scales_variables_modules,
             crs = crs,
             region_DA_IDs = census_scales$DA$ID)
scales_variables_modules <-
  ru_lst(scales_variables_modules = scales_variables_modules,
         region_DA_IDs = census_scales$DA$ID,
         crs = crs)

scales_variables_modules <- 
  ba_ndvi(scales_variables_modules = scales_variables_modules, 
          master_polygon = base_polygons$master_polygon, 
          all_scales = all_scales, data_output_path = "dev/data/ndvi/", 
          crs = crs)

# # Add access to amenities module
# traveltimes <-
#   accessibility_get_travel_times(region_DA_IDs = census_scales$DA$ID)
# qs::qsave(traveltimes, "dev/data/built/traveltimes.qs")
traveltimes <- qs::qread("dev/data/built/traveltimes.qs")

future::plan(future::multisession(), workers = 2)
scales_variables_modules <-
  ba_accessibility_points(scales_variables_modules = scales_variables_modules,
                          region_DA_IDs = census_scales$DA$ID,
                          traveltimes = traveltimes, 
                          crs = crs)

save.image("dev/data/built/scales_variables_modules.RData")
load("dev/data/built/scales_variables_modules.RData")


# Post process
scales_variables_modules$scales <- 
  cc.buildr::post_processing(scales = scales_variables_modules$scales)


qs::qsavem(census_scales, scales_variables_modules, crs, census_variables,
           scales_dictionary, regions_dictionary, all_tables, base_polygons,
           all_scales,
           file = "dev/data/built/scales_variables_modules.qsm")
qs::qload("dev/data/built/scales_variables_modules.qsm")

# Postal codes ------------------------------------------------------------

# postal_codes <- build_postal_codes(census_scales$DA$ID)
# qs::qsave(postal_codes, "data/postal_codes.qs")


# Map zoom levels ---------------------------------------------------------

map_zoom_levels <- map_zoom_levels_create_all(
  all_tables = all_tables,
  zoom_levels = list(first = 0, CT = 10, DA = 12, building = 16))

map_zoom_levels_save(data_folder = "data/", map_zoom_levels = map_zoom_levels)


# Tilesets ----------------------------------------------------------------

# Prepare by getting the census scales with geometries which spans over water
# Build census scales
full_census_scales <- 
  build_census_scales(master_polygon = base_polygons$master_polygon,
                      regions = base_polygons$province_cancensus_code,
                      levels = c("CSD", "CT", "DA"),
                      crs = crs,
                      switch_full_geos = TRUE)


# Do not upload grids, as there is a function just for it.
all_scales_t <- scales_variables_modules$scales
map_zoom_levels_t <- map_zoom_levels

# Before loading the tilesets, switch the geometries.
all_scales_t <- cc.buildr::map_over_scales(
  all_scales_t,
  fun = \(geo = geo, scales = scales,
          scale_name = scale_name, scale_df = scale_df) {

    # If it'S not census, return raw
    if (!scale_name %in% names(full_census_scales)) return(scale_df)

    df <- sf::st_drop_geometry(scale_df)
    merge(df, full_census_scales[[scale_name]]["ID"], by = "ID", all.x = TRUE,
          all.y = FALSE)

  })


tileset_upload_all(all_scales = all_scales_t,
                   map_zoom_levels = map_zoom_levels_t,
                   prefix = "van",
                   username = "curbcut",
                   access_token = .cc_mb_token)



# Did you know ------------------------------------------------------------

library(tidyverse)
vars_dyk <- dyk_prep(scales_variables_modules, all_tables)
dyk <- dyk_uni(vars_dyk, 
               svm = scales_variables_modules, 
               translation_df = NULL,
               langs = c("en"), 
               scales_dictionary = scales_dictionary)
# dyk <- rbind(dyk, dyk_delta(vars_dyk, scales_variables_modules))
# dyk <- rbind(dyk, dyk_bivar(vars_dyk, scales_variables_modules))
qs::qsave(dyk, "data/dyk.qs")


# Produce colours ---------------------------------------------------------

colours_dfs <- cc.buildr::build_colours()

qs::qsave(colours_dfs, "data/colours_dfs.qs")


# Write stories -----------------------------------------------------------

# # stories <- build_stories()
# stories_mapping <- stories$stories_mapping
# qs::qsave(stories, file = "data/stories.qs")
# stories_create_tileset(stories = stories,
#                        prefix = "mtl",
#                        username = "curbcut",
#                        access_token = .cc_mb_token)

scales_variables_modules$modules <-
  scales_variables_modules$modules |>
  add_module(
    id = "stories",
    theme = "Urban life",
    nav_title = "Vancouver stories",
    title_text_title = "Vancouver stories",
    title_text_main = paste0(
      "Explore narrative case studies about specific urban sustainability and ",
      "planning issues in the Vancouver region."),
    title_text_extra = paste0(
      "<p>These narrative case studies are written by the Curbcut team and its contributors."),
    metadata = FALSE,
    dataset_info = ""
  )

# Place explorer page ----------------------------------------------------

# Add the place explorer in the modules dataframe
scales_variables_modules$modules <-
  add_module(modules = scales_variables_modules$modules,
             id = "place_explorer",
             theme = "Explorer",
             nav_title = "Place explorer",
             title_text_title = "Place explorer",
             title_text_main = paste0(
               "Select a location by entering a postal code or clicking on the map to ",
               "see how it compares to the rest of the region across a variety of sust",
               "ainability indicators."
             ),
             title_text_extra = paste0(
               "<p>The data in the Place Explorer is taken from other Curbcut pages with ",
               "the exception of <a href = 'https://www.canuedata.ca/tmp/CANUE_METADATA",
               "_NO2LUR_A_YY.pdf'>Air pollution</a>."
             ),
             metadata = FALSE,
             dataset_info = "",
             regions = regions_dictionary$region[regions_dictionary$pickable])


# Home page ---------------------------------------------------------------

home_page()


# Save variables ----------------------------------------------------------

qs::qsave(scales_variables_modules$variables, file = "data/variables.qs")


# Save QS data ------------------------------------------------------------

save_all_scales_qs(data_folder = "data/", 
                   all_scales = scales_variables_modules$scales,
                   variables = scales_variables_modules$variables)


# Save .qsm ---------------------------------------------------------------

save_short_tables_qs(data_folder = "data/", 
                     all_scales = scales_variables_modules$scales,
                     skip_scales = c("building"))
save_geometry_export(data_folder = "data/", 
                     all_scales = scales_variables_modules$scales)


# Save large dfs as sqlite ------------------------------------------------

save_bslike_sqlite("building", all_scales = scales_variables_modules$scales)


# Save other global data --------------------------------------------------

qs::qsave(census_variables, file = "data/census_variables.qs")
qs::qsave(scales_variables_modules$modules, file = "data/modules.qs")
qs::qsave(scales_dictionary, file = "data/scales_dictionary.qs")
qs::qsave(regions_dictionary, file = "data/regions_dictionary.qs")
tictoc::toc()

# Write data to AWS bucket
# cc.data::bucket_write_folder(folder = "data", bucket = "curbcut.toronto.data")


# Place explorer content creation -----------------------------------------

# Should be done once the data is saved

future::plan(future::multisession(), workers = 2)
# pe_main_card_data <- placeex_main_card_data(scales = scales_variables_modules$scales,
#                                             DA_table = census_scales$DA,
#                                             region_DA_IDs = census_scales$DA$ID,
#                                             crs = crs,
#                                             regions_dictionary = regions_dictionary)
# # ONLY KEEP FIRST SCALE
# pe_main_card_data$main_card_data <- lapply(pe_main_card_data$main_card_data, lapply, `[`, 1)
# pe_main_card_data$avail_df <- dplyr::distinct(pe_main_card_data$avail_df, region, .keep_all = TRUE)
# 
# qs::qsave(pe_main_card_data, file = "data/pe_main_card_data.qs")
pe_main_card_data <- qs::qread("data/pe_main_card_data.qs")

svm_first_scale <- scales_variables_modules
svm_first_scale$scales <- lapply(svm_first_scale$scales, `[`, 1)

svm_first_scale$variables$pe_include[36:53] <- rep(F, length(svm_first_scale$variables$pe_include[36:53]))

library(curbcut)
placeex_main_card_rmd(scales_variables_modules = svm_first_scale,
                      pe_main_card_data = pe_main_card_data,
                      regions_dictionary = regions_dictionary,
                      scales_dictionary = scales_dictionary,
                      lang = "en",
                      tileset_prefix = "van",
                      mapbox_username = "curbcut",
                      rev_geocode_from_localhost = TRUE,
                      overwrite = TRUE)


# Save the place explorer files, which serves as a 'does it exist' for `curbcut`
pe_docs <- list.files("www/place_explorer/", full.names = TRUE)
qs::qsave(pe_docs, "data/pe_docs.qs")

# Deploy app --------------------------------------------------------------

# renv::activate()
# heroku_deploy("cc-toronto") # Production
# heroku_deploy("cc-toronto-2") # Dev
