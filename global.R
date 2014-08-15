suppressPackageStartupMessages({
  library(shinyIncubator)
  library(dplyr)
  library(tidyr)
  library(CalEnviroScreen)
  library(leaflet)
  library(RColorBrewer)
  library(rgeos)
})

options(shiny.json.digits = 8) # Affects precision of polygon coordinates
options(digits = 3)

data(CES2, package = "CalEnviroScreen")
data(CES2_metadata, package = "CalEnviroScreen")
data(California, package = "CalEnviroScreen")

CES2_VARS <- c(CES2_POLLUTION_VARS, CES2_POPCHAR_VARS)

REGION_COLORS <- c(`Bay Area` = "#009E73", `South Coast` = "#0072B2",
                   `San Joaquin` = "#D55E00", `Other` = "#999999")

data(California, package = "CalEnviroScreen")
region_tbl <- do.call(
  rbind,
  lapply(names(CA_regions),
         function (x) as.tbl(data.frame(FIPS = as.character(CA_regions[[x]]$FIPS), Region = x))))

with_region <- function (.data) {
  .data %>% inner_join(region_tbl, by = "FIPS")
}

#print(CES2_data %>% with_region() %>% group_by(Region) %>% tally())

tract_boundaries <- geometry(CA_tracts)

scale_color_regions <- function (...) scale_color_manual("Region", values=region_colors)
scale_fill_regions <- function (...) scale_fill_manual("Region", values=region_colors)
scale_x_score <- function (...) scale_x_continuous(..., limits=c(0, 10), expand=c(0, 0))
scale_y_score <- function (...) scale_y_continuous(..., limits=c(0, 10), expand=c(0, 0))

xapply <- function (...) {
  unlist(sapply(..., simplify = FALSE))
}

coord_getter <- function (j) {
  f <- function (x) c(x@coords[, j, drop=TRUE], NA)
  function (obj) xapply(obj@Polygons, f)
}

poly_x <- setNames(lapply(CA_tracts@polygons, coord_getter(1)), CA_tracts$FIPS)
poly_y <- setNames(lapply(CA_tracts@polygons, coord_getter(2)), CA_tracts$FIPS)
poly_id <- setNames(lapply(CA_tracts@polygons, function (obj) rep(obj@ID, length(obj@Polygons))), CA_tracts$FIPS)

# Sanity check: should be the same:
#plot(subset(CA_tracts, FIPS == "06001402500"))
#polygon(poly_x[["06001402500"]], poly_y[["06001402500"]])

defaultOptions <- list(fillOpacity = 0.5, stroke = FALSE)

color_ramp <- function (x, pal = "RdYlGn") {
  palette <- colorRampPalette(rev(brewer.pal(9, pal)))
  palette(n = 1 + length(levels(x)))[x]
}

#pctl_tbl <- CES2_data %>% mutate_each(funs(pctl), -FIPS)

group_tbl <- as.tbl(data.frame(Variable = CES2_VARS)) %>%
    mutate(Group = factor(ifelse(Variable %in% CES2_POPCHAR_VARS, "PopChar", "Pollution")))

pctl_tbl <- CES2_pctls
CES2_tbl <- pctl_tbl %>%
  gather(Variable, Pctl, -FIPS) %>%
  inner_join(group_tbl, by = "Variable")
