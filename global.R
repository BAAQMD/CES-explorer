data(CES2, package = "CalEnviroScreen")
data(CES2_metadata, package = "CalEnviroScreen")
data(California, package = "CalEnviroScreen")

CES2_VARS <- c(CES2_POLLUTION_VARS, CES2_POPCHAR_VARS)

REGION_COLORS <- c(`Bay Area` = "#009E73", `South Coast` = "#0072B2",
                   `San Joaquin` = "#D55E00", `Other` = "#999999")

region_tbl <- do.call(rbind, lapply(names(CA_regions), function (x) as.tbl(data.frame(FIPS = as.character(CA_regions[[x]]$FIPS), Region = x))))
with_region <- function (.data) .data %>% inner_join(region_tbl, by = "FIPS")

library(rgeos)
tract_boundaries <- geometry(CA_tracts)

#theme_set(theme_bw())
#theme_update(
#  plot.title = element_text(size=rel(1), face="bold", vjust=1.75),
#  axis.title.x = element_text(size=rel(0.9), lineheight=1.1, face="bold", vjust=-0.5),
#  axis.title.y = element_text(size=rel(0.9), lineheight=1.1, face="bold", angle=90)
#)

options(digits=3)

scale_color_regions <- function (...) scale_color_manual("Region", values=region_colors)
scale_fill_regions <- function (...) scale_fill_manual("Region", values=region_colors)
scale_x_score <- function (...) scale_x_continuous(..., limits=c(0, 10), expand=c(0, 0))
scale_y_score <- function (...) scale_y_continuous(..., limits=c(0, 10), expand=c(0, 0))

region_colors <- c(`Bay Area`="#009E73", `South Coast`="#0072B2", `San Joaquin`="#D55E00", `Other`="#999999")

region_tbl <- do.call(rbind, lapply(names(CA_regions), function (x) as.tbl(data.frame(FIPS = as.character(CA_regions[[x]]$FIPS), Region = x))))

with_region <- function (.data) {
  .data %>% inner_join(region_tbl, by = "FIPS")
}

xapply <- function (...) {
  unlist(sapply(..., simplify = FALSE))
}

coord_getter <- function (j) {
  f <- function (x) c(x@coords[, j, drop=TRUE], NA)
  function (obj) xapply(obj@Polygons, f)
}

poly_x <- setNames(lapply(CA_tracts@polygons, coord_getter(1)), row.names(CA_tracts))
poly_y <- setNames(lapply(CA_tracts@polygons, coord_getter(2)), row.names(CA_tracts))
poly_id <- setNames(lapply(CA_tracts@polygons, function (obj) rep(obj@ID, length(obj@Polygons))), row.names(CA_tracts))

defaultOptions <- list(fill=TRUE, fillOpacity=0.5, stroke=TRUE, opacity=1, color="#000000", weight=0.1)

color_ramp <- function (x, pal = "RdYlGn") {
  palette <- colorRampPalette(rev(brewer.pal(9, pal)))
  palette(n = 1 + length(levels(x)))[x]
}
