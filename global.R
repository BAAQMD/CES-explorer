suppressPackageStartupMessages({
  library(shinyIncubator)
  library(dplyr)
  library(tidyr)
  library(CalEnviroScreen)
  library(leaflet)
  library(RColorBrewer)
  library(rgeos)
})

msg <- function (...) if (FALSE) message(...)

options(shiny.json.digits = 8) # Affects precision of polygon coordinates
options(digits = 3)

Q <- function (x, ..., na.rm = TRUE) {
  quantile(x, ..., na.rm = na.rm)
}

cut_quantile <- function (x, n = 20, ..., na.rm = TRUE) {
  q <- quantile(x, seq(0, 1, len=n+1), na.rm = na.rm)
  cut(x, breaks = q, labels = names(q)[-length(q)], include.lowest = TRUE, ordered_result = TRUE, na.rm = na.rm)
}

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

#pctl_tbl <- CES2_pctls
#CES2_tbl <- pctl_tbl %>%
#  gather(Variable, Pctl, -FIPS) %>%
#  inner_join(group_tbl, by = "Variable")

# Relevel based on correlations with PopChar and Pollution, respectively
SORTED_POPCHAR_VARS <- rev(c("Edu", "Poverty", "Asthma", "Unemp", "LingIso", "LBW", "Age"))
SORTED_POLLUTION_VARS <- c("PM25", "ToxRel", "DieselPM", "Traffic", "DrinkWat", "Ozone", "PestUse", "HazWaste", "Cleanup", "GndWat", "SolWaste", "ImpWat")
CES2_VARS <- c(SORTED_POLLUTION_VARS, SORTED_POPCHAR_VARS)

CES2_tbl <- inner_join(
  CES2_data %>% gather(Variable, Value, -FIPS),
  CES2_pctls %>% gather(Variable, Pctl, -FIPS),
  by = c("FIPS", "Variable")) %>%
  mutate(Variable = factor(Variable, levels = c(SORTED_POLLUTION_VARS, SORTED_POPCHAR_VARS)),
         Rank = comp_rank(Value),
         Percentile = pctl(Value)) %>%
  inner_join(group_tbl, by = "Variable")

calc_logRP_scores <- function (.data) {

  .data %>%
    filter(!is.na(Value)) %>%
    group_by(Variable) %>%
    mutate(Rank = rank(-Value, ties.method="min"), Frac = Rank / n()) %>%
    group_by(FIPS) %>%
    summarise(N = sum(!is.na(Frac)), `-log(Score)` = weighted.mean(-log(Frac), Weight), na.rm=TRUE) %>%
    #summarise(N = n(), `-log(Score)` = sum(-log(Frac) * Weight)) %>%
    filter(N > max(N, na.rm=TRUE) * 0.75)

}

calc_logRP_subscores <- function (.data) {

  pollution_tbl <- .data %>%
    filter(Variable %in% CES2_POLLUTION_VARS) %>%
    calc_logRP_scores() %>%
    filter(N >= floor(max(N, na.rm=TRUE) * 0.5)) %>%
    select(FIPS, `N(Pollution)` = N, `-log(Pollution)` = `-log(Score)`)

  popchar_tbl <- .data %>%
    filter(Variable %in% CES2_POPCHAR_VARS) %>%
    calc_logRP_scores() %>%
    filter(N >= floor(max(N, na.rm=TRUE) * 0.5)) %>%
    select(FIPS, `N(PopChar)` = N, `-log(PopChar)` = `-log(Score)`)

  left_join(pollution_tbl, popchar_tbl, by = "FIPS")
}
