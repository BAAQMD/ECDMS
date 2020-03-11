library(inventory)

FILL_COLORS <- c(
  "Residential" = "#1fbfc3",
  "Non-Residential" = "#f57670",
  "Total" = "#000000")

stripchart_by_county <- function (.data, values, ...) {
  paneled_stripchart(.data, values = values, fill_by = "sector", facet_by = "cnty_name", colors = FILL_COLORS)
}

gas_county %>%
  filter(cnty_name %in% names(DST_COUNTY_NAMES)) %>%
  stripchart_by_county(values = "tput_qty")

gas_county %>%
  filter(cnty_name %in% names(DST_COUNTY_NAMES)) %>%
  stripchart_by_county(values = "ems_qty")

GAS_FUEL_CODES <-
  c(708, 188, 189, 237, 198, 338, 238,
    235, 758, 493, 511, 417, 160) # DMH: omitting 242, 98, and 816 (vs last time)

gas_county %>%
  filter(
    cnty_name %in% names(DST_COUNTY_NAMES)) %>%
  pivot_chart(
    values = "tput_qty",
    rows = "cnty_name",
    aggregator = "Integer Sum",
    columns = c("tput_unit", "year"))

# PY(1993:2016) %>%
#   point_source_emissions() %>%
#   filter(cnty_abbr == "CC") %>%
#   mutate(mat_id = substr(src_code, 6, 8)) %>%
#   filter(mat_id %in% as.character(GAS_FUEL_CODES)) %>%
#   filter(pol_abbr == "CO") %>%
#   pivot_table(rows = c("fac_id"))

###############################################################################
#
# Now some of the same, but for electricity instead of natural gas
#

electric_county <- local({

  raw_data <-
    read_excel(here::here(
      "Data", "CEC", "ECDMS", "2019-10-03",
      "ElectricityByCounty-20191003.xlsx"))

  names(raw_data)[1] <- "County" # fix double-quoted column name
  renamed <- rename(raw_data, cnty_name = County, sector = Sector)
  reshaped <- renamed %>% gather_years("tput_qty")
  reshaped %>%
    filter(sector != "Total Usage") %>%
    mutate(cnty_name = str_to_title(cnty_name)) %>%
    mutate(tput_unit = "GWh")
})

electric_county %>%
  filter(cnty_name %in% names(DST_COUNTY_NAMES)) %>%
  stripchart_by_county(values = "tput_qty")

###############################################################################

electric_planning_area <- local({

  raw_data <-
    read_excel(here::here(
      "Data", "CEC", "ECDMS", "2019-10-03",
      "ElectricityByPlanningArea-201910035.xlsx"))

  names(raw_data)[1] <- "region"
  renamed <- rename(raw_data, year = Year)
  reshaped <- renamed %>% gather(sector, tput_qty, -year, -region)
  reshaped %>%
    filter(sector != "Total Usage") %>%
    mutate(tput_unit = "GWh")
})


stripchart_by_region <- function (.data, values, ...) {
  paneled_stripchart(.data, values = values, fill_by = "sector", facet_by = "region")
}

SELECTED_REGIONS <- c("Pacific Gas and Electric", "Southern California Edison", "San Diego Gas and Electric")

electric_planning_area %>%
  mutate(region = if_else(region %in% SELECTED_REGIONS, region, "All Other")) %>%
  annual_throughputs_by(region, sector) %>%
  stripchart_by_region(values = "tput_qty")

electric_planning_area %>%
  pivot_chart(rows = "sector")
