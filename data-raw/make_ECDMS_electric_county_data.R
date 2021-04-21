make_ECDMS_electric_county_data <- function (
  path
) {

  raw_data <-
    tbltools::read_tbl(
      path,
      verbose = TRUE)

  names(raw_data)[1] <-
    "County" # fix double-quoted column name

  renamed <-
    rename(
      raw_data,
      county = County,
      sector = Sector)

  reshaped <-
    renamed %>%
    gather_years("tput_qty") %>%
    mutate_at(
      vars(year),
      ~ CY(elide_year(.))) %>%
    vartools::drop_vars(
      `Total Usage`)

  validated <-
    reshaped %>%
    spread(
      sector,
      tput_qty) %>%
    mutate(
      Ratio = (`Non-Residential` + `Residential`) / Total,
      Diff = `Non-Residential` + `Residential` - Total) %>%
    ensurer::ensure(
      all(abs(.$Diff) < 0.1, na.rm = TRUE),
      all(abs(log(.$Ratio)) < 0.1, na.rm = TRUE)) %>%
    drop_vars(
      Total, Ratio, Diff) %>%
    gather(
      sector,
      tput_qty,
      `Non-Residential`,
      `Residential`)

  tidied <-
    validated %>%
    filter(
      sector != "Total") %>%
    mutate(
      county = str_to_title(county)) %>%
    mutate(
      tput_unit = "GWh")

  return(tidied)

}
