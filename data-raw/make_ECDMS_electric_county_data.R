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
      ~ CY(elide_year(.)))

  tidied <-
    reshaped %>%
    filter(
      sector != "Total") %>%
    mutate(
      county = str_to_title(county)) %>%
    mutate(
      tput_unit = "GWh")

  return(tidied)

}
