make_ECDMS_electric_plan_data <- function (
  path
) {

  raw_data <-
    tbltools::read_tbl(
      path,
      verbose = TRUE)

  names(raw_data)[1] <-
    "plan"

  renamed <-
    rename(
      raw_data,
      year = Year)

  reshaped <-
    renamed %>%
    gather(
      sector,
      tput_qty,
      -year,
      -plan)

  tidied <-
    reshaped %>%
    filter(
      sector != "Total Usage") %>%
    mutate_at(
      vars(year),
      ~ CY(elide_year(.))) %>%
    mutate(
      tput_unit = "GWh")

  return(tidied)

}
