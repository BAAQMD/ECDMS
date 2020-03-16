make_ECDMS_electric_plan_data <- function (
  xlsx_path
) {

  raw_data <-
    read_excel(
      xlsx_path)

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
