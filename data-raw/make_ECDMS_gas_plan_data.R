make_ECDMS_gas_plan_data <- function (
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
    mutate(
      tput_unit = "MMthm") %>%
    mutate_at(
      vars(year),
      ~ CY(elide_year(.))) %>%
    # mutate(,
    #   ef_qty = 0.005302,
    #   ems_qty = tput_qty * ef_qty,
    #   ems_unit = "MMTCO2eq") %>%
    mutate(
      plan = relevel(
        factor(plan),
        "Pacific Gas and Electric")) %>%
    select(
      year,
      sector,
      plan,
      starts_with("tput_")) %>%
    arrange(
      year,
      sector,
      plan)

  return(tidied)

}
