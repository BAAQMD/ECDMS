#' make_ECDMS_gas_county_data
#'
#' @details
#' 0.1 mmbtu/1 therm × 14.46 kg C/mmbtu × 44 kg CO2/12 kg C × 1 metric ton/1,000 kg =
#' 0.005302 metric tons CO2/therm (ref: https://www.epa.gov/energy/ghg-equivalencies-calculator-calculations-and-references)
#'
make_ECDMS_gas_county_data <- function (
  xlsx_path
) {

  raw_data <-
    read_excel(
      xlsx_path)

  names(raw_data)[1] <-
    "County" # fix double-quoted column name

  renamed <-
    rename(
      raw_data,
      county = County,
      sector = Sector)

  reshaped <-
    renamed %>%
    select(
      -`Total Usage`) %>%
    gather_years(
      "tput_qty") %>%
    mutate_at(
      vars(year),
      ~ CY(elide_year(.)))

  validated <-
    reshaped %>%
    spread(
      sector,
      tput_qty) %>%
    ensurer::ensure(
      all_true(
        ((.$`Non-Residential` + .$`Residential`) / .$`Total`) %>%
          between(0.999, 1.001))) %>%
    drop_vars(
      `Total`) %>%
    gather(
      sector,
      tput_qty,
      `Non-Residential`,
      `Residential`)

  tidied <-
    validated %>%
    mutate(
      county = str_to_title(
        county)) %>%
    mutate(
      tput_unit = "MMthm") %>%
    # mutate(,
    #   ef_qty = 0.005302,
    #   ems_qty = tput_qty * ef_qty,
    #   ems_unit = "MMTCO2eq") %>%
    mutate(
      sector = relevel(
        factor(sector),
        "Non-Residential")) %>%
    select(
      year,
      sector,
      county,
      starts_with("tput_")) %>%
    arrange(
      year,
      sector,
      county)

  return(tidied)

}
