#' prepare_long_format
#'
#' @param data_list a csv data file in the wide format
#'
#' @return a data frame in the long format

prepare_long_format <- function(data_list, data_list_names){
  long_sable <- data_list
  colnames(long_sable) <- stringr::str_replace(colnames(long_sable),
                                               pattern = "kcal_",
                                               replacement = "kcal-")
  system_name <- stringr::str_extract(data_list_names,
                                      pattern = "(?<=_)[:alnum:]+$")
  long_sable <- long_sable |>
    dplyr::mutate(system = system_name,
                  elapsed_h = as.numeric(Date_Time_1-Date_Time_1[1])/3600,
                  elapsed_min = as.numeric(Date_Time_1-Date_Time_1[1])/60)

  if("envirolightlux_2"%in%colnames(long_sable)){
    long_sable <- long_sable |>
      dplyr::select(-DurationMin_1,
                    -envirorh_2,
                    -envirooccupancy_2,
                    -envirosound_2,
                    -envirotemp_2) |>
      dplyr::rename(envirolightlux_1 = envirolightlux_2)
  }

  else if("DurationMin_1"%in% colnames(long_sable)){
    long_sable <- long_sable |>
      dplyr::select(-DurationMin_1,
                    -envirorh_1,
                    -envirooccupancy_1,
                    -envirosound_1,
                    -envirotemp_1)
  }

  else{
    long_sable <- long_sable |>
      dplyr::select(-durationmin,
                    -envirorh_1,
                    -envirooccupancy_1,
                    -envirosound_1,
                    -envirotemp_1)
  }
    long_sable <- long_sable |>
    dplyr::filter(!is.na(Date_Time_1)) |>
    tidyr::pivot_longer(cols = -c("Date_Time_1",
                                  "envirolightlux_1",
                                  "system",
                                  "elapsed_h",
                                  "elapsed_min"),
                        names_to = c("parameter","cage_id"),
                        names_sep = "_") |>
    tidyr::pivot_wider(names_from = "parameter",
                       values_from = "value") |>
    #convert to more CalR resembling format
    dplyr::rename(ee = `kcal-hr`,
                  feed = foodupa,
                  water = waterupa
    ) |>
    dplyr::group_by(cage_id) |>
    dplyr::mutate(vo2 = vo2*60,
                  #converts vo2 from ml/min to ml/h
                  vco2 = vco2*60,
                  ee.acc = cumsum(ee),
                  feed.acc = cumsum(feed),
                  water.acc = cumsum(water),
                  xytot = xbreak + ybreak
    )
  return(long_sable)

}
