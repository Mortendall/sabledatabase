library(duckplyr)
#import Sabledata

data_files <- fs::dir_ls(here::here("data-raw"),
                         glob = "*.csv"
                         )

data_files_list <- vector(mode = "list",
                          length = length(data_files))

data_files_list <- purrr::map(data_files,
                              ~vroom::vroom(.,show_col_types = F))

#convert first unit to date-time
data_files_list <- purrr::map(data_files_list,
                              ~dplyr::mutate(.,
                                             Date_Time_1 =
                                               as.POSIXct(Date_Time_1,
                                                          format = "%m/%d/%Y %H:%M:%OS")))
#extract the study names

studies_list <- stringr::str_extract(
  names(data_files_list),
  pattern = "(?<=/)RMPP-[:digit:]{4}-[:digit:]{3}B?_?[:alnum:]*_[:alnum:]+(?=\\.csv)"
                                     )

names(data_files_list)<- studies_list

#convert to long format
long_sable <- data_files_list[[1]]
colnames(long_sable) <- stringr::str_replace(colnames(long_sable),
                                             pattern = "kcal_",
                                             replacement = "kcal-")
system_name <- stringr::str_extract(names(data_files_list[1]),
                                    pattern = "(?<=_)[:alnum:]+")
long_sable <- long_sable |>
  dplyr::mutate(system = system_name,
                elapsed_h = as.numeric(Date_Time_1-Date_Time_1[1])/360,
                elapsed_min = as.numeric(Date_Time_1-Date_Time_1[1])/60)|>
  dplyr::select(-DurationMin_1,
                -envirorh_1,
                -envirooccupancy_1,
                -envirosound_1,
                -envirotemp_1) |>
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

source(here::here("doc/metadata_assembly.R"))




sabledatabase <- duckdb::dbConnect(duckdb::duckdb())

sabledatabase

con <- dbConnect(duckdb::duckdb())
dm::copy_dm_to(
  con,
  dm::dm_pixarfilms(),
  set_key_constraints = FALSE,
  temporary = FALSE
)
