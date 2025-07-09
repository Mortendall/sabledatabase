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
  pattern = "(?<=/)RMPP-[:digit:]{4}-[:digit:]{3}A?B?_?[:alnum:]*_[:alnum:]+(?=\\.csv)"
                                     )

names(data_files_list)<- studies_list

#convert to long format
long_files_list <- purrr::map2(data_files_list,studies_list,
                              ~prepare_long_format(.x,.y))
source(here::here("doc/metadata_assembly.R"))

#create list
study_list <- vector(mode = "list",length = length(metadata_list))
study_list <- purrr::map(study_list,
                         ~list())
name_vector<- dplyr::distinct(metadata_frame, RMPP_ID) |>
  dplyr::pull(RMPP_ID)
study_list <- magrittr::set_names(study_list, name_vector)

for (i in 1:length(study_list)){
  #detect matches pr. target
  matching_studies <- stringr::str_detect(names(long_files_list),
                                          pattern = names(study_list)[i])
  study_list[i]<- list(long_files_list[matching_studies])
}

study_list_collapsed <- purrr::map(study_list,
                                   ~as.data.frame(do.call(
                                     rbind, .)))



sabledatabase <- duckdb::dbConnect(
  duckdb::duckdb(
    dbdir = here::here("data/sabledata.duckdb")
  ))

name_vector <- names(study_list)
purrr::map(name_vector,
           ~ duckdb::dbWriteTable(sabledatabase,
                                  name = .,
                                  value = study_list_collapsed[[.]],
                                  overwrite = T
                                    )
           )

duckdb::dbWriteTable(sabledatabase,
                     name = "metadata",
                     value = metadata_frame, overwrite = T)

duckdb::dbDisconnect(sabledatabase)

# test <-
# dplyr::tbl(sabledatabase, "RMPP-2024-062") |>
#   dplyr::filter(cage_id == "1") |>
#   head(5) |>
#   dplyr::collect()
#
# #add metadata
#
#
# #load metadata (for optimization)
#
# test <- dm::tbl(sabledatabase, "metadata") |>
#   dplyr::collect()


#test summary generator
