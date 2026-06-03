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
  pattern = "(?<=/)RMPP-[:digit:]{4}-[:digit:]{3}A?B?C?_?[:alnum:]*_[:alnum:]+(?=\\.csv)"
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

#####Manual fixes applied to modified database####

#check studies that cannot be loaded
# problematic_studies <- c("RMPP-2025-001",
#                          "RMPP-2025-014",
#                          "RMPP-2025-035",
#                          "RMPP-2026-013A",
#                          "RMPP-2026-027")
# metadata_subset <- metadata |>
#   dplyr::filter(RMPP_ID %in% problematic_studies) |>
#   View()
#documentation for fixed studies
#RMPP-2026-013A
# metadata <- metadata |>
#   dplyr::mutate(
#     Cage =
#       dplyr::case_when(
#         RMPP_ID == "RMPP-2026-013A"~ `Cage number`,
#         .default = Cage
#       )
#   )

#RMPP-2025-001
# database_data <- dm::tbl(sabledatabase,"RMPP-2025-001") |>
#   dplyr::collect() |>
#   dplyr::mutate(system = dplyr::case_when(
#     system == "Sable3"~"sable3"
#   ))

# duckdb::dbWriteTable(sabledatabase,
#                      name = "RMPP-2025-001",
#                      value = database_data,
#                      overwrite = T)




#RMPP-2025-014
#wrong datafiles added - this should be fixed by replacing the file and running the script again

# duckdb::dbWriteTable(sabledatabase,"RMPP-2025-014",study_list_collapsed$`RMPP-2025-014`, overwrite = T)

#RMPP-2025-035

#wrongly annotated to Sable4

# database_data <- dm::tbl(sabledatabase,"RMPP-2025-035") |>
#   dplyr::collect() |>
#   dplyr::mutate(system = dplyr::case_when(
#     system == "sable4"~"sable3"
#   ))

# duckdb::dbWriteTable(sabledatabase,
#                      name = "RMPP-2025-035",
#                      value = database_data,
#                      overwrite = T)

#RMPP-2026-027

# database_data <- dm::tbl(sabledatabase,"RMPP-2026-027") |>
#   dplyr::collect() |>
#   dplyr::mutate(system = dplyr::case_when(
#     system == "sable1"~"sable2"
#   ))

# duckdb::dbWriteTable(sabledatabase,
#                        name = "RMPP-2026-027",
#                        value = database_data,
#                        overwrite = T)
#this study should be removed from database
#duckdb::dbRemoveTable(sabledatabase, "RMPP-2026-027")
# metadata <- dplyr::tbl(sabledatabase, "metadata") |>
#   dplyr::collect() |>
#   dplyr::filter(!RMPP_ID=="RMPP-2026-027")

# duckdb::dbWriteTable(sabledatabase,
#                      name = "metadata",
#                      value = metadata, overwrite = T)
