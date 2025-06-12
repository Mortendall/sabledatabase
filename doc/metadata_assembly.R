#####Import metadata data####

#list files
metadata_files <- fs::dir_ls(here::here("data-raw/metadata/"),
                             glob = "*.xlsx"
)
metadata_list <- vector(mode = "list",
                        length = length(metadata_files))

metadata_list <- purrr::map(metadata_files,
                            ~readxl::read_xlsx(.x,
                                               sheet = 2,
                                               col_names = T,col_types = "text"))

metadata_frame <- dplyr::bind_rows(metadata_list)

#standardize nomenclature in metadata
metadata_frame <- metadata_frame |>
  dplyr::mutate(
    System = dplyr::case_when(
      System == "Sable 1"~"1",
      System == "Sable 2"~"2",
      System == "SABLE 2"~"2",
      System == "Sable 3"~"3",
      System == "Sable system used during run (1)"~"1",
      System == "Sable system used during run (2)"~"2",
      .default = System
    ),
    Gender = dplyr::case_when(
      Gender == "-"~NA,
      Gender == "empty"~NA,
      tolower(Gender) == "f"~"Female",
      Gender == "male"~"Male",
      tolower(Gender)== "m"~"Male",
      .default = Gender
    ),
    `Age (weeks)` = stringr::str_remove(`Age (weeks)`, " "),
    `Age (weeks)` = stringr::str_remove_all(`Age (weeks)`, "[:alpha:]"),
    #go through studies and assign KO identity
    Strain = dplyr::case_when(
      tolower(Strain) == "c57bl6/n"~"C57BL/6N",

      tolower(Strain) == "c57bl6/j"~"C57BL/6J",
      tolower(Strain) =="c57bl/6rj"~"C57BL/6JRJ",
      tolower(Strain)== "c57bl/6jrj"~"C57BL/6JRJ",
      Strain == "B6"~"C57BL/6J",
      Strain == "BL6"~"C57BL/6J",
      .default = Strain
    ),
    Diet = dplyr::case_when(
      Diet == "C"|Diet == "Chow"|Diet == "Chow (normal)"| Diet == "Chow diet"|
        Diet == "Normal chow diet"~ "chow",
      Diet == "H"~"HFD",
      .default = Diet
    ),
    Temperature = dplyr::case_when(
      Temperature == "22C"~"22",
      Temperature == "23°C (normal)"~"23",
      Temperature == "4℃"|Temperature=="4 degrees"~"4",
      Temperature == "Room temperature"~"RT",
      Temperature == "-"~"empty",
      .default = Temperature
    ),
    Treatment = dplyr::case_when(
      Treatment == "-"~"empty",
      Treatment == "NA"|Treatment == "n/a"| Treatment == "no"|
        Treatment == "NA- Genotype/Diet responces assessed here"|
        Treatment == "None"~"none",
      Treatment == "Veh - EB1002"~"Vehicle - EB1002",
      Treatment == "EB1002 - Veh"~"EB1002 - Vehicle",
      .default = Treatment
    )
  )
