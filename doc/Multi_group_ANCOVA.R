library(duckplyr)


#get data from DB
sabledata <- duckdb::dbConnect(duckdb::duckdb(
  dbdir = here::here("data/sabledata.duckdb")
))

metadata <- dplyr::tbl(sabledata, "metadata") |>
  dplyr::collect() |>
  dplyr::filter(RMPP_ID == "RMPP-2025-079")

study_data <- dplyr::tbl(sabledata, "RMPP-2025-079") |>
  dplyr::collect()

duckdb::dbDisconnect(sabledata)

#filter metadata
metadata <- metadata |>
  dplyr::mutate(System = paste0("sable",System),
                Unique_ID = paste(System, Cage, sep = "_")) |>
dplyr::rename(Age = `Age (weeks)`) |>
  dplyr::filter(!is.na(System))

study_data_mod <- study_data |> dplyr::filter(elapsed_h<311)


#get summary data
displayed_data <-  study_data_mod |>
  dplyr::mutate(Unique_ID = paste(system, cage_id,sep = "_")) |>
  dplyr::group_by(Unique_ID) |>
  dplyr::summarise(ee = mean(ee),
                   feed = mean(feed),
                   bodymass = mean(bodymass),
                   vo2 = mean(vo2),
                   vco2 = mean(vco2)) |>
  dplyr::left_join( metadata,
                    by = c("Unique_ID"="Unique_ID")) |>
  dplyr::filter(Unique_ID != "sable2_8"& Unique_ID != "sable2_11")

#make XY plot
library(broom)
xy_plot <- ggpubr::ggscatter(
  data = displayed_data,
    x = "bodymass",
    y  = "ee",
    color = "Treatment",
  add = "reg.line"
  )+
  ggpubr::stat_regline_equation(
    ggplot2::aes( label = paste(..eq.label..,
                                      ..rr.label..,
                                      sep = "~~~~"),
                                color = Treatment))


xy_plot

#get ANOVA table

  anova_table <- displayed_data |> rstatix::anova_test(

    formula =  stats::as.formula("ee ~ bodymass * Treatment")
  )

  #calculate ANCOVA

  ancova_table <- rstatix::anova_test(ee ~ bodymass + Treatment,
             data = displayed_data)


  pwc <-  displayed_data |>
    rstatix::emmeans_test(ee ~ Treatment,
                                 covariate = bodymass,
                                 p.adjust.method = "bonferroni")


  #try 2-way ANCOVA

displayed_data <- displayed_data |>
  dplyr::mutate(
    GH = case_when(
      Treatment == "gh"| Treatment == "sema+gh"~"gh",
      .default = "veh"
    ),
    Sema = case_when(
      Treatment == "sema"|Treatment == "sema+gh"~"sema",
      .default = "veh"
    )
  )
anova_table_3way <- rstatix::anova_test(ee ~ bodymass * GH*Sema,
                                         data = displayed_data)
ancova_table_2way <- rstatix::anova_test(ee ~ bodymass + GH*Sema,
                    data = displayed_data)

ancova_table_2way
#significant interaction. Therefore, run two emmeans tests

model <- aov(ee ~ bodymass + GH * Sema, data = displayed_data)

#test effect of GH
emmeans::emmeans(model, pairwise ~GH|Sema)

emmeans::emmeans(model, pairwise ~Sema|GH)
