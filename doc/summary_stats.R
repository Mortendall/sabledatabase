library(duckplyr)
library(tidyverse)
library(patchwork)

sabledata <- duckdb::dbConnect(duckdb::duckdb(
  dbdir = here::here("data/sabledata.duckdb")
))



metadata <- dplyr::tbl(sabledata, "metadata") |>
  dplyr::collect()

duckdb::dbDisconnect(sabledata)

metadata <- metadata |>
  dplyr::filter(!is.na(Strain)) |>
  dplyr::mutate(`Mouse Strain` = dplyr::case_when(
    !is.na(stringr::str_extract(Strain,
                                "^C57"))~Strain,
    Strain == "TallyHo/JNG"~ Strain,
    Strain == "empty"~Strain,
    Strain == NA ~ NA,
    Strain == "-"~Strain,
    .default = "GMO line"

  ))

gender <- ggplot(metadata,
                 aes(x = Gender,
                     fill = Gender) ) +
  geom_bar()+
  scale_fill_manual(values = c( "salmon","skyblue", "grey" ))+
  theme_bw()+
  ggtitle("Gender")+
  theme(
    plot.title= element_text(hjust = 0.5,
                             size = 24),
    legend.position = "none",
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )
gender

strain <- ggplot(metadata,
                 aes(x = `Mouse Strain`,
                     fill =`Mouse Strain`) ) +
  geom_bar()+
  scale_fill_manual(values = viridis::inferno(length(unique(metadata$`Mouse Strain`))))+
  theme_bw()+
  ggtitle("Strain")+
  theme(
    plot.title= element_text(hjust = 0.5,
                             size = 24),
    legend.position = "right",
    axis.text.x = element_text(size = 0),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 18)
  )
strain

metadata <- metadata |>
  dplyr::mutate(
    Temperature = dplyr::case_when(
      Temperature == "22 C for days 1-5, 4 C dor days 5-12"~"22 C for days 1-5,\n 4 C for days 5-12",
      .default = Temperature
    )
  )

temperature <- ggplot(metadata,
                      aes(x = Temperature,
                          fill = Temperature) ) +
  geom_bar()+
  scale_fill_manual(values = viridis::magma(length(unique(metadata$Temperature))))+
  theme_bw()+
  ggtitle("Temperature in degrees Celcius")+
  theme(
    plot.title= element_text(hjust = 0.5,
                             size = 24),
    legend.position = "right",
    axis.text.x = element_text(size = 0),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 18)
  )
temperature

diet <- ggplot(metadata,
               aes(x = Diet,
                   fill = Diet) ) +
  geom_bar()+
  scale_fill_manual(values = viridis::rocket(length(unique(metadata$Diet))))+
  theme_bw()+
  ggtitle("Diet")+
  theme(
    plot.title= element_text(hjust = 0.5,
                             size = 24),
    legend.position = "right",
    axis.text.x = element_text(size = 0),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 18)
  )
diet

age <- ggplot(metadata,
              aes(x = `Age (weeks)`,
                  fill = `Age (weeks)`) ) +
  geom_bar()+
  scale_fill_manual(values = viridis::mako(length(unique(metadata$`Age (weeks)`))))+
  theme_bw()+
  ggtitle("Age")+
  theme(
    plot.title= element_text(hjust = 0.5,
                             size = 24),
    legend.position = "right",
    axis.text.x = element_text(size = 0),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 18)
  )
age

system <- ggplot(metadata,
              aes(x = System,
                  fill = System )) +
                geom_bar()+
                scale_fill_manual(values = viridis::turbo(length(unique(metadata$System))))+
                theme_bw()+
                ggtitle("Sable System ID")+
                theme(
                  plot.title= element_text(hjust = 0.5,
                                           size = 24),
                  legend.position = "none",
                  axis.text.x = element_text(size = 18),
                  axis.text.y = element_text(size = 14),
                  axis.title.x = element_text(size = 18),
                  axis.title.y = element_text(size = 18)
                )
system

graph_panel_layout <- "
111222
111222
333444
333444
555666
555666
"

graph_panel <- gender+
  strain+
  age+
  temperature+
  diet+
  system+
  patchwork::plot_layout(design = graph_panel_layout)

grDevices::png(filename = here::here("doc/metadata_overview.png"),
               width = 60,
               height = 30,
               units = "cm",res = 200)
graph_panel
grDevices::dev.off()

