library(readr)
library(dplyr)
library(arrow)
# 
perfil_eleitor <- readr::read_csv2("./posts/eleicao_mga_2024/pt/data/perfil_eleitor_secao_2024_PR.csv",
                                   locale = locale(encoding = "ISO-8859-1")) |> 
  janitor::clean_names()
# 
# perfil_eleitor |> 
#   filter(NM_MUNICIPIO == "MARINGÁ")
# 
perfil_eleitor |>
  group_by(cd_municipio) |>
  write_dataset(path = "./posts/eleicao_mga_2024/pt/data/perfil_eleitor",
                format = "parquet")

perfil_eleitor_mga <- open_dataset("./posts/eleicao_mga_2024/pt/data/perfil_eleitor") |> 
  filter(cd_municipio == 76910) |> 
  collect()


genero_eleitor <- perfil_eleitor_mga |> 
  group_by(nr_zona, nr_secao, ds_genero) |> 
  summarise(
    eleitores = sum(qt_eleitores_perfil)
  )

idade_eleitor_local <- perfil_eleitor_mga |> 
  group_by(nr_zona, nr_secao, ds_faixa_etaria) |> 
  summarise(
    eleitores = sum(qt_eleitores_perfil)
  ) |> 
  left_join(locais_mga, by = c("nr_zona" = "zona",
                               "nr_secao" = "secao")) |> 
  ungroup() |> 
  group_by(nome_do_local, ds_faixa_etaria, latitude, longitude) |>
  mutate(ds_faixa_etaria = case_when(
    ds_faixa_etaria == "16 anos" ~ "16 a 20 anos",
    ds_faixa_etaria == "17 anos" ~ "16 a 20 anos",
    ds_faixa_etaria == "18 anos" ~ "16 a 20 anos",
    ds_faixa_etaria == "19 anos" ~ "16 a 20 anos",
    ds_faixa_etaria == "20 anos" ~ "16 a 20 anos",
    TRUE ~ ds_faixa_etaria
  )) |> 
  summarise(
    eleitores = sum(eleitores)
  )|> 
  ungroup() |> 
    mutate(grupo = case_when(longitude > -52 ~ "Maringá",
                             latitude < -23.5 ~ "Floriano",
                             TRUE ~ "Iguatemi/São Domingos"))|>
    sf::st_as_sf(coords = c("longitude", "latitude"),remove = FALSE)%>%
    sf::st_set_crs(4326)


library(ggplot2)


for(i in unique(idade_eleitor_local$ds_faixa_etaria)){
  
  cli::cli_alert_info(i)
  
  plot <- idade_eleitor_local |>
    filter(ds_faixa_etaria == i) |>
    tidyr::uncount(eleitores) |> 
    ggplot()+
    stat_density2d(aes(fill = after_stat(level),
                       x=longitude, y=latitude),
                   h = 0.02,
                   n = 500,
                   geom = "polygon")+
    geom_sf(data = new_streets)+
    ggplot2::scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(7, "Spectral")),
                                  # limits = c(150,NA),
                                  na.value = NA
    )+
    # geom_sf(shape = 21,
    #         fill = "#dd1c77",
    #         # color = "white",
    #         # size = 2
    #         aes(size = eleitores,
    #             geometry = geometry)
    # )+
    # scale_size_area(breaks = c(0.025,0.05,0.075,0.1))+
    coord_sf(
      xlim = c(-51.99, -51.865),
      ylim = c(-23.37,-23.47)
    )+
    labs(
      title = "Maringá"
    )+
    theme_void()+
    theme(
      plot.background = element_rect(fill = "#fafafa", color = NA),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )
  
  nome <- stringr::str_replace_all(i, " ", "_")
  
  ggsave(plot = plot, filename = glue::glue("./posts/eleicao_mga_2024/pt/figures/perfil/{nome}_heat.png"),
         width = 7, height = 6)
}  

flor <- idade_eleitor_local |>
  filter(ds_faixa_etaria == "50 a 54 anos") |>
  tidyr::uncount(eleitores) |>
  ggplot()+
  stat_density2d(aes(fill = after_stat(level),
                     x=longitude, y=latitude),
                 h = 0.02,
                 n = 500,
                 geom = "polygon")+
  geom_sf(data = new_streets)+
  ggplot2::scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(7, "Spectral")),
                                # limits = c(150,NA),
                                na.value = NA
  )+
  # geom_sf(shape = 21,
  #         fill = "#dd1c77",
  #         # color = "white",
  #         # size = 2
  #         aes(#size = eleitores,
  #             geometry = geometry)
  # )+
  coord_sf(
    xlim = c(-52.06, -52.04),
    ylim = c(-23.52,-23.53)
  )+
  labs(
    title = "Floriano"
  )+
  theme_void()+
  theme(
    plot.background = element_rect(fill = "#fafafa", color = "black"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
flor  


igua <- idade_eleitor_local |>
  filter(ds_faixa_etaria == "50 a 54 anos") |>
  # tidyr::uncount(eleitores) |> 
  ggplot()+
  # stat_density2d(aes(fill = after_stat(level),
  #                    x=longitude, y=latitude),
  #                h = 0.02,
  #                n = 500,
  #                geom = "polygon")+
  geom_sf(data = new_streets)+
  geom_sf(shape = 21,
          fill = "#dd1c77",
          # color = "white",
          # size = 2
          aes(size = eleitores,
              geometry = geometry)
  )+
  coord_sf(
    xlim = c(-52.075, -52.053),
    ylim = c(-23.363,-23.385)
  )+
  labs(
    title = "Iguatemi"
  )+
  theme_void()+
  theme(
    plot.background = element_rect(fill = "#fafafa", color = "black"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

sao <- idade_eleitor_local |>
  filter(ds_faixa_etaria == "50 a 54 anos") |>
  # tidyr::uncount(eleitores) |> 
  ggplot()+
  # stat_density2d(aes(fill = after_stat(level),
  #                    x=longitude, y=latitude),
  #                h = 0.02,
  #                n = 500,
  #                geom = "polygon")+
  geom_sf(data = new_streets)+
  geom_sf(shape = 21,
          fill = "#dd1c77",
          # color = "white",
          # size = 2
          aes(size = eleitores,
              geometry = geometry)
  )+
  coord_sf(
    xlim = c(-52.034, -52.026),
    ylim = c(-23.383,-23.386)
  )+
  labs(
    title = "São Domingos"
  )+
  theme_void()+
  theme(
    plot.background = element_rect(fill = "#fafafa", color = "black"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

sao
library(patchwork)

igua+sao+flor+plot+
  patchwork::plot_layout(
    design = 
      "ADDD
       BDDD
       CDDD"
  )

