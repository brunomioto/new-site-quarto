library(ggplot2)
library(patchwork)

resultado_votos_locais_prop <- sf::read_sf("./posts/eleicao_mga_2024/pt/data/resultado_votos_locais_prop.geojson")

resultado_votos_locais_eleitos <- sf::read_sf("./posts/eleicao_mga_2024/pt/data/resultado_votos_locais_eleitos.geojson")


for(i in ver_eleitos){
  
  cli::cli_alert_info("Criando heatmap - {i}")


teste_df <- resultado_votos_locais_eleitos |>
  filter(nr_votavel == i) |>
  tidyr::uncount(votos_local)

d <- MASS::kde2d(teste_df$longitude,
                 teste_df$latitude, 
                 n = 700,h = 0.02,
                 lims = c(-52.06975-0.1, -51.86916+0.1, -23.52543-0.1, -23.36496+0.1)
                 )
dens <- data.frame(expand.grid(x = d$x, y = d$y), z = as.vector(d$z))


plot <- resultado_votos_locais_eleitos |>
  ggplot()+
  geom_contour_filled(data = dens, aes(x = x, y = y, z = z,
                                       fill = after_stat(nlevel)))+
  geom_sf(data = new_streets,
          lineend = "round")+
  geom_sf(shape = 21,
        fill = "white",
        color = "black",
        size = 1.5,
        stroke = 0.7,
        aes(geometry = geometry),
        stat = "unique"
  )+
  ggplot2::scale_fill_gradientn(
    colors = rev(RColorBrewer::brewer.pal(10, "Spectral"))[1:10],
    limits = c(0.2,NA),
    na.value = NA
  )+
  theme_void()+
  theme(
    plot.background = element_rect(fill = "#fafafa", color = NA),
    legend.position = "none"
  )

# plot
mga <- plot+
  annotate(
    "text",
    x = -51.86916,
    y = -23.469,
    label = "Bruno Mioto\nwww.brunomioto.com",
    hjust = 1,
    lineheight = 0.8,
    size = 2
  )+
coord_sf(
  xlim = c(-51.99, -51.865),
  ylim = c(-23.37,-23.47),
)+
  labs(
    title = "Maringá"
  )+
  theme_void()+
  theme(
    plot.background = element_rect(fill = "#fafafa"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
# mga
floriano <- plot+
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

iguatemi <- plot+
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

saodomingos <- plot+
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


plot_all <- iguatemi+saodomingos+floriano+mga+
  patchwork::plot_layout(
    # widths = c(0.4,0.3,0.3),
    design = 
      "ADDD
       BDDD
       CDDD"
  )&
  theme(
    strip.text = element_text(face = "bold"),
    # strip.background = element_blank(),
    # plot.background = element_rect(fill = "#fafafa", color = NA),
    # panel.background = element_rect(fill = "#fafafa", color = NA)
  )
# plot_all
ggsave(plot = plot_all, filename = glue::glue("./posts/eleicao_mga_2024/pt/figures/votacao/{i}_heat.png"),
       width = 6, height = 4, dpi = 600)
}

#teste


library(ggiraph)

gg <- resultado_votos_locais_eleitos |>
  ggplot()+
  geom_contour_filled(data = dens, aes(x = x, y = y, z = z,
                                       fill = after_stat(nlevel)))+
  geom_sf(data = new_streets,
          lineend = "round")+
  geom_sf_interactive(
    data = resultado_votos_locais_eleitos |> 
      distinct(nome_do_local, .keep_all = TRUE),
    aes(
      tooltip = glue::glue(
        "<b>Local:</b> {stringr::str_wrap(nome_do_local,width = 20)}
        <b>Votos no local:</b> {votos_local}
        <b>Proporção dos votos:</b> {scales::percent(prop_votos,accuracy = 0.1)}"
      )),
    shape = 21,
    fill = "white",
    color = "black",
    size = 1.5,
    stroke = 0.7
  )+
  ggplot2::scale_fill_gradientn(
    colors = rev(RColorBrewer::brewer.pal(10, "Spectral"))[1:10],
    limits = c(0.2,NA),
    na.value = NA
  )+
  theme_void(base_family = "Arial")+
  theme(
    plot.background = element_rect(fill = "#fafafa", color = NA),
    legend.position = "none"
  )+
    coord_sf(
      xlim = c(-51.99, -51.865),
      ylim = c(-23.37,-23.47)
    )

x <- girafe( ggobj = gg)
x


library(htmlwidgets)


# Salve como arquivo HTML
saveWidget(x, "./posts/eleicao_mga_2024/pt/figures/votacao/teste_int.html")
# scales::percent(prop_votos,accuracy = )

#








# library(ggplot2)
# 
# # 
# # 
# # resultado_votos_locais_prop_sf <- resultado_votos_locais_prop |> 
# #   mutate(grupo = case_when(longitude > -52 ~ "Maringá",
# #                            latitude < -23.5 ~ "Floriano",
# #                            TRUE ~ "Iguatemi/São Domingos"))|> 
# #   sf::st_as_sf(coords = c("longitude", "latitude"))%>%
# #   sf::st_set_crs(4326) 
# # mga_vot_ver |> 
# #   filter(stringr::str_detect(NM_VOTAVEL, "ANA LUCIA"))
# 
# 
# 
# 
# #Votos
# 
# for(i in ver_eleitos){
#   
#   cli::cli_alert_info("Criando heatmap - {i}")
#   
#   plot <- resultado_votos_locais_eleitos |>
#     filter(nr_votavel == i) |>
#     tidyr::uncount(votos_local) |>
#     ggplot()+
#     stat_density_2d(aes(
#       fill = stat(nlevel),
#       x=longitude,y=latitude),
#       # h = 0.02,
#       n = 500,
#       geom = "polygon"
#     )+
#     geom_sf(data = new_streets)+
#     ggplot2::scale_fill_gradientn(
#       colors = rev(RColorBrewer::brewer.pal(10, "Spectral"))[2:10],
#       limits = c(0.2,NA),
#       na.value = NA
#     )+
#     coord_sf(
#       xlim = c(-51.99, -51.865),
#       ylim = c(-23.37,-23.47)
#     )+
#     theme_void()+
#     theme(
#       plot.background = element_rect(fill = "#fafafa", color = NA),
#       legend.position = "none"
#     )
# 
# ggsave(plot = plot, filename = glue::glue("./posts/eleicao_mga_2024/pt/figures/votacao/{i}_heat.png"),
#        width = 7, height = 6)
# }



