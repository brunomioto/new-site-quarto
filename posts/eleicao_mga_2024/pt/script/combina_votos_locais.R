library(readr)
library(dplyr)

resultado_votos_locais <- mga_vot_ver |> 
  left_join(locais_mga, join_by("nr_zona" == "zona",
                                "nr_secao" == "secao"))


resultado_votos_locais_prop <- resultado_votos_locais |> 
  group_by(nr_votavel, nm_votavel, nome_do_local, latitude, longitude) |> 
  summarise(
    votos_local = sum(votos)
  ) |> 
  ungroup() |> 
  group_by(nr_votavel) |> 
  mutate(
    votos_candidato = sum(votos_local)
  ) |> 
  rowwise() |> 
  mutate(prop_votos = votos_local/votos_candidato)|>
  mutate(grupo = case_when(longitude > -52 ~ "Maringá",
                           latitude < -23.5 ~ "Floriano",
                           TRUE ~ "Iguatemi/São Domingos"))|>
  sf::st_as_sf(coords = c("longitude", "latitude"),remove = FALSE)%>%
  sf::st_set_crs(4326)

sf::write_sf(resultado_votos_locais_prop, "./posts/eleicao_mga_2024/pt/data/resultado_votos_locais_prop.geojson")


resultado_votos_locais_eleitos <- resultado_votos_locais_prop |> 
  ungroup() |> 
  filter(nr_votavel %in% ver_eleitos)


sf::write_sf(resultado_votos_locais_eleitos, "./posts/eleicao_mga_2024/pt/data/resultado_votos_locais_eleitos.geojson")

resultado_votos_locais_eleitos |> 
  group_by(nr_votavel, nm_votavel, nome_do_local) |> 
  summarise(
    votos = sum(votos_local)
  )