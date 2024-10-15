library(readr)
library(dplyr)
library(arrow)

locais_mga <- open_dataset("./posts/eleicao_mga_2024/pt/data/locais_votacao") |> 
  filter(municipio == "MARINGA") |> 
  mutate(grupo = case_when(longitude > -52 ~ "Maringá",
                           latitude < -23.5 ~ "Floriano",
                           TRUE ~ "Iguatemi/São Domingos"))|> 
  select(-c(possui_acessibilidade, logradouro, bairro, cep)) |> 
  collect()

# locais_mga2 <- locais_mga |> 
#   group_by(nome_do_local, latitude, longitude) |> 
#   summarise(
#     soma = sum(qtde_de_eleitores_aptos)
#   ) |> 
#   ungroup() |> 
#   slice(1:100)
  


# locais_mga_sf <- locais_mga |> 
#   sf::st_as_sf(coords = c("longitude", "latitude"))%>%
#   sf::st_set_crs(4326) 

# readr::write_csv(locais_mga2, "./posts/eleicao_mga_2024/pt/data/locais2.csv")
 