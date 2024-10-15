library(readr)
library(dplyr)
library(arrow)

eleicao_mga <- readr::read_csv2("./posts/eleicao_mga_2024/pt/data/votacao_secao-municipio_2024_pr_maringa.csv", 
                                locale = locale(encoding = "Latin1"))

mga_vot_ver <- eleicao_mga |> 
  group_by(nr_votavel,nm_votavel,
           nr_zona,nr_secao,nm_local_votacao) |> 
  summarise(
    votos = sum(qt_votos, na.rm = TRUE)
  ) |> 
  ungroup()


ver_eleitos <- c(
  30123,
  22222,
  55001,
  13300,
  44190,
  12412,
  20999,
  55555,
  45800,
  11511,
  30030,
  44456,
  11900,
  20123,
  11800,
  11222,
  10123,
  12345,
  11789,
  36333,
  12122,
  11411,
  22111
)



# glimpse(mga_vot_ver)
# mga_vot_pref <- open_dataset("./posts/eleicao_mga_2024/pt/data/votacao_cidades") |> 
#   filter(CD_MUNICIPIO == 76910,
#          DS_CARGO == "Prefeito") |> 
#   group_by(NR_VOTAVEL,NM_VOTAVEL,
#            NR_ZONA,NR_SECAO,NM_LOCAL_VOTACAO) |> 
#   summarise(
#     votos = sum(QT_VOTOS, na.rm = TRUE)
#   ) |> 
#   collect()
