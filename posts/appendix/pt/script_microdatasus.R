library(microdatasus)
library(dplyr)
library(ggplot2)
library(microdatasus)
library(tidyverse)
library(dbplyr)
library(DBI)
library(RSQLite)
options(timeout = 400)

conn <- dbConnect(RSQLite::SQLite(), "sih.SQLite")


anos <- c(2023)
meses <- 1:12
ufs <- c("PR")

for(ano in anos){
  for(mes in meses){
    for(uf in ufs){
      # Baixa o dado para um ano, mês e UF específico
      tmp <- fetch_datasus(
        year_start = ano, year_end = ano,
        month_start = mes, month_end = mes,
        uf = uf, information_system = "SIH-RD"
      )
      
      # Pré-processamento dos dados
      tmp <- process_sih(data = tmp)
      
      # Escreve na tabela "sih" no banco de dados,
      # apensando os dados de cada ano e mês
      dbWriteTable(conn = conn, name = "sih", value = tmp, append = TRUE)
      
      # Remove a tabela temporária
      rm(tmp)
    }
  }
}

datasus_final <- tbl(conn, "sih") %>%
  select(COD_IDADE, DIAG_PRINC, IDADE, SEXO) %>%
  filter(COD_IDADE == "Anos") %>%
  # filter(grepl("K35|K36",DIAG_PRINC)) %>%
  mutate(IDADE = as.numeric(IDADE),
         # bin = cut_width(IDADE, width = 5, center = 2.5)
  ) %>%
  # select(IDADE, SEXO) %>%
  # group_by(SEXO, bin) %>%
  # count() %>%
  collect()

datasus_final2 <- datasus_final %>%
  filter(grepl("K35|K36",DIAG_PRINC)) %>%
  mutate(IDADE = as.numeric(IDADE),
         bin = cut_width(IDADE, width = 5, center = 2.5,closed = "left")
  ) %>%
  select(IDADE, SEXO, bin) %>%
  group_by(SEXO, bin) %>%
  count() %>%
  mutate(bin = stringr::str_remove_all(bin, "[\\(\\)\\[\\]]"),
         bin = stringr::str_replace(bin, ",", "-")) %>%
  separate(bin,into = c("first", "second"), sep = "-") %>%
  mutate(second = as.numeric(second)-1,
         bin = paste(first, second, sep = "-")) %>%
  select(-c(first, second)) %>% 
mutate(
  bin = factor(bin, levels = c(
    "0-4",
    "5-9",
    "10-14",
    "15-19",
    "20-24",
    "25-29",
    "30-34",
    "35-39",
    "40-44",
    "45-49",
    "50-54",
    "55-59",
    "60-64",
    "65-69",
    "70-74",
    "75-79",
    "80-84",
    "85-89",
    "90-94"
    
  ))
)


##dados censo


censo <- read.csv2("posts/appendix/pt/data/Censo 2022 - Pirâmide etária - Paraná.csv")%>%
  janitor::clean_names()

censo2 <- censo  %>%
  select(1:3) %>%
  mutate(grupo_de_idade = stringr::str_replace(grupo_de_idade, " a ", "-"),
         grupo_de_idade = stringr::str_remove_all(grupo_de_idade, "[:alpha:]| ")) %>% 
  filter(grupo_de_idade != 100) %>% 
  pivot_longer(cols = c(populacao_feminina_pessoas, populacao_masculina_pessoas),
               names_to = "SEXO",
               values_to = "n_censo") %>% 
  rename(bin = grupo_de_idade) %>% 
  mutate(SEXO = ifelse(SEXO == "populacao_feminina_pessoas", "Feminino", "Masculino"))

datasus_final_censo <- datasus_final2 %>% 
  left_join(censo2) %>% 
  mutate(prop = n/n_censo*100) %>% 
  ungroup() %>% 
  mutate(
    bin = factor(bin, levels = c(
      "0-4",
      "5-9",
      "10-14",
      "15-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65-69",
      "70-74",
      "75-79",
      "80-84",
      "85-89",
      "90-94"
      
    ))
  )




# datasus_aprovadas <- microdatasus::fetch_datasus(
#   information_system = "SIH-RD",
#   year_start = 2023,year_end = 2023,
#   month_start = 01,month_end = 12)
#
#
# datasus_aprovadas2 <- microdatasus::process_sih(datasus_aprovadas)
#
# dplyr::glimpse(datasus_aprovadas2)
#
# datasus_aprovadas3 <- datasus_aprovadas2 %>%
#   filter(grepl("K35|K36",DIAG_PRINC))
#
# datasus_aprovadas4 <- datasus_aprovadas3 %>%
#   filter(COD_IDADE == "Anos") %>%
#   mutate(IDADE = as.numeric(IDADE),
#          bin = cut_width(IDADE, width = 5, center = 2.5)) %>%
#   select(IDADE, SEXO, bin) %>%
#   group_by(SEXO, bin) %>%
#     count()


# plot --------------------------------------------------------------------

#geral

datasus_final2 %>%
  mutate(n = ifelse(SEXO == "Masculino", n*-1, n)) %>%
  ggplot(aes(x = n, y = bin))+
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = 0.5, ymax = 7.5,
    fill = "white"
  )+
  geom_vline(
    xintercept = seq(-1200,1200,200),
    lineend = "round",
    linetype = "dashed",
    color = "#dedede"
  )+
  geom_col(
    aes(fill = SEXO),
    color = "black"
  )+
  annotate(
    "label",
    x = c(-500,500),
    y = 15,
    label = c("Masculino", "Feminino"),
    fontface = "bold",
    color = c("#1c9099","#756bb1"),
    fill = "#f1f1f1",
    label.size = NA,
    family = "Open Sans"
  )+
  scale_x_continuous(
    breaks = seq(-1000,1000,200),
    labels = function(x) abs(x)
  )+
  scale_y_discrete(
    labels = function(x) stringr::str_remove_all(x, "[\\(\\)\\[\\]]") %>%
      stringr::str_replace(",", "-")
  )+
  scale_fill_manual(
    values = c(
      "Masculino" = "#1c9099",
      "Feminino" = "#756bb1"
    )
  )+
  labs(
    title = "Jovens do sexo masculino têm mais apendicite",
    subtitle = "Casos de Apendicite (CID K35/K36) no PR em 2023",
    caption = "Bruno Mioto @BrunoHMioto - Dados: microdatasus",
    x = "Número de casos",
    y = "Idade",
    fill = "Sexo"
  )+
  # guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  theme(
    text = element_text(color = "#1c1c1c", family = "Open Sans"),
    # panel.grid.major.x = element_line(),
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "#f1f1f1", color = NA),
    panel.background = element_rect(fill = "#f1f1f1", color = NA),
    line = element_line(lineend = "round", color = "#1c1c1c"),
    axis.ticks = element_line(color = "#1c1c1c"),
    axis.line = element_line(color = "#1c1c1c"),
    axis.text = element_text(color = "#1c1c1c")
    # panel.grid = element_line(lineend = "round",
    #                           linetype = "dashed",
    #                           color = "#dedede")
  )+
  coord_cartesian(
    xlim = c(-1000,1000)
  )

ggsave("posts/appendix/pt/figures/plot.png", width = 6, height = 4.5, dpi = 600)



#proporcional

datasus_final_censo %>%
  mutate(prop = ifelse(SEXO == "Masculino", prop*-1, prop)) %>%
  ggplot(aes(x = prop, y = bin))+
  # annotate(
  #   "rect",
  #   xmin = -Inf, xmax = Inf,
  #   ymin = 0.5, ymax = 7.5,
  #   fill = "white"
  # )+
  # geom_vline(
  #   xintercept = seq(-700,700,100),
  #   lineend = "round",
  #   linetype = "dashed",
  #   color = "#dedede"
  # )+
  geom_col(
    aes(fill = SEXO),
    color = "black"
  )+
  annotate(
    "label",
    x = c(-0.2,0.2),
    y = 14,
    label = c("Masculino", "Feminino"),
    fontface = "bold",
    color = c("#1c9099","#756bb1"),
    fill = "#f1f1f1",
    label.size = NA,
    family = "Open Sans"
  )+
  scale_x_continuous(
    breaks = seq(-0.5,0.5,0.1),
    labels = function(x) paste0(abs(x),"%")
  )+
  scale_y_discrete(
    labels = function(x) stringr::str_remove_all(x, "[\\(\\)\\[\\]]") %>%
      stringr::str_replace(",", "-")
  )+
  scale_fill_manual(
    values = c(
      "Masculino" = "#1c9099",
      "Feminino" = "#756bb1"
    )
  )+
  labs(
    title = "A incidência de apendicite é maior em jovens do sexo masculino",
    subtitle = "Incidência na população de casos de Apendicite (CID K35/K36) no PR em 2023",
    caption = "Bruno Mioto @BrunoHMioto - Dados: microdatasus e Censo 2022",
    x = "Incidência na população",
    y = "Idade",
    fill = "Sexo"
  )+
  # guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  theme(
    text = element_text(color = "#1c1c1c", family = "Open Sans"),
    # panel.grid.major.x = element_line(),
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "#f1f1f1", color = NA),
    panel.background = element_rect(fill = "#f1f1f1", color = NA),
    line = element_line(lineend = "round", color = "#1c1c1c"),
    axis.ticks = element_line(color = "#1c1c1c"),
    axis.line = element_line(color = "#1c1c1c"),
    axis.text = element_text(color = "#1c1c1c"),
    panel.grid.major.x = element_line(lineend = "round",
                              linetype = "dashed",
                              color = "#dedede")
  )+
  coord_cartesian(
    xlim = c(-0.3,0.3)
  )



ggsave("posts/appendix/pt/figures/plot_incidencia_pop.png", width = 6, height = 4.5, dpi = 600)

