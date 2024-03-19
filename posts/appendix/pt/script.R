library(dplyr)
library(readr)
library(lubridate)

resultado <- read_csv("posts\\appendix\\pt\\data\\Takeout\\Fit\\Métricas de atividades diárias\\Métricas de atividades diárias.csv")

resultado2 <- resultado %>% 
  janitor::clean_names() %>% 
  select(data, distancia_m, contagem_de_passos) %>% 
  filter(stringr::str_detect(data, "2024\\-02"))


# Sys.setlocale("LC_ALL", "en_US")
daily_totals <- resultado2 %>% 
  mutate(issue_date = as_date(data)) %>%
  # group_by(issue_date) %>%
  # summarise(Tickets = n()) %>% 
  # ungroup() %>%
  mutate(wday = weekdays(issue_date, abbr = TRUE),
         month_day = day(issue_date),
         month = month(issue_date),
         week_increment = ifelse(month_day == 1 | wday == "dom", 1, 0))%>%
  group_by(month) %>% 
  mutate(week = cumsum(week_increment),
         text_month = months(issue_date)) %>% 
  ungroup()


wday_vec <- c("dom", "seg", "ter", "qua", "qui", "sex", "sáb")
daily_totals$wday <- factor(daily_totals$wday, levels = wday_vec)

library(ggplot2)

ggplot(daily_totals, aes(x = wday, y = week)) + 
  geom_tile(aes(fill = contagem_de_passos),
            colour = "black",
            linewidth = 0.5) +
  # geom_point(aes(size = contagem_de_passos,
  #                fill = contagem_de_passos),
  #            shape = 21)+
  geom_text(aes(label = month_day,
                # hjust = ifelse(contagem_de_passos < 2000,
                #                  0.5,
                #                  1.5)
  ),
  vjust = 1,
  nudge_y = 0.45,
  hjust = 0,
  nudge_x = -0.45,
  fontface = "bold"
  )+
  geom_text(aes(label = contagem_de_passos),
  vjust = 1,
  nudge_y = -0.2,
  # hjust = 0,
  # nudge_x = -0.45,
  fontface = "bold",
  size = 3,
  )+
  annotate(
    "text",
    x = c(5,6,2,3,4,5,6,4,5),
    y = c(1,1,2,2,2,2,2,3,3),
    label = "🥼️",
    size = 5
  )+
  annotate(
    "text",
    x = 7,
    y = 1,
    label = "🏬",
    size = 5
  )+
  annotate(
    "text",
    x = 1,
    y = 2,
    label = "🎞️",
    size = 5
  )+
  annotate(
    "text",
    x = 7,
    y = 2,
    label = "🛒",
    size = 5
  )+
  # annotate(
  #   "rect",
  #   xmin = 0.6, xmax = 3.4,
  #   ymin = 3.45, ymax = 3.25,
  #   fill = "#fafafa"
  # )+
  # annotate(
  #   "text",
  #   x = 2,
  #   y = 3.35,
  #   label = "Carnaval"
  # )+
  annotate(
    "text",
    x = c(1,2,3),
    y = c(3,3,3),
    label = "😴️",
    size = 5
  )+
  annotate(
    "point",
    x = 6,
    y = 3.05,
    shape = 21,
    size = 18,
    color = "#bd0624",
    stroke = 1
  )+
  annotate(
    "text",
    x = 6,
    y = 3,
    label = "🔪",
    size = 5
  )+
  annotate(
    "text",
    x = c(7,1,2,3,4),
    y = c(3,4,4,4,4),
    label = "🛏️",
    size = 5
  )+
  annotate(
    "text",
    x = 5,
    y = 4,
    label = "🏥",
    size = 5
  )+
  annotate(
    "polygon",
    x = c(1.3,1.5,1.5),
    y = c(3.5,3.5,3.3),
    fill = "#2b8cbe",
    color = "black"
  )+ 
  annotate(
    "polygon",
    x = c(2.3,2.5,2.5),
    y = c(3.5,3.5,3.3),
    fill = "#2b8cbe",
    color = "black"
  )+ 
  annotate(
    "polygon",
    x = c(3.3,3.5,3.5),
    y = c(3.5,3.5,3.3),
    fill = "#2b8cbe",
    color = "black"
  )+ 
  #legend
  annotate(
    "polygon",
    x = c(5.9,6.1,6.1),
    y = c(4.1,4.1,3.9),
    fill = "#2b8cbe",
    color = "black"
  )+ 
  annotate(
    "text",
    x = 6.15,
    y = 4,
    label = "Carnaval",
    size = 5,
    hjust = 0
  )+
  geom_tile(
    fill = NA,
            colour = "black",
            linewidth = 0.5) +
  facet_wrap(~text_month, scales = "free") + 
  scale_x_discrete(position = "top") +
  scale_y_reverse() + 
  scale_fill_distiller(palette = "YlOrRd",direction = 1) + 
  scale_size(range = c(5,30))+
  labs(
    title = "Número de passos por dia em Fevereiro/2024",
    caption = "Dados: Google Fit"
  )+
  theme_void()+
  theme(
    text = element_text(family = "Open Sans"),
    strip.placement = "outside",
    strip.text = element_blank(),
    legend.position = "none",
    axis.text.x.top = element_text(face = "bold"),
    plot.margin = margin(10,10,10,10,"pt"),
    plot.title = element_text(face = "bold", 
                              size = 14,
                              margin = margin(0,0,10,0)),
    plot.caption = element_text(face = "bold")
  )+
  coord_cartesian(clip = "off")



ggsave("posts\\appendix\\pt\\figures\\calendar.svg", bg = "#fafafa",
       width = 6, height = 4.5, 
       dpi = 1200)



# censo -------------------------------------------------------------------

censo <- readr::read_csv2("posts\\appendix\\pt\\data\\Censo 2022 - Pirâmide etária - Paraná.csv")



