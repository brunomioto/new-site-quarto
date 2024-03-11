
url <- "https://proj.org/en/9.3/operations/projections/index.html"
a <- rvest::read_html(url)
c <- rvest::html_nodes(a, ".internal")
d <- c %>% 
  rvest::html_attr('href')
d[9:161]

url <- "https://proj.org/en/9.3/operations/projections/aea.html"
a <- rvest::read_html(url)
b <- rvest::html_elements(a,css = ".caption-text")
c <- rvest::html_text(b)

tabela <- tibble()
for (i in d[9:161]) {
  url <- paste0("https://proj.org/en/9.3/operations/projections/",i)
  a <- rvest::read_html(url)
  b <- rvest::html_elements(a,css = ".caption-text")
  c <- rvest::html_text(b)
  tabela <- tabela %>% 
    rbind(c)
  print(c)
}

tabela <- tabela %>% 
  rename(projections = 1)
readr::write_csv(tabela, "./posts/_2023-11-09-projections-to-use-with-ggplot2-maps/proj_list.csv")
