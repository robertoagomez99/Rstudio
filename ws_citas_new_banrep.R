#-------------------------------------------------------#
# WEB SCRAPING DE CITAS DE DOCUMENTOS DEL BANREP ----
  #DTSERU, CHE Y BORRADORES
#-------------------------------------------------------#

#--------------------------#
# packages ----

rm(list=ls()) 
packageList<-c("tidyverse", "rvest", "xml2", "stringr", "XML", "RCurl", "glue", "dplyr")
lapply(packageList,require,character.only=TRUE) 

#--------------------------#
# carpetas ----

datos_ori <- "Datos_originales/"
datos <- "Datos_para_usar/"
anio_actual <- 2025

#-------------------------------------------------------#
# 1. Funciones ----
#-------------------------------------------------------#
#--------------------------#
# fun_links: extraye links de paginas con papers 
#--------------------------#

fun_links <- function(link_serie){
  
  pages <- xml2::read_html(paste0("https://ideas.repec.org/s/bdr/", link_serie, ".html")) %>%
    html_nodes(xpath = "//*[@id='content-block']/div[2]/div/nav/ul") %>%
    html_children() %>% 
    html_text() %>%
    str_trim
  
  # extrayendo numero maximo de paginas
  page_number <- pages[pages != "«"]
  page_number <- page_number[page_number != "»"]
  page_number <- page_number[page_number != "..."]
  page_number <- as.numeric(page_number)
  page_max <- max(page_number)
  
  # construyendo links de paginas de las que se extraera la info
  # crea link para cada pagina segun el numero max de paginas
  link_ori <- "https://ideas.repec.org/s/bdr/"
  links_disponibles <- paste0(link_ori, link_serie, ".html")
  
  # condicional en caso de que la serie tenga 1 (una) pagina 
  ifelse(pages >= 2,
         links_disponibles <- c(links_disponibles, paste0(link_ori, link_serie, 2:page_max, ".html")),
         links_disponibles <- links_disponibles)
  
  return(links_disponibles)
  
}

#--------------------------#
# fun_lista_anios: entra a cada pagina desplegable y extrae los anios que contiene cada una
#--------------------------#
fun_lista_anios <- function(y){
  
  pagina <- xml2::read_html(y) %>%
    html_nodes(xpath = "//*[@id='content']/h3") %>%
    html_text() %>%
    str_trim() %>%
    tibble(year = .) %>%
    mutate(page_link = y)
  return(pagina)
  
} 

#--------------------------#
# fun_info: entra a las paginas con los anios de interes y extrae info basica del paper
# id, anio de publicacion, autores
#--------------------------#

fun_info <- function(year, page_link, position){
  
  print(year)
  
  # extrayendo id, autores y anio de publicacion del paper
  docs_info <- xml2::read_html(page_link) %>%       
    html_nodes(xpath = paste0("//*[@id='content']/div[", position, "]/ul")) %>%
    html_children() %>%
    html_text() %>%
    str_trim() %>%
    tibble(paper_aut = .) %>%
    mutate(id = substr(paper_aut, 1, 4), authors = gsub(".*by", "", paper_aut), publication_date = year)
  
  # correcciones a id y publication_date
  docs_info$id <- str_replace(docs_info$id, "\\s[^ ]+$", "")
  docs_info$id <- str_replace(docs_info$id, "\\s", "")
  docs_info$publication_date <- as.numeric(docs_info$publication_date)
  
  return(docs_info)
  
}
#--------------------------#
# fun_cita: entra a las paginas con los anios de interes y extrae las citas acumuladas de los papers
#--------------------------#
fun_cita <- function(paper_aut, id, authors, publication_date, serie){
  print(glue("numero de citas de {serie}, doc {id}"))
  cita2 <- xml2::read_html(paste0("https://ideas.repec.org/p/bdr/", link_serie, "/", id, ".html")) %>%
    html_nodes(xpath = "//*[@id='cites-tab']") %>%
    html_text() %>%
    str_trim() %>%
    tibble(cita2 = .) %>%
    mutate(cita2 = gsub("[[:alpha:]]", "", cita2)) %>% as.data.frame()
  
  length(row.names.data.frame(cita2))
  if (length(row.names.data.frame(cita2))== 0 ) {
    cita2<- 0 %>% as.data.frame()
    cita2 <- mutate(cita2, cita2= .) %>%
      select(cita2)
  }
  base_cita <- cita2 %>%
    mutate(id = id, title = paper_aut, cita2 = cita2, publication_date = publication_date, serie = serie)
  cols_num <- "cita2"
  base_cita[cols_num] <- sapply(base_cita[cols_num],as.numeric)
  return(base_cita)
}

#--------------------------#
# fun_descargar: ejecuta las demas funciones y regresa una base con informacion de la serie escogida para los anios definidos
#--------------------------#

fun_descargar <- function(link_serie, serie, start, end){
  
  print(glue("descargando {serie}"))
  
  # links de paginas con papers
  links_disponibles <- fun_links(link_serie)
  
  # dataframe con la ubicacion de los anios en cada pagina
  lista_anios <- lapply(links_disponibles, fun_lista_anios) %>% bind_rows()
  
  # extrayendo posicion de los anios dentro de las paginas 
  # (ejm, papers de 2015 pueden estar en las paginas 1 y 2)
  lista_anios$position <- ave(lista_anios$page_link, lista_anios$page_link, FUN = seq_along)
  lista_anios$year[lista_anios$year == "Undated"] <- "0"
  lista_anios <- lista_anios %>% mutate(year = as.numeric(year))
  
  # eligiendo los anios a analizar
  lista_anios <- lista_anios %>% subset(year >= start & year <= end)
  #lista_anios <- lista_anios %>% subset(year >=2018 & year <=2020)
  # extrayendo info de los documentos (titulo, autores, etc)
  info <- pmap(lista_anios, fun_info) %>% bind_rows() %>%
    mutate(serie = serie)
  
  # bajando citas de cada documento
  base_cita <- pmap(info, fun_cita) %>% bind_rows() %>%
    select(id, title, serie, publication_date, cita2)
  return(base_cita)
}


#-------------------------------------------------------#
# 2. extrayendo informacion de ----
#-------------------------------------------------------#

#--------------------------#
# A. BORRADORES DE ECONOMIA (borrec) ----
# INTERVALOS: 1994-1999, 2000-2005, 2006-2010, 2011- anio en curso
#--------------------------#

serie <- "BORRADOR"
link_serie <- "borrec"
anios <- tibble(start = c(1994, 2000, 2006, 2011), end = c(1999, 2005, 2010, anio_actual))

lapply(1:nrow(anios), function(y){
  
  print(glue("intervalo {anios$start}, {anios$end}"))
  intervalo_anios <- anios[y,]
  
  start_time <- Sys.time()
  base_borradores <- fun_descargar(link_serie = link_serie, serie = serie, start = intervalo_anios$start, end = intervalo_anios$end)
  end_time <- Sys.time()
  print(end_time - start_time)
  
  # save
  saveRDS(base_borradores, glue("{datos}/bases_completas/base_citas_{serie}_{intervalo_anios$start}_{intervalo_anios$end}.rds"))
  
})
#--------------------------#
# B. DOCUMENTOS DE ECONOMIA REGIONAL (DTSERU) ----
# INTERVALOS: 1997-2004, 2005-2009, 2010- anio en curso
#--------------------------#

link_serie <- "region" 
serie <- "DTSERU"
anios <- tibble(start = c(1997, 2005, 2010), end = c(2004, 2009, anio_actual))

lapply(1:nrow(anios), function(y){
  
  print(glue("intervalo {anios$start}, {anios$end}"))
  intervalo_anios <- anios[y,]
  
  start_time <- Sys.time()
  base_dtseru <- fun_descargar(link_serie = link_serie, serie = serie, start = intervalo_anios$start, end = intervalo_anios$end)
  end_time <- Sys.time()
  print(end_time - start_time)
  
  # save
  saveRDS(base_dtseru, glue("{datos}bases_completas/base_citas_{serie}_{intervalo_anios$start}_{intervalo_anios$end}.rds"))
  
})

#-------------------------------------------------------#
# C. CUADERNOS DE HISTORIA ECONOMICA ----
# INTERVALOS: 1999- anio en curso
#-------------------------------------------------------#

link_serie <- "cheedt" 
serie <- "CHE"
intervalo_anios <- tibble(start = 1999, end = anio_actual)

start_time <- Sys.time()
base_chee <- fun_descargar(link_serie = link_serie, serie = serie, start = intervalo_anios$start, end = intervalo_anios$end)
end_time <- Sys.time()
end_time - start_time

# save
saveRDS(base_chee, glue("{datos}bases_completas/base_citas_{serie}_{intervalo_anios$start}_{intervalo_anios$end}.rds"))



