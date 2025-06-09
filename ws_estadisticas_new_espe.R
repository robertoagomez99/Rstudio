#-------------------------------------------------------#
# WEB SCRAPING DE ESTADISTICAS DE PUBLICACIONES DE BANREP ----
# NEW LINKS
# ENSAYOS DE POLITICA ECONOMICA
#-------------------------------------------------------#

# ESTE SCRIPT ENTRA A LA PAGINA DE REPEC Y EXTRAE LAS ESTADISTICAS DE CADA DOCUMENTO DE ESPE
# GUARDA UNA BASE CON LAS ESTADISTICAS DE ESPE SEGUN LOS ANIOS ELEGIDOS 
# ESPE SE DESCARGA POR APARTE AL TENER VOLUMENES E ISSUES

#--------------------------#
# packages ----

rm(list=ls())
#install.packages("vctrs")
packageList<-c("tidyverse", "rvest", "xml2", "stringr", "XML", "RCurl", "glue", "dplyr", "vctrs", "httr", "tibble")
lapply(packageList,require,character.only=TRUE)

#--------------------------#
# carpetas ----

datos_ori <- "Datos_originales/Webscraping/"
datos <- "Datos_para_usar/"

#-------------------------------------------------------#
# INDICAR ANIO MAS RECIENTE ----
# definir cual es el anio en curso, para descargar estadisticas que incluyan los documentos mas recientes
#-------------------------------------------------------#

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
    tibble(info = .) %>%
    mutate(page_link = y) %>% 
    mutate(year = gsub("\\,.*", "", info)) %>%
    mutate(year = gsub("[[:alpha:]]", "", year)) %>%
    mutate(year = gsub("\\s", "", year)) %>%
    mutate(month = gsub("\\,.*", "", info)) %>%
    mutate(month = gsub("[[:digit:]]", "", month)) %>%
    mutate(month = gsub("\\s", "", month)) %>%
    mutate(issue = sub(".*Issue\\s", "", info)) %>%
    mutate(volume = sub(".*[,] *(.*?) *[,] Issue.*", "\\1", info)) %>%
    mutate(volume = gsub("Volume ", "", volume))
  
  pagina$volume[grepl("Issue", pagina$volume)] <- NA
  
  return(pagina)
  
} 

#--------------------------#
# fun_info: entra a las paginas con los anios de interes y extrae info basica del paper
# id, anio de publicacion, autores
#--------------------------#

fun_info <- function(info, page_link, year, month, issue, volume, position){
  
  print(glue("{info}, {month}"))

  docs_info <- xml2::read_html(page_link) %>%       
    html_nodes(xpath = paste0("//*[@id='content']/div[", position, "]/ul")) %>%
    html_children() %>%
    html_text() %>%
    str_trim() %>%
    tibble(paper_aut = .) %>%
    mutate(pages = substr(paper_aut, 1, 7), 
           authors = gsub(".*by", "", paper_aut), 
           publication_date = year,
           year = year,
           issue = issue,
           volume = volume) 
  
  # correcciones a pages y publication_date
  docs_info$pages <- str_replace(docs_info$pages, "\\s[^ ]+$", "")
  docs_info$pages <- str_replace(docs_info$pages, "\\s", "")
  docs_info$publication_date <- as.numeric(docs_info$publication_date)
  
  docs_info <- docs_info %>% 
    mutate(id = ifelse(is.na(volume) == TRUE, paste0("y:",year,":i:",issue,":p:",pages),
                       paste0("v:",volume,":y:",year,":i:",issue,":p:",pages))) %>% select(-c(year, issue, volume, pages))
  
  return(docs_info)
  
}

#--------------------------#
# fun_stats: entra a cada paper segun su id, extrae estadisticas y construye base con
# id, autores, fecha de publicacion, estadisticas por mes, link de la pagina, serie, nombre paper, etc.
#--------------------------#

fun_stats <- function(id, paper_aut, authors, publication_date) {
  print(glue("Descargando doc {id}, año {publication_date}"))
  
  paper_series <- serie
  page_link <- paste0("https://logec.repec.org/scripts/paperstat.pf?h=RePEc:bdr:", link_serie, ":", id)
  
  success <- FALSE
  attempt <- 1
  
  while (!success) {
    tryCatch({
      response <- GET(page_link)
      
      if (status_code(response) == 429) {
        print(glue("Error 429 detectado en id {id}. Pausando..."))
        wait_time <- as.numeric(headers(response)$`retry-after`)
        if (is.na(wait_time)) wait_time <- 100
        print(glue("Esperando {wait_time} segundos antes de reintentar..."))
        Sys.sleep(wait_time)
        
      } else if (status_code(response) == 200) {
        html_doc <- read_html(page_link)
        
        stats <- html_doc %>%
          html_nodes("table") %>%
          .[[2]] %>%
          html_table(fill = TRUE)
        
        if (nrow(stats) == 0) stop(glue("Este documento no tiene estadísticas, id: {id}"))
        
        names(stats) <- c("date", "downloads", "abstract_views")
        
        stats <- stats %>%
          mutate(
            month = as.numeric(substr(date, 6, 7)),
            year = as.numeric(substr(date, 1, 4))
          ) %>%
          select(-date)
        
        paper_name <- html_doc %>%
          html_nodes('.gptitle') %>%
          html_text() %>%
          str_trim()
        
        stats_paper <- stats %>%
          mutate(
            title = paper_name,
            serie = paper_series,
            id = id,
            pub_date = publication_date,
            author = authors,
            link = page_link
          ) %>%
          select(c(id, title, author, serie, pub_date, year, month, downloads, abstract_views, link)) %>%
          separate(
            author,
            into = c("author_1", "author_2", "author_3", "author_4", "author_5"),
            sep = "&",
            remove = FALSE
          ) %>%
          select(-author)
        
        # Filtrar por cuarto trimestre:
        stats_paper <- stats_paper %>%
          filter(year == anio_actual & month >= 1 & month <= 3)
        
        success <- TRUE
        return(stats_paper)
        
      } else {
        stop(glue("Error inesperado: código de estado {status_code(response)}"))
      }
      
    }, error = function(e) {
      if (grepl("Este documento no tiene estadísticas", e$message)) {
        print(glue("Este documento no tiene estadísticas, id: {id}"))
        success <- TRUE
        return(tibble(
          id = id,
          title = "sin datos",
          author = authors,
          serie = paper_series,
          pub_date = as.numeric(publication_date),
          year = 0,
          month = 0,
          downloads = 0,
          abstract_views = 0,
          link = page_link,
          author_1 = NA,
          author_2 = NA,
          author_3 = NA,
          author_4 = NA,
          author_5 = NA
        ))
      } else {
        print(glue("Error inesperado: {e$message}"))
      }
      
      attempt <- attempt + 1
      if (attempt > 5) {
        print(glue("Intentos excedidos para el id {id}. Se omite este documento."))
        success <- TRUE
        return(tibble(
          id = id,
          title = "sin datos",
          author = authors,
          serie = paper_series,
          pub_date = as.numeric(publication_date),
          year = 0,
          month = 0,
          downloads = 0,
          abstract_views = 0,
          link = page_link,
          author_1 = NA,
          author_2 = NA,
          author_3 = NA,
          author_4 = NA,
          author_5 = NA
        ))
      }
      
      print(glue("Reintentando el id {id}, intento {attempt}..."))
    })
  }
}



#--------------------------#
# fun_descargar: ejecuta las demas funciones y regresa una base con las estadisticas de la serie escogida para los anios definidos
#--------------------------#

fun_descargar <- function(link_serie, serie, start, end) {
  
  print(glue("Descargando {serie}, años {start} a {end}"))
  
  links_disponibles <- fun_links(link_serie)
  
  # DataFrame con la ubicación de los años en cada página
  lista_anios <- lapply(links_disponibles, fun_lista_anios) %>% bind_rows()
  
  # Extrayendo posición de los años dentro de las páginas
  lista_anios$position <- ave(lista_anios$page_link, lista_anios$page_link, FUN = seq_along)
  lista_anios$year <- as.numeric(lista_anios$year)  
  
  # Filtrar años de interés
  anio <- lista_anios %>% subset(year >= start & year <= end)
  
  # Validar si anio tiene datos
  if (nrow(anio) == 0) {
    print(glue("No hay datos disponibles para el intervalo {start} - {end}. Saltando..."))
    return(NULL)  
  }
  
  # Base con datos de papers: autores, id, fecha de publicación ----
  print(glue("Extrayendo información de los papers para {nrow(anio)} registros..."))
  base_info <- pmap(anio, fun_info) %>% bind_rows()
  
  # reemplazando id's, algunos tienen inconsistencias (viene de REPEC)
  # solo ocurre en anios 2000, 1999, 1991, 1986
  base_info$id[base_info$id == "v:18:y:2000:i:38:p:45-69"] <- "v:18:y:2000:i:38:p:54-69"
  base_info$id[base_info$id == "y:1999:i:35:p:5-53"] <- "v::y:1999:i:35:p:5-53"
  base_info$id[base_info$id == "y:1999:i:35:p:55-85"] <- "v::y:1999:i:35:p:55-85"
  base_info$id[base_info$id == "y:1999:i:35:p:87-121"] <- "v::y:1999:i:35:p:87-121"
  base_info$id[base_info$id == "y:1991:i:20:p:53-85"] <- "v::y:1991:i:20:p:53-85"
  base_info$id[base_info$id == "v:5:y:1986:i:9:p:137-165"] <- "v:5:y:1986:i:9:p:137-166"
  
  print(glue("Descargando estadísticas..."))
  
  # Seccionando base_info en partes iguales
  intervalos <- split(base_info, rep(1:7, length.out = nrow(base_info), each = ceiling(nrow(base_info) / 7)))
  
  # Descarga las estadísticas por intervalos, manejando errores y pausas
  base_stats <- lapply(intervalos, function(intervalo) {
    tryCatch({
      pmap(intervalo, fun_stats) %>% bind_rows()
    }, error = function(e) {
      print(glue("Error durante la descarga del intervalo. Detalle: {e$message}"))
      return(NULL)
    })
  }) %>% bind_rows()
  # Convertimos columnas  a numéricas
  base_stats <- base_stats %>%
    mutate(
      year = as.numeric(year),
      month = as.numeric(month),
      downloads = as.numeric(downloads),
      abstract_views = as.numeric(abstract_views)
    )
  
  # Filtrar documentos que no tienen estadísticas
  base_stats <- base_stats %>% filter(title != "sin datos")
  
  # Devolver la base final
  return(base_stats)
}


#-------------------------------------------------------#
# 2. Descargar datos ----
#-------------------------------------------------------#

#--------------------------#
# A. ENSAYOS DE POLITICA ECONOMICA (ESPE) ----
# INTERVALOS: 1982-1989, 1990-1999, 2000-2005, 2006-2010, 2011- anio en curso
#--------------------------#
link_serie <- "ensayo" 
serie <- "ESPE" 
#anios <- tibble(start = c(1982, 1990, 2000, 2006, 2011), end = c(1989, 1999, 2005, 2010, anio_actual
anios <- tibble(start = c(2011), end = c(anio_actual))
for (y in 1:nrow(anios)) {
  
  print(glue("Intervalo: {anios$start[y]}, {anios$end[y]}"))
  intervalo_anios <- anios[y, ]
  
  start_time <- Sys.time()
  
  retries <- 5
  attempt <- 0  
  success <- FALSE  
  
  while (!success && attempt < retries) {
    attempt <- attempt + 1
    
    # Descarga de las estadísticas
    base_espe <- tryCatch({
      fun_descargar(link_serie = link_serie, serie = serie, start = intervalo_anios$start, end = intervalo_anios$end)
    }, error = function(e) {
      message(glue("Error en la descarga del intervalo {intervalo_anios$start} - {intervalo_anios$end}: {e$message}"))
      
      # Si el mensaje contiene 429, se espera 100 segundos
      if (grepl("429", e$message)) {
        print(glue("Error 429 detectado. Esperando 100 segundos antes de reintentar..."))
        Sys.sleep(100)
      }
      
      return(NULL)  
    })
    
    # Confirmamos sí la descarga fue correcta
    if (!is.null(base_espe) && nrow(base_espe) > 0) {
      print(glue("Datos descargados correctamente para el intervalo: {intervalo_anios$start} - {intervalo_anios$end}"))
      success <- TRUE
    } else {
      print(glue("Descarga fallida en el intento {attempt} para el intervalo {intervalo_anios$start} - {intervalo_anios$end}."))
      
      if (attempt < retries) {
        print(glue("Esperando 100 segundos antes del próximo intento..."))
        Sys.sleep(100)  # Esperamos antes del siguiente intento
      } else {
        print(glue("No se pudo descargar datos para el intervalo {intervalo_anios$start} - {intervalo_anios$end} después de {retries} intentos."))
      }
    }
  }
  
  end_time <- Sys.time()
  print(glue("Tiempo total de descarga para {anios$start[y]} - {anios$end[y]}: {end_time - start_time}"))
  
  # Guardamos los datos si la descarga fue correcra
  if (success) {
    saveRDS(base_espe, glue("{datos_ori}MES_base_stats_new_{serie}_{intervalo_anios$start}_{intervalo_anios$end}.rds"))
  }
  
  # Pausa entre las solicitudes para no sobrecargar el servidor
  Sys.sleep(15)
}
rm(base_espe)
#--------------------------------------------------------------------------------------------------------
# Cambiar el anio de base_stats{serie}_{2011}_{anios_actual-1} a base_stats{serie}_{2011}_{anios_actual} porque se cambio el anio_actual
#-------------------------------------------------2022-2024-----------------------------------------
base_stats_new_ESPE_2011_2025 <- readRDS("Datos_originales/Webscraping/base_stats_new_ESPE_2011_2024.rds")
saveRDS(base_stats_new_ESPE_2011_2025, glue("Datos_originales/Webscraping/base_stats_new_ESPE_2011_2025.rds"))
rm(base_stats_new_ESPE_2011_2025)

#--------------------------------------------------------------------------------------------------------
# Pegar las bases con las estadisticas del mes actual
fun_pegar <- function(serie, start, end) {
  base_stats <- readRDS(glue("{datos_ori}base_stats_new_{serie}_{intervalo_anios$start}_{intervalo_anios$end}.rds"))
  base_mes <- readRDS(glue("{datos_ori}MES_base_stats_new_{serie}_{intervalo_anios$start}_{intervalo_anios$end}.rds"))
  base_completa <- bind_rows(base_stats, base_mes)
  saveRDS(base_completa, glue("{datos_ori}base_stats_new_{serie}_{intervalo_anios$start}_{intervalo_anios$end}.rds"))
  return(base_completa)
}
#--------------------------#
# A. ESPE ----
#--------------------------#
serie <- "ESPE"
intervalo_anios <- tibble(start = 1982, end = 1989)
base_espe1 <- fun_pegar(serie = serie, start = intervalo_anios$start, end = intervalo_anios$end)
intervalo_anios <- tibble(start = 1990, end = 1999)
base_espe2 <- fun_pegar(serie = serie, start = intervalo_anios$start, end = intervalo_anios$end)
intervalo_anios <- tibble(start = 2000, end = 2005)
base_espe3 <- fun_pegar(serie = serie, start = intervalo_anios$start, end = intervalo_anios$end)
intervalo_anios <- tibble(start = 2006, end = 2010)
base_espe4 <- fun_pegar(serie = serie, start = intervalo_anios$start, end = intervalo_anios$end)
intervalo_anios <- tibble(start = 2011, end = anio_actual)
base_espe5 <- fun_pegar(serie = serie, start = intervalo_anios$start, end = intervalo_anios$end)
rm(base_espe1, base_espe2, base_espe3, base_espe4, base_espe5)

#En esta parte se elimino los duplicados de las estadisticas nuevas de ESPE de los años 2015-2020. Solo se tomara base_stats_new_ESPE
#-------------------------------------1982-1989----------------------
base_stats_new_ESPE_1982_1989 <- readRDS("Datos_originales/Webscraping/base_stats_new_ESPE_1982_1989.rds")

#------------------------------------1990-1999--------------------------
base_stats_new_ESPE_1990_1999 <- readRDS("Datos_originales/Webscraping/base_stats_new_ESPE_1990_1999.rds")

#-----------------------------------2000-2005-----------------------------
base_stats_new_ESPE_2000_2005 <- readRDS("Datos_originales/Webscraping/base_stats_new_ESPE_2000_2005.rds")

#-----------------------------------2006-2010--------------------------------
base_stats_new_ESPE_2006_2010 <- readRDS("Datos_originales/Webscraping/base_stats_new_ESPE_2006_2010.rds")

#-----------------------------------2011-anio_actual----------------
base_stats_new_ESPE_2011_2025 <- readRDS("Datos_originales/Webscraping/base_stats_new_ESPE_2011_2025.rds")


#-------------------------------------------------------#
# 3. Correcciones ----
# descarga de docs que se borraron del portal pero sus links siguen activos, 
# se guarda una base con las estadisticas nuevas de espe, completas
#-------------------------------------------------------#

# base con todos los documentos publicados a la fecha, incluyendo los eliminados del portal actual
# este archivo proviene de una descarga de estadisticas al mes de agosto
docs_espe <- readRDS(paste0(datos_ori, "estadisticas_nuevas/estadisticas_new_ESPE_1982_2019.rds")) %>% distinct(., id, .keep_all = TRUE)
# se une con las estadisticas actualizadas a septiembre, por si hay documentos nuevos
#Unir las bases de espe
ESPE <- bind_rows(base_stats_new_ESPE_1982_1989, base_stats_new_ESPE_1990_1999, base_stats_new_ESPE_2000_2005, base_stats_new_ESPE_2006_2010, base_stats_new_ESPE_2011_2025)
#new_espe <- list.files(path = datos_ori, pattern = paste0("stats_new_ESPE"), full.names = T)%>% 
 # map_dfr(readRDS) %>% bind_rows()

#Eliminar duplicates
new_espe <- ESPE[!(ESPE$serie=="ESPE" & ESPE$id=="v:24:y:2006:i:50:p:48-97" & ESPE$author_1=="Carlos Andrés Amaya *"  & ESPE$year==2020 & ESPE$month==5 & ESPE$downloads==1 & ESPE$abstract_views==4),]
new_espe <- new_espe[!(new_espe$serie=="ESPE" & new_espe$id=="v:24:y:2006:i:50:p:48-97" & new_espe$author_1=="Carlos Andrés Amaya *" & new_espe$year==2020 & new_espe$month==6 & new_espe$downloads==0 & new_espe$abstract_views==5),]
new_espe <- new_espe[!(new_espe$serie=="ESPE" & new_espe$id=="v:23:y:2005:i:48:p:64-117" & new_espe$author_1=="Jesús Antonio Bejarano RojasAuthor-Email: jbejarro@banrep.gov.co.Author-Homepage:"  & new_espe$month==5 &  new_espe$year==2020 & new_espe$downloads==9 & new_espe$abstract_views==28),]
new_espe <- new_espe[!(new_espe$serie=="ESPE" & new_espe$id=="v:23:y:2005:i:48:p:64-117" & new_espe$author_1=="Jesús Antonio Bejarano RojasAuthor-Email: jbejarro@banrep.gov.co.Author-Homepage:"  & new_espe$month==6 &  new_espe$year==2020 & new_espe$downloads==3 & new_espe$abstract_views==8),]
#Anomalias
new_espe <- new_espe %>% mutate(id = gsub("[:]","", id)) 
#Correcion de titulos
new_espe$title[new_espe$id == "v1y1982i1p89-147"] <- "Estimación de la demanda colombiana de importación de bienes intermedios y de consumo en los años setenta"
new_espe$author_1[new_espe$id == "v1y1982i1p89-147"] <- "Hernando José Gómez"
new_espe$title[new_espe$id == "v1y1982i1p21-43"] <- "Notas sobre la reciente evolución económica e institucional del sector financiero"
new_espe$title[new_espe$id == "v1y1982i1p7-19"] <- "El proceso de liberación del mercado financiero colombiano"
new_espe$title[new_espe$id == "v1y1982i1p45-52"] <- "Compañias de financiamiento comercial"
new_espe$title[new_espe$id == "v1y1982i1p53-87"] <- "Los depósitos previos de importación: su operatividad y comportamiento reciente"
directorio_espe <- new_espe %>% distinct(., id, .keep_all = TRUE) %>% select(id, title, pub_date, link)

# tomando solo los que no estan en el portal
base_missing <- anti_join(docs_espe, directorio_espe, by = c("id", "pub_date", "link")) %>% 
  rename(paper_aut = title, publication_date = pub_date) %>% 
  mutate(authors = paste(author_1, author_2, author_3, author_4, author_5, sep = " & "), authors = gsub("& NA", "", authors)) %>% 
  select(paper_aut, authors, publication_date, id) %>% 
  mutate(vol = grepl("v", id), id = gsub("i", ":i:", id), id = gsub("p", ":p:", id), id = gsub("v", "v:", id),
         id = ifelse(vol == TRUE, gsub("y", ":y:", id), gsub("y", "y:", id))) %>% select(-vol)
rm(docs_espe)

# descargando datos (te tiene que salir Error in distinct, significa que no se tiene missing)
base_stats_completas <- pmap(base_missing, fun_stats) %>% bind_rows()
nrow(base_stats_completas %>% distinct(id))

# uniendo con new_espe 
base_new <- bind_rows(new_espe, base_stats_completas) %>% mutate(id = gsub("[:]","", id))
nrow(base_new %>% distinct(id))

saveRDS(base_new, paste0(datos_ori, "estadisticas_nuevas/", "base_stats_new_ESPE_1982_",anio_actual, ".rds"))

#Si se quiere comprobar que no existe duplicado en las estadisticas nuevas en ESPE de 1982_anio_actual
base_stats_new_ESPE_1982_2022 <- readRDS("Datos_originales/Webscraping/estadisticas_nuevas/base_stats_new_ESPE_1982_2022.rds")
library(openxlsx)
#En excel, se dirige a datos, en Herramientas de datos, en la mitad, el segundo logo se selecciona (id, serie, title, pub_date, year, month, downloads, abstract_views)
write.xlsx(base_stats_new_ESPE_1982_2022, file = "Correciones/ESPE/New/base_stats_new_ESPE_1982_2022.xlsx", asTable = FALSE)


#-----------------------------------------------------------FIN----------------------------------------------


#Se tuvo  que eliminar las descargas en el 2021 porque presentan anomalias y los id no aparecian
#ESPE intervalo anios: 1982-1989, 1990-1999, 2000-2005, 2006-2010, 2011- anio en curso
#1982-1989
base_stats_new_ESPE_1982_1989 <- readRDS("Datos_originales/Webscraping/estadisticas_nuevas/base_stats_new_ESPE_1982_1989.rds")
library(openxlsx)
write.xlsx(base_stats_new_ESPE_1982_1989, file = "Correciones/ESPE/New/base_stats_new_ESPE_1982_1989.xlsx", asTable = FALSE)
rm(base_stats_new_ESPE_1982_1989)
#importamos el archivo de excel
base_stats_new_ESPE_1982_1989 <- read_excel("Correciones/ESPE/New/base_stats_new_ESPE_1982_1989.xlsx")
#Ahora se guarda a rds 
saveRDS(base_stats_new_ESPE_1982_1989, glue("Correciones/ESPE/New/base_stats_new_ESPE_1982_1989.rds"))
#Se elimina el archivo del datos originales-webscraping y se guarda 
saveRDS(base_stats_new_ESPE_1982_1989, glue("Datos_originales/Webscraping/base_stats_new_ESPE_1982_1989.rds"))
#1990-1999
base_stats_new_ESPE_1990_1999 <- readRDS("Datos_originales/Webscraping/base_stats_new_ESPE_1990_1999.rds")
library(openxlsx)
write.xlsx(base_stats_new_ESPE_1990_1999, file = "Correciones/ESPE/New/base_stats_new_ESPE_1990_1999.xlsx", asTable = FALSE)
#importamos el archivo de excel
base_stats_new_ESPE_1990_1999 <- read_excel("Correciones/ESPE/New/base_stats_new_ESPE_1990_1999.xlsx")
#Ahora se guarda a rds 
saveRDS(base_stats_new_ESPE_1990_1999, glue("Correciones/ESPE/New/base_stats_new_ESPE_1990_1999.rds"))
#Se elimina el archivo del datos originales-webscraping y se guarda 
saveRDS(base_stats_new_ESPE_1990_1999, glue("Datos_originales/Webscraping/base_stats_new_ESPE_1990_1999.rds"))
#2000-2005
base_stats_new_ESPE_2000_2005 <- readRDS("Datos_originales/Webscraping/base_stats_new_ESPE_2000_2005.rds")
library(openxlsx)
write.xlsx(base_stats_new_ESPE_2000_2005, file = "Correciones/ESPE/New/base_stats_new_ESPE_2000_2005.xlsx", asTable = FALSE)
rm(base_stats_new_ESPE_2000_2005)
#importamos el archivo de excel
library(readxl)
base_stats_new_ESPE_2000_2005 <- read_excel("Correciones/ESPE/New/base_stats_new_ESPE_2000_2005.xlsx")
#Ahora se guarda a rds 
saveRDS(base_stats_new_ESPE_2000_2005, glue("Correciones/ESPE/New/base_stats_new_ESPE_2000_2005.rds"))
#Se elimina el archivo del datos originales-webscraping y se guarda 
saveRDS(base_stats_new_ESPE_2000_2005, glue("Datos_originales/Webscraping/base_stats_new_ESPE_2000_2005.rds"))
#2006-2010
base_stats_new_ESPE_2006_2010 <- readRDS("Datos_originales/Webscraping/base_stats_new_ESPE_2006_2010.rds")
library(openxlsx)
write.xlsx(base_stats_new_ESPE_2006_2010, file = "Correciones/ESPE/New/base_stats_new_ESPE_2006_2010.xlsx", asTable = FALSE)
rm(base_stats_new_ESPE_2006_2010)
#importamos el archivo de excel
base_stats_new_ESPE_2006_2010 <- read_excel("Correciones/ESPE/New/base_stats_new_ESPE_2006_2010.xlsx")
#Ahora se guarda a rds
saveRDS(base_stats_new_ESPE_2006_2010, glue("Correciones/ESPE/New/base_stats_new_ESPE_2006_2010.rds"))
#Se elimina el archivo del datos originales-webscraping y se guarda 
saveRDS(base_stats_new_ESPE_2006_2010, glue("Datos_originales/Webscraping/base_stats_new_ESPE_2006_2010.rds"))
#2011-2021
base_stats_new_ESPE_2011_2021 <- readRDS("Datos_originales/Webscraping/base_stats_new_ESPE_2011_2021.rds")
library(openxlsx)
write.xlsx(base_stats_new_ESPE_2011_2021, file = "Correciones/ESPE/New/base_stats_new_ESPE_2011_2021.xlsx", asTable = FALSE)
rm(base_stats_new_ESPE_2011_2021)
#importamos el archivo de excel
base_stats_new_ESPE_2011_2021 <- read_excel("Correciones/ESPE/New/base_stats_new_ESPE_2011_2021.xlsx")
#Ahora se guarda a rds
saveRDS(base_stats_new_ESPE_2011_2021, glue("Correciones/ESPE/New/base_stats_new_ESPE_2011_2021.rds"))
#Se elimina el archivo del datos originales-webscraping y se guarda 
saveRDS(base_stats_new_ESPE_2011_2021, glue("Datos_originales/Webscraping/base_stats_new_ESPE_2011_2021.rds"))




