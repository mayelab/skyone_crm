
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(rvest)
library(leaflet)
library(stringr)
library(DT)
library(data.table)
#library(rgdal)
#library(raster)
library(leaflet.extras)
#library(geosphere)
library(sp)
library(sf)
library(editData)
library(shinyalert)
library(shinyWidgets)
#library(remotes)
#library(openrouteservice)
library(hereR)
#library(osrm)
library(shinybusy)
library(RMySQL)
library(lwgeom)
library(DBI)
library(shinyjs)
library(digest)
library(lubridate)
library(stringi)
library(blastula)
library(tcltk)
library(httr)
library(jsonlite)
library(rlist)

library(vembedr)
library(rgeos)

library(aws.s3)
library(base64enc)

library(magick)
library(colorspace)

library(callr)

library(fontawesome)
library(toastui)

library(tinytex)
#install_tinytex()
library(glue)

#update.packages(ask = FALSE)
library(sortable) #arrastrar para ordenar

#library(slickR) #carousel de fotos

library(metathis) #metadatos para el reporte html

library(qrcode)
library(rclipboard)
library(zip)

library(htmltools)

library(textutils)

#source("multimedia.R")

#utf8mb4     utf8mb4_0900_ai_ci

#options(shiny.trace = TRUE)
options(encoding = "UTF-8")
options(rmarkdown.html_vignette.check_title = FALSE)

estado_per_page <- 6
estado_max_pages <- 4
clien_per_page <- 20
clien_max_pages <- 20
inmo_per_page <- 4
inmo_max_pages <- 8
office_prop_per_page <- 20
office_prop_max_pages <- 20
busqueda_per_page <- 8
busqueda_max_pages <- 20
myplace_per_page <- 20
myplace_max_pages <- 20
scroll_per_page <- 6
scroll_per_page_trans <- 20
all_scroll <- 500
all_scroll_noticias <- 100

empresa <- "SkyOne"
co <- str_to_lower(empresa)

dir <- as.character(tempdir())

main_color <- "#0f2b68"
logo_header <- "skyone_logo_blanco.png"
logo_browser <- "logo_browser_skyone.png"
browser_name <- "SkyOne"
foto_inicio <- "foto_inicio.png"

actividades <- c("Llamar",
                 "Seguimiento",
                 "Coordinar visita",
                 "Enviar nuevas opciones",
                 "Coordinar firma",
                 "Re agendar visita",
                 "Enviar lo requerido",
                 "Llamada mensual")
acciones <- data.frame(accion = actividades, id_accion = seq(1:length(actividades)))

cargos_levels <- factor(c("Global Master Office", "Global Office Manager", "Global Office Assistant", "Country Master Office",
                   "Country Office Manager", "Country Office Assistant", "Broker Office", "Broker Manager",
                   "Broker Assistant", "Asesor"), ordered = TRUE, levels = c("Asesor", "Broker Assistant", "Broker Manager", "Broker Office", 
                                                                             "Country Office Assistant", "Country Office Manager",     
                                                                             "Country Master Office", "Global Office Assistant", 
                                                                             "Global Office Manager", "Global Master Office"))
levels_cargos <- c("Asesor", "Broker Assistant", "Broker Manager", "Broker Office", 
                   "Country Office Assistant", "Country Office Manager", 
                   "Country Master Office", "Global Office Assistant", 
                   "Global Office Manager", "Global Master Office")

global_ruc <- "80126403-0"
global_razon <- "SkyOne EAS"

phone_codes <- data.frame(name = c("Paraguay🇵🇾+595", "Alemania🇩🇪+49", "Argentina🇦🇷+54", "Bolivia🇧🇴+591", "Brasil🇧🇷+55", 
                                   "Chile🇨🇱+56", "Colombia🇨🇴+57", "Ecuador🇪🇨+593", "España🇪🇸+34", "Estados Unidos🇺🇸+1", 
                                   "Holanda🇳🇱+31", "Italia🇮🇹+39", "México🇲🇽+52", "Peru🇵🇪+51", "Rca Dominicana🇩🇴+809",
                                   "Uruguay🇺🇾+598", "Venezuela🇻🇪+58",
                                   "Honduras🇭🇳+504"), 
                          code = c("+595", "+49", "+54", "+591", "+55", 
                                   "+56", "+57", "+593", "+34", "+1", 
                                   "+31", "+39", "+52", "+51", 
                                   "+598", "+58", "+809",
                                   "+504"))

meses <- data.frame(name = c("enero", "febrero", "marzo",
                             "abril", "mayo", "junio",
                             "julio", "agosto", "setiembre",
                             "octubre", "noviembre", "diciembre"),
                    number = 1:12)

key_here <- "CzPzrsKIhxP01tQE4XIkxpgIiSPmXffvzV5Eep0irYU"
set_key(key_here)

whatsapp_token <- "EAAL4VE6rm3IBACclVKbpaA9WL24AwDE9fZB7UEZAfd9Qx0w0hzdGLXZCk1wYBT9Ru5ln9KAqNG5d3RF5ATEHgHsa1Q0xCBgBev9rFSP82IvyW0ZCRnm4npKP0Vo3E7j2ClHcwNbjCZAZAanIlFZAEWVy7EzE9C2no6zF7COZBMLNO9M2S2LGwCUw71ZAN7pTal8BSOjXcDLdiDgZDZD"

jscode <- '
$(function() {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});
'


ini_view <- c(-25.287773045354367, -57.600541080756344)
place <- data.frame(lat = ini_view[1], lng = ini_view[2])

                #create_smtp_creds_key(
                #  id = "skyone",
                #  user = "skyone.realestatelatam@gmail.com",
                #  provider = "gmail",
                #  use_ssl = TRUE,
                #  overwrite = TRUE
                #)
                #wswsdqectfzdtqmk

Sys.setenv(MAIL_PASS='wswsdqectfzdtqmk')
Sys.setenv(MAIL_PASS_PLACE='mdbpdqombeolhgnc')



options(mysql = list(
  "host" = "placeanalyzerdb-do-user-9117395-0.b.db.ondigitalocean.com",
  "port" = 25060,
  "user" = "doadmin",
  "password" = "k03msju7c86b4pmy",
  "databaseName" = "defaultdb"
))

options(digOcean = list(
  "spaces_region" = "nyc3.digitaloceanspaces.com",
  "spaces_key" = "42KYULBUAY4XLCOU3JY5",
  "spaces_secret" = "MTPwfFqaaxghPEy4oV/6eX4yOKGuL302UIq46/LNMEY",
  "spaces_base" = "place-storage",
  "bucket" = "place-storage"
))

create_folder_do <- function(folder){
  aws.s3::s3HTTP(verb = "PUT",
                 path = paste0(folder),
                 request_body = raw(0),
                 region = options()$digOcean$spaces_base,
                 key = options()$digOcean$spaces_key,
                 secret = options()$digOcean$spaces_secret,
                 base_url = options()$digOcean$spaces_region,
                 headers = list(`x-amz-acl` = "public-read"))
}

delete_object_do <- function(object){
  aws.s3::s3HTTP(verb = "DELETE",
                 path = object,
                 region = options()$digOcean$spaces_base,
                 key = options()$digOcean$spaces_key,
                 secret = options()$digOcean$spaces_secret,
                 base_url = options()$digOcean$spaces_region,
                 headers = list(`x-amz-acl` = "public-read"))
}

put_object_do <- function(destiny, object){
  aws.s3::s3HTTP(verb = "PUT",
                 path = destiny,
                 request_body = object,
                 region = options()$digOcean$spaces_base,
                 key = options()$digOcean$spaces_key,
                 secret = options()$digOcean$spaces_secret,
                 base_url = options()$digOcean$spaces_region,
                 headers = list(`x-amz-acl` = "public-read"))
}

get_object_do <- function(object){
  r <- aws.s3::s3HTTP(verb = "GET",
                 path = paste0("/", object),
                 region = options()$digOcean$spaces_base,
                 key = options()$digOcean$spaces_key,
                 secret = options()$digOcean$spaces_secret,
                 base_url = options()$digOcean$spaces_region,
                 headers = list(`x-amz-acl` = "public-read"))
  cont <- httr::content(r, as = "raw")
  return(cont)
}

list_spaces_do <- function(prefix = NULL, delimiter = NULL, max = 1000){
  query <- list(prefix = prefix, delimiter = delimiter, "max-keys" = NULL, marker = NULL)
  r <- aws.s3::s3HTTP(verb = "GET",
                      query = query,
                      region = options()$digOcean$spaces_base,
                      key = options()$digOcean$spaces_key,
                      secret = options()$digOcean$spaces_secret,
                      base_url = options()$digOcean$spaces_region,
                      headers = list(`x-amz-acl` = "public-read"))

  query_2 <- list(prefix = prefix, delimiter = delimiter, "max-keys" = max, marker = tail(r, 1)[["Contents"]][["Key"]])
  extra <- aws.s3::s3HTTP(verb = "GET",
                          query = query_2,
                          parse_response = TRUE,
                          region = options()$digOcean$spaces_base,
                          key = options()$digOcean$spaces_key,
                          secret = options()$digOcean$spaces_secret,
                          base_url = options()$digOcean$spaces_region,
                          headers = list(`x-amz-acl` = "public-read"))
  
  new_r <- c(r, tail(extra, -5))
  new_r[["MaxKeys"]] <- as.character(extra[["MaxKeys"]])
  new_r[["IsTruncated"]] <- extra[["IsTruncated"]]
  attr(new_r, "x-amz-id-2") <- attr(r, "x-amz-id-2")
  attr(new_r, "x-amz-request-id") <- attr(r, "x-amz-request-id")
  attr(new_r, "date") <- attr(r, "date")
  attr(new_r, "x-amz-bucket-region") <- attr(r, "x-amz-bucket-region")
  attr(new_r, "content-type") <- attr(r, "content-type")
  attr(new_r, "transfer-encoding") <- attr(r, "transfer-encoding")
  attr(new_r, "server") <- attr(r, "server")
  r <- new_r
  
  for (i in which(names(r) == "Contents")) {
    r[[i]][["Size"]] <- as.numeric(r[[i]][["Size"]])
    attr(r[[i]], "class") <- "s3_object"
  }
  att <- r[names(r) != "Contents"]
  r[names(r) != "Contents"] <- NULL
  
  # collapse CommonPrefixes elements
  cp <- att[names(att) == "CommonPrefixes"]
  att[names(att) == "CommonPrefixes"] <- NULL
  att[["CommonPrefixes"]] <- as.character(cp)

  out <- structure(r, class = "s3_bucket")
  attributes(out) <- c(attributes(out), att)
  if(length(out) > 0){
    out$Contents$Bucket <- NA
    dat <- as.data.frame(out)
    dat <- dat %>% select(Key, LastModified, Size) %>% 
      mutate(Type = ifelse(str_ends(Key, "/"), "dir", "file"),
             Order = str_count(Key, "/"))
    dat <- dat[!duplicated(dat$Key),]
  }else{
    dat <- structure(list(Key = character(0),
                          LastModified = character(0),
                          Size = character(0),
                          Type = character(0),
                          Order = character(0)),
                     class = "data.frame",
                     row.names = character(0))
  }
  return(dat)
}

nrow_table_mysql <- function(table){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  nrow <- dbGetQuery(conn, paste0("SELECT COUNT (*) FROM ", table, ";")) %>% as.integer()
  dbDisconnect(conn)
  return(nrow)
}

list_n1_to_n2_mysql <- function(table, n1, n2){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  data <- dbGetQuery(conn, paste0("SELECT * FROM ", table, " LIMIT ", n1, ", ", n2,";"))
  dbDisconnect(conn)
  if(nrow(data) != 0){
    data <- data %>% mutate_all(~replace(., . == "NA", NA))
    
    if("geometry" %in% colnames(data)){
      data <- st_as_sf(data, wkt = "geometry", crs = crs)
    }
  }
  return(data)
}

new_id_table_mysql <- function(id, table, prefix, digits){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  data <- dbGetQuery(conn, paste0("SELECT MAX (", id, ") FROM ", table, ";"))
  dbDisconnect(conn)
  if(is.na(data)){
    serie <- 1
  }else{
    #format(monto*as.integer(input$add_user_oficina_number), big.mark = ".", decimal.mark = ",")
    serie <- format(as.numeric(str_extract(data, "\\d+")) + 1, scientific = FALSE)
  }
  new_id <- paste0(prefix, str_pad(serie, digits, pad = "0"))
  return(new_id)
}

clear_table_mysql <- function(table){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  dbGetQuery(conn, paste0("DELETE FROM ", table, ";"))
  dbDisconnect(conn)
}

create_table_mysql <- function(table, data, primary){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  names <-colnames(data)
  classes <- lapply(data, FUN = function(x) ifelse(length(class(x)) > 1, class(x)[1], class(x))) %>%
    unlist()
  if(sum(str_detect(classes, "sfc_")) == 1){
    ind <- which(str_detect(classes, "sfc_"))
    geom <- data[, ind]
    names[ind] <- "geometry"
    data$geometry <- NULL
    data$geometry <- st_astext(geom)
    classes[ind] <- "character"
  }
  new_class <- c()
  for(i in 1:length(classes)){
    if(classes[i] == "character"){
      max <- max(str_count(data[,i]), na.rm = TRUE)
    }else{
      if(classes[i] == "integer"){
        max <- max(data[,i])
      }else{
        max <- 0
      }
    }
    if(classes[i] == "Date"){
      classes[i] <- "character"
      max <- 30
    }
    new_class[i] <- case_when(classes[i] == "character" & max <= 2 ~ paste0(names[i], " VARCHAR(2)"),
                              classes[i] == "character" & max <= 31 ~ paste0(names[i], " VARCHAR(31)"),
                              classes[i] == "character" & max > 31 & max <= 63 ~ paste0(names[i], " VARCHAR(63)"),
                              classes[i] == "character" & max > 63 & max <= 255 ~ paste0(names[i], " VARCHAR(255)"),
                              classes[i] == "character" & max > 255 & max <= 1023 ~ paste0(names[i], " VARCHAR(1023)"),
                              classes[i] == "character" & max > 1023 & max <= 4095 ~ paste0(names[i], " VARCHAR(4095)"),
                              classes[i] == "character" & max > 4095 & max <= 7500 ~ paste0(names[i], " VARCHAR(7500)"),
                              classes[i] == "character" & max > 7500 ~ paste0(names[i], " VARCHAR(15000)"),
                              classes[i] == "integer" & max < 255 ~ paste0(names[i], " TINYINT"),
                              classes[i] == "integer" & max >= 255 ~ paste0(names[i], " INT"),
                              classes[i] == "numeric" ~ paste0(names[i], " DOUBLE"))
  }
  text <- paste0("CREATE TABLE ", table, " (", 
                 paste(new_class, collapse = ", "), ", PRIMARY KEY (", primary, "));")
  dbGetQuery(conn, text)
  dbDisconnect(conn)
}

delete_table_mysql <- function(table){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  dbGetQuery(conn, paste0("DROP TABLE ", table, ";"))
  dbDisconnect(conn)
}

describe_table_mysql <- function(table){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  dat <- dbGetQuery(conn, paste0("DESCRIBE ", table, ";"))
  dbDisconnect(conn)
  return(dat)
}

save_mysql <- function(data, table, overwrite) {
  names <- colnames(data)
  classes <- lapply(data, FUN = function(x) ifelse(length(class(x)) > 1, class(x)[1], class(x))) %>%
    unlist()
  if(sum(str_detect(classes, "sfc_")) == 1){
    ind <- which(str_detect(classes, "sfc_"))
    geom <- data[, ind]
    names[ind] <- "geometry"
    colnames(data) <- names
    data$geometry <- NULL
    data$geometry <- st_astext(geom)
  }
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  if(overwrite == TRUE){
    dbGetQuery(conn, "SET NAMES 'utf8mb4'")
    dbGetQuery(conn, paste0("TRUNCATE ", table, ";"))
  }
  query <- df_to_sql(data, table)
  dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  dbGetQuery(conn, query)
  dbDisconnect(conn)
}

load_mysql <- function(table){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  query <- sprintf("SELECT * FROM %s", table)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  data <- dbGetQuery(conn, query)
  dbDisconnect(conn)
  if(nrow(data) != 0){
    if(nrow(data) == 1){
      data <- rbind(data, data)
      one_row <- TRUE
    }else{
      one_row <- FALSE
    }
    for(i in 1:ncol(data)){
      if(class(data[,i]) == "character"){
        data[,i] <- str_replace_all(data[,i], "U\\+0027", "'") %>% data.frame()
      }
    }
    data <- data %>% mutate_all(~replace(., . == "NA", NA))
    if(one_row == TRUE){data <- data[1,]}
    if("geometry" %in% colnames(data)){
      data <- st_as_sf(data, wkt = "geometry", crs = crs)
    }
  }
  return(data)
}

killDbConnections <- function () {
  all_cons <- dbListConnections(MySQL())
  print(all_cons)
  for(con in all_cons)
    +  dbDisconnect(con)
  print(paste(length(all_cons), " connections killed."))
}

df_to_sql <- function(data, table){
  if(nrow(data) == 1){
    data <- rbind(data, data)
    one_row <- TRUE
  }else{
    one_row <- FALSE
  }
  values <- apply(data, 2, str_replace_all, "'", "U+0027") %>% data.frame
  if(one_row == TRUE){values <- values[1,]}
  values <- paste0(apply(values, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
  colname <- paste0(" (", paste0(colnames(data), collapse = ", "), ")")
  paste0("INSERT INTO ", table, colname," VALUES ", values, ";")
}

str_detect_mysql <- function(table, var, pattern){
  pattern <- str_replace_all(pattern, "'", "U+0027")
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  data <- dbGetQuery(conn, paste0("SELECT * FROM ", table, " WHERE ", var, " LIKE '%", pattern, "%';"))
  dbDisconnect(conn)
  if(nrow(data) != 0){
    data <- data %>% mutate_all(~replace(., . == "NA", NA))
    for(i in 1:ncol(data)){
      if(class(data[,i]) == "character"){
        data[,i] <- str_replace_all(data[,i], "U\\+0027", "'") %>% data.frame()
      }
    }
    if("geometry" %in% colnames(data)){
      data <- st_as_sf(data, wkt = "geometry", crs = crs)
    }
  }
  return(data)
}

filter_point_table_mysql <- function(point, table){
  coor <- st_coordinates(point)
  lat <- coor[2]
  lon <- coor[1]
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  data <- dbGetQuery(conn, paste0("SELECT * FROM ", table, " WHERE ",lat, 
                                  " BETWEEN ", table, ".lat_min AND ", table, ".lat_max AND ", lon, 
                                  " BETWEEN ", table, ".lon_min AND ", table, ".lon_max;"))
  dbDisconnect(conn)
  if(nrow(data) != 0){
    data <- data %>% mutate_all(~replace(., . == "NA", NA))
    for(i in 1:ncol(data)){
      if(class(data[,i]) == "character"){
        data[,i] <- str_replace_all(data[,i], "U\\+0027", "'") %>% data.frame()
      }
    }
    if("geometry" %in% colnames(data)){
      data <- st_as_sf(data, wkt = "geometry", crs = crs)
    }
  }
  return(data)
}

filter_box_table_mysql <- function(lat_min, lat_max, lon_min, lon_max, table){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  col <- dbGetQuery(conn, paste0("SHOW COLUMNS FROM ", table))
  if("lon" %in% col$Field){
    longitud <- "lon"
  }else{
    longitud <- "lng"
  }
  data <- dbGetQuery(conn, paste0("SELECT * FROM ", table, " WHERE ", table, ".lat  BETWEEN ", lat_min, 
                                  " AND ", lat_max, " AND ", table, ".", longitud, " BETWEEN ", lon_min, 
                                  " AND ", lon_max, ";"))
  dbDisconnect(conn)
  
  if(nrow(data) != 0){
    data <- data %>% mutate_all(~replace(., . == "NA", NA))
    for(i in 1:ncol(data)){
      if(class(data[,i]) == "character"){
        data[,i] <- str_replace_all(data[,i], "U\\+0027", "'") %>% data.frame()
      }
    }
    if("geometry" %in% colnames(data)){
      data <- st_as_sf(data, wkt = "geometry", crs = crs)
    }
  }
  return(data)
}

select_tipo_by_rubro_mysql <- function(rubro){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  data <- dbGetQuery(conn, paste0("SELECT DISTINCT tipo FROM comercios WHERE rubro = '", rubro, "';"))
  dbDisconnect(conn)
  return(data)
}

update_var_table_mysql <- function(new_info, var, var_id, id, table){
  new_info <- str_replace_all(new_info, "'", "U+0027")
  id <- paste("(", paste(paste0("'", id,"'"),collapse = ","), ")")
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  dbGetQuery(conn, paste0("UPDATE ", table, " SET ", table, ".", var, " = '", new_info, 
                          "' WHERE ", table, ".", var_id, " in ", id, ";"))
  dbDisconnect(conn)
}

update_var_cond1_cond2_table_mysql <- function(new_info, var, var_cond1, cond1, var_cond2, cond2, table){
  new_info <- str_replace_all(new_info, "'", "U+0027")
  cond1 <- paste("(", paste(paste0("'", cond1,"'"),collapse = ","), ")")
  cond2 <- paste("(", paste(paste0("'", cond2,"'"),collapse = ","), ")")
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  dbGetQuery(conn, paste0("UPDATE ", table, " SET ", table, ".", var, " = '", new_info, 
                          "' WHERE ", table, ".", var_cond1, " in ", cond1, " AND ",
                          table, ".", var_cond2, " in ", cond2, ";"))
  dbDisconnect(conn)
}

list_table_var1_var2_var3_mysql <- function(table, var1, info1, var2, info2, var3, cond3, info3){
  info1 <- str_replace_all(info1, "'", "U+0027")
  info2 <- str_replace_all(info2, "'", "U+0027")
  info3 <- str_replace_all(info3, "'", "U+0027")
  info1 <- paste("(", paste(paste0("'", info1,"'"),collapse = ","), ")")
  info2 <- paste("(", paste(paste0("'", info2,"'"),collapse = ","), ")")
  info3 <- paste("(", paste(paste0("'", info3,"'"),collapse = ","), ")")
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  data <- dbGetQuery(conn, paste0("SELECT * FROM ", table, " WHERE ", var1, " in ", info1,
                                  " AND ", var2, " IN ", info2, " AND ", var3, " ", cond3, " ", info3, ";"))
  dbDisconnect(conn)
  if(nrow(data) != 0){
    data <- data %>% mutate_all(~replace(., . == "NA", NA))
    for(i in 1:ncol(data)){
      if(class(data[,i]) == "character"){
        data[,i] <- str_replace_all(data[,i], "U\\+0027", "'") %>% data.frame()
      }
    }
    if("geometry" %in% colnames(data)){
      data <- st_as_sf(data, wkt = "geometry", crs = crs)
    }
  }
  return(data)
}

list_table_var1_var2_mysql <- function(table, var1, info1, var2, info2){
  info1 <- str_replace_all(info1, "'", "U+0027")
  info2 <- str_replace_all(info2, "'", "U+0027")
  info1 <- paste("(", paste(paste0("'", info1,"'"),collapse = ","), ")")
  info2 <- paste("(", paste(paste0("'", info2,"'"),collapse = ","), ")")
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  data <- dbGetQuery(conn, paste0("SELECT * FROM ", table, " WHERE ", var1, " in ", info1,
                                  " AND ", var2, " in ", info2, ";"))
  dbDisconnect(conn)
  if(nrow(data) != 0){
    data <- data %>% mutate_all(~replace(., . == "NA", NA))
    for(i in 1:ncol(data)){
      if(class(data[,i]) == "character"){
        data[,i] <- str_replace_all(data[,i], "U\\+0027", "'") %>% data.frame()
      }
    }
    if("geometry" %in% colnames(data)){
      data <- st_as_sf(data, wkt = "geometry", crs = crs)
    }
  }
  return(data)
}

list_table_var1_var2_cond_mysql <- function(table, var1, info1, var2, cond, info2, ext_conn = FALSE, conn = NA){
  info1 <- paste("(", paste(paste0("'", info1,"'"),collapse = ","), ")")
  info2 <- paste("(", paste(paste0("'", info2,"'"),collapse = ","), ")")
  if(ext_conn == FALSE){
    conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                      port = options()$mysql$port, user = options()$mysql$user, 
                      password = options()$mysql$password)
  }
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  data <- dbGetQuery(conn, paste0("SELECT * FROM ", table, " WHERE ", var1, " in ", info1, " AND ", var2, " ", cond, " ", info2, ";"))
  if(ext_conn == FALSE){
    dbDisconnect(conn)
  }
  if(nrow(data) != 0){
    data <- data %>% mutate_all(~replace(., . == "NA", NA))
    for(i in 1:ncol(data)){
      if(class(data[,i]) == "character"){
        data[,i] <- str_replace_all(data[,i], "U\\+0027", "'") %>% data.frame()
      }
    }
    if("geometry" %in% colnames(data)){
      data <- st_as_sf(data, wkt = "geometry", crs = crs)
    }
  }
  return(data)
}

list_table_var_mysql <- function(table, var, info){
  info <- str_replace_all(info, "'", "U+0027")
  info <- paste("(", paste(paste0("'", info,"'"),collapse = ","), ")")
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  data <- dbGetQuery(conn, paste0("SELECT * FROM ", table, " WHERE ", var, " in ", info, ";"))
  dbDisconnect(conn)
  if(nrow(data) != 0){
    data <- data %>% mutate_all(~replace(., . == "NA", NA))
    for(i in 1:ncol(data)){
      if(class(data[,i]) == "character"){
        data[,i] <- str_replace_all(data[,i], "U\\+0027", "'") %>% data.frame()
      }
    }
    if("geometry" %in% colnames(data)){
      data <- st_as_sf(data, wkt = "geometry", crs = crs)
    }
  }
  return(data)
}
table <- "myplace_inmuebles"
company <- co
prefijo <- "inmueble"
cantidad <- 10


new_id_myplace_mysql <- function(table, company, prefijo, cantidad){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  data <- dbGetQuery(conn, paste0("SELECT MAX(id) FROM ", table, " WHERE id LIKE '", company, "%';"))
  dbDisconnect(conn)
  
  serie <- as.numeric(str_extract(data, "\\d+"))
  id <- paste0(company, "_", prefijo, "_", str_pad(serie + 1, cantidad, pad = "0"))
  
  return(id)
}

query_mysql <- function(query){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  data <- dbGetQuery(conn, query)
  dbDisconnect(conn)
  if(nrow(data) != 0){
    data <- data %>% mutate_all(~replace(., . == "NA", NA))
    for(i in 1:ncol(data)){
      if(class(data[,i]) == "character"){
        data[,i] <- str_replace_all(data[,i], "U\\+0027", "'") %>% data.frame()
      }
    }
    if("geometry" %in% colnames(data)){
      data <- st_as_sf(data, wkt = "geometry", crs = crs)
    }
  }
  return(data)
}

list_last_fotos_mysql <- function(user, fecha_min){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  data <- dbGetQuery(conn, paste0("SELECT * FROM fotos_street360_db WHERE user = '", user, 
                                  "' AND fecha >= '", fecha_min, "';"))
  dbDisconnect(conn)
  if(nrow(data) != 0){
    data <- data %>% mutate_all(~replace(., . == "NA", NA))
  }  
  return(data)
}

last_id_table_mysql <- function(table){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  data <- dbGetQuery(conn, paste0("SELECT MAX(id) FROM ", table, ";"))
  dbDisconnect(conn)
  return(data)
}

del_var_table_mysql <- function(var, info, table){
  info <- paste("(", paste(paste0("'", info,"'"),collapse = ","), ")")
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  dbGetQuery(conn, paste0("DELETE FROM ", table, " WHERE ", var, " in ", info, ";"))
  dbDisconnect(conn)
}

del_var1_var2_table_mysql <- function(var1, info1, var2, info2, table, ext_conn = FALSE, conn = NA){
  info1 <- paste("(", paste(paste0("'", info1,"'"),collapse = ","), ")")
  info2 <- paste("(", paste(paste0("'", info2,"'"),collapse = ","), ")")
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  dbGetQuery(conn, paste0("DELETE FROM ", table, " WHERE ", var1, " in ", info1, " AND ", var2, " in ", info2,";"))
  dbDisconnect(conn)
}

load_last_venpru_mysql <- function(cuenta){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  data <- dbGetQuery(conn, paste0("SELECT * FROM venpru WHERE venpru.cuenta = '", cuenta, "' ORDER BY venpru.date DESC LIMIT 1;"))
  dbDisconnect(conn)
  for(i in 1:ncol(data)){
    if(class(data[,i]) == "character"){
      data[,i] <- str_replace_all(data[,i], "U\\+0027", "'") %>% data.frame()
    }
  }
  data <- data %>% mutate_all(~replace(., . == "NA", NA))
  return(data)
}

exist_var_table_mysql <- function(info, var, table){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  data <- dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  data <- dbGetQuery(conn, paste0("SELECT * FROM ", table, " WHERE ", var, " LIKE '", info, "';"))
  dbDisconnect(conn)
  data <- ifelse(nrow(data) != 0, TRUE, FALSE)
  return(data)
}

delete_id_mysql <- function(new_info, table){
  conn <- dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
  id <- new_info$id
  dbGetQuery(conn, "SET NAMES 'utf8mb4'")
  dbGetQuery(conn, paste0("DELETE FROM ", table, " WHERE id = ", id, ";"))
  dbDisconnect(conn)
}

connect_mysql <- function(){
  dbConnect(MySQL(), dbname = options()$mysql$databaseName, host = options()$mysql$host,
            port = options()$mysql$port, user = options()$mysql$user, 
            password = options()$mysql$password)
}

disconnect_mysql <- function(conn){
  dbDisconnect(conn)
}

#dollarExchange <- function(){
#  maxi <- "https://www.cambioschaco.com.py"
#  spans <- read_html(maxi) %>% html_elements("span")
#  span_class <- spans %>% html_attr("class")
#  compra <- spans[!is.na(span_class) & span_class == "purchase"][1] %>% html_text() %>% 
#    str_remove_all("\\.") %>% as.numeric()
#  venta <- spans[!is.na(span_class) & span_class == "sale"][1] %>% html_text() %>% 
#    str_remove_all("\\.") %>% as.numeric()
#  return(data.frame(compra=compra, venta=venta))
#}

dollarExchange <- function(){
  result <- GET("https://www.xe.com/es/currencyconverter/convert/?Amount=1&From=USD&To=PYG")
  text <- read_html(result) %>% html_text2()
  camb1 <- str_split(text, fixed("PYG\":"))[[1]][5]
  cambio <- str_split(camb1, fixed(","))[[1]][1] %>% str_replace_all(",", ".") %>% as.numeric() %>% round()
  return(data.frame(compra=cambio, venta=cambio))
}

cambio_dolar <- dollarExchange()

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

new_orilla_top <- function(new_sf, name, factor, dist, curv, orillas_top){
  new_line <- new_sf %>% dplyr::select(geometry)
  new_line$name <- name
  new_line$factor <- factor
  new_line$dist <- dist
  new_line$curv <- curv
  orillas_top <- rbind(orillas_top, new_line)
  save_mysql(orillas_top, "ajustes_algo", TRUE)
  return(orillas_top)
}

rosarioIcon <- makeIcon(
  iconUrl = "Marker Place.png",
  iconWidth = 54, iconHeight = 67,
  iconAnchorX = 21, iconAnchorY = 65
)

icon_inmo <- function(tipo){
  name = case_when(tipo %in% c("Terreno", "Propiedad rural") ~ "terreno_marker_azul.png",
                   tipo == "Duplex" ~ "duplex_marker_azul.png",
                   tipo %in% c("Casa", "Casa quinta") ~ "inmo_marker_azul.png",
                   tipo %in% c("Departamento", "Proyecto", "Edificio") ~ "depa_marker_azul.png",
                   tipo %in% c("Deposito", "Tinglado") ~ "deposito_marker_azul.png",
                   tipo %in% c("Oficina", "Salón comercial") ~ "oficina_marker_azul.png")
  icon <- makeIcon(
    iconUrl = paste0("./www/", name),
    iconWidth = 50, iconHeight = 50,
    iconAnchorX = 25, iconAnchorY = 50
  )
  return(icon)
}

icon_tipo <- function(tipo, size){
  name = case_when(tipo == "academia de danza" ~ "dance_class.png",
                   tipo == "acuario" ~ "aquarium.png",
                   tipo == "agencia de viajes" ~ "travel_agency.png",
                   tipo == "agricultura" ~ "rice.png",
                   tipo %in% c("agroganaderia", "ganaderia", "frigorifico") ~ "cow-export.png",
                   tipo == "alimentos importados" ~ "foodcan.png",
                   tipo == "ambulancia" ~ "ambulance.png",
                   tipo == "ande" ~ "powerlinepole.png",
                   tipo == "arquitectura" ~ "arch.png",
                   tipo == "artes marciales" ~ "judo.png",
                   tipo == "artesania" ~ "art-museum-2.png",
                   tipo == "atractivo turistico" ~ "waterfall-2.png",
                   tipo == "audiovisuales" ~ "photography.png",
                   tipo == "automoviles" ~ "car.png",
                   tipo == "autoservice" ~ "conveniencestore.png",
                   tipo %in% c("banco", "financiera", "creditos") ~ "bank_euro.png",
                   tipo == "bar" ~ "bar_coktail.png",
                   tipo == "bazar" ~ "grocery.png",
                   tipo == "bodega" ~ "bar.png",
                   tipo == "bomberos" ~ "firemen.png",
                   tipo %in% c("cabaña", "posada") ~ "cabin-2.png",
                   tipo == "cafeteria" ~ "coffee.png",
                   tipo == "cajero automatico" ~ "atm-2.png", 
                   tipo == "calzados" ~ "shoes.png",
                   tipo == "cambios" ~ "currencyexchange.png",
                   tipo == "camping" ~ "campfire-2.png",
                   tipo == "canchas" ~ "soccer.png",
                   tipo == "carniceria" ~ "restaurant_steakhouse.png",
                   tipo == "carteles" ~ "signpost-2.png",
                   tipo == "carteras" ~ "bags.png",
                   tipo == "casa de bolsa" ~ "chart-2.png",
                   tipo == "casino" ~ "poker.png",
                   tipo == "caza y pesca" ~ "hunting.png",
                   tipo == "celulares" ~ "phones.png",
                   tipo == "cementerio" ~ "catholicgrave.png",
                   tipo == "ceramica" ~ "museum_crafts.png",
                   tipo == "cerrajeria" ~ "key.png",
                   tipo == "chaperia y pintura" ~ "convertible.png",
                   tipo == "chiperia" ~ "peyote.png",
                   tipo == "ciclismo" ~ "bicycle_shop.png",
                   tipo == "cine" ~ "movierental.png",
                   tipo == "clinica" ~ "medicine.png",
                   tipo == "club" ~ "stadium.png",
                   tipo == "cobranza" ~ "symbol_dollar.png",
                   tipo == "colchoneria" ~ "sponge.png",
                   tipo %in% c("colegio nacional", "colegio privado", "escuela") ~ "school.png",
                   tipo %in% c("comedor", "copetin") ~ "hotfoodcheckpoint.png",
                   tipo == "comercial" ~ "hifi.png",
                   tipo == "comida oriental" ~ "restaurant_chinese.png",
                   tipo == "comida saludable" ~ "restaurant_vegetarian.png",
                   tipo == "comisaria" ~ "star-3.png",
                   tipo == "compañias de celulares" ~ "phones.png",
                   tipo == "complejo" ~ "beach_icon.png",
                   tipo == "condominio" ~ "condominium.png",
                   tipo == "confiteria" ~ "patisserie.png",
                   tipo == "construccion" ~ "bulldozer.png",
                   tipo == "consultorio" ~ "emergencyphone.png",
                   tipo == "cooperativa" ~ "sozialeeinrichtung.png",
                   tipo == "copaco" ~ "telephone.png",
                   tipo == "cotillon" ~ "party-2.png",
                   tipo == "courier" ~ "air_fixwing.png",
                   tipo == "deposito de materiales" ~ "construction.png",
                   tipo == "despensa" ~ "apple.png",
                   tipo == "distribuidora" ~ "truck3.png",
                   tipo == "edificios" ~ "apartment-3.png",
                   tipo == "electricidad" ~ "poweroutage.png",
                   tipo == "electrodomesticos" ~ "laundromat.png",
                   tipo == "electronica" ~ "videogames.png",
                   tipo == "embutidos" ~ "restaurant_greek.png",
                   tipo == "empanadas" ~ "oyster-3.png",
                   tipo == "empeño" ~ "tailor.png",
                   tipo == "equipamiento" ~ "audio.png",
                   tipo == "essap"~ "watertower.png",
                   tipo == "estacionamiento" ~ "parkinggarage.png",
                   tipo %in% c("estancia", "rancho") ~ "corral.png",
                   tipo == "estudio juridico y contable" ~ "court.png",
                   tipo == "eventos" ~ "anniversary.png",
                   tipo == "fabrica" ~ "factory.png",
                   tipo == "farmacia" ~ "firstaid.png",
                   tipo %in% c("fast-food", "franquicia") ~ "fastfood.png",
                   tipo == "ferreteria" ~ "tools.png",
                   tipo == "floreria" ~ "garden.png",
                   tipo == "fraccion" ~ "forest2.png",
                   tipo == "fruteria y verduleria" ~ "fruits.png",
                   tipo == "fuerzas armadas" ~ "military.png",
                   tipo == "funeraria" ~ "headstone-2.png",
                   tipo == "gasolinera" ~ "fillingstation.png",       
                   tipo == "gimnasio" ~ "weights.png",
                   tipo %in% c("gomeria", "baterias", "neumaticos") ~ "tires.png",
                   tipo %in% c("granja", "silos") ~ "farm-2.png",
                   tipo == "granja avicola" ~ "chicken-2.png",
                   tipo == "guarderia" ~ "daycare.png",
                   tipo == "hamburgueseria" ~ "burger.png",
                   tipo == "heladeria" ~ "icecream.png",
                   tipo == "herreria" ~ "foundry-2.png",
                   tipo == "hierros" ~ "powersubstation.png",
                   tipo == "hogar" ~ "lockerroom.png",
                   tipo %in% c("hospedaje", "hostal") ~ "lodging-2.png",
                   tipo == "hospital" ~ "hospital-building.png",
                   tipo == "hotel" ~ "hotel_0star.png",
                   tipo %in% c("iglesia", "capilla", "evangelicos", "adventistas", 
                               "casa de retiro") ~ "church-2.png",
                   tipo == "iluminacion" ~ "poweroutage.png", 
                   tipo == "informatica" ~ "computers.png",
                   tipo == "inmobiliaria" ~ "home-2.png",
                   tipo == "instituto" ~ "icon-sevilla.png",
                   tipo %in% c("joyeria", "accesorios") ~ "jewelry.png",                                                      #
                   tipo == "jugueteria" ~ "toys.png",
                   tipo == "juzgado" ~ "gavel-auction-fw.png",
                   tipo == "laboratorio" ~ "laboratory.png",
                   tipo == "lacteos" ~ "milk_bottle.png",
                   tipo == "lavadero" ~ "carwash.png",
                   tipo == "lavanderia" ~ "laundromat.png",
                   tipo == "lenceria" ~ "lingerie.png",
                   tipo == "libreria" ~ "pens.png",
                   tipo == "limpieza" ~ "trash.png",
                   tipo == "lomiteria" ~ "kebab.png",
                   tipo == "lubricante" ~ "oil-2.png",
                   tipo == "maderas" ~ "sawmill-2.png",
                   tipo == "makeup" ~ "love_date.png",
                   tipo %in% c("merceria", "regalos") ~ "gifts.png",                                                       #
                   tipo == "metalurgica" ~ "welding.png",
                   tipo %in% c("ministerio", "gobernacion", 
                               "dependencias gubernamentales") ~ "reception.png",
                   tipo == "motel" ~ "motel-2.png",
                   tipo == "moto" ~ "vespa.png",
                   tipo == "moto repuestos" ~ "motorcycle.png",
                   tipo %in% c("muebleria", "tapiceria") ~ "homecenter.png",                                                 #
                   tipo == "musica" ~ "music_classical.png",
                   tipo == "nails" ~ "icon-nailsalon.png",
                   tipo == "odontologia" ~ "dentist.png",
                   tipo == "optica" ~ "glasses.png",
                   tipo == "panaderia" ~ "bread.png",
                   tipo == "parque" ~ "arbol.png",
                   tipo == "parroquia" ~ "cathedral.png",
                   tipo %in% c("peluqueria", "barberia") ~ "barber.png",
                   tipo == "perfumeria" ~ "perfumery.png",
                   tipo == "pescaderia" ~ "restaurant_fish.png",
                   tipo == "piscinas" ~ "swimming.png",
                   tipo == "pintureria" ~ "paint.png",
                   tipo == "piscicultura" ~ "restaurant_fish.png",
                   tipo == "pizzeria" ~ "pizzaria2.png",
                   tipo %in% c("granja avicola", "polleria") ~ "chicken-2.png",
                   tipo == "posada" ~ "ne_barn-2.png",
                   tipo == "productos importados" ~ "teahouse.png",
                   tipo == "pub" ~ "music.png",
                   tipo == "publicitaria" ~ "newsagent.png",
                   tipo == "puesto de salud" ~ "aed-2.png",
                   tipo == "quinta" ~ "riparianhabitat.png",
                   tipo == "radio" ~ "radio-station-2.png",
                   tipo == "refrigeracion" ~ "coldstorage.png",
                   tipo == "rent a car" ~ "carrental.png",
                   tipo == "repuesto" ~ "velocimeter.png",
                   tipo == "restaurante" ~ "restaurant.png",
                   tipo == "resto bar" ~ "music_rock.png",
                   tipo %in% c("ropa", "boutique") ~ "clothers_female.png",                                                 #
                   tipo == "ropa deportiva" ~ "jogging.png",
                   tipo == "ropa infantil" ~ "nursery.png",
                   tipo == "ropa usada" ~ "2hand.png",
                   tipo == "refrigeracion" ~ "snowy-2.png",
                   tipo == "sanitarios" ~ "waterdrop.png",
                   tipo == "sastreria" ~ "clothers_male.png",
                   tipo == "seguridad" ~ "amphitheater-2.png",
                   tipo == "seguros" ~ "caraccident.png",
                   tipo == "serigrafia" ~ "smiley_happy.png",
                   tipo == "servicios graficos" ~ "printer-2.png",
                   tipo %in% c("shopping", "paseo") ~ "mall.png",
                   tipo == "spa" ~ "massage.png",
                   tipo == "supermercado" ~ "supermarket.png",
                   tipo == "suplemaentos" ~ "coldfoodcheckpoint.png",
                   tipo == "taller" ~ "workshop.png",
                   tipo == "taller de motos" ~ "repair.png",
                   tipo == "tatuajes" ~ "dragon.png",
                   tipo == "taxi" ~ "taxi.png",
                   tipo == "tejidos" ~ "textiles.png",
                   tipo == "transportes" ~ "truck3.png",
                   tipo == "universidad" ~ "highschool.png",
                   tipo == "vape" ~ "smoking.png",
                   tipo %in% c("veterinaria", "agroveterinaria") ~ "veterinary.png",
                   tipo == "vidrieria" ~ "glazer.png",
                   tipo == "vivero" ~ "garden.png",
                   TRUE ~ "symbol_blank.png")
  icon <- makeIcon(
    iconUrl = name,
    iconWidth = size, iconHeight = size,
    iconAnchorX = size/2, iconAnchorY = size
  )
  return(icon)
}

sf_to_df <- function(sf){
  coords <- st_coordinates(sf) %>% data.frame() %>% dplyr::rename(lng=X, lat=Y)
  sf$geometry <- NULL
  df <- cbind(sf, coords)
  return(df)
}

leaflet_draw <- function(draw){
  if(!is.null(draw)){
    type <- draw$properties$feature_type
    if(type == "polygon"){
      sf <- draw
      sf <- sf$geometry$coordinates[[1]]
      new <- data.frame()
      for(i in 1:length(sf)){
        new <- rbind(new, data.frame(lon=sf[[i]][[1]][1], lat=sf[[i]][[2]][1]))
      }
      df <- new
      new_sf <- zona_maker(new, 1)
      new_sf$nivel_economico <- NULL
      sf <- new_sf
    }else{
      if(type == "polyline"){
        l <- unlist(draw$geometry)[-1]
        df <- data.frame(lon = as.numeric(l[seq(1,length(l), 2)]), lat = as.numeric(l[seq(2,length(l), 2)]))
        sf <- st_as_sf(df, coords = c("lon", "lat"), crs = crs)
        sf <- list(st_linestring(st_coordinates(sf))) %>% st_sfc %>% st_as_sf(crs=crs)
        sf <- rename(sf, geometry = x)
      }else{
        df <- leaf_to_df(draw)
        sf <- df %>% st_as_sf(coords = c("lon", "lat"), crs = crs)
      }
    }  
    new_zone <- list(type = type, df = df, sf = sf)
  }else{
    new_zone <- NULL
  }
  return(new_zone)
}

leaf_to_df <- function(leaf){
  if(leaf$geometry$type != "Point"){
    n <- length(leaf$geometry$coordinates)
    from <- leaf$geometry$coordinates[[1]]
    to <- leaf$geometry$coordinates[[n]]
    df <- rbind(data.frame(lat=from[[2]], lon=from[[1]]),
                data.frame(lat=to[[2]], lon=to[[1]]))
  }else{
    df <- data.frame(lat=leaf$geometry$coordinates[[2]], 
                     lon=leaf$geometry$coordinates[[1]])
  }
  return(df)
}


zona_maker <- function(df, id){
  crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  x = Polygon(df)
  x = Polygons(list(x), 1)
  x = SpatialPolygons(list(x))
  proj4string(x) = CRS(crs)
  x <- SpatialPolygonsDataFrame(x, data.frame(id=id, nivel_economico=3))
  x <- st_as_sf(x)
  x$area <- round(st_area(x))
  return(x)
}

show_login_menu <- function(){
  showModal(tags$div(id="modal_login_sky", modalDialog(
    div(style = "padding: 20px", 
        textInput("login_user", HTML("<p><span style='font-weight: 400; color: white'>Correo electrónico</span></p>")),
        br(),
        passwordInput("login_pass", HTML("<p><span style='font-weight: 400; color: white'>Contraseña</span></p>")), 
        align = "center"), br(),
    div(
      actionBttn("ingresar", "Ingresar"),
    align = "center", width = "80%"), br(),
    div(actionBttn("pass_forget", HTML("<p><span style='color: white'>¿Olvidaste la contraseña?</span></p>"), size = "sm", style = "minimal", color = "default"),
        align = "center"),
    br(),
    div(style = "padding-bottom: 5px", HTML(paste0('<img src="', foto_inicio, '" width=285>')), align = "center"),
    easyClose = FALSE,
    footer = NULL,
    size = "s",
    fade = TRUE,
    tags$head(tags$script(HTML(jscode))),
    `data-proxy-click` = "ingresar"
  )))
}

gradient_color <- function(color){
  style <- paste0("background: linear-gradient(0.15turn, ", color, ", ",
                  darken(color, 0.20), ", ",
                  color, " 70%, ",
                  lighten(color, 0.30), ", ",
                  color, ")")
  return(style)
}

gradient_box <- function(color){
  style <- paste0(gradient_color(color), ";
                  padding: 20px; border-radius: 10px; 
                  box-shadow: rgba(0, 0, 0, 0.6) 10px 20px 30px 0px,
                  inset 5px 5px 5px rgba(255,255,255,.4), 
                  inset -5px -5px 5px rgba(0,0,0,.3);
                  margin: 15px")
  return(style)
}

num_fotos_inmo <- function(img, small){
  id <- str_split(str_split(img, "fotos-inmo/")[[1]][2], "-")[[1]][1]
  serie <- str_extract(img, "_[0-9][0-9][0-9]-small") %>% str_remove("_") %>% str_remove("-small")
  
  list <- list_spaces_do(prefix = paste0("fotos-inmo/", id, "-"))
  if(small){
    list <- list[str_detect(list$Key, paste0(serie, "-small.jpg")),]
  }else{
    list <- list[str_detect(list$Key, paste0(serie, ".jpg")),]
  }
  if(nrow(list) == 0){
    return(c())
  }else{
    return(paste0("https://place-storage.nyc3.digitaloceanspaces.com/", list$Key))
  }
}
#small <- TRUE
#img <- "https://place-storage.nyc3.digitaloceanspaces.com/skyone/fotos-inmo/skyone_inmueble_0000004697-01_930-small.jpg"
#num_fotos_inmo(img, TRUE)

coincidencias <- function(search, dir, session){
  existe <- TRUE
  cambio <- dollarExchange()$venta
  print(cambio)
  inmo_search <- str_detect(class(search$geometry)[1], "POINT")
  id_coinci <- c()
  if(inmo_search){
    inmos <- search
    if(inmos$moneda_contrato == "GS"){
      inmos$precio <- inmos$precio/cambio
    }
    
    busquedas <- filter_point_table_mysql(search, "myplace_busquedas")
    busquedas <- busquedas %>% filter(activo == "si")
    
    if(nrow(busquedas) != 0){
      st_crs(busquedas) <- crs
    }
    
    if(nrow(busquedas) == 0){
      existe <- FALSE
    }else{
      busquedas$precio_max <- ifelse(busquedas$moneda == "USD", busquedas$precio_max, busquedas$precio_max/cambio)
      busquedas$precio_min <- ifelse(busquedas$moneda == "USD", busquedas$precio_min, busquedas$precio_min/cambio)
    } 
    print("Inmueble")
  }else{
    print("inicia busquedas")
    
    box <- st_bbox(search)
    lat_min <- box[2]
    lat_max <- box[4]
    lon_min <- box[1]
    lon_max <- box[3]
    inmos <- filter_box_table_mysql(lat_min, lat_max, lon_min, lon_max, "myplace_inmuebles")
    inmos <- inmos %>% filter(precio_cierre == 0 & borrador == "no")
    print(inmos)
    if(nrow(inmos) == 0){
      existe <- FALSE
    }else{
      inmos$precio <- ifelse(inmos$moneda_contrato == "USD", inmos$precio, inmos$precio/cambio)
    } 
    print("existe")
    print(existe)
    busquedas <- search
    busquedas$company <- "skyone"
    if(busquedas$moneda == "GS"){
      busquedas$precio_max <- busquedas$precio_max/cambio
      busquedas$precio_min <- busquedas$precio_min/cambio
    }
    print("Busqueda")
  }
  if(existe){
    ubi <- st_intersection(busquedas, inmos)
    
    if(nrow(ubi) != 0){
      for(i in 1:nrow(ubi)){
        print(i)
        okis <- ubi[i,]
        print(okis)
        if(inmo_search){
          new_inmo <- inmos
        }else{
          new_inmo <- inmos[inmos$id == okis$id.1, ]
        }
        if(new_inmo$tipoDato == "Oferta"){
          tipo <- ifelse(okis$tipoPropiedad != new_inmo$tipoPropiedad | okis$venta_alquiler != new_inmo$venta_alquiler, 0, 1)
          if(tipo == 1){ #coincide operacion y tipo
            precio <- ifelse(okis$precio_max != 0 && !between(new_inmo$precio, okis$precio_min, okis$precio_max), 0, 1)
            m2 <- ifelse(okis$m2_max != 0 && new_inmo$m2 != 0 && !between(new_inmo$m2, okis$m2_min, okis$m2_max), 0, 1)
            m2_cons <- ifelse(okis$m2_cons_max != 0 && new_inmo$m2_cons != 0 && !between(new_inmo$m2_cons, okis$m2_cons_min, okis$m2_cons_max), 0, 1)
            dormitorios <- ifelse(!is.na(okis$dormitorios_min) && !is.na(new_inmo$dormitorios) && new_inmo$dormitorios != okis$dormitorios_min, 0, 1)
            banios <- ifelse(!is.na(okis$banios_min) && !is.na(new_inmo$banios) &&
                               as.integer(str_extract(new_inmo$banios, "\\d")) < as.integer(str_extract(okis$banios_min, "\\d")), 0, 1)
            pisos <- ifelse(okis$pisos_min != 0 && new_inmo$pisos < okis$pisos_min, 0, 1)
            garajes <- ifelse(!is.na(okis$garajes_min) && !is.na(new_inmo$garajes) &&
                                as.integer(str_extract(new_inmo$garajes, "\\d")) < as.integer(str_extract(okis$garajes_min, "\\d")), 0, 1)
            anio <- ifelse(okis$anio_max != 0 && (year(now("America/Asuncion")) - new_inmo$anio) > okis$anio_max, 0, 1)
            print("reviso las variables")
            print(paste0("precio ", "m2 ", "m2_cons ", "dormitorios ", "banios ", "pisos ", "garajes ", "anio"))
            print(paste(precio, m2, m2_cons, dormitorios, banios, pisos, garajes, anio))
            coincide <- precio*m2*m2_cons*dormitorios*banios*pisos*garajes*anio
          }else{
            coincide <- 0
          }
          if(coincide == 1){
            
            print("----------HABEMUS----------")
            
            if(okis$company == "skyone"){
              print("busqueda de SkyOne")
              id <- new_id_myplace_mysql("myplace_coincidencias", "skyone", "coincid", 12)
              new_coincidencia <- data.frame(
                id = id, 
                company = "skyone",
                inmueble = new_inmo$id, 
                busqueda = okis$id, 
                user_inmo = new_inmo$user,
                user_busq = okis$user,
                fecha = as.character(now("America/Asuncion")),
                estado = "no revisado", 
                comment = NA,
                activo = 'si'
              )
              save_mysql(new_coincidencia, "myplace_coincidencias", FALSE)
              print("coincidencia guardada")
              print(new_coincidencia)
            }else{
              print("busqueda de MyPlace")
              id <- new_id_myplace_mysql("myplace_coincidencias", "place", "coincid", 12)
              
              new_coincidencia <- data.frame(
                id = id,
                company = "place",
                inmueble = new_inmo$id, 
                busqueda = okis$id, 
                user_inmo = new_inmo$user,
                user_busq = okis$user,
                fecha = as.character(now("America/Asuncion")),
                estado = "no revisado", 
                comment = NA
              )
              save_mysql(new_coincidencia, "myplace_coincidencias", FALSE)
              print("coincidencia guardada")
              print(new_coincidencia)
            }
            
            id_coinci <- c(id_coinci, id)
            
            if(inmo_search){  #se busca un inmo de skyone en busquedas de skyone y myplace
              print("Se busca un inmo en las busquedas")
              agente <- list_table_var_mysql("myplace_usuarios", "user", new_inmo$user)
              
              link_coinci <- tryCatch({
                file_name <- paste0("skyone/inmo-web/", new_inmo$id, ".html")
                file <- paste0("https://place-storage.nyc3.digitaloceanspaces.com/", file_name)
                link <- list_spaces_do(prefix = file_name)
                new_inmo$link <- file
                print("En digitalocean")
                print(link)
                
                if(nrow(link) == 0){
                  
                  if(new_inmo$tipoPropiedad %in% c("Proyecto", "Edificio")){
                    tipologias <- list_table_var_mysql(paste0(co, "_tipologias"), "id", new_inmo$id)
                  }else{
                    tipologias <- NA
                  }
                  
                  params <- list(inmo = new_inmo, agente = agente, tipologias = tipologias, info = TRUE)
                  #save(params, file = "params.rda")
                  
                  tempReport <- file.path(dir, paste0(session$token, "report_inmo_html.Rmd"))
                  tempHTML <- file.path(dir, paste0(session$token, "temp_inmo_web.html"))
                  file.copy("report_inmo_html.Rmd", tempReport, overwrite = TRUE)
                  
                  rmarkdown::render(tempReport, 
                                    output_file = tempHTML,
                                    params = params,
                                    envir = new.env(parent = globalenv()))
                  
                  put_object_do(paste0("/", file_name), tempHTML)
                  
                  update_var_table_mysql(file, "link", "id", new_inmo$id, "myplace_inmuebles")
                }
                "listo"},
                error = function(e){NA}
              )
              print("link coinci error")
              print(link_coinci)
              
              if(okis$company == "skyone"){   #la busqueda es de skyone
                
                mail_coinci <- tryCatch({
                  email <- compose_email(
                    body = md(c(
                      "Hemos encontrado una coincidencia para ti.<br><br>",
                      
                      "<b>", new_inmo$tipoDato, ": ", new_inmo$venta_alquiler, " de ", new_inmo$tipoPropiedad, "</b>",
                      if(!is.na(new_inmo$titulo)) paste0("<br><b>Descripción = </b>", new_inmo$titulo),
                      if(new_inmo$m2 != 0) paste0("<br><b>Area terreno = </b>", format(new_inmo$m2, big.mark=".", decimal.mark = ","), " m2"),
                      if(new_inmo$m2_cons != 0) paste0("<br><b>Area construida = </b>", format(new_inmo$m2_cons, big.mark=".", decimal.mark = ","), " m2"),
                      if(!is.na(new_inmo$estado)) paste0("<br><b>Estado = </b>", new_inmo$estado),
                      if(!is.na(new_inmo$dormitorios)) paste0("<br><b>Dormitorios = </b>", new_inmo$dormitorios),
                      if(!is.na(new_inmo$banios)) paste0("<br><b>Baños = </b>", new_inmo$banios),
                      if(!is.na(new_inmo$garajes)) paste0("<br><b>Cocheras = </b>", new_inmo$garajes),
                      if(new_inmo$pisos != 0) paste0("<br><b>Nro de pisos = </b>", new_inmo$pisos),
                      if(new_inmo$anio != 0) paste0("<br><b>Año de construcción = </b>", new_inmo$anio),
                      "<br><b>Precio = </b>", ifelse(new_inmo$moneda_contrato == "USD", "$ ", "Gs "), format(as.integer(new_inmo$precio), big.mark=".", decimal.mark = ","),
                      if(new_inmo$precio_cierre != 0) paste0("<br><b>Precio de cierre = </b>$",
                                                             format(as.integer(round(new_inmo$precio_cierre)), big.mark=".", decimal.mark = ",")),
                      if(new_inmo$precio_m2 != 0) paste0("<br><b>Precio por m2 = </b>", ifelse(new_inmo$moneda_contrato == "USD", "$ ", "Gs "),
                                                         format(as.integer(round(new_inmo$precio_m2)), big.mark=".", decimal.mark = ",")),
                      if(!is.na(new_inmo$img)) paste0("<br><br><b>Para más detalles de la propiedad: </b>", new_inmo$link),
                      "<br><br><b>Datos del agente: </b>",
                      "<br><b>Nombre: </b>", agente$name,
                      "<br><b>Empresa: </b>", agente$empresa,
                      "<br><b>Correo electrónico: </b>", agente$user,
                      "<br><b>Teléfono: </b>", agente$phone
                    ))
                  )
                  email %>%
                    smtp_send(
                      from = "skyone.realestatelatam@gmail.com",
                      to = okis$user,
                      subject = "Nuevo match busqueda inteligente",
                      credentials = creds_envvar(user = "skyone.realestatelatam@gmail.com",
                                                 pass_envvar = "MAIL_PASS",
                                                 provider = "gmail",
                                                 use_ssl = TRUE)
                    )
                  "listo"},
                  error = function(e){NA}
                )
                print("mail coinci error")
                print(mail_coinci)
                
                
                destino <- list_table_var1_var2_mysql("myplace_usuarios", "company", co, "user", okis$user)
                phone <- str_remove(destino$phone, "\\+")
                
                what_coinci <- tryCatch({
                  resul <- POST(url = "https://graph.facebook.com/v13.0/100461746150331/messages",
                                add_headers(.headers = c("Content-Type"="application/json", "Authorization"="Bearer EAAL4VE6rm3IBACclVKbpaA9WL24AwDE9fZB7UEZAfd9Qx0w0hzdGLXZCk1wYBT9Ru5ln9KAqNG5d3RF5ATEHgHsa1Q0xCBgBev9rFSP82IvyW0ZCRnm4npKP0Vo3E7j2ClHcwNbjCZAZAanIlFZAEWVy7EzE9C2no6zF7COZBMLNO9M2S2LGwCUw71ZAN7pTal8BSOjXcDLdiDgZDZD")),
                                encode = 'raw',
                                body = paste0('{
                                              "messaging_product": "whatsapp",
                                              "recipient_type": "individual",
                                              "to": "', phone, '",
                                              "type": "template",
                                              "template": {
                                                "name": "match_busqueda4",
                                                "language": {
                                                  "code": "en_US"
                                                },
                                                "components": [
                                                  {
                                                    "type" : "header",
                                                    "parameters": [
                                                      {
                                                        "type": "image",
                                                        "image": {
                                                          "link": "', new_inmo$img, '"
                                                        }
                                                      }
                                                    ]
                                                  },
                                                  {
                                                    "type" : "body",
                                                    "parameters": [
                                                      {
                                                          "type": "text",
                                                          "text": "', agente$name, '"
                                                      },
                                                      {
                                                          "type": "text",
                                                          "text": "', agente$phone, '"
                                                      },
                                                      {
                                                          "type": "text",
                                                          "text": "', new_inmo$link, '"
                                                      }
                                                    ] 
                                                  }
                                                ]
                                              }
                                            }')
                  )
                  print("status")
                  print(resul)
                  "listo"},
                  error = function(e){NA}
                )
                print("what coinci error")
                print(what_coinci)
                
                new_alarma <- data.frame(
                  id = new_id_table_mysql("id", paste0(co, "_alarmas"), "alarma_", 12),
                  fecha = as.character(as_date(now("America/Asuncion"))),
                  time = as.character(now("America/Asuncion")),
                  user = agente$user,
                  destino = okis$user,
                  id_inmo = new_inmo$id,
                  id_clien = NA,
                  id_estado = NA,
                  id_busq = NA,
                  id_noticia = NA,
                  id_calificacion = NA,
                  tipo = "Match de búsqueda inteligente",
                  visto = "no",
                  img = new_inmo$img,
                  comment = paste0("El asesor ", agente$name, " ha subido ", ifelse(new_inmo$venta_alquiler == "Venta", "una ", "un "), 
                                   str_to_lower(new_inmo$venta_alquiler), " de ",  str_to_lower(new_inmo$tipoPropiedad, " que podría interesarte"))
                )
                save_mysql(new_alarma, paste0(co, "_alarmas"), FALSE)
                
                
              }else{  #la busqueda es de myplace
                
                mail_coinci <- tryCatch({
                  email <- compose_email(
                    body = md(c(
                      "Hemos encontrado una coincidencia para ti.<br><br>",
                      
                      "<b>", new_inmo$tipoDato, ": ", new_inmo$venta_alquiler, " de ", new_inmo$tipoPropiedad, "</b>",
                      if(!is.na(new_inmo$titulo)) paste0("<br><b>Descripción = </b>", new_inmo$titulo),
                      if(new_inmo$m2 != 0) paste0("<br><b>Area terreno = </b>", format(new_inmo$m2, big.mark=".", decimal.mark = ","), " m2"),
                      if(new_inmo$m2_cons != 0) paste0("<br><b>Area construida = </b>", format(new_inmo$m2_cons, big.mark=".", decimal.mark = ","), " m2"),
                      if(!is.na(new_inmo$estado)) paste0("<br><b>Estado = </b>", new_inmo$estado),
                      if(!is.na(new_inmo$dormitorios)) paste0("<br><b>Dormitorios = </b>", new_inmo$dormitorios),
                      if(!is.na(new_inmo$banios)) paste0("<br><b>Baños = </b>", new_inmo$banios),
                      if(!is.na(new_inmo$garajes)) paste0("<br><b>Cocheras = </b>", new_inmo$garajes),
                      if(new_inmo$pisos != 0) paste0("<br><b>Nro de pisos = </b>", new_inmo$pisos),
                      if(new_inmo$anio != 0) paste0("<br><b>Año de construcción = </b>", new_inmo$anio),
                      "<br><b>Precio = </b>", ifelse(new_inmo$moneda_contrato == "USD", "$ ", "Gs "), format(as.integer(new_inmo$precio), big.mark=".", decimal.mark = ","),
                      if(new_inmo$precio_cierre != 0) paste0("<br><b>Precio de cierre = </b>$",
                                                             format(as.integer(round(new_inmo$precio_cierre)), big.mark=".", decimal.mark = ",")),
                      if(new_inmo$precio_m2 != 0) paste0("<br><b>Precio por m2 = </b>", ifelse(new_inmo$moneda_contrato == "USD", "$ ", "Gs "),
                                                         format(as.integer(round(new_inmo$precio_m2)), big.mark=".", decimal.mark = ",")),
                      if(!is.na(new_inmo$img)) paste0("<br><br><b>Para más detalles de la propiedad: </b>", new_inmo$link),
                      "<br><br><b>Datos del agente: </b>",
                      "<br><b>Nombre: </b>", agente$name,
                      "<br><b>Empresa: </b>", agente$empresa,
                      "<br><b>Correo electrónico: </b>", agente$user,
                      "<br><b>Teléfono: </b>", agente$phone
                    ))
                  )
                  email %>%
                    smtp_send(
                      from = "placeanalyzer@gmail.com",
                      to = okis$user,
                      subject = "Nuevo match busqueda inteligente",
                      credentials = creds_envvar(user = "placeanalyzer@gmail.com",
                                                 pass_envvar = "MAIL_PASS_PLACE",
                                                 provider = "gmail",
                                                 use_ssl = TRUE)
                    )
                  "listo"},
                  error = function(e){NA}
                )
                print("mail coinci error")
                print(mail_coinci)
                
                
                new_alarma <- data.frame(
                  id = new_id_table_mysql("id", "place_alarmas", "alarma_", 12),
                  fecha = as.character(as_date(now("America/Asuncion"))),
                  time = as.character(now("America/Asuncion")),
                  user = agente$user,
                  destino = okis$user,
                  id_inmo = new_inmo$id,
                  id_clien = NA,
                  id_estado = NA,
                  id_busq = NA,
                  id_noticia = NA,
                  id_calificacion = NA,
                  tipo = "Match de búsqueda inteligente",
                  visto = "no",
                  img = new_inmo$img,
                  comment = paste0("El asesor ", agente$name, " ha subido ", ifelse(new_inmo$venta_alquiler == "Venta", "una ", "un "), 
                                   str_to_lower(new_inmo$venta_alquiler), " de ",  str_to_lower(new_inmo$tipoPropiedad, " que podría interesarte"))
                )
                save_mysql(new_alarma, "place_alarmas", FALSE)
                
              }
            }
          }
        }
      }
    }
  }
  if(length(id_coinci) == 0){
    id_coinci <- NA
  }
  return(id_coinci)
}

create_tablas <- function(data, vars, corte_fecha, rem_geometry, color){
  data <- data[, vars]
  if(nrow(data) != 0){
    if(rem_geometry) data <- st_drop_geometry(data)
    if("img" %in% vars){
      if(rem_geometry){
        blank <- "https://place-storage.nyc3.digitaloceanspaces.com/fotos-inmo/blank_inmo.jpg"
      }else{
        blank <- "https://place-storage.nyc3.digitaloceanspaces.com/avatar-users/blank_avatar.png"
      }
      data <- data %>% mutate(img = ifelse(is.na(img), 
                                           paste0('<img src= "', blank, '" height="52"></img>'), 
                                           paste0('<img src="', img, '" height="52"></img>')))
    }
    table <- datatable(data,
              options = list(initComplete = JS(paste0("
                                                      function(settings, json) {
                                                      $(this.api().table().header()).css({
                                                      'background-color': '", color, "',
                                                      'color': '#fff'});}")),
                             pageLength = 8, order = list(list(ifelse("fecha" %in% vars, 3, 2),
                                                               ifelse("fecha" %in% vars, 'desc', 'asc'))),
                             dom = 'ftp'), 
              escape = FALSE, selection = "single")
    
    
    if("fecha" %in% vars){
#      table
      table %>% 
        formatStyle('fecha',
          backgroundColor = styleInterval(c(as.character(now("America/Asuncion") - case_when(corte_fecha == "month" ~ months(2), 
                                                                                             corte_fecha == "day" ~ days(2))),
                                            as.character(now("America/Asuncion") - case_when(corte_fecha == "month" ~ months(1), 
                                                                                             corte_fecha == "day" ~ days(1)))), 
                                          c('#ff9696', '#fff996', '#9dff96')))
    }else{
      table
    }
  }else{
    datatable(data)
  }
}

modal_busqueda_description <- function(){
  showModal(modalDialog(
    uiOutput("formulario_busqueda_edit"),
    br(),
    tags$div(id="bttn_modal", 
             actionBttn("close_busqueda", "OK"),
             align = "center"),
    title = "Búsqueda",
    easyClose = FALSE,
    footer = NULL,
    size = "l",
    fade = TRUE
  ))
}

modal_clien_description <- function(){
  showModal(modalDialog(
    uiOutput("formulario_clien_edit"),
    title = "Datos del cliente",
    easyClose = FALSE,
    footer = NULL,
    size = "l",
    fade = TRUE
  ))
}

modal_inmo_description <- function(){
  showModal(tags$div(id="modal_inmo_double", modalDialog(
    uiOutput("show_inmo_ui"),
    br(),
    br(),
    tags$div(id="bttn_modal", 
             actionBttn("close_show_inmo", "OK"),
             align = "center"),
    title = paste0("Detalles de la propiedad"),
    easyClose = FALSE,
    footer = NULL,
    size = "l",
    fade = TRUE
  )))
}

modal_estado_description <- function(){
  showModal(modalDialog(
    uiOutput("formulario_estado_ui"),
    br(),
    tags$div(id="bttn_modal", 
             actionBttn("close_estado", "OK"),
             align = "center"),
    title = paste0("Detalles del estado"),
    easyClose = FALSE,
    footer = NULL,
    size = "l",
    fade = TRUE
  ))
}

modal_trans_description <- function(){
  showModal(modalDialog(
    uiOutput("formulario_trans_ui"),
    title = paste0("Detalles de la transacción"),
    easyClose = FALSE,
    footer = NULL,
    size = "l",
    fade = TRUE
  ))
}

modal_agen_description <- function(){
  showModal(modalDialog(
    uiOutput("formulario_agen_ui"),
    br(),
    tags$div(id="bttn_modal", 
             actionBttn("close_agen_descripcion", "OK"),
             align = "center"),
    title = paste0("Datos del usuario"),
    easyClose = FALSE,
    footer = NULL,
    size = "m",
    fade = TRUE
  ))
}

modal_foto_description <- function(){
  showModal(modalDialog(
    uiOutput("formulario_foto_ui"),
    br(),
    tags$div(id="bttn_modal", 
             actionBttn("close_foto_modal", "Visto", icon = icon("check")),
             align = "center"),
    title = paste0("Place View"),
    easyClose = FALSE,
    footer = NULL,
    size = "m",
    fade = TRUE
  ))
}

modal_clien_new <- function(){
  showModal(modalDialog(
    uiOutput("formulario_clien_new"),
    title = "Datos del cliente",
    easyClose = FALSE,
    footer = NULL,
    size = "m",
    fade = TRUE
  ))
}

modal_busqueda_popup <- function(busqueda){
  showModal(modalDialog(
    fluidPage(style = "padding: 0px",
              fluidRow(style = "padding: 0px",
                       column(6, 
                              h4(busqueda$name),
                              br(),
                              HTML(paste0("<b>", busqueda$venta_alquiler, " de ", busqueda$tipoPropiedad, "</b>",
                                          ifelse(busqueda$precio_min == 0, "", paste0("<br><b>Precio mínimo: </b>$", 
                                                                                  format(as.integer(busqueda$precio_min), big.mark=".", decimal.mark = ","))),
                                          ifelse(busqueda$precio_max == 0, "", paste0("<br><b>Precio máximo: </b>$", 
                                                                                  format(as.integer(busqueda$precio_max), big.mark=".", decimal.mark = ","))),
                                          ifelse(busqueda$precio_m2_min == 0, "", paste0("<br><b>Precio/m2 del terreno mínimo: </b>$", 
                                                                                     format(as.integer(busqueda$precio_m2_min), big.mark=".", decimal.mark = ","))),
                                          ifelse(busqueda$precio_m2_max == 0, "", paste0("<br><b>Precio/m2 del terreno máximo: </b>$", 
                                                                                     format(as.integer(busqueda$precio_m2_max), big.mark=".", decimal.mark = ","))),
                                          ifelse(busqueda$m2_min == 0, "", paste0("<br><b>Area del terreno mínima: </b>", 
                                                                              format(as.integer(busqueda$m2_min), big.mark=".", decimal.mark = ","), " m2")),
                                          ifelse(busqueda$m2_max == 0, "", paste0("<br><b>Area del terreno máxima: </b>", 
                                                                              format(as.integer(busqueda$m2_max), big.mark=".", decimal.mark = ","), " m2")),
                                          ifelse(busqueda$m2_cons_min == 0, "", paste0("<br><b>Area edificada mínima: </b>", busqueda$m2_min, " m2")),
                                          ifelse(busqueda$m2_cons_max == 0, "", paste0("<br><b>Area edificada máxima: </b>", busqueda$m2_max, " m2")),
                                          ifelse(busqueda$dormitorios_min == 0, "", paste0("<br><b>Mínimo de dormitorios: </b>", busqueda$dormitorios_min)),
                                          ifelse(busqueda$banios_min == 0, "", paste0("<br><b>Mínimo de baños: </b>", busqueda$banios_min)),
                                          ifelse(busqueda$pisos_min == 0, "", paste0("<br><b>Mínimo de pisos: </b>", busqueda$pisos_min)),
                                          ifelse(busqueda$garajes_min == 0, "", paste0("<br><b>Mínimo de cocheras: </b>", busqueda$garajes_min)),
                                          ifelse(busqueda$anio_max == 0, "", paste0("<br><b>Antiguedad máxima: </b>", busqueda$anio_max, " años"))
                              ))
                       ),
                       column(6,
                              leafletOutput("map_info_busqueda", height = 200)
                       )
              ),
              br(), br(),
              tags$div(id="bttn_modal", 
                actionBttn("close_modal", "OK"),
                align = "center"),
    ),
    title = "Descripción de la búsqueda",
    easyClose = FALSE,
    footer = NULL,
    size = "m",
    fade = TRUE
  ))
}

popup_busqueda <- function(busqueda, titulo){
  paste0(ifelse(titulo, paste0("<b>", busqueda$venta_alquiler, " de ", busqueda$tipoPropiedad, "</b>"), ""),
         ifelse(busqueda$precio_min == 0, "", paste0("<br><b>Precio mínimo: </b>", ifelse(busqueda$moneda == "USD", "USD ", "Gs "), 
                                            format(busqueda$precio_min, big.mark=".", decimal.mark = ",", scientific = FALSE))),
         ifelse(busqueda$precio_max == 0, "", paste0("<br><b>Precio máximo: </b>", ifelse(busqueda$moneda == "USD", "USD ", "Gs "), 
                                            format(busqueda$precio_max, big.mark=".", decimal.mark = ",", scientific = FALSE))),
         ifelse(busqueda$m2_min == 0, "",
            ifelse(busqueda$tipoPropiedad != "Propiedad rural", 
                   paste0("<br><b>m2 del terreno mínimo: </b>", format(busqueda$m2_min, big.mark=".", decimal.mark = ",", scientific = FALSE), " m2"),
                   paste0("<br><b>Hectareas mínimas: </b>", format(busqueda$m2_min/10000, big.mark=".", decimal.mark = ",", scientific = FALSE), " ha"))),
         ifelse(busqueda$m2_max == 0, "",
                ifelse(busqueda$tipoPropiedad != "Propiedad rural", 
                       paste0("<br><b>m2 del terreno máximo: </b>", format(busqueda$m2_max, big.mark=".", decimal.mark = ",", scientific = FALSE), " m2"),
                       paste0("<br><b>Hectareas máximas: </b>", format(busqueda$m2_max/10000, big.mark=".", decimal.mark = ",", scientific = FALSE), " ha"))),
         ifelse(busqueda$m2_cons_min == 0, "", paste0("<br><b>Area edificada mínima: </b>", busqueda$m2_min, " m2")),
         ifelse(busqueda$m2_cons_max == 0, "", paste0("<br><b>Area edificada máxima: </b>", busqueda$m2_max, " m2")),
         ifelse(is.na(busqueda$dormitorios_min), "", paste0("<br><b>Mínimo de dormitorios: </b>", busqueda$dormitorios_min)),
         ifelse(is.na(busqueda$banios_min), "", paste0("<br><b>Mínimo de baños: </b>", busqueda$banios_min)),
         ifelse(busqueda$pisos_min == 0, "", paste0("<br><b>Mínimo de pisos: </b>", busqueda$pisos_min)),
         ifelse(is.na(busqueda$garajes_min), "", paste0("<br><b>Mínimo de cocheras: </b>", busqueda$garajes_min)),
         ifelse(is.na(busqueda$oficinas_min), "", paste0("<br><b>Mínimo de oficinas: </b>", busqueda$oficinas_min)),
         ifelse(busqueda$anio_max == 0, "", paste0("<br><b>Antiguedad máxima: </b>", busqueda$anio_max, " años"))
  )
}

adapt_button_text <- function(text, width){
  text <- str_split(text, "/")[[1]]
  if(text[length(text)] == ""){
    text <- text[length(text) - 1]
  }else{
    text <- text[length(text)]
  }
  text <- str_trunc(fixed(text), width)
  n <- str_length(fixed(text))
  if(n > width/2){
    text_1 <- str_trunc(fixed(text), round(width/2), ellipsis = "")
    text_2 <- str_remove(fixed(text), text_1)
    text <- paste0(text_1, "-<br>", text_2)
  }
  return(text)
}

generate_pages <- function(data, per_page, max_pages, page_selected){
  total <- nrow(data)
  if(total != 0){
    last_page <- total %% per_page
    n_pages <- (total - last_page)/per_page
    ifelse(last_page != 0, n_pages <- n_pages + 1, last_page <- per_page)
    min <- page_selected - round(max_pages/2)
    dif_min <- ifelse(min < 1, abs(min) + 1, 0)
    max <- page_selected + round(max_pages/2)
    dif_max <- ifelse(max - n_pages > 0, max - n_pages, 0)
    min <- min - dif_max
    max <- max + dif_min
    if(min < 1) min <- 1
    if(max > n_pages) max <- n_pages
    if(n_pages == 1){
      data_per_page <- data[1:last_page,]
    }else{
      if(page_selected == n_pages){
        data_per_page <- data[1:last_page,]
      }else{
        data_per_page <- data[1:per_page,]
      }
    }
    return(list(data = data_per_page, n_pages = n_pages, last_page = last_page, min = min, max = max))
  }else{
    return(list(data = data, n_pages = 0, last_page = 0, min = 0, max = 0))
  }
}

inmo_description_popup <- function(inmo){
  index <- 0
  gal <- num_fotos_inmo(inmo$id)
  showModal(modalDialog(
    fluidPage(style = paste0("border-style: solid; border-width: 2px; border-color: ", main_color, "; 
                       border-radius: 10px; background-color: white; padding: 20px; height: 70vh"),
              fluidRow(
                column(6,
                       div(style = "border-style: solid; border-width: 2px; border-color: #555555; width: 404px; height: 304px;
                           border-radius: 10px 10px 0px 0px",
                           actionButton("nada", NULL, style = paste0('background-color: transparent;
                                                             border: 0px; width:400px; height: 300px; margin: 0px;
                                                             background-image: url("', dataURI(file = gal[1]), '");
                                                             background-size: cover, 400px 300px;
                                                             border-radius: 8px 8px 0px 0px')),
                       div(style = "border-style: solid; border-width: 2px; border-color: #555555; width: 404px; height: 68px;
                                    background-color: #555555; margin-left: -2px; padding: 5px; border-radius: 0px 0px 10px 10px",
                           actionButton("gal1", NULL, style = paste0('border-radius: 5px; background-image: url("', dataURI(file = gal[1 + index]), '");
                                                                      background-size: cover, 72px 54px;
                                                                      width: 72px; height: 54px; z-index: 3;
                                                                      position: absolute; margin-left: 30px;')),
                           actionButton("gal2", NULL, style = paste0('border-radius: 5px; background-image: url("', dataURI(file = gal[2 + index]), '");
                                                                      background-size: cover, 72px 54px;
                                                                      width: 72px; height: 54px; z-index: 3;
                                                                      position: absolute; margin-left: 117px;')),
                           actionButton("gal3", NULL, style = paste0('border-radius: 5px; background-image: url("', dataURI(file = gal[3 + index]), '");
                                                                      background-size: cover, 72px 54px;
                                                                      width: 72px; height: 54px; z-index: 3;
                                                                      position: absolute; margin-left: 204px;')),
                       )
                       )
                ),
                column(6,
                       h2(inmo$titulo)
                       )
              )
    ),
    br(),
    tags$div(id="bttn_modal", 
             actionBttn("close_modal", "OK"),
             align = "center"),
    title = paste0("Detalles de la propiedad"),
    easyClose = TRUE,
    footer = NULL,
    size = "l",
    fade = TRUE
  ))
}

#llamadas <- list_table_var1_var2_mysql(paste0(co, "_llamadas"), "user", "roberto.ozorio@skyone.group", "realizado", "no")
generate_calendar <- function(llamadas){
  cal <- llamadas %>% select(title = accion, body = comment, start = fecha_agendada, end = fecha_agendada)
  cal <- left_join(cal, acciones, by = c("title" = "accion")) %>% rename(calendarId = id_accion)
  cal$category <- "allday"
  cal$calendarId <- ifelse(!is.na(llamadas$id_estado), llamadas$id_estado, llamadas$id_clien)
  cal$bgColor <- main_color
  cal$color <- "white"
  cal$borderColor <- "cyan"
  return(cal)
}

generate_eventos <- function(){
  eventos <- load_mysql(paste0(co, "_eventos"))
  if(nrow(eventos) != 0){
    eventos$start <- as_datetime(paste(eventos$fecha_agendada, eventos$hora_ini), tz = "America/Asuncion")
    eventos$end <- as_datetime(paste(eventos$fecha_agendada, eventos$hora_fin), tz = "America/Asuncion")
    cal <- eventos %>% select(title = titulo, body = comment, start = start, end = end)
    cal$category <- "time"
    cal$calendarId <- eventos$id
    cal$bgColor <- main_color
    cal$color <- "white"
    cal$borderColor <- "cyan"
  }else{
    cal <- data.frame()
  }
  return(cal)
}

inputUserid <- function(inputId, value='') {
  #   print(paste(inputId, "=", value))
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "userid", value=as.character(value), type="text", style="display:none;")
  )
}

inputIp <- function(inputId, value=''){
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "ipaddr", value=as.character(value), type="text", style="display:none;")
  )
}

comentario_mas_de <- function(text, n){
  asesor <- str_split(text, " ha ")[[1]][1] %>% str_remove("El asesor ")
  return(paste0(asesor, " y ", n - 1, ifelse(n - 1 == 1, " asesor", " asesores"), " más han comentado tu publicación"))
}

permisos_menores <- function(user){
  cargos_menores <- cargos_levels[cargos_levels < user$cargo]
  if(str_detect(user$agencia, "agencia")){
    menores_agencia <- list_table_var1_var2_var3_mysql("myplace_usuarios", "cargo", cargos_menores, "agencia", user$agencia, "company", "IN", co) %>% .$user
    menores_master <- c()
    menores_global <- c()
  }else{
    if(str_detect(user$agencia, "franquicia")){
      agencias <- list_table_var_mysql(paste0(co, "_agencias_data"), "master", user$agencia) %>% .$id
      menores_agencia <- list_table_var1_var2_mysql("myplace_usuarios", "company", co, "agencia", agencias) %>% .$user
      menores_master <- list_table_var1_var2_var3_mysql("myplace_usuarios", "cargo", cargos_menores, "agencia", user$agencia, "company", "IN", co) %>% .$user
      menores_global <- c()
    }else{
      if(user$agencia == "Global Master Office"){
        agentes <- list_table_var_mysql("myplace_usuarios", "company", co)
        menores_agencia <- agentes$user[str_detect(agentes$agencia, "agencia")]
        menores_master <- agentes$user[str_detect(agentes$agencia, "franquicia")]
        menores_global <- list_table_var1_var2_var3_mysql("myplace_usuarios", "cargo", cargos_menores, "agencia", "Global Master Office", "company", "IN", co) %>% .$user
      }
    }
  }
  permisos <- c(menores_agencia, menores_master, menores_global)
  return(permisos)
}

permisos_accion <- function(user){
  permisos <- load_mysql(paste0(co, "_permisos"))
  cargo <- case_when(user$cargo == "Global Master Office" ~ permisos$glob_off,
                     user$cargo == "Global Office Manager" ~ permisos$glob_man,
                     user$cargo == "Global Office Assistant" ~ permisos$glob_ass,
                     user$cargo == "Country Master Office" ~ permisos$coun_off,
                     user$cargo == "Country Office Manager" ~ permisos$coun_man,
                     user$cargo == "Country Office Assistant" ~ permisos$coun_ass,
                     user$cargo == "Broker Office" ~ permisos$brok_off,
                     user$cargo == "Broker Manager" ~ permisos$brok_man,
                     user$cargo == "Broker Assistant" ~ permisos$brok_ass,
                     user$cargo == "Asesor" ~ permisos$asesor,
                     )
  permi <- data.frame(accion = permisos$accion, aut = cargo)
  return(permi)
}

permiso <- function(accion, user_men, permisos_accion, permisos_men){
  aut <- FALSE
  if(permisos_accion$aut[permisos_accion$accion == accion] == "si"){
    if(is.na(user_men)){
      aut <- TRUE
    }else{
      if(user_men %in% permisos_men)
        aut <- TRUE
    }
  }
  return(aut)
}

agencia_name <- function(agencia, agencias, franquicias){
  if(agencia == "Global Master Office"){
    agencia_name <- "Global Master Office"
  }else{
    if(agencia %in% agencias$id){
      agencia_name <- agencias$name[agencias$id == agencia]
    }else{
      if(agencia %in% franquicias$id){
        agencia_name <- franquicias$name[franquicias$id == agencia]
      }
    }
  }
  return(agencia_name)
}

#agen <- list_table_var_mysql("myplace_usuarios", "user", "leandro.almirón@skyone.com.py")
#agencia_nam <- "SkyOne Family Gold"
#inmo <- list_table_var_mysql("myplace_inmuebles", "id", "skyone_inmueble_0000010010")
#operacion <- "colocación"
#dir <- tempdir()
#session <- list(token = "123456")

generate_noticia_cierre <- function(agen, agencia_nam, inmo, operacion, dir, session){
  error <- tryCatch({
    im_inmo <- image_read(str_remove(inmo$img, "-small"))
    im_inmo <- image_extent(im_inmo, "600x600", gravity = "North")
    im_back <- image_blank("600", "600", color = "#324471")
    im_inmo2 <- image_composite(im_back, im_inmo)
    im_mask <- image_read("www/cierre_fondo.png")
    im_inmo3 <- image_composite(im_inmo2, im_mask)
    im_logo <- image_read("www/skyone_logo_blanco.png")
    im_logo <- image_scale(im_logo, "150x")
    im_inmo4 <- image_composite(im_inmo3, im_logo, offset = "+420+540")
    text1 <- paste0(str_to_upper(inmo$tipoPropiedad), ifelse(inmo$tipoPropiedad %in% c("Casa", "Casa quinta", "Propiedad rural", "Oficina"), 
                                                             ifelse(inmo$venta_alquiler == "Venta", "  VENDIDA", "  ALQUILADA"),
                                                             ifelse(inmo$venta_alquiler == "Venta", "  VENDIDO", "  ALQUILADO")))
    print(text1)
    im2 <- image_annotate(im_inmo4, text1, color = "white", location = "+30+470", size = 28)
    text2 <- ifelse(inmo$distrito != "" & !is.na(inmo$distrito), paste0(inmo$distrito, ", ", inmo$ciudad), inmo$ciudad)
    print(text2)
    im3 <- image_annotate(im2, text2, color = "white", location = "+30+515", size = 20)
    im4 <- image_annotate(im3, inmo$pais, color = "white", location = "+30+550", size = 20)
    ag <- image_read(agen$img)
    ag <- image_convert(ag, "png")
    ag_mask <- image_read("www/cierre_agen_mask.png")
    ag2 <- image_composite(ag, ag_mask, operator = "CopyOpacity")
    ag3 <- image_scale(ag2, "175")
    im5 <- image_composite(im4, ag3, offset = "+400+50")
    text3 <- str_replace_all(agen$name, " ", "\n")
    print(text3)
    im6 <- image_annotate(im5, text3, color = "#324471", location = "+400+240", size = 26)
    im7 <- image_annotate(im6, agencia_nam, color = "#324471", location = "+400+400", size = 18)
    im <- image_convert(im7, "jpg")
    
    id <- new_id_table_mysql("id", paste0(co, "_noticias"), "noticia_", 12)
    rand <- str_pad(round(runif(1, min=1, max=999)), 3, pad = "0")
    name <- paste0(id, "-01_", rand)
    img = paste0("https://place-storage.nyc3.digitaloceanspaces.com/", co, "/fotos-noticias/", name, "-small.jpg")

    image_write(im, file.path(dir, paste0(session$token, "temp_foto_inmo.jpg")))
    put_object_do(paste0("/", co, "/fotos-noticias/", name, ".jpg"), file.path(dir, paste0(session$token, "temp_foto_inmo.jpg")))
    im_small <- image_scale(im, "300x")
    image_write(im_small, file.path(dir, paste0(session$token, "temp_foto_inmo_small.jpg")))
    put_object_do(paste0("/", co, "/fotos-noticias/", name, "-small.jpg"), file.path(dir, paste0(session$token, "temp_foto_inmo_small.jpg")))
    
    "listo"},
    error = function(e){NA}
  )

  if(!is.na(error)){
    print("operacion")
    print(operacion)
    col_o_cap <- ifelse(operacion == "captación", "al captador ", 
                        ifelse(operacion == "colocación", "al colocador ", ""))
    
#    col_o_cap <- case_when(operacion == "captación" ~ "al captador ",
#                           operacion == "colocación" ~ "al colocador ",
#                           .default = "")
    
    new_noticia <- data.frame(
      id = id,
      user = "info@skyone.group",
      fecha = as.character(now("America/Asuncion")),
      text = paste0("¡Felicidades ", col_o_cap, agen$name, " por un nuevo cierre!"),
      img = img,
      n_img = "1",
      n_comen = "0",
      activo = "si"
    )
    save_mysql(new_noticia, paste0(co, "_noticias"), FALSE)
    return(TRUE)
  }else{
    return(FALSE)
  }

}

verificar_pagados <- function(cuenta_user, cuenta_choosen){
  pagados <- list_table_var_mysql("venpru", "estado", "paid")
  print(pagados)
  if(nrow(pagados) != 0){
    for(i in 1:nrow(pagados)){
      venta <- pagados[i,]
      print(venta)
      
      #      cuenta <- list_table_var_mysql("cuentas2", "cuenta", venta$cuenta)
      #      saldo <- cuenta$saldo
      #      new_saldo <- saldo + as.integer(venta$cant)
      #      update_var_table_mysql(new_saldo, "saldo", "cuenta",venta$cuenta, "cuentas2")
      #      update_var_table_mysql("acreditado", "estado", "id", venta$id, "venpru")
      
      cuenta_prev <- list_table_var_mysql("cuentas2", "cuenta", venta$cuenta)
      
      if(venta$plan == "Usuario Adicional MyPlace"){
        update_var_table_mysql(cuenta_prev$myplace_lim_users + as.integer(venta$cant), "myplace_lim_users", "cuenta", venta$cuenta, "cuentas2")
        update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", venta$cuenta, "cuentas2")
      }else{
        if(venta$plan == "Recarga MyPlace 5 avalúos"){
          update_var_table_mysql(cuenta_prev$myplace_saldo_avaluos + as.integer(venta$cant), "myplace_saldo_avaluos", "cuenta", venta$cuenta, "cuentas2")
          update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", venta$cuenta, "cuentas2")
        }else{
          plan_buy <- planes[planes$plan == venta$plan,]
          
          ####  cuenta activa myplace
          
          #si la venta es de myplace y el usuario no pertenecia a myplace entonces activarla
          if(plan_buy$myplace_plan == "si" & is.na(cuenta_prev$myplace_plan)){
            update_var_table_mysql("si", "myplace_activo", "cuenta", venta$cuenta, "cuentas2")
            update_var_table_mysql(as.character(now("America/Asuncion")), "myplace_date_create", "cuenta", venta$cuenta, "cuentas2")
            update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", venta$cuenta, "cuentas2")
          }
          
          #si la venta es de myplace y el usuario ya pertenecia a myplace entonces activarla
          if(plan_buy$myplace_plan == "si" & !is.na(cuenta_prev$myplace_plan)){
            update_var_table_mysql("si", "myplace_activo", "cuenta", venta$cuenta, "cuentas2")
            update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", venta$cuenta, "cuentas2")
          }
          
          ####  plan myplace, limite de usuarios, fecha de pago
          
          #si la compra es de myplace 
          #entonces actualizo el plan y limite de usuarios
          if(plan_buy$myplace_plan == "si"){
            update_var_table_mysql(plan_buy$plan, "myplace_plan", "cuenta", venta$cuenta, "cuentas2")
            if(plan_buy$lim_users > cuenta_prev$myplace_lim_users){
              update_var_table_mysql(plan_buy$lim_users, "myplace_lim_users", "cuenta", venta$cuenta, "cuentas2")
            }
            update_var_table_mysql(as.character(now("America/Asuncion")), "myplace_fecha_pago", "cuenta", venta$cuenta, "cuentas2")
            update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", venta$cuenta, "cuentas2")
          }
          
          
          ####  actualizar saldos y fechas
          
          if(plan_buy$myplace_plan == "no"){
            print("no pago myplace")
            print(cuenta_prev)
            print(plan_buy)
            update_var_table_mysql(cuenta_prev$saldo + plan_buy$saldo_avaluos, "saldo", "cuenta", venta$cuenta, "cuentas2")
            update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", venta$cuenta, "cuentas2")
            print(paste0("Saldo avaluos: ", cuenta_prev$saldo + plan_buy$saldo_avaluos))
          }else{
            if(is.na(cuenta_prev$myplace_plan)){
              print("pago myplace y es nuevo")
              print("se acreditan los saldos y fecha de vencimiento")
              update_var_table_mysql(plan_buy$myplace_saldo_avaluos, "myplace_saldo_avaluos", "cuenta", venta$cuenta, "cuentas2")
              update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", venta$cuenta, "cuentas2")
              
              if(venta$plan == "Plan MyPlace Basic" & str_squish(venta$monto) == "950000"){
                for(i in 1:11){
                  new_saldo_pend <- data.frame(
                    id = new_id_table_mysql("id", "saldos_pendientes", "saldo_pend_", 12),
                    fecha_pago = as.character(now("America/Asuncion")),
                    cuenta = venta$cuenta,
                    fecha_acreditacion = as.character(now("America/Asuncion") %m+% months(i)),
                    plan = plan_buy$plan,
                    acreditado = "no"
                  )
                  save_mysql(new_saldo_pend, "saldos_pendientes", FALSE)
                }
                update_var_table_mysql(as.character(now("America/Asuncion") %m+% months(12)), "myplace_fecha_vencimiento", "cuenta", venta$cuenta, "cuentas2")
                update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", venta$cuenta, "cuentas2")
              }else{
                update_var_table_mysql(as.character(now("America/Asuncion") %m+% months(1)), "myplace_fecha_vencimiento", "cuenta", venta$cuenta, "cuentas2")
                update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", venta$cuenta, "cuentas2")
              }
            }else{
              if(cuenta_prev$myplace_fecha_vencimiento < as.character(now("America/Asuncion"))){
                print("pago myplace, no es nuevo y pago despues del vencimiento")
                print("se acreditan los saldos y se calcula la nueva fecha de vencimiento")
                update_var_table_mysql(plan_buy$myplace_saldo_avaluos, "myplace_saldo_avaluos", "cuenta", venta$cuenta, "cuentas2")
                update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", venta$cuenta, "cuentas2")
                
                if(venta$plan == "Plan MyPlace Basic" & str_squish(venta$monto) == "950000"){
                  for(i in 1:11){
                    new_saldo_pend <- data.frame(
                      id = new_id_table_mysql("id", "saldos_pendientes", "saldo_pend_", 12),
                      fecha_pago = as.character(now("America/Asuncion")),
                      cuenta = venta$cuenta,
                      fecha_acreditacion = as.character(now("America/Asuncion") %m+% months(i)),
                      plan = plan_buy$plan,
                      acreditado = "no"
                    )
                    save_mysql(new_saldo_pend, "saldos_pendientes", FALSE)
                  }
                  update_var_table_mysql(as.character(now("America/Asuncion") %m+% months(12)), "myplace_fecha_vencimiento", "cuenta", venta$cuenta, "cuentas2")
                  update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", venta$cuenta, "cuentas2")
                }else{
                  update_var_table_mysql(as.character(now("America/Asuncion") %m+% months(1)), "myplace_fecha_vencimiento", "cuenta", venta$cuenta, "cuentas2")
                  update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", venta$cuenta, "cuentas2")
                }
              }else{
                print("pago myplace, no es nuevo y pago en fecha")
                print("se acreditan los saldos y se calcula la nueva fecha de vencimiento")
                
                if(venta$plan == "Plan MyPlace Basic" & str_squish(venta$monto) == "950000"){
                  for(i in 0:11){
                    new_saldo_pend <- data.frame(
                      id = new_id_table_mysql("id", "saldos_pendientes", "saldo_pend_", 12),
                      fecha_pago = as.character(now("America/Asuncion")),
                      cuenta = venta$cuenta,
                      fecha_acreditacion = as.character(as_datetime(cuenta_prev$myplace_fecha_vencimiento) %m+% months(i)),
                      plan = plan_buy$plan,
                      acreditado = "no"
                    )
                    save_mysql(new_saldo_pend, "saldos_pendientes", FALSE)
                  }
                  update_var_table_mysql(as.character(as_datetime(cuenta_prev$myplace_fecha_vencimiento) %m+% months(12)), "myplace_fecha_vencimiento", "cuenta", venta$cuenta, "cuentas2")
                  update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", venta$cuenta, "cuentas2")
                }else{
                  new_saldo_pend <- data.frame(
                    id = new_id_table_mysql("id", "saldos_pendientes", "saldo_pend_", 12),
                    fecha_pago = as.character(now("America/Asuncion")),
                    cuenta = venta$cuenta,
                    fecha_acreditacion = cuenta_prev$myplace_fecha_vencimiento,
                    plan = plan_buy$plan,
                    acreditado = "no"
                  )
                  save_mysql(new_saldo_pend, "saldos_pendientes", FALSE)
                  
                  update_var_table_mysql(as.character(as_datetime(cuenta_prev$myplace_fecha_vencimiento) %m+% months(1)), "myplace_fecha_vencimiento", "cuenta", venta$cuenta, "cuentas2")
                  update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", venta$cuenta, "cuentas2")
                }
              }
            }
          }
        }
      }
      update_var_table_mysql("acreditado", "estado", "id", venta$id, "venpru")
    }
  }
  
  choosen <- list_table_var_mysql("cuentas2", "cuenta", cuenta_choosen)
  user <- list_table_var_mysql("cuentas2", "cuenta", cuenta_user)
  
  #controla si la cuenta sigue activa
  if(!is.na(choosen$myplace_plan) & choosen$myplace_activo == "si" & choosen$myplace_fecha_vencimiento < as.character(now("America/Asuncion"))){
    update_var_table_mysql("no", "myplace_activo", "cuenta", choosen$cuenta, "cuentas2")
    update_var_table_mysql(0, "myplace_saldo_avaluos", "cuenta", choosen$cuenta, "cuentas2")
    update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", choosen$cuenta, "cuentas2")
    choosen$myplace_activo <- "no"
    choosen$myplace_saldo_avaluos <- 0
  } 
  
  #controla si hay que acreditarle saldo pendiente
  choosen_pend <- list_table_var1_var2_mysql("saldos_pendientes", "cuenta", choosen$cuenta, "acreditado", "no")
  if(nrow(choosen_pend) != 0){
    choosen_pend <- choosen_pend[choosen_pend$fecha_acreditacion == min(choosen_pend$fecha_acreditacion), ]
  }else{
    if(nrow(choosen_pend) != 0 && choosen_pend$fecha_acreditacion <= as.character(now("America/Asuncion"))){
      plan_buy <- planes[planes$plan == choosen_pend$plan,]
      update_var_table_mysql(plan_buy$myplace_saldo_avaluos, "myplace_saldo_avaluos", "cuenta", choosen$cuenta, "cuentas2")
      update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", choosen$cuenta, "cuentas2")
      update_var_table_mysql("si", "acreditado", "id", choosen_pend$id, "saldos_pendientes")
      choosen$myplace_saldo_avaluos <- plan_buy$myplace_saldo_avaluos
    }
  }
  
  if(cuenta_choosen != cuenta_user){
    if(!is.na(user$myplace_plan) & user$myplace_activo == "si" & user$myplace_fecha_vencimiento < as.character(now("America/Asuncion"))){
      update_var_table_mysql("no", "myplace_activo", "cuenta", user$cuenta, "cuentas2")
      update_var_table_mysql(0, "myplace_saldo_avaluos", "cuenta", user$cuenta, "cuentas2")
      update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", user$cuenta, "cuentas2")
      user$myplace_activo <- "no"
      user$myplace_saldo_avaluos <- 0
    }
    
    user_pend <- list_table_var1_var2_mysql("saldos_pendientes", "cuenta", user$cuenta, "acreditado", "no")
    user_pend <- user_pend[user_pend$fecha_acreditacion == min(user_pend$fecha_acreditacion), ]
    if(nrow(user_pend) != 0 && user_pend$fecha_acreditacion <= as.character(now("America/Asuncion"))){
      plan_buy <- planes[planes$plan == user_pend$plan,]
      update_var_table_mysql(plan_buy$myplace_saldo_avaluos, "myplace_saldo_avaluos", "cuenta", user$cuenta, "cuentas2")
      update_var_table_mysql(as.character(now("America/Asuncion")), "date", "cuenta", user$cuenta, "cuentas2")
      update_var_table_mysql("si", "acreditado", "id", user_pend$id, "saldos_pendientes")
      user$myplace_saldo_avaluos <- plan_buy$myplace_saldo_avaluos
    }
  }
  
  return(list(user = user, choosen = choosen))
}

clean_text <- function(text){
  text <- str_replace_all(text, "á", "a")
  text <- str_replace_all(text, "â", "a")
  text <- str_replace_all(text, "à", "a")
  text <- str_replace_all(text, "ä", "a")
  text <- str_replace_all(text, "Á", "A")
  text <- str_replace_all(text, "é", "e")
  text <- str_replace_all(text, "ê", "e")
  text <- str_replace_all(text, "è", "e")
  text <- str_replace_all(text, "É", "E")
  text <- str_replace_all(text, "í", "i")
  text <- str_replace_all(text, "î", "i")
  text <- str_replace_all(text, "ì", "i")
  text <- str_replace_all(text, "Í", "I")
  text <- str_replace_all(text, "ó", "o")
  text <- str_replace_all(text, "ô", "o")
  text <- str_replace_all(text, "ò", "o")
  text <- str_replace_all(text, "ö", "o")
  text <- str_replace_all(text, "õ", "o")
  text <- str_replace_all(text, "Ó", "O")
  text <- str_replace_all(text, "Ö", "O")
  text <- str_replace_all(text, "Õ", "O")
  text <- str_replace_all(text, "ú", "u")
  text <- str_replace_all(text, "û", "u")
  text <- str_replace_all(text, "ù", "u")
  text <- str_replace_all(text, "ü", "u")
  text <- str_replace_all(text, "Ú", "U")
  text <- str_replace_all(text, "Ü", "U")
  return(text)
}

xintel_delfi_to_place <- function(web){
  scrap_error <- tryCatch({
    #web <- "https://chasepropiedades.com/apartamento-en-venta-en-villa-aurelia-ficha-dbr1311"
    #web <- "https://chasepropiedades.com/terreno-en-venta-en-mbokayaty-del-norte-ficha-dbr783"
    #web <- "https://chasepropiedades.com/casa-en-venta-en-colonia-thompson-ficha-dbr1452"
    web_html <- read_html(web)
    text <- html_text(web_html)
    
    scr1 <- str_split(text, fixed("https://xintel.com.ar/api/',\n\tcache:false"))[[1]][2]
    scr <- str_split(scr1, fixed("},\n\tsuccess:function"))[[1]][1]
    scr <- str_remove_all(scr, fixed("\n\t")) %>% str_remove_all(fixed("'"))
    
    cache1 <- str_split(scr, fixed("data:{cache:"))[[1]][2]
    cache <- str_split(cache1, fixed(","))[[1]][1]
    
    global1 <- str_split(scr, fixed(",global:"))[[1]][2]
    global <- str_split(global1, fixed(","))[[1]][1]
    
    apiK1 <- str_split(scr, fixed(",apiK:"))[[1]][2]
    apiK <- str_split(apiK1, fixed(","))[[1]][1]
    
    id1 <- str_split(scr, fixed(",id:"))[[1]][2]
    id <- str_split(id1, fixed(","))[[1]][1]
    
    ficha_url <- paste0("https://xintel.com.ar/api/?cache=", cache, 
                        "&json=fichas.propiedades&amaira=false&suc=DBR&global=", global, 
                        "&emprendimiento=True&oppel=&esweb=&apiK=", apiK,
                        "&id=", id)
    resp <- GET(ficha_url)
    fic <- content(resp)
    
    fotos <- c()
    for(i in 1:ifelse(length(fic$resultado$img) > 15, 15, length(fic$resultado$img))){
      fotos <- c(fotos, fic$resultado$img[[i]])
    }
    
    lat <- fic$resultado$ficha[[1]]$latitud %>% as.numeric()
    lat <- ifelse(is.na(lat), 0, lat)
    lon <- fic$resultado$ficha[[1]]$longitud %>% as.numeric()
    lon <- ifelse(is.na(lon), 0, lon)
    descripcion <- fic$resultado$ficha[[1]]$in_obs %>% HTMLdecode() %>% str_remove_all(fixed("??"))
    encabezado <- fic$resultado$ficha[[1]]$titulo %>% HTMLdecode()
    venta_alquiler <- ifelse(str_detect(fic$resultado$ficha[[1]]$operacion, "Venta y"), "Venta", fic$resultado$ficha[[1]]$operacion)
    moneda_contrato <- ifelse(fic$resultado$ficha[[1]]$moneda == "U$S", "USD", "GS")
    precio <- str_split(fic$resultado$ficha[[1]]$precio, " ")[[1]][2] %>%
      str_remove_all(fixed(".")) %>% str_replace(",", ".") %>% as.numeric()
    
    
    if(!is.na(precio)){
      tipoPropiedad <- fic$resultado$ficha[[1]]$in_tip
      
      tipoPropiedad <- case_when(tipoPropiedad %in% c("Apartamento") ~ "Departamento",
                                 tipoPropiedad %in% c("Terreno") ~ "Terreno",
                                 tipoPropiedad == "Casa" ~ "Casa",
                                 tipoPropiedad %in% c("Quinta") ~ "Casa quinta",
                                 tipoPropiedad %in% c("Depósito", "Nave Industrial", "Galpon", "Galpón") ~ "Deposito",
                                 tipoPropiedad %in% c("Campo", "Finca", "Chacra") ~ "Propiedad rural",
                                 tipoPropiedad %in% c("Oficina") ~ "Oficina",
                                 tipoPropiedad %in% c("Edificio Comercial", "Edificio") ~ "Edificio",
                                 tipoPropiedad %in% c("Local", "Hotel") ~ "Salón comercial")
      
      dormitorios <- fic$resultado$ficha[[1]]$ti_dor
      if(dormitorios != "0"){
        if(as.numeric(dormitorios) >= 5)
          dormitorios <- "5+"
      }else{
        dormitorios <- NA
      }
      
      banios <- fic$resultado$ficha[[1]]$in_bao
      if(banios != ""){
        if(as.numeric(banios) >= 5)
          banios <- "5+"
      }else{
        banios <- NA
      }
      
      garajes <- fic$resultado$ficha[[1]]$garage
      if(garajes != "0"){
        if(as.numeric(garajes) >= 5)
          garajes <- "5+"
      }else{
        garajes <- NA
      }
      
      pisos <- fic$resultado$ficha[[1]]$in_npi
      if(pisos != ""){
        pisos <- pisos %>% as.numeric()
      }else{
        pisos <- 0
      }
      
      anio <- fic$resultado$ficha[[1]]$in_anio
      if(anio != "0"){
        anio <- anio %>% as.numeric()
      }else{
        anio <- 0
      }
      
      bauleras <- fic$resultado$ficha[[1]]$in_bau
      if(bauleras != ""){
        if(as.numeric(bauleras) >= 5)
          bauleras <- "5+"
      }else{
        bauleras <- NA
      }
      
      ascensor <- fic$resultado$ficha[[1]]$in_asc
      if(ascensor != ""){
        if(as.numeric(ascensor) >= 5)
          ascensor <- "5+"
      }else{
        ascensor <- NA
      }
      
      m2 <- fic$resultado$superficie$dato[[3]] %>% str_remove("m2") %>% as.numeric()
      m2_cons <- fic$resultado$superficie$dato[[1]] %>% str_remove("m2") %>% as.numeric()
      tot <- fic$resultado$superficie$dato[[4]] %>% str_remove("m2") %>% as.numeric()
      if(m2 == 0 & tot != 0 & tipoPropiedad %in% c("Terreno", "Propiedad rural"))
        m2 <- tot
      if(tipoPropiedad == "Propiedad rural"){
        m2 <- m2*10000
      }
      if(m2_cons == 0 & tot != 0 & tipoPropiedad == "Departamento")
        m2_cons <- tot
      
      des <- clean_text(str_to_lower(descripcion))
      
      if(tipoPropiedad == "Departamento"){
        balcon <- ifelse(str_detect(des, "balcon"), "si", "no")
      }else{
        balcon <- NA
      }
      if(tipoPropiedad == "Departamento"){
        parrilla <- ifelse(str_detect(des, "parrilla"), "si", "no")
      }else{
        parrilla <- NA
      }
      if(tipoPropiedad %in% c("Casa", "Duplex", "Casa quinta")){
        quincho <- ifelse(str_detect(des, "quincho"), "si", "no")
      }else{
        quincho <- NA
      }
      if(tipoPropiedad %in% c("Casa", "Duplex", "Departamento", "Proyecto", "Casa quinta", "Edificio")){
        piscina <- ifelse(str_detect(des, "piscina"), "si", "no")
      }else{
        piscina <- NA
      }
      if(tipoPropiedad %in% c("Casa", "Duplex", "Casa quinta")){
        tipo_cocina <- ifelse(str_detect(des, "cocina integrada"), "Integrada", "Independiente")
      }else{
        tipo_cocina <- NA
      }
      if(tipoPropiedad %in% c("Casa", "Duplex", "Casa quinta")){
        deposito <- ifelse(str_detect(des, "deposito"), "si", "no")
      }else{
        deposito <- NA
      }
      if(tipoPropiedad %in% c("Departamento", "Proyecto", "Edificio", "Casa", "Duplex", "Casa quinta")){
        gimnasio <- ifelse(str_detect(des, "gimnasio") | str_detect(des, "gym"), "si", "no")
      }else{
        gimnasio <- NA
      }
      if(tipoPropiedad %in% c("Departamento", "Casa", "Duplex", "Casa quinta")){
        area_servicio <- ifelse(str_detect(des, "area de servicio"), "si", "no")
      }else{
        area_servicio <- NA
      }
      if(tipoPropiedad %in% c("Departamento", "Casa", "Duplex", "Casa quinta")){
        cocina_equipada <- ifelse(str_detect(des, "cocina equipada") | str_detect(des, "cocina amoblada"), "si", "no")
      }else{
        cocina_equipada <- NA
      }
      if(tipoPropiedad %in% c("Oficina", "Salón comercial")){
        cocina_propia <- ifelse(str_detect(des, "cocina") | str_detect(des, "kitchenette"), "si", "no")
      }else{
        cocina_propia <- NA
      }
      if(tipoPropiedad %in% c("Casa", "Duplex", "Departamento", "Proyecto", "Edificio", "Oficina", "Deposito", "Tinglado", "Salón comercial")){
        generador <- ifelse(str_detect(des, "generador"), "si", "no")
      }else{
        generador <- NA
      }
      if(tipoPropiedad %in% c("Departamento", "Proyecto")){
        pet_friendly <- ifelse(str_detect(des, "pet friendly"), "si", "no")
      }else{
        pet_friendly <- NA
      }
      if(tipoPropiedad %in% c("Casa", "Casa quinta", "Duplex", "Departamento", "Proyecto", "Terreno", "Edificio", "Deposito", "Tinglado", "Oficina", "Salón comercial")){
        esquina <- ifelse(str_detect(des, "esquina"), "si", "no")
      }else{
        esquina <- NA
      }
      
      #point <- data.frame(lat = lat, lon = lon) %>% st_as_sf(coords = c("lon", "lat"), crs = crs)
      #way <- reverse_geocode(point)
      #pais <- way$country
      #departamento <- way$county
      #ciudad <- way$city
      #distrito <- way$district
      #calle <- way$street
      
      pais <- fic$resultado$ficha[[1]]$in_pai %>% HTMLdecode()
      ciudad <- fic$resultado$ficha[[1]]$in_loc %>% HTMLdecode()
      distrito <- fic$resultado$ficha[[1]]$in_bar %>% HTMLdecode()
      calle <- fic$resultado$ficha[[1]]$in_cal %>% HTMLdecode()
      departamento <- NA
      
      new_inmo <- data.frame(
        id = NA, 
        fecha = as.character(now("America/Asuncion")), 
        fecha_creacion = as.character(now("America/Asuncion")), 
        cliente = NA, 
        referente = NA, 
        comment = HTML(paste0("ID: DBR", fic$resultado$ficha[[1]]$in_fic,
                              "  -  Cliente: ", fic$resultado$ficha[[1]]$in_ape,
                              "  -  Tel: ", fic$resultado$ficha[[1]]$in_tel)), 
        pais = pais, 
        departamento = departamento, 
        ciudad = ciudad,
        distrito = distrito, 
        calle = calle, 
        nro_casa = NA, 
        catastro = NA, 
        referencia = NA,
        venta_alquiler = venta_alquiler, 
        tipo_contrato = "Sin contrato", 
        tipoPropiedad = tipoPropiedad,
        user = NA, 
        comision = ifelse(venta_alquiler == "Venta", 5, 50), 
        estado_juridico = "Libre de gravamen", 
        moneda_contrato = moneda_contrato,
        precio = precio, 
        ini_contrato = NA, 
        fin_contrato = NA, 
        img = NA, 
        link = web,
        titulo = encabezado,
        descripcion = descripcion,
        publicado = "si", 
        destacado = "no",
        borrador = "si", 
        m2 = ifelse(tipoPropiedad %in% c("Terreno", "Casa", "Duplex", "Deposito", "Tinglado", "Edificio", "Casa quinta", "Propiedad rural"), m2, 0), 
        m2_cons = ifelse(tipoPropiedad %in% c("Casa", "Duplex", "Departamento", "Deposito", "Tinglado", "Edificio", "Casa quinta", "Oficina", "Salón comercial"), m2_cons, 0),  
        dormitorios = dormitorios, 
        banios = banios,
        garajes = garajes,
        tipo_cocina = tipo_cocina,
        cocina_equipada = cocina_equipada,
        quincho = quincho,
        area_servicio = area_servicio,
        piscina = piscina,
        deposito = deposito,
        generador = generador,
        frente = 0,
        fondo = 0,
        pisos = pisos,
        bauleras = bauleras,
        ascensor = ascensor,
        balcon = balcon,
        parrilla = parrilla,
        gimnasio = gimnasio,
        pet_friendly = pet_friendly,
        condominio = NA,
        anio = anio,
        estado = NA,
        banios_compartidos = NA,
        esquina = NA,
        oficinas = NA,
        cocina_propia = cocina_propia,
        trifacico = NA,
        fecha_cierre = NA,
        precio_cierre = 0,
        precio_m2 = 0,
        externa = "no",
        tipoDato = "Oferta",
        activo = "si",
        lat = lat,
        lon = lon
      )
      "listo"
    }else{
      "error"
    }
  },
  error = function(e){NA}
  )
  print(scrap_error)
  if(is.na(scrap_error) | scrap_error == "error"){
    return(list(inmo = data.frame(), fotos = c(), error = TRUE))
  }else{
    return(list(inmo = new_inmo, fotos = fotos, error = FALSE))
  }
}

extract_serie_galeria <- function(file){
  str_extract(file, "inmo\\d\\d") %>% str_remove("inmo") %>% as.integer()
}


#agen <- list_table_var_mysql("myplace_usuarios", "user", "gmayeregger@gmail.com")
#agencia_nam <- "Global Master Office"

generate_cumple_agen <- function(agen, agencia_nam, dir, session){
  error <- tryCatch({
  
    is_team <- str_detect(str_to_lower(agen$name), "team ")
    
    im_agen <- image_read(str_remove(agen$img, "-small"))
    im_agen <- image_convert(im_agen, "png")
    im_back <- image_read(ifelse(is_team, "www/fondo_aniversario.jpg", "www/fondo_cumple.jpg"))
    
    circulo <- image_read("www/circulo_250.png")
    circulo <- image_scale(circulo, "500x")
    im_agen <- image_composite(im_agen, circulo, operator='copyopacity')
    im_agen2 <- image_crop(im_agen, "+165+0")
    
    im_full <- image_composite(im_back, im_agen2, offset = "+0+85")

    im_logo <- image_read("www/skyone_logo_blanco.png")
    im_logo <- image_scale(im_logo, "150x")
    im_full <- image_composite(im_full, im_logo, offset = "+420+540")

    text <- str_replace_all(agen$name, " ", "\n")
    im_full <- image_annotate(im_full, text, color = "#ffffff", location = "+400+300", size = 26)
    im_full <- image_annotate(im_full, agencia_nam, color = "#ffffff", location = "+400+400", size = 18)
    im_full <- image_convert(im_full, "jpg")
    
    id <- new_id_table_mysql("id", paste0(co, "_noticias"), "noticia_", 12)
    rand <- str_pad(round(runif(1, min=1, max=999)), 3, pad = "0")
    name <- paste0(id, "-01_", rand)
    img = paste0("https://place-storage.nyc3.digitaloceanspaces.com/", co, "/fotos-noticias/", name, "-small.jpg")
    
    image_write(im_full, file.path(dir, paste0(session$token, "temp_foto_inmo.jpg")))
    put_object_do(paste0("/", co, "/fotos-noticias/", name, ".jpg"), file.path(dir, paste0(session$token, "temp_foto_inmo.jpg")))
    im_small <- image_scale(im_full, "300x")
    image_write(im_small, file.path(dir, paste0(session$token, "temp_foto_inmo_small.jpg")))
    put_object_do(paste0("/", co, "/fotos-noticias/", name, "-small.jpg"), file.path(dir, paste0(session$token, "temp_foto_inmo_small.jpg")))
    
    "listo"},
    error = function(e){NA}
  )
  
  if(!is.na(error)){

    new_noticia <- data.frame(
      id = id,
      user = "info@skyone.group",
      fecha = as.character(now("America/Asuncion")),
      text = paste0(ifelse(is_team, "¡Feliz aniversario ", "¡Feliz cumpleaños "), agen$name, "! 🥳🎂"),
      img = img,
      n_img = "1",
      n_comen = "0",
      activo = "si"
    )
    save_mysql(new_noticia, paste0(co, "_noticias"), FALSE)
    return(TRUE)
  }else{
    return(FALSE)
  }
  
}
#generate_cumple_agen(agen, agencia_nam, dir, session)


#tipo <- "asesor"
#oficina <- c("SkyOne Empower", "SkyOne Central", "SkyOne Home")
#user_name <- c("Deisy Duarte", "Claudia Fretes", "Kathia Villasanti")
#user_img <- c("https://place-storage.nyc3.digitaloceanspaces.com/avatar-users/deisy.duarte@skyone.group-93-small.jpg",
#              "https://place-storage.nyc3.digitaloceanspaces.com/avatar-users/deisy.duarte@skyone.group-93-small.jpg",
#              "https://place-storage.nyc3.digitaloceanspaces.com/avatar-users/deisy.duarte@skyone.group-93-small.jpg")
#titulo <- "Top Gross Commission"
#value <- c(40, 10, 5)
#session <- list(token = "sdfsdfsdf")
#user <- "gmayeregger@gmail.com"
#name <- "topGross"

create_podio_est <- function(tipo, name, titulo, user, user_name, user_img, oficina, value, dir, session){
  

  ind <- which(value > 0)
  if(length(ind) > 3)
    ind <- ind[1:3]
  
  if(length(ind) > 0){
    
    error <- tryCatch({

      im_back <- image_read("www/podio.png")
      
      #titulo
      im_back <- image_annotate(im_back, titulo, gravity = "center", color = "#ffffff", location = "+0+340", size = 70)
      
      for(i in ind){
        if(tipo == "oficina"){
          
          x_center <- ifelse(i == 1, "+0", ifelse(i == 2, "-310", "+320"))
          y_offset <- ifelse(i == 1, 0, ifelse(i == 2, 40, 80))
          
          
          fon <- image_draw(image_blank(280, 320))
          
          fon <- image_annotate(fon, paste0(" ", str_remove(oficina[i], "SkyOne "), " "), gravity = "center", color = "#ffffff", 
                                location = "+0-130", size = 40, weight = 700, font = "Comic Sans",
                                boxcolor = "#012d5c")
          im_agen <- image_read(user_img[i])
          im_agen <- image_convert(im_agen, "png")
          circulo <- image_read("www/circulo_250.png")
          im_agen <- image_composite(im_agen, circulo, operator='copyopacity')
          im_agen <- image_resize(im_agen, "150x")
          fon <- image_composite(fon, im_agen, gravity = "center", operator = "Over", offset = "+0+0")
          fon <- image_annotate(fon, user_name[i], gravity = "center", color = "#000000", location = "+0+115", size = 28)
          fon <- image_annotate(fon, "Broker Manager", gravity = "center", color = "#000000", location = "+0+150", size = 19)
          
          im_back <- image_composite(im_back, fon, gravity = "center", offset = paste0(x_center, as.character(-150+y_offset)))
          
        }else{
          x_center <- ifelse(i == 1, "+0", ifelse(i == 2, "-310", "+320"))
          y_offset <- ifelse(i == 1, 0, ifelse(i == 2, 40, 80))
          
          
          fon <- image_draw(image_blank(280, 320))
          
          im_agen <- image_read(user_img[i])
          im_agen <- image_convert(im_agen, "png")
          circulo <- image_read("www/circulo_250.png")
          im_agen <- image_composite(im_agen, circulo, operator='copyopacity')
          im_agen <- image_resize(im_agen, "200x")
          fon <- image_composite(fon, im_agen, gravity = "center", operator = "Over", offset = "+0-50")
          fon <- image_annotate(fon, user_name[i], gravity = "center", color = "#000000", location = "+0+115", size = 28)
          fon <- image_annotate(fon, oficina[i], gravity = "center", color = "#000000", location = "+0+150", size = 19)
          
          im_back <- image_composite(im_back, fon, gravity = "center", offset = paste0(x_center, as.character(-150+y_offset)))
        }
      }
      
      rand <- str_pad(round(runif(1, min=1, max=999)), 3, pad = "0")
      file = paste0("https://place-storage.nyc3.digitaloceanspaces.com/", co, "/temp/", user, "/foto_podio_", name, "_", rand, ".png")
      
      image_write(im_back, file.path(dir, paste0(session$token, "temp_podio_", name,".png")))
      put_object_do(paste0("/", co, "/temp/", user, "/foto_podio_", name, "_", rand, ".png"),
                    file.path(dir, paste0(session$token, "temp_podio_", name, ".png")))
      
      "listo"},
      error = function(e){NA}
    )
    if(is.na(error)){
      return(NA)
    }else{
      return(file)
    }
  }else{
    return(NA)
  }

}
#img_podio <- create_podio_est(tipo, name, titulo, user, user_name, user_img, oficina, value, dir, session)
