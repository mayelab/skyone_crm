co <- "skyone"
##############################################################################################################
#clientes
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  fecha = str_flatten(rep("a", 30)),
  user = str_flatten(rep("a", 60)),
  name = str_flatten(rep("a", 60)),
  phone = str_flatten(rep("a", 30)),
  mail = str_flatten(rep("a", 60)),
  addr = str_flatten(rep("a", 250)),
  company = str_flatten(rep("a", 60)),
  razon = str_flatten(rep("a", 30)),
  ruc = str_flatten(rep("a", 30)),
  img = str_flatten(rep("a", 250)),
  canal = str_flatten(rep("a", 30)),
  comment = str_flatten(rep("a", 250)),
  activo = str_flatten(rep("a", 2)),
  birthday = str_flatten(rep("a", 30)),
  tipo = str_flatten(rep("a", 30))
)
create_table_mysql(paste0(co, "_clientes"), new, "id")
describe_table_mysql(paste0(co, "_clientes"))
#delete_table_mysql(paste0(co, "_clientes"))
#clear_table_mysql(paste0(co, "_clientes"))
dat <- load_mysql(paste0(co, "_clientes"))
#save_mysql(dat, paste0(co, "_clientes"), TRUE)
skyone_clientes <- dat
save(skyone_clientes, file = "back/skyone_clientes.rda")
back <- dat

cli <- dat[3840,]
cli$id <- "client_000000006065"
cli$user <- "teamphoenix@skyone.com.py"
save_mysql(cli, "skyone_clientes", FALSE)
dat$tipo[7213] <- "Cliente"
back <- dat
dat <- dat[-c(1573),]
dat$tipo[9090] <- "Asesor Externo"
dat$user[5262] <- "dorysskyone@gmail.com"

dat$img <- str_replace_all(dat$img, "/skyone/avatar-users", "/avatar-users")

##############################################################################################################
#inmuebles
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  fecha = str_flatten(rep("a", 30)),
  fecha_creacion = str_flatten(rep("a", 30)),
  cliente = str_flatten(rep("a", 30)),
  referente = str_flatten(rep("a", 250)),
  pais = str_flatten(rep("a", 60)),
  departamento = str_flatten(rep("a", 60)),
  ciudad = str_flatten(rep("a", 60)),
  distrito = str_flatten(rep("a", 60)),
  calle = str_flatten(rep("a", 120)),
  nro_casa = str_flatten(rep("a", 30)),
  catastro = str_flatten(rep("a", 30)),
  referencia = str_flatten(rep("a", 250)),
  venta_alquiler = str_flatten(rep("a", 30)),
  tipo_contrato = str_flatten(rep("a", 30)),
  tipoPropiedad = str_flatten(rep("a", 30)),
  user = str_flatten(rep("a", 60)),
  comision = str_flatten(rep("a", 30)),
  estado_juridico = str_flatten(rep("a", 30)),
  moneda_contrato = str_flatten(rep("a", 30)),
  precio = as.numeric(44.555),
  ini_contrato = str_flatten(rep("a", 30)),
  fin_contrato = str_flatten(rep("a", 30)),
  img = str_flatten(rep("a", 250)),
  link = str_flatten(rep("a", 250)),
  titulo = str_flatten(rep("a", 250)),
  descripcion = str_flatten(rep("a", 5000)),
  publicado = str_flatten(rep("a", 30)),
  destacado = str_flatten(rep("a", 1)),
  borrador = str_flatten(rep("a", 1)),
  m2 = as.numeric(44.555),
  m2_cons = as.numeric(44.555),
  dormitorios = str_flatten(rep("a", 30)),
  banios = str_flatten(rep("a", 1)),
  garajes = str_flatten(rep("a", 1)),
  tipo_cocina = str_flatten(rep("a", 30)),
  cocina_equipada = str_flatten(rep("a", 1)),
  quincho = str_flatten(rep("a", 1)),
  area_servicio = str_flatten(rep("a", 1)),
  piscina = str_flatten(rep("a", 1)),
  deposito = str_flatten(rep("a", 1)),
  generador = str_flatten(rep("a", 1)),
  frente = as.numeric(44.555),
  fondo = as.numeric(44.555),
  pisos = as.numeric(44.555),
  bauleras = str_flatten(rep("a", 1)),
  ascensor = str_flatten(rep("a", 1)),
  balcon = str_flatten(rep("a", 1)),
  parrilla = str_flatten(rep("a", 1)),
  gimnasio = str_flatten(rep("a", 1)),
  pet_friendly = str_flatten(rep("a", 1)),
  condominio = str_flatten(rep("a", 1)),
  precio_m2 = as.numeric(44.555),
  anio = as.numeric(44.555),
  fecha_cierre = str_flatten(rep("a", 30)),
  precio_cierre = as.numeric(44.555),
  tipoDato = str_flatten(rep("a", 30)),
  estado = str_flatten(rep("a", 30)),
  esquina = str_flatten(rep("a", 1)),
  banios_compartidos = str_flatten(rep("a", 1)),
  oficinas = str_flatten(rep("a", 1)),
  cocina_propia = str_flatten(rep("a", 1)),
  trifacico = str_flatten(rep("a", 1)),
#  id_del = str_flatten(rep("a", 30)),
#  id_cierre = str_flatten(rep("a", 30)),
#  id_tipo = str_flatten(rep("a", 30)),
  comment = str_flatten(rep("a", 500)),
  externa = str_flatten(rep("a", 1)),
  activo = str_flatten(rep("a", 1)),
  lat = as.numeric(44.555),
  lon = as.numeric(44.555),
  geometry = str_flatten(rep("a", 30))
)
#create_table_mysql(paste0(co, "_inmuebles"), new, "id")
describe_table_mysql(paste0(co, "_inmuebles"))
#delete_table_mysql(paste0(co, "_inmuebles"))
#clear_table_mysql(paste0(co, "_inmuebles"))
dat <- list_inmos_mysql("SELECT * FROM skyone_inmuebles")
#save_mysql(dat, paste0(co, "_inmuebles"), TRUE)

row <- 6226
dat$user[row]
dat$id[row]
dat$tipo_contrato[row]
dat$moneda_contrato[row] <- "GS"
dat$precio[row] <- 315000000
dat$precio_cierre[row] <- 0
dat$fecha_cierre[row] <- NA
dat$borrador[row] <- "no"
dat$venta_alquiler[row]
dat$tipoPropiedad[row]
dat$externa[row]
dat$distrito[row] <- "Surubi-i"
dat$ciudad[row]
dat$banios[row] <- "+5"
dat$garajes[row] <- "+5"
dat$anio[row] <- 20
dat$comision[row]
dat$cliente[row] <- "client_000000006065"
dat$activo[row] <- "si"
dat$fecha_creacion[row]
dat$publicado[row] <- "si"
dat$img[row]
dat$activo[row] <- "no"
dat$cliente[row]

dat[row,]
ind <- which(dat$distrito == "Surubi-Y")
dat$distrito[ind] <- "Surubi-i"

adri <- dat %>% filter(user == "adriana.ayub@skyone.com.py" & borrador == "no" & activo == "si" & tipo_contrato == "Exclusividad")


ros <- dat %>% filter(user == "rosanna.pereira@skyone.com.py")

cli <- dat

dat$img[1]


which(ros$cliente %in% cli$id)


dat$precio[3237] <- 950000
dat$borrador[2008] <- "si"
dat$precio_cierre[1587] <- 4500
dat$precio[1587]
dat$fecha_cierre[1587] <- "2023-04-03"


dat$moneda_contrato[2911] <- "GS"
dat$precio[2911]

bor <- dat %>% filter(tipo_contrato == "Sin contrato" & borrador == "no") %>% 
  select(id, user, tipo_contrato, borrador, ini_contrato, fin_contrato)


dat$borrador[5] <- "no"

dat$img[1653] <- "https://place-storage.nyc3.digitaloceanspaces.com/skyone/fotos-inmo/skyone_inmueble_0000002209-01_489-small.jpg"




inmo <- dat[,]
inmo$lat
back <- dat
save(back, file = "./back.rda")
inmo$id <- c("skyone_inmueble_0000002210", "skyone_inmueble_0000002211")

dat$fecha_creacion <- dat$fecha
dat$activo <- "si"
dat$publicado <- "si"
dat$user[142] <- "ruben.dominguez@skyone.com.py"
dat$lat[1652] <- -25.297531000473853
dat$lon[1652] <- -57.269277991403676

skyone_inmueble_0000000949
dat$precio_cierre[3774] <- 0
dat$fecha_cierre[3774] <- NA
dat$borrador[3774] <- "no"


ind <- which(is.na(dat$externa))
dat$externa[ind] <- "no"
dat$descripcion[ind]

dat <- dat[-c(5775, 5778),]
dat[780,]
dat$externa[658] <- "no"
dat$lon[1016] <- -57.511

rober <- dat %>% filter(user == "roberto.ozorio@skyone.com.py")
rober_clien <- list_table_var_mysql("skyone_clientes", "id", rober$cliente)

rober$user <- "raquelcalcena@gmail.com"
rober$link <- NA
rober$referente <- NA

inmo_ids <- rober %>% select(inmo_vie = id, clien_vie = cliente)
inmo_ids$geometry <- NULL

serie <- 2285
for(i in 1:nrow(rober)){
  print(i)
  new_inmo <- rober[i,]
  new_inmo$id <- paste0("place_inmueble_", str_pad(serie, 10, pad = "0"))
  inmo_ids$inmo_new[i] <- new_inmo$id
  save_mysql(new_inmo, "place_inmuebles", FALSE)
  serie <- serie + 1
}


dat$cliente[c(1667:1677)] <- ids_full$clien_new
save_mysql(dat, "place_inmuebles", TRUE)

dat <- load_mysql("place_inmuebles")

clien_ids <- unique(inmo_ids$clien_vie)

clientes <- list_table_var_mysql("skyone_clientes", "id", clien_ids)
clientes$tipo <- NULL
clien_ids <- clientes %>% select(clien_vie = id)

for(i in 1:nrow(clientes)){
  print(i)
  new <- clientes[i,]
  new$id <- new_id_table_mysql("id", "place_clientes", "client_", 12)
  new$user <- "raquelcalcena@gmail.com"
  clien_ids$clien_new[i] <- new$id
  save_mysql(new, "place_clientes", FALSE)
}

ids_full <- left_join(inmo_ids, clien_ids, by = "clien_vie")

estados <- list_table_var_mysql("skyone_estados", "id_inmo", ids_full$inmo_vie)
estados <- estados[-c(10:11),]

estados_new <- left_join(estados, ids_full, by = c("id_inmo" = "inmo_vie"))

estados_new$id_inmo <- estados_new$inmo_new
estados_new$inmo_new <- NULL
estados_new$id_clien <- estados_new$clien_new
estados_new$clien_vie <- NULL
estados_new$clien_new <- NULL
estados_new$user <- "raquelcalcena@gmail.com"
estados_new$comment <- NULL

est_place <- load_mysql("place_estados")

for(i in 1:nrow(estados_new)){
  print(i)
  new <- estados_new[i,]
  new$id <- new_id_table_mysql("id", "place_estados", "estado_", 12)
  save_mysql(new, "place_estados", FALSE)
}



for(j in 1:nrow(ids_full)){
  print(j)
  inmo <- ids_full[j,]$inmo_vie
  
  galeria <- num_fotos_inmo(inmo)

  rand <- str_pad(round(runif(1, min=1, max=999)), 3, pad = "0")
  for(i in 1: length(galeria)){
    print(i)
    im <- image_read(galeria[i])
    image_write(im, "./temp_foto_inmo.jpg")
    
    name <- paste0(ids_full[j,]$inmo_new, "-", str_pad(i, 2, pad = "0"), "_", rand)
    put_object_do(paste0("/fotos-inmo/", name, ".jpg"), "./temp_foto_inmo.jpg")
    im <- image_scale(im, "300x")
    image_write(im, "./temp_foto_inmo.jpg")
    put_object_do(paste0("/fotos-inmo/", name, "-small.jpg"), "./temp_foto_inmo.jpg")
    
    if(i == 1){
      url <- paste0("https://place-storage.nyc3.digitaloceanspaces.com/fotos-inmo/", name, "-small.jpg")
      update_var_table_mysql(url, "img", "id", ids_full[j,]$inmo_new, "place_inmuebles")
    }
  }
}


all_clien <- list_table_var_mysql("skyone_clientes", "user", "roberto.ozorio@skyone.com.py")
all_clien <- all_clien %>% filter(!id %in% ids_full$clien_vie)
all_clien <- all_clien[-33,]
all_clien$tipo <- NULL
for(i in 1:nrow(all_clien)){
  print(i)
  new <- all_clien[i,]
  new$id <- new_id_table_mysql("id", "place_clientes", "client_", 12)
  new$user <- "raquelcalcena@gmail.com"
  save_mysql(new, "place_clientes", FALSE)
}


llamadas <- list_table_var_mysql("skyone_llamadas", "user", "roberto.ozorio@skyone.com.py")
llamadas <- llamadas %>% filter(id_inmo %in% ids_full$inmo_vie | id_clien %in% ids_full$clien_vie)

llamadas_new <- left_join(llamadas, ids_full, by = c("id_inmo" = "inmo_vie"))
llamadas_new$id_clien <- llamadas_new$clien_new
llamadas_new$clien_new <- NULL
llamadas_new$clien_vie <- NULL
llamadas_new$id_inmo <- llamadas_new$inmo_new
llamadas_new$inmo_new <- NULL

for(i in 1:nrow(llamadas_new)){
  print(i)
  new <- llamadas_new[i,]
  new$id <- new_id_table_mysql("id", "place_llamadas", "llamada_", 12)
  new$user <- "raquelcalcena@gmail.com"
  save_mysql(new, "place_llamadas", FALSE)
}

llam <- list_table_var_mysql("place_llamadas", "user", "raquelcalcena@gmail.com")
for(i in 1:nrow(llam)){
  print(i)
  new <- llam[i,]
  estado <- list_table_var1_var2_mysql("place_estados", "id_inmo", new$id_inmo, "id_clien", new$id_clien)
  llam$id_estado[i] <- estado$id
}

llam_full <- load_mysql("place_llamadas")
llam_full <- llam_full %>% filter(user != "raquelcalcena@gmail.com")
llam_full <- rbind(llam_full, llam)
save_mysql(llam_full, "place_llamadas", TRUE)


pla <- load_mysql("skyone_clientes")


  
row <- 879
dat$precio_cierre[row] <- 0
dat$fecha_cierre[row] <- NA
dat$borrador[row] <- "no"

dat$descripcion[650] <- "Externa: Alquiler de oficina"


dat %>% filter(externa == "no" & str_detect(descripcion, "Externa"))


cerradas <- dat[dat$precio_cierre != 0,] %>% select(id, user, titulo, descripcion, precio, precio_cierre, tipo_contrato, borrador) %>%
  filter(!user %in% c("gmayeregger@gmail.com", "daniel.ortiz@skyone.group"))
cerradas$geometry <- NULL
write_csv2(cerradas, "cerradas.csv")

create_table_mysql(paste0(co, "_inmo_eliminados"), new, "id_del")
describe_table_mysql(paste0(co, "_inmo_eliminados"))
delete_table_mysql(paste0(co, "_inmo_eliminados"))
clear_table_mysql(paste0(co, "_inmo_eliminados"))
dat <- load_mysql(paste0(co, "_inmo_eliminados"))
save_mysql(dat, paste0(co, "_inmo_eliminados"), TRUE)
skyone_eliminados <- dat
save(skyone_eliminados, file = "back/skyone_eliminados.rda")

create_table_mysql(paste0(co, "_cierres"), new, "id_cierre")
describe_table_mysql(paste0(co, "_cierres"))
delete_table_mysql(paste0(co, "_cierres"))
clear_table_mysql(paste0(co, "_cierres"))
dat <- load_mysql(paste0(co, "_cierres"))
save_mysql(dat, paste0(co, "_cierres"), TRUE)

create_table_mysql(paste0(co, "_tipologias"), new, "id_tipo")
describe_table_mysql(paste0(co, "_tipologias"))
delete_table_mysql(paste0(co, "_tipologias"))
clear_table_mysql(paste0(co, "_tipologias"))
dat <- load_mysql(paste0(co, "_tipologias"))
save_mysql(dat, paste0(co, "_tipologias"), TRUE)

dat$m2_cons[362] <- 68.560

name_estado <- paste0("Nathalia Peña", ifelse(new_inmo$venta_alquiler == "Venta", " vende", " alquila"),
                      ifelse(new_inmo$tipoPropiedad %in% c("Casa", "Oficina", "Propiedad rural", "Casa quinta"), " una ", " un "), str_to_lower(new_inmo$tipoPropiedad),
                      " en ", new_inmo$ciudad)

id_captacion <- new_id_table_mysql("id", paste0(co, "_estados"), "estado_", 12)
new_estado <- data.frame(
  id = id_captacion,
  fecha = "2022-08-09 10:53:44",
  user = "roberto.ozorio@skyone.group",
  name = name_estado,
  id_inmo = new_inmo$id,
  id_clien = "client_000000000138",
  nivel_cliente = NA,
  estado_cliente = NA,
  accion = NA,
  fecha_prox_contacto = as.character(as_date(now("America/Asuncion"))),
  tipo_cliente = "captacion",
  presupuesto = NA,
  urgente = "no",
  importante = "no",
  fase = "2"
)
save_mysql(new_estado, paste0(co, "_estados"), FALSE)
capa <- load_mysql(paste0(co, "_capacitaciones"))

link <- str_split(capa$link[1], "/")[[1]]
link <- paste0("/", link[length(link)])
##############################################################################################################
#estados
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  fecha = str_flatten(rep("a", 30)),
  user = str_flatten(rep("a", 60)),
  name = str_flatten(rep("a", 120)),
  id_inmo = str_flatten(rep("a", 30)),
  id_clien = str_flatten(rep("a", 60)),
  nivel_cliente = str_flatten(rep("a", 30)),
  estado_cliente = str_flatten(rep("a", 60)),
  accion = str_flatten(rep("a", 60)),
  fecha_prox_contacto = str_flatten(rep("a", 30)),
  tipo_cliente = str_flatten(rep("a", 30)),
  presupuesto = str_flatten(rep("a", 30)),
  urgente = str_flatten(rep("a", 2)),
  importante = str_flatten(rep("a", 2)),
  fase = str_flatten(rep("a", 2)),
  comment = str_flatten(rep("a", 500))
)
create_table_mysql(paste0(co, "_estados"), new, "id")
describe_table_mysql(paste0(co, "_estados"))
#delete_table_mysql(paste0(co, "_estados"))
#clear_table_mysql(paste0(co, "_estados"))
dat <- load_mysql(paste0(co, "_estados"))
#save_mysql(dat, paste0(co, "_estados"), TRUE)
back_estados <- dat

dat$fase[7666] <- "3"
dat$fase[7954] <- "2"


##############################################################################################################
#capacitaciones
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  user = str_flatten(rep("a", 60)),
  fecha = str_flatten(rep("a", 30)),
  tipo = str_flatten(rep("a", 30)),
  link = str_flatten(rep("a", 60)),
  img = str_flatten(rep("a", 255)),
  descripcion = str_flatten(rep("a", 255)),
  titulo = str_flatten(rep("a", 60))
)
create_table_mysql(paste0(co, "_capacitaciones"), new, "id")
describe_table_mysql(paste0(co, "_capacitaciones"))
#delete_table_mysql(paste0(co, "_capacitaciones"))
#clear_table_mysql(paste0(co, "_capacitaciones"))
dat <- load_mysql(paste0(co, "_capacitaciones"))
#save_mysql(dat, paste0(co, "_capacitaciones"), TRUE)

dat <- dat[-c(40:43),]
##############################################################################################################
#transacciones
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  fecha = str_flatten(rep("a", 30)),
  user = str_flatten(rep("a", 60)),
  id_inmo = str_flatten(rep("a", 30)),
  id_estado = str_flatten(rep("a", 60)),
  tipo = str_flatten(rep("a", 30)),
  fase = str_flatten(rep("a", 30)),
  venta_alquiler= str_flatten(rep("a", 30)),
  fecha_cierre = str_flatten(rep("a", 30)),
  moneda = str_flatten(rep("a", 30)),
  precio_cierre = as.numeric(44.555),
  comision_cierre = as.numeric(44.555),
  comision_a_cobrar = as.numeric(44.555),
  comision_cobrada = as.numeric(44.555),
  comision_user = as.numeric(44.555),
  comision_asesor = as.numeric(44.555),
  id_agencia = str_flatten(rep("a", 30))
)
create_table_mysql(paste0(co, "_transacciones"), new, "id")
describe_table_mysql(paste0(co, "_transacciones"))
delete_table_mysql(paste0(co, "_transacciones"))
clear_table_mysql(paste0(co, "_transacciones"))
dat <- load_mysql(paste0(co, "_transacciones"))
#save_mysql(dat, paste0(co, "_transacciones"), TRUE)


ind <- which(dat$id_agencia == "agencia_0000000007")

dat[ind,] %>% arrange(desc(fecha))

dat[453,]


dat <- dat[-3688,]

#captacion
update_var_table_mysql("5", "fase", "id", "transaccion_000000004560", "skyone_transacciones")
del_var_table_mysql("id_pagado", "transaccion_000000004560", "skyone_pagos")

row <- 3210
dat$user[row]
dat$fase[row] <- "5"
dat$moneda[row] <- "GS"
dat$comision_asesor[row] <- 472500
dat$comision_cobrada[row] <- 0
dat$user[row] <- "riverosromina18@gmail.com"

dat <- dat[-c(2028),]

tr <- dat[493,]
tr$id <- "transaccion_000000000710"
tr$user <- "juliopratt.mpower@gmail.com"
tr$id_estado <- "estado_000000005490"
tr$tipo <- "colocación"
save_mysql(tr, "skyone_transacciones", FALSE)


row <- 701
dat$comision_user[row] <- 80
dat$comision_asesor[row] <- dat$comision_a_cobrar[row]*0.9*0.8




dat$id_estado[130] <- paste0("Pago ", dat$id_estado[130])

dat$precio_cierre[132] <- 29000000
dat$comision_cierre[c(132, 133)] <- 10
dat$comision_asesor[c(132)] <- 1827000
dat$comision_asesor[c(133)] <- 216000





skyone_inmueble_0000005238

captacion
estado_000000008317
transaccion_000000001635
dat <- dat[-1174,]
dat$fase[1151] <- "4"
dat$comision_cobrada[1151] <- 0

Global
transaccion_000000001658





transaccion_000000002270

row <- c(1701)
dat$comision_cobrada[row] <- 0
dat$fase[row] <- "4"

3000000*0.90*0.80

ind <- which(dat$fase == "5")
dat$fase[ind] <- "6"

id_inmos <- dat %>% filter(!is.na(id_inmo)) %>% .$id_inmo
inmos <- list_table_var_mysql("skyone_inmuebles", "id", id_inmos) %>% select(id, venta_alquiler)
inmos$geometry <- NULL

new_dat <- left_join(dat, inmos, by = c("id_inmo" = "id"))
new_dat$comision_a_cobrar <- ifelse(new_dat$venta_alquiler == "Venta", new_dat$precio_cierre*new_dat$comision_cierre/100*0.50, new_dat$precio_cierre*new_dat$comision_cierre/100)
new_dat$comision_a_cobrar[c(120:124)] <- 0

sahia.almiron@skyone.com.py


new <- dat[393,]
4706


new$id <- "transaccion_000000000604"
new$user <- "adriana.ayub@skyone.com.py"
new$id_estado <- "estado_000000004706"
new$tipo <- "colocación"
new$comision_asesor <- 2430000
new$comision_user <- 60

save_mysql(new, "skyone_transacciones", FALSE)

dat$user[c(181, 218)] <- "celestebeatrizrolonmaciel@gmail.com"

ind <- which(str_detect(dat$id_estado, " Action") & dat$fase == 5)
dat <- dat[-ind,]

##############################################################################################################
#llamadas
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  fecha = str_flatten(rep("a", 30)),
  fecha_agendada = str_flatten(rep("a", 30)),
  user = str_flatten(rep("a", 60)),
  id_inmo = str_flatten(rep("a", 30)),
  id_clien = str_flatten(rep("a", 60)),
  id_estado = str_flatten(rep("a", 30)),
  accion = str_flatten(rep("a", 60)),
  realizado = str_flatten(rep("a", 2)),
  agendado = str_flatten(rep("a", 2)),
  time = str_flatten(rep("a", 30)),
  visto = str_flatten(rep("a", 2)),
  comment = str_flatten(rep("a", 250))
)
create_table_mysql(paste0(co, "_llamadas"), new, "id")
describe_table_mysql(paste0(co, "_llamadas"))
delete_table_mysql(paste0(co, "_llamadas"))
clear_table_mysql(paste0(co, "_llamadas"))
dat <- load_mysql(paste0(co, "_llamadas"))
save_mysql(dat, paste0(co, "_llamadas"), TRUE)
back <- dat

dat <- dat[-c(6733, 6735),]

dat$realizado[527:537] <- "no"
llamadas <- dat %>% filter(user == "roberto.ozorio@skyone.group" & realizado == "no" & !is.na(fecha_agendada))

estados <- unique(dat$id_estado)
for(i in 1:length(estados)){
  llamadas <- dat %>% filter(id_estado == estados[i] & realizado == "no" & !is.na(fecha_agendada))
  if(nrow(llamadas) != 0){
    update_var_id_table_mysql(min(llamadas$fecha_agendada), "fecha_prox_contacto", estados[i], paste0(co, "_estados"))
  }
}

dat$realizado[1101] <- "si"

##############################################################################################################
#eventos
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  fecha = str_flatten(rep("a", 30)),
  fecha_agendada = str_flatten(rep("a", 30)),
  hora_ini = str_flatten(rep("a", 30)),
  hora_fin = str_flatten(rep("a", 30)),
  user = str_flatten(rep("a", 60)),
  titulo = str_flatten(rep("a", 120)),
  comment = str_flatten(rep("a", 250))
)
create_table_mysql(paste0(co, "_eventos"), new, "id")
describe_table_mysql(paste0(co, "_eventos"))
delete_table_mysql(paste0(co, "_eventos"))
clear_table_mysql(paste0(co, "_eventos"))
dat <- load_mysql(paste0(co, "_eventos"))
save_mysql(dat, paste0(co, "_eventos"), TRUE)

##############################################################################################################
#alarmas
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  fecha = str_flatten(rep("a", 30)),
  time = str_flatten(rep("a", 30)),
  user = str_flatten(rep("a", 60)),
  destino = str_flatten(rep("a", 60)),
  id_inmo = str_flatten(rep("a", 30)),
  id_clien = str_flatten(rep("a", 60)),
  id_estado = str_flatten(rep("a", 30)),
  id_busq = str_flatten(rep("a", 30)),
  id_noticia = str_flatten(rep("a", 30)),
  id_calificacion = str_flatten(rep("a", 30)),
  tipo = str_flatten(rep("a", 30)),
  visto = str_flatten(rep("a", 2)),
  img = str_flatten(rep("a", 250)),
  comment = str_flatten(rep("a", 120))
)
create_table_mysql(paste0(co, "_alarmas"), new, "id")
describe_table_mysql(paste0(co, "_alarmas"))
delete_table_mysql(paste0(co, "_alarmas"))
clear_table_mysql(paste0(co, "_alarmas"))
dat <- load_mysql(paste0(co, "_alarmas"))
save_mysql(dat, paste0(co, "_alarmas"), TRUE)



##############################################################################################################
#agentes_data
new <- data.frame(
  name = str_flatten(rep("a", 30)),
  user = str_flatten(rep("a", 60)),
  pass = str_flatten(rep("a", 60)),
  date_create = str_flatten(rep("a", 30)),
  phone = str_flatten(rep("a", 30)),
  face = str_flatten(rep("a", 100)),
  insta = str_flatten(rep("a", 100)),
  active = as.integer(10),
  company = str_flatten(rep("a", 60)),
  ruc = str_flatten(rep("a", 30)),
  razon = str_flatten(rep("a", 120)),
  img = str_flatten(rep("a", 250)),
  birthday = str_flatten(rep("a", 30)),
  agencia = str_flatten(rep("a", 30)),
  admin = as.integer(10),
  cargo = str_flatten(rep("a", 30)),
  comision = str_flatten(rep("a", 30)),
  alarmas_vistas = as.numeric(44.444),
  comment = str_flatten(rep("a", 500)),
  fecha_ultima_alarma = str_flatten(rep("a", 30)),
  link = str_flatten(rep("a", 250))
)

create_table_mysql(paste0(co, "_agentes_data"), new, "user")
describe_table_mysql(paste0(co, "_agentes_data"))
delete_table_mysql(paste0(co, "_agentes_data"))
clear_table_mysql(paste0(co, "_agentes_data"))
dat <- load_mysql(paste0(co, "_agentes_data"))
save_mysql(dat, paste0(co, "_agentes_data"), TRUE)

dat$active
dat$name[234] <- "TEAM ROLON"
dat$cargo[211] <- "Asesor"
dat$phone[246] <- "+595986730663"
dat$pass[97] <- digest("skyone123")
dat$comision[211] <- "Asesor Elite - 80%"

dat$comision[c(10, 38, 61, 67, 84, 86)] <- 80
dat$agencia[135] <- "agencia_0000000001"
dat$link <- NA

back <- dat
dat <- skyone_agentes_data

##############################################################################################################
#agencias_data
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  name = str_flatten(rep("a", 30)),
  company = str_flatten(rep("a", 30)),
  fecha = str_flatten(rep("a", 30)),
  phone = str_flatten(rep("a", 30)),
  mail = str_flatten(rep("a", 60)),
  addr = str_flatten(rep("a", 120)),
  ruc = str_flatten(rep("a", 30)),
  razon = str_flatten(rep("a", 60)),
  img = str_flatten(rep("a", 250)),
  birthday = str_flatten(rep("a", 30)),
  pais = str_flatten(rep("a", 30)),
  master = str_flatten(rep("a", 30)),
  lat = as.numeric(44.444),
  lon = as.numeric(44.444)
)

create_table_mysql(paste0(co, "_agencias_data"), new, "id")
describe_table_mysql(paste0(co, "_agencias_data"))
delete_table_mysql(paste0(co, "_agencias_data"))
clear_table_mysql(paste0(co, "_agencias_data"))
dat <- load_mysql(paste0(co, "_agencias_data"))
save_mysql(dat, paste0(co, "_agencias_data"), TRUE)

dat$mail[1] <- "claudia.fretes@skyone.group"
dat$mail[6] <- "deisy@jarosky.com.py"
dat$mail[7] <- "ileana.stipanovich@skyone.group"
dat$mail[3] <- "daniel.ortiz@skyone.group"

dat$ruc[3] <- "80160210-6"
dat$razon[3] <- "Gestión y Administración de Negocios E.A.S."
dat$name[3] <- "SkyOne Essence"
dat$mail[3] <- "beatrizrolon.skyone@gmail.com"
dat$phone[1] <- "+595981163626"
dat$mail[8] <- "kathia.villasanti@skyone.group"
dat$addr[9] <- "Tova Mascoi esquina Paz del Chaco"

dat <- dat[-8,]

row <- 5
dat$name[row]
dat$phone[row] <- "+595992925944"
dat$mail[row] <- "romina.riveros@skyone.group"
dat$addr[row] <- "Cerró Cora 3322 casi Kubitschek"
dat$razon[row] <- "JAROSKY S.A."
dat$ruc[row] <- "80084813-6"

dat$name[8] <- "SkyOne Central"


##############################################################################################################
#franquicias_data
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  name = str_flatten(rep("a", 30)),
  company = str_flatten(rep("a", 30)),
  fecha = str_flatten(rep("a", 30)),
  phone = str_flatten(rep("a", 30)),
  mail = str_flatten(rep("a", 30)),
  addr = str_flatten(rep("a", 120)),
  ruc = str_flatten(rep("a", 30)),
  razon = str_flatten(rep("a", 30)),
  img = str_flatten(rep("a", 250)),
  birthday = str_flatten(rep("a", 30)),
  pais = str_flatten(rep("a", 30)),
  lat = as.numeric(44.444),
  lon = as.numeric(44.444)
)

create_table_mysql(paste0(co, "_franquicias_data"), new, "id")
describe_table_mysql(paste0(co, "_franquicias_data"))
delete_table_mysql(paste0(co, "_franquicias_data"))
clear_table_mysql(paste0(co, "_franquicias_data"))
dat <- load_mysql(paste0(co, "_franquicias_data"))
save_mysql(dat, paste0(co, "_franquicias_data"), TRUE)

##############################################################################################################
#fingerprint
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  fecha = str_flatten(rep("a", 30)),
  user = str_flatten(rep("a", 60)),
  fingerprint = str_flatten(rep("a", 60)),
  ip = str_flatten(rep("a", 60))
)

create_table_mysql(paste0(co, "_fingerprint"), new, "id")
describe_table_mysql(paste0(co, "_fingerprint"))
delete_table_mysql(paste0(co, "_fingerprint"))
clear_table_mysql(paste0(co, "_fingerprint"))
dat <- load_mysql(paste0(co, "_fingerprint"))
save_mysql(dat, paste0(co, "_fingerprint"), TRUE)

sort(table(dat$fingerprint))

dat <- dat[-c(761),]

ind <- which(str_detect(dat$user, "gmayeregger"))




##############################################################################################################
#noticias
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  user = str_flatten(rep("a", 60)),
  fecha = str_flatten(rep("a", 30)),
  text = str_flatten(rep("a", 500)),
  img = str_flatten(rep("a", 250)),
  n_img = str_flatten(rep("a", 2)),
  n_comen = str_flatten(rep("a", 30)),
  activo = str_flatten(rep("a", 2))
)

create_table_mysql(paste0(co, "_noticias"), new, "id")
describe_table_mysql(paste0(co, "_noticias"))
delete_table_mysql(paste0(co, "_noticias"))
clear_table_mysql(paste0(co, "_noticias"))
dat <- load_mysql(paste0(co, "_noticias"))
save_mysql(dat, paste0(co, "_noticias"), TRUE)

dat$user[64] <- "info@skyone.group"
dat <- dat[-c(2342),]
dat$activo <- "si"
dat$img[4] <- "https://place-storage.nyc3.digitaloceanspaces.com/skyone/fotos-noticias/noticia_000000000026-01_076-small.jpg"

noticias_del <- list_table_var_mysql(paste0(co, "_noticias"), "activo", "no") %>% .$id %>% unique()
del_var_table_mysql("noticia", noticias_del, paste0(co, "_comentarios"))
del_var_table_mysql("id", noticias_del, paste0(co, "_noticias"))

##############################################################################################################
#comentarios
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  user = str_flatten(rep("a", 60)),
  noticia = str_flatten(rep("a", 30)),
  fecha = str_flatten(rep("a", 30)),
  text = str_flatten(rep("a", 500)),
  activo = str_flatten(rep("a", 2))
)

create_table_mysql(paste0(co, "_comentarios"), new, "id")
describe_table_mysql(paste0(co, "_comentarios"))
delete_table_mysql(paste0(co, "_comentarios"))
clear_table_mysql(paste0(co, "_comentarios"))
dat <- load_mysql(paste0(co, "_comentarios"))
save_mysql(dat, paste0(co, "_comentarios"), TRUE)

dat$activo <- "si"
##############################################################################################################
#permisos
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  accion = str_flatten(rep("a", 30)),
  tipo_accion = str_flatten(rep("a", 30)),
  glob_off = str_flatten(rep("a", 2)),
  glob_man = str_flatten(rep("a", 2)),
  glob_ass = str_flatten(rep("a", 2)),
  coun_off = str_flatten(rep("a", 2)),
  coun_man = str_flatten(rep("a", 2)),
  coun_ass = str_flatten(rep("a", 2)),
  brok_off = str_flatten(rep("a", 2)),
  brok_man = str_flatten(rep("a", 2)),
  brok_ass = str_flatten(rep("a", 2)),
  asesor = str_flatten(rep("a", 2))
)

create_table_mysql(paste0(co, "_permisos"), new, "id")
describe_table_mysql(paste0(co, "_permisos"))
delete_table_mysql(paste0(co, "_permisos"))
clear_table_mysql(paste0(co, "_permisos"))
dat <- load_mysql(paste0(co, "_permisos"))
save_mysql(dat, paste0(co, "_permisos"), FALSE)

dat <- data.frame(id = "permiso_0000000009", accion = "editar transacciones", tipo_accion = "Office", glob_off = "si", 
                  glob_man = "si", glob_ass = "si", coun_off = "si", coun_man = "si",
                  coun_ass = "si", brok_off = "si", brok_man = "si", brok_ass = "si", 
                  asesor = "no")

##############################################################################################################
#seguimiento
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  user = str_flatten(rep("a", 60)),
  fecha = str_flatten(rep("a", 30)),
  id_inmo = str_flatten(rep("a", 30))
)

create_table_mysql(paste0(co, "_seguidas"), new, "id")
describe_table_mysql(paste0(co, "_seguidas"))
delete_table_mysql(paste0(co, "_seguidas"))
clear_table_mysql(paste0(co, "_seguidas"))
dat <- load_mysql(paste0(co, "_seguidas"))
save_mysql(dat, paste0(co, "_seguidas"), TRUE)


##############################################################################################################
#referidos
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  user = str_flatten(rep("a", 60)),
  fecha = str_flatten(rep("a", 30)),
  tipo = str_flatten(rep("a", 30)),
  id_inmo = str_flatten(rep("a", 30)),
  externo = str_flatten(rep("a", 2)),
  comision = as.numeric(44.555),
  moneda = str_flatten(rep("a", 3)),
  monto = as.numeric(44.555),
  pagado = str_flatten(rep("a", 2)),
  img = str_flatten(rep("a", 250))
)

create_table_mysql(paste0(co, "_referidos"), new, "id")
describe_table_mysql(paste0(co, "_referidos"))
delete_table_mysql(paste0(co, "_referidos"))
clear_table_mysql(paste0(co, "_referidos"))
dat <- load_mysql(paste0(co, "_referidos"))
save_mysql(dat, paste0(co, "_referidos"), TRUE)

dat$moneda[c(57,51)] <- "USD"

ind <- which(dat$user == "roberto.ozorio@skyone.group")
dat$user[ind] <- "analia.sosa@skyone.com.py"

inmo <- load_mysql("skyone_inmuebles") %>% filter(!is.na(referente))
agentes <- load_mysql("skyone_agentes_data")

for(i in 1:nrow(inmo)){
  inm <- inmo[i,]

  new_referido <- data.frame(
    id = new_id_table_mysql("id", paste0(co, "_referidos"), "referido_", 12),
    user = inm$referente,
    fecha = inm$fecha,
    tipo = "captación",
    id_inmo = inm$id,
    externo = ifelse(inm$referente %in% agentes$user, "no", "si")
  )
  save_mysql(new_referido, "skyone_referidos", FALSE)
  print(i)
}




##############################################################################################################
#pagos
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  fecha = str_flatten(rep("a", 30)),
  ingreso = str_flatten(rep("a", 2)),
  id_pagado = str_flatten(rep("a", 30)),
  tipo = str_flatten(rep("a", 30)),
  user = str_flatten(rep("a", 60)),
  agencia = str_flatten(rep("a", 30)),
  medio = str_flatten(rep("a", 30)),
  nro_trans = str_flatten(rep("a", 30)),
  img = str_flatten(rep("a", 250)),
  fact_user = str_flatten(rep("a", 60)),
  fact_fecha = str_flatten(rep("a", 30)),
  fact_concepto = str_flatten(rep("a", 250)),
  fact_numero = str_flatten(rep("a", 30)),
  fact_razon = str_flatten(rep("a", 120)),
  fact_ruc = str_flatten(rep("a", 30)),
  fact_moneda = str_flatten(rep("a", 3)),
  fact_monto = as.numeric(44.444),
  cambio = as.numeric(44.444)
)

create_table_mysql(paste0(co, "_pagos"), new, "id")
describe_table_mysql(paste0(co, "_pagos"))
delete_table_mysql(paste0(co, "_pagos"))
clear_table_mysql(paste0(co, "_pagos"))
dat <- load_mysql(paste0(co, "_pagos"))
#save_mysql(dat, paste0(co, "_pagos"), TRUE)


dat$fecha[4117] <- "2024-12-30 08:14:04"

dat <- dat[-c(7012,7013),]


dat$fact_monto[375] <- 2125000
dat <- dat[-c(4691, 4692, 5233, 5338, 5466, 5714, 5715),]

dat$fact_razon[175, 205] <- "Place Analyzer EAS"
dat$fact_ruc[175, 205] <- "80117081-8"

dat$fact_moneda[c(3318, 3320, 3317, 3319)] <- "GS"
dat$cambio[] <- 7430
dollarExchange()$venta


##############################################################################################################
#logueos
new <- data.frame(
  id = str_flatten(rep("a", 30)),
  fecha = str_flatten(rep("a", 30)),
  user = str_flatten(rep("a", 60))
)

create_table_mysql(paste0(co, "_logueos"), new, "id")
describe_table_mysql(paste0(co, "_logueos"))
delete_table_mysql(paste0(co, "_logueos"))
clear_table_mysql(paste0(co, "_logueos"))
dat <- load_mysql(paste0(co, "_logueos"))
save_mysql(dat, paste0(co, "_logueos"), TRUE)


##############################################################################################################
##############################################################################################################
##############################################################################################################
#backups

skyone_clientes <- load_mysql("skyone_clientes")
save(skyone_clientes, file = "back/skyone_clientes.rda")
skyone_inmo_eliminados <- load_mysql("skyone_inmo_eliminados")
save(skyone_inmo_eliminados, file = "back/skyone_inmo_eliminados.rda")
skyone_cierres <- load_mysql("skyone_cierres")
save(skyone_cierres, file = "back/skyone_cierres.rda")
skyone_tipologias <- load_mysql("skyone_tipologias")
save(skyone_tipologias, file = "back/skyone_tipologias.rda")
skyone_estados <- load_mysql("skyone_estados")
save(skyone_estados, file = "back/skyone_estados.rda")
skyone_transacciones <- load_mysql("skyone_transacciones")
save(skyone_transacciones, file = "back/skyone_transacciones.rda")
skyone_llamadas <- load_mysql("skyone_llamadas")
save(skyone_llamadas, file = "back/skyone_llamadas.rda")
skyone_eventos <- load_mysql("skyone_eventos")
save(skyone_eventos, file = "back/skyone_eventos.rda")
skyone_alarmas <- load_mysql("skyone_alarmas")
save(skyone_alarmas, file = "back/skyone_alarmas.rda")
skyone_agentes_data <- load_mysql("skyone_agentes_data")
save(skyone_agentes_data, file = "back/skyone_agentes_data.rda")
skyone_agencias_data <- load_mysql("skyone_agencias_data")
save(skyone_agencias_data, file = "back/skyone_agencias_data.rda")
skyone_franquicias_data <- load_mysql("skyone_franquicias_data")
save(skyone_franquicias_data, file = "back/skyone_franquicias_data.rda")
skyone_fingerprint <- load_mysql("skyone_fingerprint")
save(skyone_fingerprint, file = "back/skyone_fingerprint.rda")
skyone_noticias <- load_mysql("skyone_noticias")
save(skyone_noticias, file = "back/skyone_noticias.rda")
skyone_comentarios <- load_mysql("skyone_comentarios")
save(skyone_comentarios, file = "back/skyone_comentarios.rda")
skyone_permisos <- load_mysql("skyone_permisos")
save(skyone_permisos, file = "back/skyone_permisos.rda")
skyone_seguidas <- load_mysql("skyone_seguidas")
save(skyone_seguidas, file = "back/skyone_seguidas.rda")
skyone_referidos <- load_mysql("skyone_referidos")
save(skyone_referidos, file = "back/skyone_referidos.rda")
skyone_pagos <- load_mysql("skyone_pagos")
save(skyone_pagos, file = "back/skyone_pagos.rda")
skyone_logueos <- load_mysql("skyone_logueos")
save(skyone_logueos, file = "back/skyone_logueos.rda")
skyone_capacitaciones <- load_mysql("skyone_capacitaciones")
save(skyone_capacitaciones, file = "back/skyone_capacitaciones.rda")

#Pago Place

brokers <- list_table_var1_var2_mysql("myplace_usuarios", "company", "skyone", "cargo", "Broker Manager")
brokers <- brokers[!brokers$user %in% c("ana.cazal@skyone.com.py", "gustavo.silva@skyone.group") & brokers$agencia != "agencia_0000000002",]
oficinas <- load_mysql(paste0(co, "_agencias_data")) %>% select(id, oficina = name)
brokers <- left_join(brokers, oficinas, by = c("agencia" = "id"))

fecha_pago <- "2025-09"

for(i in 1:nrow(brokers)){
  ofi <- brokers[i,]

  asesores <- list_table_var1_var2_var3_mysql("myplace_usuarios", "agencia", ofi$agencia, "cargo", c("Asesor", "Broker Manager"), "company", "IN", "skyone")
  asesores <- asesores %>% filter(date_create < paste0(fecha_pago, "-01 00:00:00") & !str_detect(str_to_lower(name), "team "))
  
  monto <- 20000*nrow(asesores)
  
  new_pagos <- data.frame(
    id = new_id_table_mysql("id", paste0(co, "_transacciones"), "transaccion_", 12),
    fecha = as.character(now("America/Asuncion")),
    user = ofi$user,
    id_inmo = NA,
    id_estado = paste0("Pago Place Analyzer ", ofi$oficina, " - ", fecha_pago),
    tipo = "Pago a Place Analyzer",
    fase = "5",
    venta_alquiler = NA,
    fecha_cierre = NA,
    precio_cierre = 0,
    comision_cierre = 0,
    comision_a_cobrar = 0,
    comision_cobrada = 0,
    comision_user = 0,
    moneda = "GS",
    comision_asesor = monto,
    id_agencia = ofi$agencia
  )
  save_mysql(new_pagos, paste0(co, "_transacciones"), FALSE)
}


#Recarga Place a SkyOne
sky <- query_mysql("SELECT * FROM myplace_usuarios WHERE company = 'skyone' AND 
                   agencia = 'agencia_0000000004'" )
pla <- load_mysql("cuentas2")
#pla$myplace_date_create[pla$cuenta %in% sky$user] <- as.character(now("America/Asuncion"))
#pla$myplace_fecha_pago[pla$cuenta %in% sky$user] <- as.character(now("America/Asuncion"))
pla$myplace_fecha_vencimiento[pla$cuenta %in% sky$user] <- "2025-10-05 23:00:00"
pla$myplace_plan[pla$cuenta %in% sky$user] <- "Plan MyPlace Basic"
pla$myplace_activo[pla$cuenta %in% sky$user] <- "si"
pla$myplace_saldo_avaluos[pla$cuenta %in% sky$user] <- 10
pla$date[pla$cuenta %in% sky$user] <- as.character(now("America/Asuncion"))
save(pla, file = "./pla.rda")
save_mysql(pla, "cuentas2", TRUE)

#Enc ago25 04    Essense
#Asu set25 01
#Cpl ago25 06
#Emp set25 07
#Hom set25 11
#Nov set25 12


#Sil may25 10
#Lam may25 02

mati <- skyone_inmuebles %>% filter(user == "matias.fadul@skyone.group")


mati$user <- "allansosag@gmail.com"



clie <- list_table_var_mysql("skyone_clientes", "user", "matias.fadul@skyone.group")



update_var_table_mysql("allansosag@gmail.com", "user", "id", clie$id, "skyone_clientes")



list_table_var_mysql("skyone_inmuebles", "id", mati$id)

update_var_table_mysql("no", "activo", "id", mati$id, "skyone_inmuebles")

list_table_var_mysql("skyone_inmuebles", "user", "matias.fadul@skyone.group")


list_table_var_mysql("skyone_clientes", "user", "allansosag@gmail.com")




agen <- load_mysql("place_agentes_data")

ven <- load_mysql("venpru") #5068
ven_pagos <- ven$cuenta[ven$monto != 0 & ven$estado == "acreditado"]
ven_pagos <- unique(ven_pagos) #1120

pagaron <- agen$user[agen$user %in% ven_pagos]

cuen <- load_mysql("cuentas2")

myplace <- cuen$cuenta[!is.na(cuen$myplace_plan)]
no_myplace <- agen$user[!agen$user %in% myplace]

pagaron_y_no_myplace <- c(no_myplace, pagaron)
pagaron_y_no_myplace <- pagaron_y_no_myplace[duplicated(pagaron_y_no_myplace)]





inmos_pub <- query_mysql("SELECT id FROM myplace_inmuebles WHERE borrador = 'no' AND 
                         publicado = 'si' AND activo = 'si' AND precio_cierre = 0")
inmos_ids <- paste("(", paste(paste0("'", inmos_pub$id,"'"),collapse = ","), ")")
coinci_del <- query_mysql(paste0("SELECT id FROM myplace_coincidencias WHERE inmueble NOT IN ", inmos_ids))

write_excel_csv2(coinci_del, file = "delete_coincidencias.csv")




inmos_coinci_delete <- query_mysql(paste0("SELECT id FROM myplace_inmuebles WHERE inmueble NOT IN ", inmos_ids))




