# Preliminares ------------------------------------------------------------
rm(list = ls())
library(pacman)
p_load(tidyverse,haven,readxl)

# Ejercicios dplyr y forcats ----------------------------------------------
# Ejecutamos el código de la anterior sesión
source(file = "ejercicios/ejerciciosTAPI2_clase.R")

#### Group by ####
# Agrupamos la base de datos original por provincias
elec19 <- elec19 |> group_by(prov)
head(elec19) # muestra de las primeras observaciones de la base de datos

# Agrupamos la base de datos original por CCAA
elec19 <- elec19 |> group_by(ca)
head(elec19) # muestra de las primeras observaciones de la base de datos

# Agrupamos la base de datos original por número de mesas en el municipio
elec19 <- elec19 |> group_by(ca,mesas)
head(elec19) # muestra de las primeras observaciones de la base de datos

#### Summarise ####

elec19_prov <- elec19 |> 
  group_by(prov) |> 
  summarise(censo = sum(censo),
            votantes = sum(votantes),
            blanco = sum(blanco),
            nulo = sum(nulo),
            PP = sum(PP),
            PSOE = sum(PSOE),
            VOX = sum (VOX),
            PODEMOS = sum(PODEMOS+ECP+MAREAS))

# Mesas electorales
elec19_mesas <- elec19 |> group_by(ca,mesas) |> 
  summarise(num_muni = n())

# Agrupar por tipo de municipio
elec19_tipomuni <- elec19 |> 
  # creamos la variable de tipo de municipio
  mutate(tipomuni = case_when(poblacion<1000 ~ "Rural",
                              poblacion<20000 ~ "Semi-urbano",
                              T ~ "Urbano",)) |> 
  group_by(tipomuni) |> # agrupamos
  # definimos datos
  summarise(num_muni = n(),
            censo = sum(censo),
            votantes = sum(votantes),
            blanco = sum(blanco),
            nulo = sum(nulo),
            PP = sum(PP),
            PSOE = sum(PSOE),
            VOX = sum (VOX),
            PODEMOS = sum(PODEMOS+ECP+MAREAS)) |> 
  # creamos nuevas variables
  mutate(part = votantes/censo*100,
         PP_por = PP/votantes*100,
         PSOE_por = PSOE/votantes*100,
         VOX_por = VOX/votantes*100,
         PODEMOS_por = PODEMOS/votantes*100)

#### More mutate ####
# Todas las variables numéricas estandardizadas
elec19_est <- elec19 |> 
  mutate(PP_por = PP/votantes*100,) |> 
  mutate_if(is.numeric,scale)

# Estandardizar voto a PP y PSOE
elec19_estpp <- elec19 |> 
  mutate(PP_por = PP/votantes*100,
         PSOE_por = PSOE/votantes*100,) |> 
  mutate_at(c("PP_por", "PSOE_por"),scale)

# Convierte la variable población a logaritmo de población
elec19 <- elec19 |> 
  mutate(logpob = log(poblacion))

elec19_logpob <- elec19 |> 
  mutate_at("poblacion", log)

# Crea un marco de datos con todas las variables en formato carácter
elec19_caracter <- elec19 |> 
  mutate_all(as.character)

# Estandardiza solo las variables que empiezen por “vot”
elec19_est <- elec19 |> 
  mutate(across(starts_with("vot"),scale))

#### Factores ####
elec19 <- elec19 |> 
  mutate(pob_cat = ifelse(poblacion<1000,"Rural",
                          ifelse(poblacion<25000,"Semi-urbano","Urbano")))

# Creamos un factor a partir de una variable character
elec19 <- elec19 |> 
  mutate(pob_cat=fct(pob_cat,levels = c("Urbano","Semi-urbano","Rural")))

# Creamos un factor a partir de una variable numérica
elec19 <- elec19 |> 
  mutate(PP_por = PP/votantes*100,
         pp_cat = if_else(PP_por<mean(PP_por,na.rm = T), # si el valor de % PP está por debajo de la media
                          0, 1))

elec19 <- elec19 |> 
  mutate(pp_cat = as.factor(pp_cat))

# Relevel
table(elec19$pob_cat)

elec19 <-elec19 |> mutate(pob_cat = fct_relevel(pob_cat,
                                                c("Semi-urbano","Rural", "Urbano")))

table(elec19$pob_cat)

# Ordenar por frecuencia

elec19 <-elec19 |> mutate(pob_cat = fct_infreq(pob_cat))
table(elec19$pob_cat)

# Agrupar categorías
elec19 <- elec19 |> 
  mutate(mesas_fact = as.factor(mesas))

table(elec19$mesas_fact)

elec19 <- elec19 |>
  mutate(mesas_fact = fct_lump(mesas_fact,n = 5))

table(elec19$mesas_fact)


# Cambiar nombres de levels
table(elec19$pp_cat)

elec19 <- elec19 |> 
  mutate(pp_cat = fct_recode(pp_cat,
                             "Debajo media" = "0",
                             "Encima media" = "1"))

table(elec19$pp_cat)


# Ejercicio Lluís ---------------------------------------------------------

# Datos -------------------------------------------------------------------------
# Electorales
df_e <- read_xlsx(path = "datos/datos/results2023.xlsx") |> # cargar resultados electorales
  filter(party %in% c("PSOE","PP","VOX","SUMAR")) |> # seleccionar los 4 partidos relevantes
  select(-percent) # solo votos al partido

# Encuesta de polarización
df_p <- read_dta(file = "datos/datos/encuesta_polarizacion.dta") |> 
  # Seleccionar datos básicos de la encuesta
  select(starts_with("P11_"),P27) |> 
  mutate(across(starts_with("P11_"), function(x) ifelse(x > 10, NA, x))) |> 
  rename(PP=P11_1, # cambiar nombres variables
         PSOE=P11_2,
         VOX=P11_3,
         SUMAR=P11_5) |> 
  select(-starts_with("P11_"))

# Crear % de apoyo a partidos outgroup ------------------------------------
df_e_pp <- df_e |> filter(party!="PP") |> 
  mutate(total = sum(votes),
         prop = votes/total)

df_e_psoe <- df_e |> filter(party!="PSOE") |> 
  mutate(total = sum(votes),
         prop = votes/total)

df_e_vox <- df_e |> filter(party!="VOX") |> 
  mutate(total = sum(votes),
         prop = votes/total)

df_e_sumar <- df_e |> filter(party!="SUMAR") |> 
  mutate(total = sum(votes),
         prop = votes/total)


# Trabajar con la encuesta de polarización ------------------------------------------------

#### PSOE ####
v_pp <- df_e_psoe$prop[df_e_psoe$party=="PP"]
v_vox <- df_e_psoe$prop[df_e_psoe$party=="VOX"]
v_sumar <- df_e_psoe$prop[df_e_psoe$party=="SUMAR"]

df_p <- df_p |> 
  mutate(polar_psoe = v_pp*abs(PP-PSOE)+v_sumar*abs(SUMAR-PSOE)+v_vox*abs(VOX-PSOE))

#### PP ####
v_psoe <- df_e_pp$prop[df_e_pp$party=="PSOE"]
v_vox <- df_e_pp$prop[df_e_pp$party=="VOX"]
v_sumar <- df_e_pp$prop[df_e_pp$party=="SUMAR"]

df_p <- df_p |> 
  mutate(polar_pp = v_psoe*abs(PSOE-PP)+v_sumar*abs(SUMAR-PP)+v_vox*abs(VOX-PP))

#### VOX ####
v_psoe <- df_e_vox$prop[df_e_vox$party=="PSOE"]
v_pp <- df_e_vox$prop[df_e_vox$party=="PP"]
v_sumar <- df_e_vox$prop[df_e_vox$party=="SUMAR"]

df_p <- df_p |> 
  mutate(polar_vox = v_psoe*abs(PSOE-VOX)+v_sumar*abs(SUMAR-VOX)+v_pp*abs(PP-VOX))

#### SUMAR ####
v_psoe <- df_e_sumar$prop[df_e_sumar$party=="PSOE"]
v_pp <- df_e_sumar$prop[df_e_sumar$party=="PP"]
v_vox <- df_e_sumar$prop[df_e_sumar$party=="VOX"]

df_p <- df_p |> 
  mutate(polar_sumar = v_psoe*abs(PSOE-SUMAR)+v_vox*abs(VOX-SUMAR)+v_pp*abs(PP-SUMAR))

#### UN ÚNICO VALOR DE POLARIZACIÓN ####

df_p <- df_p |> 
  mutate(polar = case_when(
    P27==1 ~ polar_pp, # Votantes del PP, polarización de PP vs. resto
    P27==2 ~ polar_psoe, # Idem PSOE
    P27==3 ~ polar_vox, # Idem VOX
    P27==4 | P27==11 ~ polar_sumar, # Idem SUMAR
  ))


# Cual es la polarización por partidos

df_p |> 
  group_by(P27) |> 
  summarise(polar = mean(polar,na.rm = T)) |> 
  filter(!is.na(polar))
