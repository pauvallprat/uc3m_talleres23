# Preliminares ------------------------------------------------------------
rm(list = ls())

# Libraries
library(pacman)
p_load(readxl,tidyverse)


# Cargar los datos --------------------------------------------------------

elec19 <- read_xlsx("datos/resultados_muni_congreso2019.xlsx")

elec19 |> 
  filter(muni=="Barcelona")

# 1r municipio de la provinca
elec19 |> 
  filter(idmuni==1)

# dos provincias: león y zamora
elec19_lz <- elec19 |> 
  filter(prov=="León" | prov=="Zamora")

elec19_lz_bis <- elec19 |> 
  filter(prov %in% c("León", "Zamora"))

# Municipios donde PP mejores resultados que PSOE

elec19_pp_psoe <- elec19 |> 
  filter(PP>PSOE)

elec19_psoe_pp <- elec19 |> 
  filter(PSOE>PP)

# Eliminar Sabadell
elec19_nosabadell <- elec19 |> 
  filter(muni!="Sabadell")

# Select ------------------------------------------------------------------
# Variables para calcular participación electoral

elec19_part <- elec19 |> 
  select(codigoine,muni,censo,votantes)

# eliminamos las variables otros y cup
elec19_sinotros <- elec19 |> 
  select(-c("OTROS","CUP"))

elec19_pae <- elec19 |> 
  select(codigoine,muni,PP,PSOE,
         VOX,PODEMOS,Cs,PACMA)



# Mutate ------------------------------------------------------------------

# Creamos la variable de participación

elec19_part <- elec19_part |> 
  mutate(part = votantes/censo*100)

# Relación PSOE - PP
elec19 <- elec19 |> 
  mutate(relpppsoe = PP - PSOE)

# porcentaje de apoyo a dos partidos

elec19 <- elec19 |> 
  filter(ca == "Galicia") |> 
  mutate(Pacma_por = PACMA/votantes*100,
         BNG_por = BNG/votantes*100)


# Arrange -----------------------------------------------------------------

elec19 |> arrange(desc(PP)) |> 
  select(prov,muni,PP) |> 
  head()
