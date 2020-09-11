library(data.table, bit64)

# Cargamos la raw data
panel <- fread('cps_data.csv')

# Releyendo lo de Nedelkoska et al, creo que no tiene sentido lo de industrias
# acá voy a marcar con change_occ cuando hubo un cambio en ocupación y
# change_ind cuando hubo un cambio en la industria
# Uso IND1990 y no IND porque la primera esta harmonizada y la segunda no.
panel <- panel[order(CPSIDP,YEAR,MONTH),
               ':=' (change_occ=ifelse(OCC2010 != shift(OCC2010),1,0),
                     change_ind=ifelse(IND1990 != shift(IND1990),1,0)),
               by='CPSIDP']

# Creamos la variable que detecta individuos que cambiaron al menos una vez de
# ocupación en el periodo 1996-2018
panel <- panel[, ind_with_change := any(change_occ %in% 1), by='CPSIDP']

# Nos quedamos solo con los individuos que alguna vez cambiaron de empleo 
# (principalmente, para bajar la cant. de obs.) las que eliminamos no nos interesan
panel <- panel[ind_with_change %in% TRUE ]

# Nos quedamos con la ocupacion e industria anterior
panel <- panel[, ':=' (old_ind = shift(IND1990), 
                       old_occ = shift(OCC2010),
                       old_wage = shift(HOURWAGE)), 
               by='CPSIDP']

# Nos quedamos con las observaciones donde se cambio de ocupación
panel_changes <- panel[change_occ==1]

# Cargamos los crosswalks individuales
onet_soc <- fread("onet-soc.csv")
soc_census <- fread("census-soc.csv")
census_occ <- fread("occ-census.csv")

# Creamos un crosswalk unico
crosswalk <- onet_soc[soc_census, on = .(`2010 SOC Code` = `Soc 2010`)]
crosswalk <- crosswalk[census_occ, on = .(`Census 2010` = `2010 census`)]

crosswalk <- crosswalk[!is.na(`O*NET-SOC 2010 Code`)]
crosswalk <- crosswalk[, .(`O*NET-SOC 2010 Code`, Occ2010)]
colnames(crosswalk) <- c('Onet_codes','Occ2010_codes')

# Funcion que lleva las categorias de KSA desde ONET SOC 2010 a Occ2010 (~CENSUS 2010)
onet_soc_census_translation <- function(dataframe, corte_dummy){
  # O*NET SOC --> Occ2010
  dataframe <- dataframe[crosswalk, on=c("O*NET-SOC Code"="Onet_codes")] # Hacemos el JOIN con el crosswalk
  dataframe <- dataframe[, list(prom = mean(`Data Value`)), by=c("Occ2010_codes", "Element Name")] # calculamos el promedio del level en nivel SOC
  dataframe <- dataframe[!is.na(`Element Name`)] # Eliminamos codigos que tiene NAn en el nombre del elemnto, porque no sirven
  dataframe <- dataframe[, .(Occ2010_codes, `Element Name`, prom)] # nos quedamos con las variables que nos interesan
  dataframe <- unique(dataframe) # eliminamos las filas repetidas
  dataframe <- dataframe[, prom :=ifelse(prom<corte_dummy,0,1)] # hacemos la dummy en funcion del valor de corte
  dataframe <- dcast(dataframe, Occ2010_codes ~ `Element Name`, value.var="prom") # Reshape to wide format
}

# Cargamos la data de KSA (codigos en O*NET SOC)
corte = 2.5 # Nedelkoska et al (2018) usan 2.5 como corte pero capaz es interesante ver que pasa en cuando se cambia a 3

# Knowledge
KNG <- fread("Knowledge.csv")
KNG <- onet_soc_census_translation(KNG, corte)
knowledge <- colnames(KNG)

# Skills
SKL <- fread("Skills.csv")
SKL <- onet_soc_census_translation(SKL, corte)
skills <- colnames(SKL)

# Abilities
ABL <- fread("Abilities.csv")
ABL <- onet_soc_census_translation(ABL, corte)
abilities <- colnames(ABL)

# Armamos un data frame con todo (Knowledge, Skills & Abilities)
KSA <- KNG[SKL, on=.(Occ2010_codes = Occ2010_codes)]
KSA <- KSA[ABL, on=.(Occ2010_codes = Occ2010_codes)]

# Dejo acá el código para generar una matriz con la distancia de jaccard.
# Uso que max(Jaccard distance) = 1 ~ Jaccard Similarity
library(philentropy)
row.names(KSA) <- KSA$Occ2010_codes
x <- 1 - distance(KSA, method = "jaccard")
colnames(x) <- rownames(KSA)
x <- data.table(x)
x[,sector_fila:=rownames(KSA)]
simMat <- melt(x,id.vars = c("sector_fila"), variable.name = "sector_columna")

# Eliminamos objetos que ya no vamos a utilizar
rm(list=c("onet_soc", "soc_census", "census_occ", "crosswalk", 
          "KNG", "SKL", "ABL", "KSA", "corte", "simMat"))

# Hacemos el JOIN y cambiamos los nombres de las varaibles
simMat$sector_fila <- as.integer(as.character(simMat$sector_fila))
simMat$sector_columna <- as.integer(as.character(simMat$sector_columna))
panel_changes <- panel_changes[simMat, 
                               on = .(OCC2010 = sector_fila, 
                                      old_occ = sector_columna),
                               value := i.value]

# Bajan drásticamente las observaciones cuando filtramos por !is.na(value), el 47% tiene Na en 'value' 
# pero esto se explica por dos cosas, uno porque había observaciones con 9920 en OCC2010 o old_occ y 
# el 9920 esta reservado para desocupados, por ahí podemos filtrar antes por esto. Y segundo hay 18 
# códigos que están en panel_changes pero no en simMat estos están en no_sim. Si hace falta los reviso 
# uno por uno mañana

# # codigo sobre comentario anterior:
# ocupaciones_originales <- sort(unique(panel_changes$OCC2010))
# ocupaciones_similitud <- sort(unique(simMat$sector_fila))
# 
# pares <- panel_changes[is.na(value)]
# pares <- pares[, .(OCC2010, old_occ)]
# pares <- unique(pares)
# # causa 1
# pares <- pares[OCC2010 != 9920 & old_occ != 9920]
# # causa 2
# no_sim <- setdiff(ocupaciones_originales, ocupaciones_similitud)
# pares <- pares[, codigo_faltante := ifelse(OCC2010 %in% no_sim | old_occ %in% no_sim, 1, 0)]
# pares <- pares[codigo_faltante == 0]
# rm(list = c("simMat", "ocupaciones_originales", "ocupaciones_similitud", "pares", "no_sim"))

# Cargamos mas variables del cps para hacer el match con la complementary data
cps_variables <- fread("cps_3.csv")
cpsidp_interest <- unique(panel_changes$CPSIDP)
cps_variables <- cps_variables[CPSIDP %in% cpsidp_interest]

variables_match <- intersect(colnames(cps_variables), colnames(panel_changes))

panel_changes <- panel_changes[cps_variables, on=c("YEAR"="YEAR",
                                                   "SERIAL"="SERIAL",
                                                   "MONTH"="MONTH",
                                                   "HWTFINL"="HWTFINL",
                                                   "CPSID"="CPSID",
                                                   "ASECFLAG"="ASECFLAG",
                                                   "PERNUM"="PERNUM",
                                                   "WTFINL"="WTFINL",
                                                   "CPSIDP"="CPSIDP")]
panel_changes <- panel_changes[!is.na(YEAR) & 
                               !is.na(SERIAL) &
                               !is.na(MONTH) &
                               !is.na(HWTFINL) & 
                               !is.na(CPSID) &
                               !is.na(ASECFLAG) &
                               !is.na(PERNUM) & 
                               !is.na(WTFINL) & 
                               !is.na(CPSIDP)]

rm(list=c("cps_variables", "variables_match", "cpsidp_interest"))

# Cargamos la data complementaria
files <- paste(1996:2018, ".csv", sep = '')
data_tables <- lapply(files, 
                      function(x) {
                        fread(x)
                      })

complementary_data <- rbindlist(data_tables)

rm(list=c("data_tables","files"))

panel_changes <- panel_changes[complementary_data, on = c('HRHHID'='hhid',
                                                          'HRHHID2'='hhid2',
                                                          'YEAR'='year',
                                                          'MONTH'='month')]

panel_changes <- panel_changes[!is.na(HRHHID) &
                               !is.na(HRHHID2) &
                               !is.na(YEAR) & 
                               !is.na(MONTH)]

