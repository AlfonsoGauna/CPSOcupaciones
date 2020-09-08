library(data.table)

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

# Cargamos el crosswalk 2010Census - 2010ONET
crosswalk <- fread("crosswalk.csv")

# Vemos que codigos de ocupacion no esta en el crosswalk
not_in_cross <- setdiff(panel_changes$OCC2010, crosswalk$`2010 Census Code`)

# Sacamos las observaciones que tienen codigo de ocupacion que no esta en el crosswalk
panel_changes <- panel_changes[!OCC2010 %in% not_in_cross & !old_occ %in% not_in_cross]

# Antes de hacer el join entre los códigos de ocupación del census con
# el SOC, vamos a resumir KSAs a nivel de SOC (y quizás haya que hacer
# otra agregación para que cada census occ 2010 se corresponda con solo
# un soc 2010)

# Este dt te puede servir para ver cuáles son los census occ 2010 que tienen
# más de un matcheo con SOC
census_soc_n <- crosswalk[,.N,by=`2010 Census Code`]
# census_soc_n[order(N,decreasing = TRUE)]

# Levantamos el clasificador o*net_soc to soc
onetsoc_to_soc <- readxl::read_excel("classifiers_mess/soc-onet.xlsx") %>% 
  as.data.table() %>%
  setnames(old = c("O*NET-SOC 2010 Code","SOC Code"),
           new = c("onet_soc","soc"))

# Funcion que lleva las categorias de KSA desde ONET SOC 2010 a CENSUS 2010, a traves de SOC 2010
onet_soc_census_translation <- function(dataframe, corte_dummy){
  # O*NET SOC --> SOC
  dataframe <- dataframe[onetsoc_to_soc,
                         on=c("O*NET-SOC Code"="onet_soc"),
                         SOC_code := soc]
  dataframe <- dataframe[, list(prom = mean(`Data Value`)), by=c("SOC_code", "Element Name")] # calculamos el promedio del level en nivel SOC
  # SOC --> 2010 CENSUS
  dataframe <- dataframe[crosswalk, on=.(SOC_code = `2010 SOC Code`)] # Hacemos el JOIN con el crosswalk
  dataframe <- dataframe[!is.na(`Element Name`)] # Eliminamos codigos que tiene NAn en el nombre del elemnto, porque no sirven
  dataframe <- dataframe[, level_prom := mean(prom), by=c("Element Name", "2010 Census Code")] # calculamos el promedio en nivel de censo 2010
  dataframe <- dataframe[, .(`2010 Census Code`, `Element Name`, level_prom)] # nos quedamos con las variables que nos interesan
  dataframe <- unique(dataframe) # eliminamos las filas repetidas
  dataframe <- dataframe[, level_prom :=ifelse(level_prom<corte_dummy,0,1)] # hacemos la dummy en funcion del valor de corte
  dataframe <- dcast(dataframe, `2010 Census Code` ~ `Element Name`, value.var="level_prom") # Reshape to wide format
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
KSA <- KNG[SKL, on=.(`2010 Census Code` = `2010 Census Code`)]
KSA <- KSA[ABL, on=.(`2010 Census Code` = `2010 Census Code`)]
fwrite(KSA, "ksa.csv")

# Dejo acá el código para generar una matriz con la distancia de jaccard.
# Uso que max(Jaccard distance) = 1 ~ Jaccard Similarity
library(philentropy)
dta <- read.csv(file = 'ksa.csv')
row.names(dta) <- dta$X2010.Census.Code
x <- 1 - distance(dta, method = "jaccard")
colnames(x) <- rownames(dta)
x <- data.table(x)
x[,sector_fila:=rownames(dta)]
simMat <- melt(x,id.vars = c("sector_fila"), variable.name = "sector_columna")


# Hacemos el JOIN y cambiamos los nombres de las varaibles
simMat$sector_fila <- as.integer(as.character(simMat$sector_fila))
simMat$sector_columna <- as.integer(as.character(simMat$sector_columna))
panel_changes <- panel_changes[simMat, on=.(OCC2010 = sector_fila,
                                            old_occ = sector_columna),
                               value:=i.value]
