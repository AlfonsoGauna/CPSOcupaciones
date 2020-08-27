library(data.table)

# Cargamos la raw data
panel <- fread('cps_data.csv/cps_data.csv')

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
census_soc_n[order(N,decreasing = TRUE)]

# Cargamos la data de KSA (codigos en O*NET SOC)
corte = 2.5 # Nedelkoska et al (2018) usan 2.5 como corte pero capaz es interesante ver que pasa en cuando se cambia a 3

KNG <- fread("Knowledge.csv")
SKL <- fread("Skills.csv")
ABL <- fread("Abilities.csv")

# Agregamos de O*NET SOC a SOC, a través de un promedio simple
# Trabajamos con knowledge
KNG <- KNG[, SOC_code := substr(`O*NET-SOC Code`, start = 1, stop = 7)] # nos quedamos con 'xx-xxxx'
KNG <- KNG[, prom := mean(`Data Value`), by=c("SOC_code", "Element Name")] # calculamos el promedio del level
KNG <- KNG[, .(SOC_code, `Element Name`, prom)] # nos quedamos con las variables que nos interesan
KNG <- unique(KNG) # eliminamos las filas repetidas
KNG <- KNG[, prom :=ifelse(prom<corte,0,1)] # hacemos la dummy en funcion del valor de corte
KNG <- dcast(KNG, SOC_code ~ `Element Name`, value.var="prom") # Reshape to wide format
knowledge <- colnames(KNG)
# Hacemos lo mismo con skills
SKL <- SKL[, SOC_code := substr(`O*NET-SOC Code`, start = 1, stop = 7)]
SKL <- SKL[, prom := mean(`Data Value`), by=c("SOC_code", "Element Name")] 
SKL <- SKL[, .(SOC_code, `Element Name`, prom)] 
SKL <- unique(SKL) 
SKL <- SKL[, prom :=ifelse(prom<corte,0,1)] 
SKL <- dcast(SKL, SOC_code ~ `Element Name`, value.var="prom") 
skills <- colnames(SKL)
# Hacemos lo mismo con abilities
ABL <- ABL[, SOC_code := substr(`O*NET-SOC Code`, start = 1, stop = 7)]
ABL <- ABL[, prom := mean(`Data Value`), by=c("SOC_code", "Element Name")] 
ABL <- ABL[, .(SOC_code, `Element Name`, prom)] 
ABL <- unique(ABL) 
ABL <- ABL[, prom :=ifelse(prom<corte,0,1)] 
ABL <- dcast(ABL, SOC_code ~ `Element Name`, value.var="prom") 
abilities <- colnames(ABL)

# Armamos un data frame con todo (Knowledge, Skills & Abilities)
KSA <- KNG[SKL, on=.(SOC_code = SOC_code)]
KSA <- KSA[ABL, on=.(SOC_code = SOC_code)]
fwrite(KSA, "ksa.csv")

# Dejo acá el código para generar una matriz con la distancia de jaccard.
# Uso que max(Jaccard distance) = 1 ~ Jaccard Similarity
dta <- read.csv(file = 'ksa.csv', row.names = 'SOC_code')
x <- 1 - distance(dta, method = "jaccard")
colnames(x) <- rownames(dta)
x <- data.table(x)
x[,sector_fila:=rownames(dta)]
simMat <- melt(x,id.vars = c("sector_fila"), variable.name = "sector_columna")


# Hacemos el JOIN y cambiamos los nombres de las varaibles

crosswalk[,list(n_census=.N),list(`2010 Census Code`)]

panel_changes <- panel_changes[crosswalk,
                               on = c("OCC2010" = "2010 Census Code")]
names(panel_changes)[names(panel_changes) == "2010 SOC Code"] <- "OCC_ONET"
panel_changes <- panel_changes[crosswalk, on=.(old_occ = `2010 Census Code`)]
names(panel_changes)[names(panel_changes) == "2010 SOC Code"] <- "OCC_old_ONET"
