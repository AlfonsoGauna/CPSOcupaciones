library(data.table)

# Cargamos la raw data
panel <- fread('cps_data.csv')
### 35,162,005

# Detectamos si hay un cambio de trabajo
panel <- panel[, change := ifelse(OCC2010 != shift(OCC2010) | IND1990 != shift(IND1990),1,0), by='CPSIDP']
# Uso IND1990 y no IND porque la primera esta harmonizada y la segunda no.

# Creamos la variable que detecta individuos que cambiaron al menos una vez de empleo en el periodo 1996-2018
panel <- panel[, ind_with_change := any(change %in% 1), by='CPSIDP']

# Nos quedamos solo con los individuos que alguna vez cambiaron de empleo 
# (principalmente, para bajar la cant. de obs.) las que eliminamos no nos interesan
panel <- panel[ind_with_change %in% TRUE ]
### 13,250,451

# Nos quedamos con la ocupacion e industria anterior
panel <- panel[, ':=' (old_ind = shift(IND1990), 
                       old_occ = shift(OCC2010),
                       old_wage = shift(HOURWAGE)), 
               by='CPSIDP']

# Nos quedamos con las observaciones donde se cambio de empleo
panel_changes <- panel[change==1]
### 3,397,592

# Cargamos el crosswalk 2010Census - 2010ONET
crosswalk <- fread("crosswalk.csv")

# Vemos que codigos de ocupacion no esta en el crosswalk
not_in_cross <- setdiff(panel_changes$OCC2010, crosswalk$`2010 Census Code`)

# Sacamos las observaciones que tienen codigo de ocupacion que no esta en el crosswalk
panel_changes <- panel_changes[!OCC2010 %in% not_in_cross & !old_occ %in% not_in_cross]
### 3,079,537

# Hacemos el JOIN y cambiamos los nombres de las varaibles
panel_changes <- panel_changes[crosswalk, on=.(OCC2010 = `2010 Census Code`)]
names(panel_changes)[names(panel_changes) == "2010 SOC Code"] <- "OCC_ONET"
panel_changes <- panel_changes[crosswalk, on=.(old_occ = `2010 Census Code`)]
names(panel_changes)[names(panel_changes) == "2010 SOC Code"] <- "OCC_old_ONET"

# Cargamos la data de KSA (codigos en O*NET SOC)
corte = 2.5 # Nedelkoska et al (2018) usan 2.5 como corte pero capaz es interesante ver que pasa en cuando se cambia a 3

KNG <- fread("Knowledge.csv")
SKL <- fread("Skills.csv")
ABL <- fread("Abilities.csv")

# Agregamos de O*NET SOC a SOC, atravez de un promedio simple
# Trabajamos con knoledge
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

# Calculamos la distancia entre las ocupaciones
### LO HICE EN PYTHON, importo el resultado [ksa.py] ###
### Consegui hacerlo en R [ksa.R] ###

ksa_sim <-fread('ksa_sim.csv')
ksa_sim_R <- fread('simKSA.csv')
ksa_sim_R <- ksa_sim_R[, c(2, 3, 4)]

