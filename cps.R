library(data.table)

panel <- fread('cps_data.csv')

panel <- panel[, change := ifelse(OCC != shift(OCC) | IND != shift(IND),1,0), by='CPSIDP']

panel <- panel[, ':=' (old_ind = shift(IND), 
                       old_occ = shift(OCC),
                       old_wage = shift(HOURWAGE)), 
               by='CPSIDP']

panel_changes <- panel[change==1]

fwrite(panel_changes, "cps_changes.csv")