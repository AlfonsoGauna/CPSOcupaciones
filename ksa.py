import pandas as pd

ksa = pd.read_csv("ksa.csv", index_col="SOC_code")

listas = {}
for x in ksa.index:
    listas["{0}".format(x)] = []

for elemento_ksa in ksa.columns:
    for soc_code in ksa.index:
        if ksa.loc[soc_code, elemento_ksa] == 1:
            listas[soc_code].append(elemento_ksa)
        else:
            pass


def similitud(occ_1, occ_2):
    "funcion que calcula la similitud entre dos ocupaciones"
    interseccion = set(occ_1).intersection(occ_2)
    union = set(occ_1).union(occ_2)
    x = (len(interseccion))/(len(union))
    return x


# Forma matricial 774x774
ksa_sim = pd.DataFrame(index=ksa.index, columns=ksa.index)
for occ_1 in ksa.index:
    for occ_2 in ksa.index:
        ksa_sim.loc[occ_1, occ_2] = similitud(listas[occ_1], listas[occ_2])

# A la forma matricial la convertimos en la forma larga
ksa_sim['id'] = ksa. index
ksa_long = pd.melt(ksa_sim, id_vars=['id'], value_vars=ksa.index)
ksa_long.columns = ['occ_1', 'occ_2', 'simi']
ksa_long.to_csv("ksa_sim.csv")
