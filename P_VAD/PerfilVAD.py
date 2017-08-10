
# coding: utf-8


import matplotlib.pyplot as plt
from statsmodels.formula.api import ols
import numpy as np
import glob
import pyart
import pandas
import scipy
import csv
import LowPassFilter
#import RadarBeamPropagation
#import radarfun


# La idea es comparar los datos de un tiempo con tiempos anteriores antes de construir el perfil final de viento.
# Osea, necesito abrir los archivos preliminares para algún tiempo anterior y hacer alguna prueba o promediar con
# el tiempo actual.

NameRadar = "PARANA"

# Parámetros
minlev = 0.1  #Nivel inferior en kilometros
maxlev = 2.5  #Nivel superior en kilometros
deltalev = 0.1 #Intervalo de la grilla

# Generamos la grilla del perfil
pnlevs = ((maxlev - minlev)/deltalev)+1 #Cantidad de niveles

#Inicializo el dataframe
pht = np.zeros(shape=(int(pnlevs),))*np.nan


#Leemos los archivos
path_user = '../../20160114_240/'
FileList = np.sort(glob.glob(path_user + 'elev*'))

for f in range(len(FileList)):
    totalvad = pandas.DataFrame(np.zeros(shape=(int(pnlevs),7))*np.nan, columns = ['ht', 'spd', 'rmse', 'rmse2', 'rmse3', 'di', 'rings'])
    vad = pandas.read_csv(FileList[f], sep=';', header=0, na_values=-9999.0)

    for l in range(int(pnlevs)):

        pht[l] = minlev + l * deltalev
        upperl = pht[l] + deltalev/2
        lowerl = pht[l] - deltalev/2

        #Subset de los datos que se encuentran en la capa
        temp = vad.loc[(vad.ht >= lowerl) & (vad.ht < upperl) & (vad.spd.notnull())]
        #Cuento la cantidad de anillos válidos en la capa
        ringpl = len(temp.spd)

        #Calculo el promedio de spd para la capa
        totalvad.ht[l] = pht[l]
        w = temp.rmse + temp.rh
        if not w.any():
            continue
        spd_wavg = np.average(np.asarray(temp.spd), weights=np.asarray(w))
        di_wavg = np.average(np.asarray(temp.di), weights=np.asarray(temp.rh))

		#Calculo el RMSE
        var = sum(np.power(temp.spd-spd_wavg,2)/temp.rmse)/sum(1/temp.rmse)
        rmsec1 = var/np.sqrt(ringpl)
        rmsec2 = np.sqrt(1/(sum(1/np.power(temp.rmse,2))))
        rmsec3 = sum(np.power(temp.spd-spd_wavg,2)/np.sqrt(temp.rmse))/(sum(1/np.sqrt(temp.rmse))*(ringpl-1))

		#Guardo en el dataframe

        totalvad.spd[l] = spd_wavg
        totalvad.rmse[l] = rmsec1
        totalvad.rmse2[l] = rmsec2
        totalvad.rmse3[l] = rmsec3
        totalvad.di[l] = di_wavg
        totalvad.rings[l] = ringpl
    #Escribo un .csv que se guarda con la fecha y la hora del volumen de datos
    totalvad.to_csv('../../20160114_240/'+ FileList[f][24:64], sep = ';', na_rep = '-9999')
    #print totalvad
