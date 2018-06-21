#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Mon Aug 14 16:07:18 2017

@author: pao
"""

import matplotlib.pyplot as plt
from statsmodels.formula.api import ols
import numpy as np
import glob
import pyart
import pandas
import scipy
import csv
import netCDF4
from os.path import basename
#import LowPassFilter
import RadarBeamPropagation


#Calculo del perfil final
#========================

path_user = '../VAD/caso1_YSU_l/VAD/'
FileList = np.sort(glob.glob(path_user + 'elev*'))

# Parámetros
minlev = 0.1  #Nivel inferior en kilometros
maxlev = 3.0  #Nivel superior en kilometros
deltalev = 0.1 #Intervalo de la grilla

# Generamos la grilla del perfil
pnlevs = ((maxlev - minlev)/deltalev)+1 #Cantidad de niveles
pht = np.zeros(shape=(int(pnlevs),))*np.nan

for f in range(len(FileList)):
    
    vad = pandas.read_csv(FileList[f], sep=';', header=0, na_values=-9999.0)
    totalvad = pandas.DataFrame(np.zeros(shape=(int(pnlevs),6))*np.nan, columns = ['ht', 'spd', 'rmse1', 'rmse2', 'di', 'rings'])

    for l in range(int(pnlevs)):

        pht[l] = minlev + l * deltalev
        upperl = pht[l] + deltalev/2
        lowerl = pht[l] - deltalev/2

        #Subset de los datos que se encuentran en la capa
        temp = vad.loc[(vad.ht >= lowerl) & (vad.ht < upperl) & vad.spd  & (vad.spd.notnull())]
        #Cuento la cantidad de anillos válidos en la capa
        temp['u'] = temp.spd * np.sin(temp.di*np.pi/180)
        temp['v'] = temp.spd * np.cos(temp.di*np.pi/180)
        
        ringpl = len(temp.spd)

        #Calculo el promedio de spd para la capa
        totalvad.ht[l] = pht[l]
        w = temp.rmse + temp.rh
        
        if not w.any():
            continue

        spd_wavg = np.average(np.asarray(temp.spd), weights=np.asarray(w))
        #di_wavg = np.average(np.asarray(temp.di), weights=np.asarray(temp.rh))
        u_wavg = np.average(np.asarray(temp.u), weights=np.asarray(temp.rh))
        v_wavg = np.average(np.asarray(temp.v), weights=np.asarray(temp.rh))
        di_wavg = np.arctan2(u_wavg, v_wavg)*180/np.pi

        #Calculo el RMSE
        var = sum(np.power(temp.spd-spd_wavg,2)/temp.rmse)/sum(1/temp.rmse)
        rmsec1 = var/np.sqrt(ringpl)
        rmsec2 = np.sqrt(1/(sum(1/np.power(temp.rmse,2))))
        
        #Guardo en el dataframe
        
        totalvad.spd[l] = spd_wavg
        totalvad.rmse1[l] = rmsec1
        totalvad.rmse2[l] = rmsec2
        totalvad.di[l] = di_wavg
        totalvad.rings[l] = ringpl

    file = basename(FileList[f])
    #Escribo un .csv que se guarda con la fecha y la hora del volumen de datos
    totalvad.to_csv('../VAD/caso1_YSU:l/VAD/'+ file[5:], sep = ';', na_rep = '-9999')

    print "Listo " + file
