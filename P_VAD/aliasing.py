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
#import LowPassFilter
import RadarBeamPropagation

path_user = '../../Radar/VAD/PARANA/20160121/240/*.nc'
path_user = '../../Radar/VAD/PARANA/20170128/240/cfrad.20170128_103003.000_to_20170128_103427.998_PAR_SUR.nc'

FileList = np.sort(glob.glob(path_user))
file2read=FileList[101]

# Creamos el objeto "radar"
radar = pyart.io.read(file2read)

#%%

### Grafico PPI

display = pyart.graph.RadarDisplay(radar)

xlabel = 'Distancia en X (km)'
ylabel = 'Distancia en Y (km)'

fig = plt.figure(figsize=[20,15])

ax = fig.add_subplot(211,aspect=1.0)

display.plot_ppi('dBZ', sweep=2, 
                 axislabels=(xlabel,ylabel),
                 cmap='seismic',
                 #vmin=-20, vmax=20,
                 colorbar_label='dBZ')

display.plot_range_rings([240],lw=1.0,col='k',ls='-')
display.plot_range_rings([10,20,40],lw=0.5,col='k',ls='-')

display.plot_cross_hair(5.)

display.set_limits(xlim=(-100, 100), ylim=(-100, 100))


fig = plt.figure(figsize=[20,15])

ax = fig.add_subplot(212,aspect=1.0)

display.plot_ppi('V', sweep=2, 
                 axislabels=(xlabel,ylabel),
                 cmap='seismic',
                 #vmin=-20, vmax=20,
                 colorbar_label='V')

display.plot_range_rings([240],lw=1.0,col='k',ls='-')
display.plot_range_rings([10,20,40],lw=0.5,col='k',ls='-')

display.plot_cross_hair(5.)

display.set_limits(xlim=(-100, 100), ylim=(-100, 100))

plt.show()
#%%

#%%

# Rutina para quitar el aliasing
#path = 'PARANA/' + '*.nc'
data_dates = FileList
for j in data_dates:
	filename = j

# read in the file, create a RadarMapDisplay object

	radar = pyart.io.read(filename)
	display = pyart.graph.RadarMapDisplay(radar)

#Obtengo la maxima velocidad radial segun la estrategia de escaneo.
	nyquistv=radar.get_nyquist_vel(0,check_uniform=True)

#Obtengo la cantidad de niveles verticales.
	nlevels=np.size(radar.sweep_number['data'])

#Obtengo la fecha
	fecha=radar.time['units']
	fecha=fecha[14:18]+fecha[19:21]+fecha[22:24]+fecha[25:27]+fecha[28:30]+fecha[31:33]

	lon_radar=radar.longitude['data'][0]
	lat_radar=radar.latitude['data'][0]
	mw=1.5


#Recuperamos el viento a partir del objeto radar.
#El viento se guarda en un array enmascarado que tiene atributos .data y .mask 
#True equivale a elementos no validos (datos faltantes) y false a elementos validos.
	wind=np.ma.array( radar.fields['V']['data']  )
	ref=np.ma.array( radar.fields['dBZ']['data']  )

#Elimino los puntos con reflectividad mayor a 20 dBZ.
	#ref_threshold = 20.0
	#wind.mask[ ref.data > ref_threshold ]=True

#Elimino los puntos con viento menor a 1
	#wind_threshold = 1.0
	#wind.mask[wind.data > wind_threshold] = True

#Uso una de las funciones de dealiasing de pyart con los parametros por defecto
	winddealias=pyart.correct.region_dealias.dealias_region_based(radar, interval_splits=3, interval_limits=None, skip_between_rays=100, skip_along_ray=100, centered=True, nyquist_vel=None, check_nyquist_uniform=True, gatefilter=False, rays_wrap_around=True, keep_original=False, set_limits=True, vel_field='V', corr_vel_field=None)

#Modificamos el objeto radar.
	radar.fields['Vda'] = winddealias
	radar.fields['Vda']['coordinates']=radar.fields['V']['coordinates']
	radar.fields['Vda']['units']=radar.fields['V']['units']
	radar.fields['Vda']['long_name']=radar.fields['V']['long_name']
	radar.fields['Vda']['standard_name']=radar.fields['V']['standard_name']

#Agrego el viento corregido al archivo original. (por algun motivo usar write_cfradial tira un error y por el momento no pude encontrar el problema,
#por ese motivo uso esta funcion para agregar una variable al archivo existente).
	dataset=netCDF4.Dataset(filename,'r+',format='NETCDF4')
	pyart.io.cfradial._create_ncvar(radar.fields['Vda'],dataset,'Vda',['time','range'] )
	print filename   

#%%

#%%

# Leemos los archicos .nc
rango = '240/' #Elejimos con que rango queremos trabajar
path_user = '../../Radar/VAD/PARANA/20160121/'
FileList = np.sort(glob.glob(path_user + rango + '*.nc'))

# Parametros

field  = 'Vda'    #Nombre de la variable en el archivo de datos
angmin = 2        #Ángulo de elevación mínimo expresado como indice y empezando en 0
angmax = 10       #Ángulo de elevación máximo expresado como indice
rint   = 0.3      #Radio interior de la arandala a calcular en Km
rext   = 40.0     #Radio exterior de la arandela a calcular
maxgap = 30       #Máximo gap sin datos permitido, en grados
maxNaN = 72       #Cantidad de de datos faltantes en un anillo
rmin   = 0.8      #R cuadrado mínimo para que el fit del anillo sea válido
lpf    = 'False'  #Usa o no filtro pasa bajo
N      = 12       #Cantidad de datos a usar en el filtro, tiene que ser par!

# Arranca el cálculo para cada archivo y angulo de elevación
#===========================================================

for f in range(len(FileList)):
    
    file2read=FileList[f]

    # Creamos el objeto "radar"
    radar = pyart.io.read(file2read)

    # Algunas variables que necesito
    elev_ang  = np.unique(radar.elevation['data'])
    angmax    = elev_ang.size - 3
    nelev     = np.arange(angmin, angmax, 1)
    rings     = np.zeros(shape=(len(elev_ang),2))
    rings[:,1] = elev_ang
    rango = radar.range['data']
    r        = (radar.range['data'])/1000 #En km
    DateTime = radar.metadata['start_datetime']
    NameRadar= radar.metadata['site_name']

    #Inicializo los arrays que van a guardar las variables spd, di, SEspd, SEdi con NaNs
    spd   = np.zeros(shape=(len(rango),len(elev_ang)))*np.nan
    di    = np.zeros(shape=(len(rango),len(elev_ang)))*np.nan
    SEspd = np.zeros(shape=(len(rango),len(elev_ang)))*np.nan
    SEdi  = np.zeros(shape=(len(rango),len(elev_ang)))*np.nan
    SEdi  = np.zeros(shape=(len(rango),len(elev_ang)))*np.nan
    SEdi  = np.zeros(shape=(len(rango),len(elev_ang)))*np.nan
    spd_up = np.zeros(shape=(len(rango),len(elev_ang)))*np.nan
    spd_dw = np.zeros(shape=(len(rango),len(elev_ang)))*np.nan
    di_up = np.zeros(shape=(len(rango),len(elev_ang)))*np.nan
    di_dw = np.zeros(shape=(len(rango),len(elev_ang)))*np.nan
    var   = np.zeros(shape=(len(rango),len(elev_ang)))*np.nan
    rmse  = np.zeros(shape=(len(rango),len(elev_ang)))*np.nan
    elev  = np.zeros(shape=(len(rango),len(elev_ang)))*np.nan
    a  = np.zeros(shape=(len(rango),len(elev_ang)))*np.nan
    b  = np.zeros(shape=(len(rango),len(elev_ang)))*np.nan
    rs  = np.zeros(shape=(len(rango),len(elev_ang)))*np.nan

    #Calculo la propagación del haz
    [r, ht, rh, lea]  = RadarBeamPropagation.propagation(r, elev_ang, plot = 'False')

    #Fijo el ángulo de elevación
    for j in nelev:
        start_index = radar.sweep_start_ray_index['data'][j]
        end_index   = radar.sweep_end_ray_index['data'][j]

        tiempo  = radar.time['data'][start_index:end_index]
        azimuth = radar.azimuth['data'][start_index:end_index]
        Vda_raw = pandas.DataFrame(radar.fields[field]['data'][start_index:end_index], index=azimuth, columns=rango)
        cos_phi = np.cos((azimuth*np.pi)/180)
        sin_phi = np.sin((azimuth*np.pi)/180)
        elev    = elev_ang[j]
        countrings = 0


        for i in range(len(rango)): #Revisa cada gate

            #Subset para hacer VAD en una arandela de radio interior = rint y exterior = rext
            if (rh[i,j] < rint) | (rh[i,j] > rext):
                continue

            # Control de calidad de los datos 
            ## Errores aleatorios => Filtro pasa bajo

            if lpf == 'True':

                temp_Vda = np.array(Vda_raw.iloc[:,i]) #Tomo los datos de un anillo o  gate
                Vda = LowPassFilter.lowpassfl(temp_Vda, N) #Pasa por el filtro si corresponde

            else:
                Vda = Vda_raw.iloc[:,i] #Si no pasa por el filtro, solo extraigo la información para el modelo

            #Lo convierto a un DataFrame porque ols usa este tipo de formato
            temp    = pandas.DataFrame({'Vda':Vda, 'cosphi':cos_phi, 'sinphi':sin_phi})

            ## Cantidad de datos por anillo

            if temp.isnull().sum().Vda > maxNaN:
                #print 'muchos NaNs en anillo %s' % (i) 
                continue

            ## Gaps en cada anillo

            countNaN = 0
            for k in range(len(Vda)):
                if Vda_raw.isnull().iloc[k,i]:
                    countNaN = countNaN + 1
                    if countNaN == maxgap:
                        #print 'gap muy grande en anillo %s' % (i) 
                        continue
                else: countNaN = 0
            #print 'anillo %s válido' % (i)   
            countobs = 360 - temp.isnull().sum().Vda

            # Ahora ajusto los datos a Vr = a0 + a1cos phi + b1 sin phi

            mod     = ols('Vda ~ cos_phi + sin_phi', temp).fit()

            countrings = countrings + 1
            #print(mod.summary())

            ## Extraigo los coeficientes y sus errores estandar
            a0      = mod.params.Intercept
            a1      = mod.params.cos_phi
            b1      = mod.params.sin_phi
            rs1      = mod.rsquared
            Vda_mod = a0 + a1*cos_phi + b1*sin_phi
            a[i,j] = a1
            b[i,j] = b1
            rs[i,j] = rs1

            ## Varianza según RadxEvad
            var[i,j]= (np.sum(np.power(temp.Vda-Vda_mod,2)))/(countobs-3)
            rmse[i,j]= np.sqrt(var[i,j])
            SEa1    = mod.bse.cos_phi
            SEb1    = mod.bse.sin_phi
            #MSEt     = mod.mse_model
            #RMSEt    = np.sqrt(MSEt)
            #u0      = b1/np.sin((elev*np.pi)/180)
            #v0      = a1/np.cos((elev*np.pi)/180)

            # Calculo el módulo y dirección del viento
            ## spd sale de Browning_and_Wexler pero no logro lograr la igualdad con el modulo, so...
            # Si el R cuadrado del modelo es menor al umbral, el anillo se descarta
            if (rs1 > rmin):
                spd[i,j]    = np.sqrt(np.power(a1,2) + np.power(b1,2))/np.cos((elev*np.pi)/180)
            
            #spd     = np.sqrt(np.power(u0,2) + np.power(v0,2))

            if (b1 < 0):
                di[i,j] = (np.pi/2 - np.arctan(a1/b1))*180/np.pi #Lo paso a grados
                di_up[i,j] = (np.pi/2 - np.arctan((a1+SEa1)/(b1+SEb1)))*180/np.pi
                di_dw[i,j] = (np.pi/2 - np.arctan((a1-SEa1)/(b1-SEb1)))*180/np.pi
            else:
                di[i,j] = (3*np.pi/2 - np.arctan(a1/b1))*180/np.pi
                di_up[i,j] = (3*np.pi/2 - np.arctan((a1+SEa1)/(b1+SEb1)))*180/np.pi
                di_dw[i,j] = (3*np.pi/2 - np.arctan((a1-SEa1)/(b1-SEb1)))*180/np.pi

            # Calculos el error estandar para cada variable

            #SEspd[i,0]  = (abs(a1)*SEa1 + abs(b1)*SEb1)/np.sqrt(np.power(a1,2) + np.power(b1,2))*np.cos((elev*np.pi)/180) 
            #SEdi[i,0]   = (abs(a1)*SEb1 + abs(b1)*SEa1)/np.sqrt(np.power(a1,2) + np.power(b1,2))
            #Error: calculado como 2*SE de cada parámetro, cual intervalo de confianza
            #spd_up[i,j]  = np.sqrt(np.power((a1+2*SEa1),2) + np.power((b1+2*SEb1),2))/np.cos((elev*np.pi)/180) 
            #spd_dw[i,j]  = np.sqrt(np.power((a1-2*SEa1),2) + np.power((b1-2*SEb1),2))/np.cos((elev*np.pi)/180) 
            #MSE[i,j]    = MSEt
            #RMSE[i,j]   = np.sqrt(MSEt)
        rings[j,0] = countrings
        #print countrings

    # Guardo todo en un data frame bonito
    spd = spd.flatten('F')
    rmse = rmse.flatten('F')
    di = di.flatten('F')
    rh = rh.flatten('F')
    ht = ht.flatten('F')
    e = np.repeat(elev_ang, len(rango), axis=0)
    elev = e.reshape([len(elev_ang), len(rango)]).transpose()
    elev = elev.flatten('F')
    a = a.flatten('F')
    b = b.flatten('F')
    rs = rs.flatten('F')
    vad = pandas.DataFrame({'spd':spd, 'rmse':rmse, 'di':di, 'rh':rh, 'ht':ht, 'elev':elev, 'a':a, 'b':b, 'rs':rs})

    vad.to_csv('20160121_240/elev_vda-'+ DateTime + '_' + NameRadar + '.csv', sep = ';', na_rep = '-9999')
    #Muestra por pantalla la cantidad de anillos válidos para cada ángulo de elevación
    
#%%

#%%
    #Calculo del perfil final
    #========================
 
#Leemos los archivos
path_user = '20160121_240/'
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
    totalvad = pandas.DataFrame(np.zeros(shape=(int(pnlevs),7))*np.nan, columns = ['ht', 'spd', 'rmse1', 'rmse2', 'rmse3', 'di', 'rings'])

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
        rmsec3 = sum(np.power(temp.spd-spd_wavg,2)/np.sqrt(temp.rmse))/(sum(1/np.sqrt(temp.rmse))*(ringpl-1))

        #Guardo en el dataframe
        
        totalvad.spd[l] = spd_wavg
        totalvad.rmse1[l] = rmsec1
        totalvad.rmse2[l] = rmsec2
        totalvad.rmse3[l] = rmsec3
        totalvad.di[l] = di_wavg
        totalvad.rings[l] = ringpl

    print "Listo " + FileList[f]
    #Escribo un .csv que se guarda con la fecha y la hora del volumen de datos
    #totalvad.to_csv('20160114_240/vda-'+ DateTime + '_' + NameRadar + '.csv', sep = ';', na_rep = '-9999')
    totalvad.to_csv('20160121_240/'+ FileList[f][18:58], sep = ';', na_rep = '-9999')




#%%


#%%
    #Calculo del perfil final
    #========================
 
#Leemos los archivos
path_user = '20160107_240/'
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
    totalvad = pandas.DataFrame(np.zeros(shape=(int(pnlevs),7))*np.nan, columns = ['ht', 'spd', 'rmse1', 'rmse2', 'rmse3', 'di', 'rings'])

    for l in range(int(pnlevs)):

        pht[l] = minlev + l * deltalev
        upperl = pht[l] + deltalev/2
        lowerl = pht[l] - deltalev/2

        #Subset de los datos que se encuentran en la capa
        temp = vad.loc[(vad.ht >= lowerl) & (vad.ht < upperl) & vad.spd  & (vad.spd.notnull())]
        #Cuento la cantidad de anillos válidos en la capa
        temp['v'] = temp.spd * np.sin(temp.di*np.pi/180)
        temp['u'] = temp.spd * np.cos(temp.di*np.pi/180)
        
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
        rmsec3 = sum(np.power(temp.spd-spd_wavg,2)/np.sqrt(temp.rmse))/(sum(1/np.sqrt(temp.rmse))*(ringpl-1))

        #Guardo en el dataframe
        
        totalvad.spd[l] = spd_wavg
        totalvad.rmse1[l] = rmsec1
        totalvad.rmse2[l] = rmsec2
        totalvad.rmse3[l] = rmsec3
        totalvad.di[l] = di_wavg
        totalvad.rings[l] = ringpl

    print "Listo " + FileList[f]
    #Escribo un .csv que se guarda con la fecha y la hora del volumen de datos
    #totalvad.to_csv('20160114_240/vda-'+ DateTime + '_' + NameRadar + '.csv', sep = ';', na_rep = '-9999')
    totalvad.to_csv('20160117_240/'+ FileList[f][18:58], sep = ';', na_rep = '-9999')




#%%

