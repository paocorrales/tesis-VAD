
# coding: utf-8

# In[ ]:

###################################################################
#                       Radar Beam Propagation                    #                
#                      ========================                   #
#                                                                 #
# 4R/3 approximation                                              #
#                                                                 #
# Imputs:                                                         #
#     r = array with the range data                               #
#     theta = array with the elevation angles                     #
#                                                                 # 
# Output:                                                         #
#     objects type array                                          #
#     r  = range                                                  #
#     ht = height in kilomiters                                   #
#     rh = horizontal range                                       #
#     lea = local elevation angle                                 #
###################################################################


# In[1]:

def propagation(r, theta, plot='False'):
    import numpy as np
    
    
    R = 6371.0 #km
    Rp = 4*R/3
    ht  = np.zeros(shape=(len(r),len(theta)))
    rh  = np.zeros(shape=(len(r),len(theta)))
    lea  = np.zeros(shape=(len(r),len(theta)))

    for i in range(len(theta)):
        for j in range(len(r)):
            #Calcula la altura teniendo en cuenta la apróximación
            temp1 = np.sqrt(np.power(r[j], 2)+np.power(Rp, 2)+2*r[j]*Rp*np.sin(np.pi*theta[i]/180))-Rp
            ht[j,i] = temp1
            #Calcula el rango horizontal
            temp2 = r[j]*np.cos(np.pi*theta[i]/180)
            rh[j,i] = temp2
            #Calcula el angulo de elevación efectivo teniendo en cuenta la aproximación
            temp3 = np.pi*theta[i]/180 + np.arctan((r[j]*np.cos(np.pi*theta[i]/180))/(r[j]*np.sin(np.pi*theta[i]/180)+Rp))
            lea[j,i] = temp3
    
    if plot == 'True':
        import matplotlib.pyplot as plt
        # Grafico la propagación de los rayos
        f = plt.figure(figsize = [15,10])
        plt.plot(r,ht,'-b', label='rango')
        plt.plot(rh,ht,'-r', label='rango horizontal')
        plt.xlabel('Distancia al radar [km]',fontsize='16')
        plt.ylabel('Altura [km]',fontsize='16')
        plt.legend(loc='lower right')
        plt.xlim([0, 250])
        plt.ylim([0, 14])
        plt.xticks(np.arange(0, 260, 10.0),fontsize='11')
        plt.yticks(np.arange(0, 16, 1.0),fontsize='11')
        plt.grid()
        plt.show()
        
    return r, ht, rh, lea


 

