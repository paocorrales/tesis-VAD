
# coding: utf-8


###################################################################
#                      Low Pass Filter Function                   #                
#                      ========================                   #
#                                                                 #
# After Gao et.al. 2014                                           #
#                                                                 #
# Imputs:                                                         #
#     data = array with the data to be filtering                  #
#     N = numbers of poits to be use in the filter, must be even. #
#                                                                 # 
# Output:                                                         #
#     object type array                                           #                
###################################################################


def lowpassfl(data, N):
    
    import numpy as np

    if N%2 != 0:
        return 'N must be even!'
        
    
    vector = np.zeros(len(data)+N)*np.nan
    for i in range(N/2, len(data)+N/2):
        vector[i] = data[i-N/2]
    for i in range(0, N/2):
        vector[i] = data[abs(N/2-len(data))+i]
        vector[len(data)+N/2+i] = data[i]
    
    # Weights
    W = np.array(range(0, N+1))
    W = 1 - abs(W -(N/2))/float(N)
    W_total = sum(W)
    
    Vda_filter = np.zeros(len(data))*np.nan
    
    for i in range(N/2, len(vector)-N/2):
        vector_sub = vector[(i-(N/2)):(i+(N/2)+1)]
        
        if np.isnan(vector_sub[N/2]):
            continue
            
        if np.isnan(vector_sub).sum() > (N/2+1):
            continue
            
        else: 
            Vda_filter[i-N/2] = np.nansum(W*vector_sub)/W_total
        
            
    return Vda_filter
    

