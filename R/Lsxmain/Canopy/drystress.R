# modified by CJK 1/25/2005 for dynamic root water uptake adjustment
# approach of K. Li

drystressR <- function(envi, time,jday) {
  
  # stressfac determines the 'strength' of the soil moisture
  # stress on physiological processes
  # 
  # strictly speaking, stresst* is multiplied to net photosynthesis 
  # parameters used in the photosynthesis calculations
  # 
  # stressfac determines the shape of the soil moisture response
  # 
  # stressfac = -5.0
  #  
  # znorm = 1.0 - exp(stressfac)
  
  # initialize stress parameter
  
  stresstl <- 0.0
  stresstu <- 0.0
  
  # set values of lambda
  # 
  # sdep = 0.    ! total soil depth
  # do 105 k = 1, nsoilay
  # sdep = sdep + hsoi(k) 
  # 105    continue 
  # 
  # tthird = sdep * 0.3333   ! calculate 1/3 of total depth
  # sdep = 0.
  # klay = 0
  # do 110 k = 1, nsoilay
  # sdep = sdep + hsoi(k)
  # if (sdep .le. tthird) klay = k
  # 110    continue      
  # 
  # topw   = 0.
  # botw   = 0.
  # topmxw = 0.
  # botmxw = 0.
  # topawc = 0.
  # botawc = 0.
  # do 120 k = 1, klay 
  # topw   = topw + (wsoi(i,k)*(1-wisoi(i,k)) - swilt(i,k)) 
  # topmxw = topmxw + (sfield(i,k) - swilt(i,k)) 
  #  120    continue
  # topawc = topw / topmxw 
  # 
  # do 130 k = klay+1, nsoilay 
  # botw   = botw + (wsoi(i,k)*(1-wisoi(i,k)) - swilt(i,k)) 
  # botmxw = botmxw + (sfield(i,k) - swilt(i,k)) 
  #  130    continue
  # botawc = botw / botmxw 
  #       
  #  assign lambda values
  # 
  # if (topawc .lt. 0.20 .and. botawc .gt. 0.5) then
  # lambda = 0.50 
  # else if (topawc .lt. 0.20 .and.
  # >           (botawc .ge. 0.2 .and. botawc .le. 0.5)) then
  # lambda = 0.75
  # else if ((topawc .ge. 0.2 .and. topawc .le. 0.5) .and.
  # >            botawc .gt. 0.5) then
  # lambda = 0.75         
  # else if ((topawc .ge. 0.2 .and. topawc .le. 0.5) .and.
  # >            botawc .lt. 0.2) then
  # lambda = 1.25         
  # else if (topawc .gt. 0.5 .and.
  # >           ( botawc .ge. 0.2 .and. botawc .le. 0.5)) then
  # lambda = 1.25         
  # else if (topawc .gt. 0.5 .and. botawc .lt. 0.2) then
  # lambda = 1.50         
  # else
  # lambda = 1.00
  # endif
  # 
  # fraction of soil water uptake in each layer
  # 
  for (k in 1:nsoilay) {
    
    awc <- min (1.0, max (0.0, (wsoi[k]*(1 - wisoi[k]) - swilt[k]) / (sfield[k] - swilt[k])))
    
    # original IBIS formulation for zwilt 
    # 
    # zwilt = (1. - exp(stressfac * awc)) / znorm
    # 
    #  
    # J. Norman soil water availability factor (zwilt) 
    # 1/25/2004
    
    # zwilt <- 1.0 - (log(1+900.0*exp(-20.0*awc))/log(900.))  
    # zwilt <- 1.0 - (log(1+900.0*exp(-10.0*awc))/log(900.))
    
    # WARNING: Escrito manualmente para testar a vegetação natural. Está como parâmetro no plant_params. Verificar se deve ser parãmetro global ou local.
    # TODO: Verificar se o parametro é específico ou global (agora parece que é global)
    # stressBeta0 <- -20
    # stressBeta1 <- 900
    
    zwilt <- 1.0 - (log(1+stressBeta1*exp(stressBeta0[1]*awc))/log(stressBeta1[1]))
    
    # update for each layer
    stressl[k] <- froot[k,1] * max (0.0, min (1.0, zwilt))
    stressu[k] <- froot[k,2] * max (0.0, min (1.0, zwilt))
    
    # cstressl[k] = froot(k,1)^lambda * max (0.0, min (1.0, zwilt))
    # cstressu[k] = froot(k,2)^lambda * max (0.0, min (1.0, zwilt))
    # 
    # calculate maximum dimensionless water uptake rate (ranging from 0-1)
    #  
    # mxwrate = 1. - (1 + 1.3 * awc)**(-bex(i,k)) 
    # stressl(i,k) = stressl(i,k) * mxwrate
    # stressu(i,k) = stressu(i,k) * mxwrate
    # 
    # integral over rooting profile
    # 
    stresstl <- stresstl + stressl[k]
    
    stresstu <- stresstu + stressu[k]
    # stresstl(i) = 1.0   !No Stress effect due to moisture
    
  }
  
  # sant	print*, sfield(1,1), swilt(1,1),sfield(1,9), swilt(1,9)
  
  # if(time == 39600)
  # >  write(225,34)jday,wsoi(1,1),wsoi(1,2),wsoi(1,3),wsoi(1,4),wsoi(1,5),wsoi(1,6),
  # >wsoi(1,7),wsoi(1,8),wsoi(1,9),wsoi(1,10),wsoi(1,11), sfield(1,11),swilt(1,11)
  
  # if(time.eq.39600)
  # >write(225,34)jday,stressl(1,1),stressl(1,2),stressl(1,3),stressl(1,4),stressl(1,5)
  # >,stressl(1,6),stressl(1,7),stressl(1,8),stressl(1,9),stressl(1,10),stressl(1,11),stresstl(1)
  
  # 34   format (1x,i3,1x,13(1x,f4.2))
  
  # To do Henrique : if crop ==soybean stresstl= CROPGRO ???
  stresstl = 1
    
    
  assign("stresstl", stresstl, envir = env)
  assign("stresstu", stresstu, envir = env)
  assign("stressl", stressl, envir = env)
  assign("stressu", stressu, envir = env)
  
  # return to main program
}
