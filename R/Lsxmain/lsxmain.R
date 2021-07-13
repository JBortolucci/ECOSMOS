

lsxmain <- function(time_param = 0, day, month, year, jday = 0) {
  
  environment(setsoi) <- env
  environment(fwetcal) <- env
  environment(solset) <- env
  environment(solsur) <- env
  environment(solalb) <- env
  environment(solarf) <- env
  environment(irrad) <- env
  environment(cascade) <- env
  environment(canini) <- env
  environment(drystress) <- env
  environment(turcof) <- env
  environment(turvap) <- env
  environment(cascad2) <- env
  environment(noveg) <- env
  environment(Stomata2) <- env
  
  environment(soilctl) <- env
  # environment(soilctlR) <- env
  
  setsoi(env)
  
  fwetcal(env)
  
  solset(env)
  
  for(ib in seq(1,nband)) {
    
    solsur(env, ib)
    solalb(env, ib)
    solarf(env, ib)
    
  }

  irrad(env)
  
  cascade(env)
  
  fwetcal(env)
  
  # canopy begin
  canini(env, jday)
  
  drystress(env, jday, time_param)

  niter <- 3
  for(iter in seq(1,niter)) {
    
    #turcofDLL(iter, time_param, jday)  
    turcof(env, iter, time_param, jday)
    
    # PFT_UPDATE: Fazer troca dependendo do tipo de planta (c3/c4) (tem que ver qual a diferença entre os outros tipos)
    #stomataDLL(iter, time_param, jday) 
    # stomata(iter, time_param, jday) 
    # StomataCrops(iter, time_param, jday)
    for(i in seq(1,npft)) {
      if(!plantList[[i]]$active) next
      Stomata2(i)
    }
    
    # na rotina turvap algumas variáveis são salvas para a próxima iteração deste laço
    # por conta da dll não é possível salvar esses valores.
    # Criar variáveis locais no inicio do laço e passar como parâmetro para rotina.
    # turvapDLL(iter, niter)
    turvap(env, iter, niter)
    
  }
  
  # canopy end
  cascad2(env)
  
  noveg(env)
  
  # set net surface heat fluxes for soil and snow models
  heatg <- solg + firg - fseng - hvasug * fvapg
  heati <- soli + firi - fseni - hvasui * fvapi
  assign("heatg", heatg, envir = env)
  assign("heati", heati, envir = env)
  
  # snow() # not implemented yet.
  
  soilctl() # the compiled function remains to this model.
  # soilctlR()
  
  
}
