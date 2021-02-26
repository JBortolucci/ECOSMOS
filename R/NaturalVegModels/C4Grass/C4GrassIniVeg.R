
C4GrassIniVeg <- function(jday, i) {
  
  if (plantList[[i]]$active & croplive[i] != 1) {
    
    a10td[i] <- td
    
    croplive[i] <- 1
  
    iniplai <- list('SAVANA'  = 2.00,
                    'GRASS'   = 2.50,
                    'SHRUD'   = 0.50,
                    'SHRUO'   = 0.25,
                    'TUNDRA'  = 1.00,
                    'DESERT'  = 0.05,
                    'DESERTP' = 0.05)
    
    plai[i] <- iniplai[[biomeName]]
    
    sapfrac <- sapfrac_init
    
    wood <- 0.0
    
    cbiol[i] <- plai[i] / specla[i]
    cbior[i] <- 0.5 * cbiol[i]
    cbiow[i] <- 0.0
    cbios[i] <- 0.0
    cbiog[i] <- 0.0
    
    biomass[i] <- cbiol[i] + cbiow[i] + cbior[i] + cbios[i] + cbiog[i]
    
    greenfrac[i] <- 1.0
    
    flagl4 <- list('on' = F, 'off' = T)
    
    assign('plai'      , plai      , envir = env)
    assign('sapfrac'   , sapfrac   , envir = env)
    assign('cbiol'     , cbiol     , envir = env)
    assign('cbior'     , cbior     , envir = env)
    assign('cbiow'     , cbiow     , envir = env)
    assign('cbios'     , cbios     , envir = env)
    assign('cbiog'     , cbiog     , envir = env)
    assign('wood'      , wood      , envir = env)
    assign('biomass'   , biomass   , envir = env)
    assign('greenfrac' , greenfrac , envir = env)
    assign('croplive'  , croplive  , envir = env)
    assign('a10td'     , a10td     , envir = env)
    assign('flagl4'    , flagl4    , envir = env)
    assign('Deadleaves', Deadleaves, envir = env)
    assign('Deadfroots', Deadfroots, envir = env)
    
  }
  
}