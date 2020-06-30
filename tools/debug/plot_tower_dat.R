plot_tower <- function(dat_R, dat_fortran, columns) {
  nm <- c("year", "gday", "plai_x_grnfraccrop", "plai", "swin", "swout", 
          "pari", "paro", "apar", "rn_x_dtimes", "fsena1_x_dtimes",
          "fvapa1_x_hvap_x_dtime", "soihfl1_x_dtime", "solsoi1_x_dtime" ,"tn_ee_tot1",
          "cbior_/_cfrac", "cbiol_/_cfrac", "cbios_/_cfrac", "cbiog_/_cgrain",
          "gsuvap1_x_dtime", "gtrans1_x_dtime", "tsoi1", "tsoi12", "tsoi15", "wsoi12",
          "wsoi14", "wsoi16", "wsoi18", "wsoi110")
  tower_R <- read.table(dat_R, header = F, sep = ",", stringsAsFactors = F, col.names = nm)
  tower_F <- read.table(dat_fortran, header = F, sep = ",", stringsAsFactors = F, col.names = nm)
  
  for(col in columns)
    plot(tower_F[[col]] - tower_R[[col]], type = "l", main = nm[col])
  
  return(list(tower_R = tower_R, tower_F = tower_F))
}

res <- plot_tower("tools/debug/out_daily_tower_R.dat", "tools/debug/out_daily_tower_fortran.dat", c(1:29))
tower_R <- res$tower_R
tower_F <- res$tower_F