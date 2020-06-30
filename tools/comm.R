##### Comment Variables tools/out.R #####

# Global Vars:
# a10ancl3       # 10-day average canopy photosynthesis rate - c3 grasses (mol_co2 m-2 s-1)
# a10ancl4       # 10-day average canopy photosynthesis rate - c4 grasses (mol_co2 m-2 s-1)
# a10ancls       # 10-day average canopy photosynthesis rate - shrubs (mol_co2 m-2 s-1)
# a10ancub       # 10-day average canopy photosynthesis rate - broadleaf (mol_co2 m-2 s-1)
# a10ancuc       # 10-day average canopy photosynthesis rate - conifer (mol_co2 m-2 s-1)
# a10daylightl   # 10-day average day-time PAR - lower canopy (micro-Ein m-2 s-1)
# a10daylightu   # 10-day average day-time PAR - upper canopy (micro-Ein m-2 s-1)
# a10scalparaml  # 10-day average day-time scaling parameter - lower canopy (dimensionless)
# a10scalparamu  # 10-day average day-time scaling parameter - upper canopy (dimensionless)
# a10td          # 10-day average daily air temperature (K)
# a11soiltd      # 11-day average daily soil temperature (K)
# a3tdmin        # 3-day running average minimum air temperature (deg C)
# ancl3          # canopy average net photosynthesis rate - c3 grasses   (mol_co2 m-2 s-1)
# ancl4          # canopy average net photosynthesis rate - c4 grasses   (mol_co2 m-2 s-1)
# ancls          # canopy average net photosynthesis rate - shrubs       (mol_co2 m-2 s-1)
# ancub          # canopy average net photosynthesis rate - broadleaf    (mol_co2 m-2 s-1)
# ancuc          # canopy average net photosynthesis rate - conifer      (mol_co2 m-2 s-1)
# beta1          # parameter for Jackson rooting profile (lower canopy)
# beta2          # parameter for Jackson rooting profile (upper canopy)
# biomass        # total biomass of each plant functional type  (kg_C m-2)
# cbiog          # carbon in grain biomass pool (kg_C m-2)
# cbiol          # carbon in leaf biomass pool (kg_C m-2)
# cbior          # carbon in fine root biomass pool (kg_C m-2)
# cbios          # carbon in stem biomass pool (kg_C m-2)
# cbiow          # carbon in woody biomass pool (kg_C m-2)
# cnroot         # cn ratio of plant roots 
# cntops         # cn ratio of plant residue
# croplive       # 0 crops have been planted and living : 1 crops not living  
# daylength      # length of day (minutes)
# exist          # probability of existence of each plant functional type in a gridcell
# falll          # annual leaf litter fall                      (kg_C m-2/year)
# fallr          # annual root litter input                     (kg_C m-2/year)
# fallw          # annual wood litter fall                      (kg_C m-2/year)
# fl             # fraction of snow-free area covered by lower  canopy
# froot          # fraction of root in soil layer 
# fu             # fraction of overall area covered by upper canopy
# harvdate       # day of year that crop pft was harvested
# hsoi           # soil layer thickness (m)
# icropsum       # index - number of crop types planted in each grid cell
# iwheat         # 0: wheat not planted 1: spring wheat planted 2: winter wheat (ibis.infile)
# lai            # canopy single-sided leaf area index (area leaf/area veg)
# orieh          # fraction of leaf/stems with horizontal orientation
# oriev          # fraction of leaf/stems with vertical
# plai           # total leaf area index of each plant functional type
# plai_init      # initial total LAI for each vegtype (used in iniveg)
# plailower      # Potental LAI of lower canopy (uniform initial vegetation)
# plaiupper      # Potental LAI of upper canopy (uniform initial vegetation)
# precipsum      # precipitation summation for lower canopy onset (cm)
# sai            # current single-sided stem area index
# sapfrac        # fraction of woody biomass that is in sapwood
# sapfrac_init   # Initial value of sapwood fraction used for all woody PFTs
# specla         # specific leaf area (m**2/kg)
# stresstl       # sum of stressl over all 6 soil layers (dimensionless)
# stresstu       # sum of stressu over all 6 soil layers (dimensionless)
# stsuml         # soil temperature summation for lower canopy (deg C)
# stsumu         # soil temperature summation for upper canopy (deg C)
# td             # daily average temperature (K)
# tnplant        # total nitrogen in plant dry matter
# totbiol        # total biomass in the lower canopy (kg_C m-2)
# totbiou        # total biomass in the upper canopy (kg_C m-2)
# totlail        # total leaf area index for the lower canopy
# totlaiu        # total leaf area index for the upper canopy
# tw             # warmest monthly temperature (C)
# vegtype0       # annual vegetation type - ibis classification
# woodnorm       # value of woody biomass for upper canopy closure (ie when wood = woodnorm fu = 1.0) (kg_C m-2)
# xinveg         # fixed vegetation map
# xminlai        # Minimum LAI for each existing PFT
# za             # height above the surface of atmospheric forcing (m)
# zbot           # height of lowest branches above ground (m)
# ztop           # height of plant top above ground (m)
# ztopmxsgc      # height maximum (m) for sugarcane

# Global Assigns tools/out.R 

assign("cntops", cntops, envir = globalenv())
assign("cnroot", cnroot, envir = globalenv())
assign("tnplant", tnplant, envir = globalenv())
assign("harvdate", harvdate, envir = globalenv())
assign("stresstu", stresstu, envir = globalenv())
assign("stresstl", stresstl, envir = globalenv())
assign("a10td", a10td, envir = globalenv())
assign("a11soiltd", a11soiltd, envir = globalenv())
assign("daylength", daylength, envir = globalenv())
assign("a3tdmin", a3tdmin, envir = globalenv())
assign("precipsum", precipsum, envir = globalenv())
assign("stsumu", stsumu, envir = globalenv())
assign("stsuml", stsuml, envir = globalenv())
assign("a10ancub", a10ancub, envir = globalenv())
assign("a10ancuc", a10ancuc, envir = globalenv())
assign("a10ancls", a10ancls, envir = globalenv())
assign("a10ancl4", a10ancl4, envir = globalenv())
assign("a10ancl3", a10ancl3, envir = globalenv())
assign("a10scalparamu", a10scalparamu, envir = globalenv())
assign("a10scalparaml", a10scalparaml, envir = globalenv())
assign("a10daylightu", a10daylightu, envir = globalenv())
assign("a10daylightl", a10daylightl, envir = globalenv())
assign("falll", falll, envir = globalenv())
assign("fallr", fallr, envir = globalenv())
assign("fallw", fallw, envir = globalenv())
assign("croplive", croplive, envir = globalenv())
assign("vegtype0", vegtype0, envir = globalenv())
assign("plai", plai, envir = globalenv())
assign("sapfrac", sapfrac, envir = globalenv())
assign("cbiol", cbiol, envir = globalenv())
assign("cbior", cbior, envir = globalenv())
assign("cbiow", cbiow, envir = globalenv())
assign("cbios", cbios, envir = globalenv())
assign("cbiog", cbiog, envir = globalenv())
assign("biomass", biomass, envir = globalenv())
assign("totlaiu", totlaiu, envir = globalenv())
assign("totlail", totlail, envir = globalenv())
assign("totbiou", totbiou, envir = globalenv())
assign("totbiol", totbiol, envir = globalenv())
assign("sai", sai, envir = globalenv())
assign("fu", fu, envir = globalenv())
assign("fl", fl, envir = globalenv())
assign("lai", lai, envir = globalenv())
assign("za", za, envir = globalenv())
assign("zbot", zbot, envir = globalenv())
assign("ztop", ztop, envir = globalenv())
assign("exist", exist, envir = globalenv())
assign("oriev", oriev, envir = globalenv())
assign("orieh", orieh, envir = globalenv())
assign("froot", froot, envir = globalenv())
