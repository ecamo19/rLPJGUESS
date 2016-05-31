#Internal data
#setwd("/home/trashtos/GitHub/ownBranches/RLPJ/Rlpj")
# list.files
files <- list.files("./data-raw", full.names = TRUE)
# open globals and save them
files.global <- sort(files[grepl("global", files)])
# create global object
global <- vector("list", length(files.global))
names(global) <- gsub(".ins","", gsub("./data-raw/", "", files.global))
for (i in 1:length(global)){
  global[[i]] <- readLines(files.global[i])
}
# open globals and save them
files.europe <- sort(files[grepl("europe", files)])
# create global object
europe <- vector("list", length(files.europe))
names(europe) <- gsub(".ins","", gsub("./data-raw/", "", files.europe))
for (i in 1:length(europe)){
  europe[[i]] <- readLines(files.europe[i])
}
#
templates <- list("global"= global, "europe" = europe)

rm(global)
rm(europe)

# Typelist
typelist.default <- c("aaet", "agpp", "aiso", "amon", "anpp", "cflux","clitter",
                      "cmass", "cpool", "cton_leaf", "dens", "firert", "fpc",
                      "speciesheight", "lai", "maet", "mevap", "mgpp","mintercep",
                      "miso", "mlai", "mmon", "mnee", "mnpp", "mpet", "mra", "mrh",
                      "mrunoff", "mwcont_lower", "mwcont_upper", "nflux", "ngases",
                      "nlitter", "nmass", "npool", "nsources", "nuptake", "runoff",
                      "vmaxnlim")

# default parameters
files.parameters <- list(cf = NULL, cru = NULL)
files.parameters$cf <- list(file.co2 = c("path_to_co2_1901-2006.txt", NULL),
                           file.cru = c("path_to_cru_1901_2006.bin", NULL),
                           file.cru.misc = c("path_to_cru_1901_2006misc.bin", NULL),
                           file.ndep = c("path_to_GlobalNitrogenDeposition.bin", NULL),
                           file.temp = c("path_to_temp.nc", NULL),
                           file.prec = c("path_to_prec.nc", NULL),
                           file.insol = c("path_to_rad.nc", NULL))

files.parameters$cru <- list(file.co2 = c("path_to_co2_1901-2006.txt", NULL),
                           file.cru = c("path_to_cru_1901_2006.bin", NULL),
                           file.cru.misc = c("path_to_cru_1901_2006misc.bin", NULL),
                           file.ndep = c("path_to_GlobalNitrogenDeposition.bin" , NULL))

#Sometimes functions need pre-computed data tables.
# If you put these in data/ they’ll also be available to package users, which is not appropriate.
# Instead, you can save them in R/sysdata.rda. For example, two coloured related packages,
# munsell and dichromat, use R/sysdata.rda to store large tables of colour data.
# You can use devtools::use_data() to create this file with the argument internal = TRUE:
#   x <- sample(1000)
#   devtools::use_data(x, mtcars, internal = TRUE)
# Again, to make this data reproducible it’s a good idea to include the code used to generate it.
# Put it in data-raw/.
# Objects in R/sysdata.rda are not exported (they shouldn’t be), so they don’t need to be documented.
# They’re only available inside your package.

parameterList.default <- vector("list", 2)
names(parameterList.default) <- c("global","europe")

# Checking paramaterList and if null, setting to default values
# 256 physiological parameters can be calibrated right now. Exceptions are the fine roots distributions for all PFTs
global <- list()
# Parameters common to all PFTs
global[["run_lamda_max"]] <- 0.8
global[["run_emax"]] <- 5
global[["run_reprfrac"]] <- 0.1
global[["run_wscal_min"]] <- 0.35
global[["run_drought_tolerance"]] <- 0.0001
global[["run_turnover_harv_prod"]] <- 1
  # Parameters common to all trees
global[["tree_crownarea_max"]] <- 50
global[["tree_ltor_max"]] <- 1
global[["tree_turnover_root"]] <- 0.7
global[["tree_k_allom2"]] <- 60
global[["tree_k_allom3"]] <- 0.67
global[["tree_k_rp"]] <- 1.6
global[["tree_wooddens"]] <- 200
global[["tree_cton_root"]] <- 29
global[["tree_cton_sap"]] <- 330
global[["tree_nuptoroot"]] <- 0.0028
global[["tree_km_volume"]] <- 0.000001477
global[["tree_respcoeff"]] <- 1.0
global[["tree_kest_repr"]] <- 200
global[["tree_kest_bg"]] <- 0.1
global[["tree_kest_pres"]] <- 1
global[["tree_k_chilla"]] <- 0
global[["tree_k_chillb"]] <- 100
global[["tree_k_chillk"]] <- 0.05
global[["tree_litterme"]] <- 0.3
global[["tree_harv_eff"]] <- 0.7
global[["tree_res_outtake"]] <- 0.75
global[["tree_harvest_slow_frac"]] <- 0.33
global[["tree_turnover_harv_prod"]] <- 0.04
  # Parameters specific for shrubs
global[["shrub_crownarea_max"]] <- 10
global[["shrub_ltor_max"]] <- 1
global[["shrub_k_allom1"]] <- 100
global[["shrub_k_allom2"]] <- 5
global[["shrub_k_allom3"]] <- 0.67
global[["shrub_k_rp"]] <- 1.6
global[["shrub_wooddens"]] <- 250
global[["shrub_cton_leaf_min"]] <- 16
global[["shrub_cton_root"]] <- 29
global[["shrub_cton_sap"]] <- 330
global[["shrub_nuptoroot"]] <- 0.0028
global[["shrub_km_volume"]] <- 0.000001477
global[["shrub_fnstorage"]] <- 0.3
global[["shrub_kest_repr"]] <- 200
global[["shrub_kest_bg"]] <- 0.1
global[["shrub_kest_pres"]] <- 1
global[["shrub_litterme"]] <- 0.3
global[["shrub_longevity"]] <- 100
  # Parameters common to all grasses
global[["grass_ltor_max"]] <- 0.5
global[["grass_gmin"]] <- 0.5
global[["grass_phengdd5ramp"]] <- 100
global[["grass_leaflong"]] <- 0.5
global[["grass_turnover_leaf"]] <- 1
global[["grass_turnover_root"]] <- 0.7
global[["grass_cton_leaf_min"]] <- 16
global[["grass_cton_root"]] <- 29
global[["grass_nuptoroot"]] <- 0.00551
global[["grass_km_volume"]] <- 0.000001876
global[["grass_fnstorage"]] <- 0.3
global[["grass_litterme"]] <- 0.2
global[["grass_parff_min"]] <- 1000000
global[["grass_fireresist"]] <- 0.5
global[["grass_intc"]] <- 0.01
global[["grass_ga"]] <- 0.030
  # Parameters common to all broadleaved trees
global[["broadleaf_cton_leaf_min"]] <- 16
global[["broadleaf_k_allom1"]] <- 250
global[["broadleaf_k_latosa"]] <- 6000
global[["broadleaf_gmin"]] <- 0.5
global[["broadleaf_intc"]] <- 0.02
global[["broadleaf_ga"]] <- 0.040
  # Parameters common to all needleleaved trees
global[["needleleaf_cton_leaf_min"]] <- 28
global[["needleleaf_k_allom1"]] <- 150
global[["needleleaf_k_latosa"]] <- 5000
global[["needleleaf_gmin"]] <- 0.3
global[["needleleaf_intc"]] <- 0.06
global[["needleleaf_ga"]] <- 0.140
 # Parameters common to all evergreen trees
global[["evergreen_fnstorage"]] <- 0.05
global[["evergreen_phengdd5ramp"]] <- 0
  # Parameters common to all summergreen trees
global[["summergreen_fnstorage"]] <- 0.15
global[["summergreen_phengdd5ramp"]] <- 200
global[["summergreen_leaflong"]] <- 0.5
global[["summergreen_turnover_leaf"]] <- 1
  # Parameters common to all boreal trees
global[["boreal_pstemp_min"]] <- -4
global[["boreal_pstemp_low"]] <- 10
global[["boreal_pstemp_high"]] <- 25
global[["boreal_pstemp_max"]] <- 38
global[["boreal_respcoeff"]] <- 1.0
  # Parameters common to all temperate trees
global[["temperate_pstemp_min"]] <- -2
global[["temperate_pstemp_low"]] <- 15
global[["temperate_pstemp_high"]] <- 25
global[["temperate_pstemp_max"]] <- 38
global[["temperate_respcoeff"]] <- 1.0
  # Parameters common to all tropical trees
global[["tropical_tcmin_surv"]] <- 15.5
global[["tropical_tcmin_est"]] <- 15.5
global[["tropical_tcmax_est"]] <- 1000
global[["tropical_twmin_est"]] <- -1000
global[["tropical_gdd5min_est"]] <- 0
global[["tropical_pstemp_min"]] <- 2
global[["tropical_pstemp_low"]] <- 25
global[["tropical_pstemp_high"]] <- 30
global[["tropical_pstemp_max"]] <- 55
global[["tropical_respcoeff"]] <- 0.15
  # Parameters common to all shade tolerant trees
global[["ShTol_est_max"]] <- 0.05
global[["ShTol_parff_min"]] <- 350000
global[["ShTol_alphar"]] <- 3.0
global[["ShTol_greff_min"]] <- 0.04
global[["ShTol_turnover_sap"]] <- 0.05
  # Parameters common to all intermediate shade tolerant trees
global[["IntShTol_est_max"]] <- 0.15
global[["IntShTol_parff_min"]] <- 2000000
global[["IntShTol_alphar"]] <- 7.0
global[["IntShTol_greff_min"]] <- 0.06
global[["IntShTol_turnover_sap"]]<- 0.075
  # Parameters common to all shade intolerant trees
global[["ShIntol_est_max"]] <- 0.2
global[["ShIntol_parff_min"]] <- 2500000
global[["ShIntol_alphar"]] <- 10.0
global[["ShIntol_greff_min"]] <- 0.08
global[["ShIntol_turnover_sap"]] <- 0.1
  # Boreal needleleaved evergreen tree
global[["BNE_include"]] <- 1
global[["BNE_leaflong"]] <- 3
global[["BNE_turnover_leaf"]] <- 0.33
global[["BNE_tcmin_surv"]] <- -31
global[["BNE_tcmin_est"]] <- -30
global[["BNE_tcmax_est"]] <- -1
global[["BNE_twmin_est"]] <- 5
global[["BNE_gdd5min_est"]] <- 500
global[["BNE_longevity"]] <- 500
global[["BNE_fireresist"]] <- 0.3
global[["BNE_eps_iso"]] <- 8.0
global[["BNE_seas_iso"]] <- 0
global[["BNE_eps_mon"]] <- 4.8
global[["BNE_storfrac_mon"]] <- 0.5
  # Boreal needleleaved evergreen tree
global[["BINE_include"]] <- 1
global[["BINE_leaflong"]] <- 3
global[["BINE_turnover_leaf"]] <- 0.33
global[["BINE_tcmin_surv"]] <- -31
global[["BINE_tcmin_est"]] <- -30
global[["BINE_tcmax_est"]] <- -1
global[["BINE_twmin_est"]] <- 5
global[["BINE_gdd5min_est"]] <- 500
global[["BINE_longevity"]] <- 500
global[["BINE_fireresist"]] <- 0.3
global[["BINE_eps_iso"]] <- 8.0
global[["BINE_seas_iso"]] <- 0
global[["BINE_eps_mon"]] <- 4.8
global[["BINE_storfrac_mon"]] <- 0.5
  # Boreal needleleaved summergreen tree
global[["BNS_include"]] <- 1
global[["BNS_tcmin_surv"]] <- -1000
global[["BNS_tcmin_est"]] <- -1000
global[["BNS_tcmax_est"]] <- -2
global[["BNS_twmin_est"]] <- -1000
global[["BNS_twminusc"]] <- 43
global[["BNS_gdd5min_est"]] <- 350
global[["BNS_phengdd5ramp"]] <- 100
global[["BNS_longevity"]] <- 300
global[["BNS_fireresist"]] <- 0.3
global[["BNS_eps_iso"]] <- 8.0
global[["BNS_seas_iso"]] <- 1
global[["BNS_eps_mon"]] <- 4.8
global[["BNS_storfrac_mon"]] <- 0.5
  # Temperate needleleaved evergreen tree
global[["TeNE_include"]] <- 1
global[["TeNE_leaflong"]] <- 3
global[["TeNE_turnover_leaf"]] <- 0.33
global[["TeNE_tcmin_surv"]] <- -2
global[["TeNE_tcmin_est"]] <- -2
global[["TeNE_tcmax_est"]] <- 10
global[["TeNE_twmin_est"]] <- 5
global[["TeNE_gdd5min_est"]] <- 2000
global[["TeNE_longevity"]] <- 300
global[["TeNE_fireresist"]] <- 0.3
global[["TeNE_eps_iso"]] <- 8.0
global[["TeNE_seas_iso"]] <- 0
global[["TeNE_eps_mon"]] <- 4.8
global[["TeNE_storfrac_mon"]] <- 0.5
  # Shade-tolerant temperate broadleaved summergreen tree
global[["TeBS_include"]] <- 1
global[["TeBS_tcmin_surv"]] <- -14
global[["TeBS_tcmin_est"]]<- -13
global[["TeBS_tcmax_est"]] <- 6
global[["TeBS_twmin_est"]] <- 5
global[["TeBS_gdd5min_est"]] <- 1100
global[["TeBS_longevity"]] <- 400
global[["TeBS_fireresist"]] <- 0.1
global[["TeBS_eps_iso"]] <- 45.0
global[["TeBS_seas_iso"]] <- 1
global[["TeBS_eps_mon"]] <- 1.6
global[["TeBS_storfrac_mon"]] <- 0.
  # Shade-intolerant broadleaved summergreen tree
global[["IBS_include"]] <- 1
global[["IBS_tcmin_surv"]] <- -30
global[["IBS_tcmin_est"]] <- -30
global[["IBS_tcmax_est"]] <- 7
global[["IBS_twmin_est"]] <- -1000
global[["IBS_gdd5min_est"]] <- 350
global[["IBS_longevity"]] <- 300
global[["IBS_fireresist"]] <- 0.1
global[["IBS_eps_iso"]] <- 45.0
global[["IBS_seas_iso"]] <- 1
global[["IBS_eps_mon"]] <- 1.6
global[["IBS_storfrac_mon"]] <- 0.
  # Temperate broadleaved evergreen tree
global[["TeBE_include"]] <- 1
global[["TeBE_leaflong"]] <- 3
global[["TeBE_turnover_leaf"]] <- 0.33
global[["TeBE_tcmin_surv"]] <- -1
global[["TeBE_tcmin_est"]] <- 0
global[["TeBE_tcmax_est"]] <- 18.8
global[["TeBE_twmin_est"]] <- 5
global[["TeBE_gdd5min_est"]] <- 2000
global[["TeBE_longevity"]] <- 300
global[["TeBE_fireresist"]] <- 0.3
global[["TeBE_eps_iso"]] <- 24.0
global[["TeBE_seas_iso"]] <- 0
global[["TeBE_eps_mon"]] <- 1.6
global[["TeBE_storfrac_mon"]] <- 0.
  # Tropical shade tolerant broadleaved evergreen tree
global[["TrBE_include"]] <- 1
global[["TrBE_leaflong"]] <- 2
global[["TrBE_turnover_leaf"]] <- 0.5
global[["TrBE_longevity"]] <- 500
global[["TrBE_fireresist"]] <- 0.1
global[["TrBE_eps_iso"]] <- 24.0
global[["TrBE_seas_iso"]] <- 0
global[["TrBE_eps_mon"]] <- 0.8
global[["TrBE_storfrac_mon"]] <- 0.
  # Tropical shade intolerant broadleaved evergreen tree
global[["TrIBE_include"]] <- 1
global[["TrIBE_leaflong"]] <- 2
global[["TrIBE_turnover_leaf"]] <- 0.5
global[["TrIBE_longevity"]] <- 200
global[["TrIBE_fireresist"]] <- 0.1
global[["TrIBE_eps_iso"]] <- 24.0
global[["TrIBE_seas_iso"]] <- 0
global[["TrIBE_eps_mon"]] <- 0.8
global[["TrIBE_storfrac_mon"]] <- 0.0
  # Tropical broadleaved raingreen tree
global[["TrBR_include"]] <- 1
global[["TrBR_fnstorage"]] <- 0.15
global[["TrBR_leaflong"]] <- 0.5
global[["TrBR_turnover_leaf"]] <- 1
global[["TrBR_longevity"]] <- 400
global[["TrBR_fireresist"]]<- 0.3
global[["TrBR_eps_iso"]] <- 45.0
global[["TrBR_seas_iso"]] <- 0
global[["TrBR_eps_mon"]] <- 2.4
global[["TrBR_storfrac_mon"]] <- 0.0
  # Cool (C3) grass
global[["C3G_include"]] <- 1
global[["C3G_respcoeff"]] <- 1.0
global[["C3G_pstemp_min"]] <- -5
global[["C3G_pstemp_low"]] <- 10
global[["C3G_pstemp_high"]] <- 30
global[["C3G_pstemp_max"]] <- 45
global[["C3G_tcmin_surv"]] <- -1000
global[["C3G_tcmin_est"]] <- -1000
global[["C3G_tcmax_est"]] <- 1000
global[["C3G_twmin_est"]] <- -1000
global[["C3G_gdd5min_est"]] <- 0
global[["C3G_eps_iso"]] <- 16.0
global[["C3G_seas_iso"]] <- 1
global[["C3G_eps_mon"]] <- 1.6
global[["C3G_storfrac_mon"]] <- 0.5
  # Warm (C4) grass
global[["C4G_include"]] <- 1
global[["C4G_respcoeff"]] <- 0.15
global[["C4G_pstemp_min"]] <- 6
global[["C4G_pstemp_low"]] <- 20
global[["C4G_pstemp_high"]] <- 45
global[["C4G_pstemp_max"]] <- 55
global[["C4G_tcmin_surv"]] <- 15.5
global[["C4G_tcmin_est"]] <- 15.5
global[["C4G_tcmax_est"]] <- 1000
global[["C4G_twmin_est"]] <- -1000
global[["C4G_gdd5min_est"]] <- 0
global[["C4G_eps_iso"]] <- 8.0
global[["C4G_seas_iso"]] <- 0
global[["C4G_eps_mon"]] <- 2.4
global[["C4G_storfrac_mon"]] <- 0.5
# Add to the list
parameterList.default[['global']] <- global


# Checking paramaterList and if null, setting to default values
# 585 physiological parameters, both general and species-specific, can be calibrated right now.
# Exceptions are the fine roots distributions for all species and PFTs
europe <-list()
  # Parameters common to all PFTs
europe[["run_lamda_max"]] <- 0.8
europe[["run_emax"]] <- 5
europe[["run_reprfrac"]] <- 0.1
europe[["run_wscal_min"]] <- 0.35
europe[["run_drought_tolerance"]] <- 0.0001
europe[["run_turnover_harv_prod"]] <- 1
  # Parameters common to all trees
europe[["tree_crownarea_max"]] <- 40
europe[["tree_turnover_root"]] <- 0.7
europe[["tree_ltor_max"]] <- 1
europe[["tree_k_allom2"]] <- 40
europe[["tree_k_allom3"]] <- 0.67
europe[["tree_k_rp"]] <- 1.6
europe[["tree_wooddens"]] <- 200
europe[["tree_cton_root"]] <- 29
europe[["tree_cton_sap"]] <- 330
europe[["tree_nuptoroot"]] <- 0.0028
europe[["tree_km_volume"]] <- 0.000001477
europe[["tree_kest_repr"]] <- 200
europe[["tree_kest_bg"]] <- 0.1
europe[["tree_kest_pres"]] <- 1
europe[["tree_litterme"]] <- 0.3
europe[["tree_harv_eff"]] <- 0.7
europe[["tree_res_outtake"]] <- 0.75
europe[["tree_harvest_slow_frac"]] <- 0.33
europe[["tree_turnover_harv_prod"]] <- 0.04
  # Parameters common to all shrubs
europe[["shrub_crownarea_max"]] <- 10
europe[["shrub_turnover_root"]] <- 0.7
europe[["shrub_ltor_max"]] <- 1
europe[["shrub_k_allom1"]] <- 100
europe[["shrub_k_allom2"]] <- 5
europe[["shrub_k_allom3"]] <- 0.67
europe[["shrub_k_rp"]] <- 1.6
europe[["shrub_wooddens"]] <- 250
europe[["shrub_cton_root"]] <- 29
europe[["shrub_cton_sap"]] <- 330
europe[["shrub_nuptoroot"]] <- 0.0028
europe[["shrub_km_volume"]] <- 0.000001477
europe[["shrub_fnstorage"]] <- 0.3
europe[["shrub_kest_repr"]] <- 200
europe[["shrub_kest_bg"]] <- 0.1
europe[["shrub_kest_pres"]] <- 1
europe[["shrub_litterme"]] <- 0.3
europe[["shrub_longevity"]] <- 100
  # Parameters common to all needleleaved trees
europe[["needlelleaf_k_allom1"]] <- 150
europe[["needlelleaf_k_latosa"]] <- 4000
europe[["needlelleaf_ga"]] <- 0.140
  # Parameters common to all boadleaved trees
europe[["broadleaf_k_allom1"]] <- 250
europe[["broadleaf_k_latosa"]] <- 5000
europe[["broadleaf_leaflong"]] <- 0.5
europe[["broadleaf_turnover_leaf"]] <- 1
europe[["broadleaf_ga"]] <- 0.040
  # Parameters common to all grasses
europe[["grass_ltor_max"]] <- 0.5
europe[["grass_cton_root"]] <- 29
europe[["grass_nuptoroot"]] <- 0.00551
europe[["grass_km_volume"]] <- 0.000001876
europe[["grass_fnstorage"]] <- 0.3
europe[["grass_respcoeff"]] <- 1.0
europe[["grass_litterme"]] <- 0.2
europe[["grass_ga"]] <- 0.030
  # Parameters common to all intermediate shade tolerant trees
europe[["IntShTol_est_max"]] <- 0.15
europe[["IntShTol_parff_min"]] <- 2000000
europe[["IntShTol_alphar"]] <- 7.0
europe[["IntShTol_greff_min"]] <- 0.06
europe[["IntShTol_turnover_sap"]] <- 0.075
  # Parameters common to all shade tolerant trees
europe[["ShTol_est_max"]] <- 0.05
europe[["ShTol_parff_min"]] <- 350000
europe[["ShTol_alphar"]] <- 3.0
europe[["ShTol_greff_min"]] <- 0.04
europe[["ShTol_turnover_sap"]] <- 0.05
  # Parameters common to all shade intolerant trees
europe[["ShIntol_est_max"]] <- 0.2
europe[["ShIntol_parff_min"]] <- 2500000
europe[["ShIntol_alphar"]] <- 10.0
europe[["ShIntol_greff_min"]] <- 0.08
europe[["ShIntol_turnover_sap"]] <- 0.1
  # Parameters common to all boreal trees
europe[["boreal_respcoeff"]] <- 1.0
europe[["boreal_pstemp_min"]] <- -4
europe[["boreal_pstemp_low"]] <- 10
europe[["boreal_pstemp_high"]] <- 25
europe[["boreal_pstemp_max"]] <- 38
  # Parameters common to all temperate trees
europe[["temperate_respcoeff"]] <- 1.0
europe[["temperate_pstemp_min"]] <- -2
europe[["temperate_pstemp_low"]] <- 15
europe[["temperate_pstemp_high"]] <- 25
europe[["temperate_pstemp_max"]] <- 38
  # Parameters specific to Abies alba
europe[["Abi_alb_include"]] <- 1
europe[["Abi_alb_sla"]] <- 9.3
europe[["Abi_alb_gmin"]] <- 0.3
europe[["Abi_alb_fnstorage"]] <- 0.05
europe[["Abi_alb_leaflong"]] <- 3
europe[["Abi_alb_turnover_leaf"]] <- 0.33
europe[["Abi_alb_phengdd5ramp"]] <- 0
europe[["Abi_alb_tcmin_surv"]] <- -2
europe[["Abi_alb_tcmin_est"]] <- -2
europe[["Abi_alb_tcmax_est"]] <- 2
europe[["Abi_alb_twmin_est"]] <- 6
europe[["Abi_alb_gdd5min_est"]] <- 1600
europe[["Abi_alb_k_chilla"]] <- 0
europe[["Abi_alb_k_chillb"]] <- 100
europe[["Abi_alb_k_chillk"]] <- 0.05
europe[["Abi_alb_fireresist"]] <- 0.1
europe[["Abi_alb_intc"]] <- 0.06
europe[["Abi_alb_longevity"]] <- 350
europe[["Abi_alb_drought_tolerance"]] <- 0.35
europe[["Abi_alb_eps_iso"]] <- 0.05
europe[["Abi_alb_seas_iso"]] <- 0
europe[["Abi_alb_eps_mon"]] <- 1.8
europe[["Abi_alb_storfrac_mon"]] <- 0.5
  # Parameters specific to boreal evergreen shrub
europe[["BES_include"]] <- 1
europe[["BES_crownarea_max"]] <- 3
europe[["BES_k_allom1"]] <- 20
europe[["BES_k_allom2"]] <- 5
europe[["BES_k_latosa"]] <- 500
europe[["BES_sla"]] <- 9.3
europe[["BES_gmin"]] <- 0.3
europe[["BES_leaflong"]] <- 2
europe[["BES_turnover_leaf"]] <- 0.5
europe[["BES_turnover_sap"]] <- 0.05
europe[["BES_phengdd5ramp"]] <- 0
europe[["BES_tcmin_surv"]] <- -1000
europe[["BES_tcmin_est"]] <- -1000
europe[["BES_tcmax_est"]] <- -1.0
europe[["BES_twmin_est"]] <- -1000
europe[["BES_gdd5min_est"]] <- 200
europe[["BES_longevity"]] <- 50
europe[["BES_k_chilla"]] <- 0
europe[["BES_k_chillb"]] <- 100
europe[["BES_k_chillk"]] <- 0.05
europe[["BES_fireresist"]] <- 0.1
europe[["BES_intc"]] <- 0.06
europe[["BES_drought_tolerance"]] <- 0.25
europe[["BES_eps_iso"]] <- 2.0
europe[["BES_seas_iso"]] <- 0
europe[["BES_eps_mon"]] <- 4.0
europe[["BES_storfrac_mon"]] <- 0.5
europe[["BES_ga"]] <- 0.040
  # Parameters specific to Betula pendula
europe[["Bet_pen_include"]] <- 1
europe[["Bet_pen_sla"]] <- 24.3
europe[["Bet_pen_gmin"]] <- 0.5
europe[["Bet_pen_fnstorage"]] <- 0.15
europe[["Bet_pen_phengdd5ramp"]] <- 200
europe[["Bet_pen_tcmin_surv"]] <- -30
europe[["Bet_pen_tcmin_est"]] <- -30
europe[["Bet_pen_tcmax_est"]] <- 7
europe[["Bet_pen_twmin_est"]] <- 5
europe[["Bet_pen_gdd5min_est"]] <- 700
europe[["Bet_pen_k_chilla"]] <- 0
europe[["Bet_pen_k_chillb"]] <- 350
europe[["Bet_pen_k_chillk"]] <- 0.05
europe[["Bet_pen_fireresist"]] <- 0.1
europe[["Bet_pen_intc"]] <- 0.02
europe[["Bet_pen_longevity"]] <- 200
europe[["Bet_pen_drought_tolerance"]] <- 0.42
europe[["Bet_pen_eps_iso"]] <- 0.2
europe[["Bet_pen_seas_iso"]] <- 1
europe[["Bet_pen_eps_mon"]] <- 6.
europe[["Bet_pen_storfrac_mon"]] <- 0.
  # Parameters specific to Betula pubescens
europe[["Bet_pub_include"]] <- 1
europe[["Bet_pub_sla"]] <- 24.3
europe[["Bet_pub_gmin"]] <- 0.5
europe[["Bet_pub_fnstorage"]] <- 0.15
europe[["Bet_pub_phengdd5ramp"]] <- 200
europe[["Bet_pub_tcmin_surv"]] <- -30
europe[["Bet_pub_tcmin_est"]] <- -30
europe[["Bet_pub_tcmax_est"]] <- 3
europe[["Bet_pub_twmin_est"]] <- 5
europe[["Bet_pub_gdd5min_est"]] <- 350
europe[["Bet_pub_k_chilla"]] <- 0
europe[["Bet_pub_k_chillb"]] <- 350
europe[["Bet_pub_k_chillk"]] <- 0.05
europe[["Bet_pub_fireresist"]] <- 0.1
europe[["Bet_pub_intc"]] <- 0.02
europe[["Bet_pub_longevity"]] <- 200
europe[["Bet_pub_drought_tolerance"]] <- 0.5
europe[["Bet_pub_eps_iso"]] <- 0.
europe[["Bet_pub_seas_iso"]] <- 1
europe[["Bet_pub_eps_mon"]] <- 1.0
europe[["Bet_pub_storfrac_mon"]] <- 0.
  # Parameters specific to Carpinus betulus
europe[["Car_bet_include"]] <- 1
europe[["Car_bet_sla"]] <- 24.3
europe[["Car_bet_gmin"]] <- 0.5
europe[["Car_bet_fnstorage"]] <- 0.15
europe[["Car_bet_phengdd5ramp"]] <- 200
europe[["Car_bet_tcmin_surv"]] <- -8
europe[["Car_bet_tcmin_est"]] <- -8
europe[["Car_bet_tcmax_est"]] <- 5
europe[["Car_bet_twmin_est"]] <- 5
europe[["Car_bet_gdd5min_est"]] <- 1200
europe[["Car_bet_k_chilla"]] <- 0
europe[["Car_bet_k_chillb"]] <- 600
europe[["Car_bet_k_chillk"]] <- 0.05
europe[["Car_bet_fireresist"]] <- 0.1
europe[["Car_bet_intc"]] <- 0.02
europe[["Car_bet_longevity"]] <- 350
europe[["Car_bet_drought_tolerance"]] <- 0.33
europe[["Car_bet_eps_iso"]] <- 0.
europe[["Car_bet_seas_iso"]] <- 1
europe[["Car_bet_eps_mon"]] <- 0.08
europe[["Car_bet_storfrac_mon"]] <- 0.
  # Parameters specific to Corylus avellana
europe[["Cor_ave_include"]] <- 1
europe[["Cor_ave_crownarea_max"]] <- 15
europe[["Cor_ave_k_latosa"]] <- 4000
europe[["Cor_ave_sla"]] <- 12
europe[["Cor_ave_gmin"]] <- 0.5
europe[["Cor_ave_fnstorage"]] <- 0.15
europe[["Cor_ave_phengdd5ramp"]] <- 200
europe[["Cor_ave_tcmin_surv"]] <- -11
europe[["Cor_ave_tcmin_est"]] <- -11
europe[["Cor_ave_tcmax_est"]] <- 7
europe[["Cor_ave_twmin_est"]] <- 5
europe[["Cor_ave_gdd5min_est"]] <- 800
europe[["Cor_ave_k_chilla"]] <- 0
europe[["Cor_ave_k_chillb"]] <- 350
europe[["Cor_ave_k_chillk"]] <- 0.05
europe[["Cor_ave_fireresist"]] <- 0.1
europe[["Cor_ave_intc"]] <- 0.02
europe[["Cor_ave_longevity"]] <- 100
europe[["Cor_ave_drought_tolerance"]] <- 0.3
europe[["Cor_ave_eps_iso"]] <- 0.
europe[["Cor_ave_seas_iso"]] <- 1
europe[["Cor_ave_eps_mon"]] <- 0.
europe[["Cor_ave_storfrac_mon"]] <- 0.
  # Parameters specific to Fagus sylvatica
europe[["Fag_syl_include"]] <- 1
europe[["Fag_syl_sla"]] <- 24.3
europe[["Fag_syl_gmin"]] <- 0.5
europe[["Fag_syl_fnstorage"]] <- 0.15
europe[["Fag_syl_phengdd5ramp"]] <- 200
europe[["Fag_syl_tcmin_surv"]] <- -3.5
europe[["Fag_syl_tcmin_est"]] <- -3.5
europe[["Fag_syl_tcmax_est"]] <- 6
europe[["Fag_syl_twmin_est"]] <- 5
europe[["Fag_syl_gdd5min_est"]] <- 1500
europe[["Fag_syl_k_chilla"]] <- 0
europe[["Fag_syl_k_chillb"]] <- 600
europe[["Fag_syl_k_chillk"]] <- 0.05
europe[["Fag_syl_fireresist"]] <- 0.1
europe[["Fag_syl_intc"]] <- 0.02
europe[["Fag_syl_longevity"]] <- 500
europe[["Fag_syl_drought_tolerance"]] <- 0.3
europe[["Fag_syl_eps_iso"]] <- 0.
europe[["Fag_syl_seas_iso"]] <- 1
europe[["Fag_syl_eps_mon"]] <- 10.
europe[["Fag_syl_storfrac_mon"]] <- 0.
  # Parameters specific to Fraxinus excelsior
europe[["Fra_exc_include"]] <- 1
europe[["Fra_exc_sla"]] <- 24.3
europe[["Fra_exc_gmin"]] <- 0.5
europe[["Fra_exc_fnstorage"]] <- 0.15
europe[["Fra_exc_phengdd5ramp"]] <- 200
europe[["Fra_exc_tcmin_surv"]] <- -16
europe[["Fra_exc_tcmin_est"]] <- -16
europe[["Fra_exc_tcmax_est"]] <- 6
europe[["Fra_exc_twmin_est"]] <- 5
europe[["Fra_exc_gdd5min_est"]] <- 1100
europe[["Fra_exc_k_chilla"]] <- 0
europe[["Fra_exc_k_chillb"]] <- 100
europe[["Fra_exc_k_chillk"]] <- 0.05
europe[["Fra_exc_fireresist"]] <- 0.1
europe[["Fra_exc_intc"]] <- 0.02
europe[["Fra_exc_longevity"]] <- 350
europe[["Fra_exc_drought_tolerance"]] <- 0.4
europe[["Fra_exc_eps_iso"]] <- 0.
europe[["Fra_exc_seas_iso"]] <- 1
europe[["Fra_exc_eps_mon"]] <- 0.
europe[["Fra_exc_storfrac_mon"]] <- 0.
  # Parameters specific to Juniperus oxycedrus
europe[["Jun_oxy_include"]] <- 1
europe[["Jun_oxy_k_latosa"]] <- 1500
europe[["Jun_oxy_crownarea_max"]] <- 10
europe[["Jun_oxy_sla"]] <- 10
europe[["Jun_oxy_gmin"]] <- 0.5
europe[["Jun_oxy_fnstorage"]] <- 0.05
europe[["Jun_oxy_leaflong"]] <- 1.5
europe[["Jun_oxy_turnover_leaf"]] <- 0.6667
europe[["Jun_oxy_phengdd5ramp"]] <- 0
europe[["Jun_oxy_tcmin_surv"]] <- 0
europe[["Jun_oxy_tcmin_est"]] <- 1
europe[["Jun_oxy_tcmax_est"]] <- 1000
europe[["Jun_oxy_twmin_est"]] <- -1000
europe[["Jun_oxy_gdd5min_est"]] <- 2200
europe[["Jun_oxy_k_chilla"]] <- 0
europe[["Jun_oxy_k_chillb"]] <- 100
europe[["Jun_oxy_k_chillk"]] <- 0.05
europe[["Jun_oxy_fireresist"]] <- 0.4
europe[["Jun_oxy_intc"]] <- 0.02
europe[["Jun_oxy_longevity"]] <- 200
europe[["Jun_oxy_drought_tolerance"]] <- 0.01
europe[["Jun_oxy_eps_iso"]] <- 0.
europe[["Jun_oxy_seas_iso"]] <- 0
europe[["Jun_oxy_eps_mon"]] <- 2.0
europe[["Jun_oxy_storfrac_mon"]] <- 0.5
  # Parameters specific to Mediterranean raingreen low shrub: Cistus, Rosmarinus, Genista, Lavandula, Erica
europe[["MRS_include"]] <- 1
europe[["MRS_k_latosa"]] <- 1500
europe[["MRS_sla"]] <- 10
europe[["MRS_gmin"]] <- 0.5
europe[["MRS_leaflong"]] <- 0.5
europe[["MRS_turnover_leaf"]] <- 1
europe[["MRS_phengdd5ramp"]] <- 0
europe[["MRS_tcmin_surv"]] <- 0
europe[["MRS_tcmin_est"]] <- 1
europe[["MRS_tcmax_est"]] <- 1000
europe[["MRS_twmin_est"]] <- -1000
europe[["MRS_gdd5min_est"]] <- 2200
europe[["MRS_k_chilla"]] <- 0
europe[["MRS_k_chillb"]] <- 100
europe[["MRS_k_chillk"]] <- 0.05
europe[["MRS_fireresist"]] <- 0.3
europe[["MRS_intc"]] <- 0.02
europe[["MRS_longevity"]] <- 100
europe[["MRS_drought_tolerance"]] <- 0.01
europe[["MRS_eps_iso"]] <- 2.0
europe[["MRS_seas_iso"]] <- 0
europe[["MRS_eps_mon"]] <- 4.0
europe[["MRS_storfrac_mon"]] <- 0.5
europe[["MRS_ga"]] <- 0.040
  # Parameters specific to Picea abies
europe[["Pic_abi_include"]] <- 1
europe[["Pic_abi_sla"]] <- 9.3
europe[["Pic_abi_gmin"]] <- 0.3
europe[["Pic_abi_fnstorage"]] <- 0.05
europe[["Pic_abi_leaflong"]] <- 3
europe[["Pic_abi_turnover_leaf"]] <- 0.33
europe[["Pic_abi_phengdd5ramp"]] <- 0
europe[["Pic_abi_tcmin_surv"]] <- -30
europe[["Pic_abi_tcmin_est"]] <- -30
europe[["Pic_abi_tcmax_est"]] <- -1.5
europe[["Pic_abi_twmin_est"]] <- 5
europe[["Pic_abi_gdd5min_est"]] <- 600
europe[["Pic_abi_k_chilla"]] <- 0
europe[["Pic_abi_k_chillb"]] <- 100
europe[["Pic_abi_k_chillk"]] <- 0.05
europe[["Pic_abi_fireresist"]] <- 0.1
europe[["Pic_abi_intc"]] <- 0.06
europe[["Pic_abi_longevity"]] <- 500
europe[["Pic_abi_drought_tolerance"]] <- 0.43
europe[["Pic_abi_eps_iso"]] <- 0.5
europe[["Pic_abi_seas_iso"]] <- 0
europe[["Pic_abi_eps_mon"]] <- 6.0
europe[["Pic_abi_storfrac_mon"]] <- 0.5
  # Parameters specific to Picea sitchensis
europe[["Pic_sit_include"]] <- 1
europe[["Pic_sit_k_latosa"]] <- 4500
europe[["Pic_sit_sla"]] <- 9.3
europe[["Pic_sit_gmin"]] <- 0.3
europe[["Pic_sit_fnstorage"]] <- 0.05
europe[["Pic_sit_leaflong"]] <- 4
europe[["Pic_sit_turnover_leaf"]] <- 0.25
europe[["Pic_sit_phengdd5ramp"]] <- 0
europe[["Pic_sit_tcmin_surv"]] <- -18
europe[["Pic_sit_tcmin_est"]] <- -17
europe[["Pic_sit_tcmax_est"]] <- 5.5
europe[["Pic_sit_twmin_est"]] <- -1000
europe[["Pic_sit_gdd5min_est"]] <- 450
europe[["Pic_sit_k_chilla"]] <- 0
europe[["Pic_sit_k_chillb"]] <- 100
europe[["Pic_sit_k_chillk"]] <- 0.05
europe[["Pic_sit_fireresist"]] <- 0.1
europe[["Pic_sit_intc"]] <- 0.06
europe[["Pic_sit_longevity"]] <- 700
europe[["Pic_sit_drought_tolerance"]] <- 0.5
europe[["Pic_sit_eps_iso"]] <- 0.
europe[["Pic_sit_seas_iso"]] <- 0
europe[["Pic_sit_eps_mon"]] <- 0.
europe[["Pic_sit_storfrac_mon"]] <- 0.5
  # Parameters specific to Pinus sylvestris
europe[["Pin_syl_include"]] <- 1
europe[["Pin_syl_k_latosa"]] <- 3000
europe[["Pin_syl_sla"]] <- 9.3
europe[["Pin_syl_gmin"]] <- 0.3
europe[["Pin_syl_fnstorage"]] <- 0.05
europe[["Pin_syl_leaflong"]] <- 2
europe[["Pin_syl_turnover_leaf"]] <- 0.5
europe[["Pin_syl_phengdd5ramp"]] <- 0
europe[["Pin_syl_tcmin_surv"]] <- -30
europe[["Pin_syl_tcmin_est"]] <- -30
europe[["Pin_syl_tcmax_est"]] <- -1.0
europe[["Pin_syl_twmin_est"]] <- 5
europe[["Pin_syl_gdd5min_est"]] <- 500
europe[["Pin_syl_k_chilla"]] <- 0
europe[["Pin_syl_k_chillb"]] <- 100
europe[["Pin_syl_k_chillk"]] <- 0.05
europe[["Pin_syl_fireresist"]] <- 0.2
europe[["Pin_syl_intc"]] <- 0.06
europe[["Pin_syl_longevity"]] <- 350
europe[["Pin_syl_drought_tolerance"]] <- 0.25
europe[["Pin_syl_eps_iso"]] <- 0.
europe[["Pin_syl_seas_iso"]] <- 0
europe[["Pin_syl_eps_mon"]] <- 4.0
europe[["Pin_syl_storfrac_mon"]] <- 0.5
  # Parameters specific to Pinus halepensis
europe[["Pin_hal_include"]] <- 1
europe[["Pin_hal_k_latosa"]] <- 3000
europe[["Pin_hal_sla"]] <- 9.3
europe[["Pin_hal_gmin"]] <- 0.3
europe[["Pin_hal_fnstorage"]] <- 0.05
europe[["Pin_hal_leaflong"]] <- 2
europe[["Pin_hal_turnover_leaf"]] <- 0.5
europe[["Pin_hal_phengdd5ramp"]] <- 0
europe[["Pin_hal_tcmin_surv"]] <- 3
europe[["Pin_hal_tcmin_est"]] <- 3
europe[["Pin_hal_tcmax_est"]] <- 9
europe[["Pin_hal_twmin_est"]] <- 21
europe[["Pin_hal_gdd5min_est"]] <- 3000
europe[["Pin_hal_k_chilla"]] <- 0
europe[["Pin_hal_k_chillb"]] <- 100
europe[["Pin_hal_k_chillk"]] <- 0.05
europe[["Pin_hal_fireresist"]] <- 0.2
europe[["Pin_hal_intc"]] <- 0.06
europe[["Pin_hal_longevity"]] <- 350
europe[["Pin_hal_drought_tolerance"]] <- 0.05
europe[["Pin_hal_eps_iso"]] <- 0.
europe[["Pin_hal_seas_iso"]] <- 0
europe[["Pin_hal_eps_mon"]] <- 10.0
europe[["Pin_hal_storfrac_mon"]] <- 0.5
  # Parameters specific to Populus tremula
europe[["Pop_tre_include"]] <- 1
europe[["Pop_tre_sla"]] <- 24.3
europe[["Pop_tre_gmin"]] <- 0.5
europe[["Pop_tre_fnstorage"]] <- 0.15
europe[["Pop_tre_phengdd5ramp"]] <- 200
europe[["Pop_tre_tcmin_surv"]] <- -31
europe[["Pop_tre_tcmin_est"]] <- -30
europe[["Pop_tre_tcmax_est"]] <- 6
europe[["Pop_tre_twmin_est"]] <- -1000
europe[["Pop_tre_gdd5min_est"]] <- 500
europe[["Pop_tre_k_chilla"]] <- 0
europe[["Pop_tre_k_chillb"]] <- 350
europe[["Pop_tre_k_chillk"]] <- 0.05
europe[["Pop_tre_fireresist"]] <- 0.2
europe[["Pop_tre_intc"]] <- 0.02
europe[["Pop_tre_longevity"]] <- 160
europe[["Pop_tre_drought_tolerance"]] <- 0.4
europe[["Pop_tre_eps_iso"]] <- 20.
europe[["Pop_tre_seas_iso"]] <- 1
europe[["Pop_tre_eps_mon"]] <- 4.0
europe[["Pop_tre_storfrac_mon"]] <- 0.
  # Parameters specific to Quercus coccifera
europe[["Que_coc_include"]] <- 1
europe[["Que_coc_k_latosa"]] <- 2500
europe[["Que_coc_sla"]] <- 10
europe[["Que_coc_gmin"]] <- 0.5
europe[["Que_coc_leaflong"]] <- 1.5
europe[["Que_coc_turnover_leaf"]] <- 0.6667
europe[["Que_coc_phengdd5ramp"]] <- 0
europe[["Que_coc_tcmin_surv"]] <- 0
europe[["Que_coc_tcmin_est"]] <- 0
europe[["Que_coc_tcmax_est"]] <- 11
europe[["Que_coc_twmin_est"]] <- 21
europe[["Que_coc_gdd5min_est"]] <- 2200
europe[["Que_coc_k_chilla"]] <- 0
europe[["Que_coc_k_chillb"]] <- 100
europe[["Que_coc_k_chillk"]] <- 0.05
europe[["Que_coc_fireresist"]] <- 0.3
europe[["Que_coc_intc"]] <- 0.02
europe[["Que_coc_longevity"]] <- 350
europe[["Que_coc_drought_tolerance"]] <- 0.1
europe[["Que_coc_eps_iso"]] <- 0.1
europe[["Que_coc_seas_iso"]] <- 0
europe[["Que_coc_eps_mon"]] <- 10.0
europe[["Que_coc_storfrac_mon"]] <- 0.
europe[["Que_coc_ga"]] <- 0.040
  # Parameters specific to Querqus ilex
europe[["Que_ile_include"]] <- 1
europe[["Que_ile_k_latosa"]] <- 3000
europe[["Que_ile_sla"]] <- 9.3
europe[["Que_ile_gmin"]] <- 0.5
europe[["Que_ile_fnstorage"]] <- 0.05
europe[["Que_ile_leaflong"]] <- 2
europe[["Que_ile_turnover_leaf"]] <- 0.5
europe[["Que_ile_phengdd5ramp"]] <- 0
europe[["Que_ile_tcmin_surv"]] <- 3
europe[["Que_ile_tcmin_est"]] <- 3
europe[["Que_ile_tcmax_est"]] <- 7
europe[["Que_ile_twmin_est"]] <- 5
europe[["Que_ile_gdd5min_est"]] <- 1800
europe[["Que_ile_k_chilla"]] <- 0
europe[["Que_ile_k_chillb"]] <- 100
europe[["Que_ile_k_chillk"]] <- 0.05
europe[["Que_ile_fireresist"]] <- 0.3
europe[["Que_ile_intc"]] <- 0.02
europe[["Que_ile_longevity"]] <- 350
europe[["Que_ile_drought_tolerance"]] <- 0.1
europe[["Que_ile_eps_iso"]] <- 0.05
europe[["Que_ile_seas_iso"]] <- 0
europe[["Que_ile_eps_mon"]] <- 16.0
europe[["Que_ile_storfrac_mon"]] <- 0.
  # Parameters specific to Quercus pubescens
europe[["Que_pub_include"]] <- 1
europe[["Que_pub_sla"]] <- 24.3
europe[["Que_pub_gmin"]] <- 0.5
europe[["Que_pub_fnstorage"]] <- 0.15
europe[["Que_pub_phengdd5ramp"]] <- 200
europe[["Que_pub_tcmin_surv"]] <- -6
europe[["Que_pub_tcmin_est"]] <- -5
europe[["Que_pub_tcmax_est"]] <- 6
europe[["Que_pub_twmin_est"]] <- -1000
europe[["Que_pub_gdd5min_est"]] <- 1900
europe[["Que_pub_k_chilla"]] <- 0
europe[["Que_pub_k_chillb"]] <- 100
europe[["Que_pub_k_chillk"]] <- 0.05
europe[["Que_pub_fireresist"]] <- 0.2
europe[["Que_pub_intc"]] <- 0.02
europe[["Que_pub_longevity"]] <- 500
europe[["Que_pub_drought_tolerance"]] <- 0.2
europe[["Que_pub_eps_iso"]] <- 50.0
europe[["Que_pub_seas_iso"]] <- 1
europe[["Que_pub_eps_mon"]] <- 0.
europe[["Que_pub_storfrac_mon"]] <- 0.
  # Parameters specific to Quercus robur
europe[["Que_rob_include"]] <- 1
europe[["Que_rob_sla"]] <- 24.3
europe[["Que_rob_gmin"]] <- 0.5
europe[["Que_rob_fnstorage"]] <- 0.15
europe[["Que_rob_phengdd5ramp"]] <- 200
europe[["Que_rob_tcmin_surv"]] <- -10
europe[["Que_rob_tcmin_est"]] <- -9
europe[["Que_rob_tcmax_est"]] <- 6
europe[["Que_rob_twmin_est"]] <- 5
europe[["Que_rob_gdd5min_est"]] <- 1100
europe[["Que_rob_k_chilla"]] <- 0
europe[["Que_rob_k_chillb"]] <- 100
europe[["Que_rob_k_chillk"]] <- 0.05
europe[["Que_rob_fireresist"]] <- 0.2
europe[["Que_rob_intc"]] <- 0.02
europe[["Que_rob_longevity"]] <- 500
europe[["Que_rob_drought_tolerance"]] <- 0.25
europe[["Que_rob_eps_iso"]] <- 40.0
europe[["Que_rob_seas_iso"]] <- 1
europe[["Que_rob_eps_mon"]] <- 0.
europe[["Que_rob_storfrac_mon"]] <- 0.
  # Parameters specific to Tila cordata
europe[["Til_cor_include"]] <- 1
europe[["Til_cor_sla"]] <- 24.3
europe[["Til_cor_gmin"]] <- 0.5
europe[["Til_cor_fnstorage"]] <- 0.15
europe[["Til_cor_phengdd5ramp"]] <- 200
europe[["Til_cor_tcmin_surv"]] <- -12
europe[["Til_cor_tcmin_est"]] <- -11
europe[["Til_cor_tcmax_est"]] <- 5
europe[["Til_cor_twmin_est"]] <- 5
europe[["Til_cor_gdd5min_est"]] <- 1100
europe[["Til_cor_k_chilla"]] <- 0
europe[["Til_cor_k_chillb"]] <- 600
europe[["Til_cor_k_chillk"]] <- 0.05
europe[["Til_cor_fireresist"]] <- 0.1
europe[["Til_cor_intc"]] <- 0.02
europe[["Til_cor_longevity"]] <- 350
europe[["Til_cor_drought_tolerance"]] <- 0.33
europe[["Til_cor_eps_iso"]] <- 0.
europe[["Til_cor_seas_iso"]] <- 1
europe[["Til_cor_eps_mon"]] <- 0.
europe[["Til_cor_storfrac_mon"]] <- 0.
  # Parameters specific to Ulmus glaucescens
europe[["Ulm_gla_include"]] <- 1
europe[["Ulm_gla_sla"]] <- 24.3
europe[["Ulm_gla_gmin"]] <- 0.5
europe[["Ulm_gla_fnstorage"]] <- 0.15
europe[["Ulm_gla_phengdd5ramp"]] <- 200
europe[["Ulm_gla_tcmin_surv"]] <- -10.5
europe[["Ulm_gla_tcmin_est"]] <- -9.5
europe[["Ulm_gla_tcmax_est"]] <- 6
europe[["Ulm_gla_twmin_est"]] <- 5
europe[["Ulm_gla_gdd5min_est"]] <- 850
europe[["Ulm_gla_k_chilla"]] <- 0
europe[["Ulm_gla_k_chillb"]] <- 100
europe[["Ulm_gla_k_chillk"]] <- 0.05
europe[["Ulm_gla_fireresist"]] <- 0.1
europe[["Ulm_gla_intc"]] <- 0.02
europe[["Ulm_gla_longevity"]] <- 350
europe[["Ulm_gla_drought_tolerance"]] <- 0.4
europe[["Ulm_gla_eps_iso"]] <- 0.
europe[["Ulm_gla_seas_iso"]] <- 1
europe[["Ulm_gla_eps_mon"]] <- 0.
europe[["Ulm_gla_storfrac_mon"]] <- 0.
  # Parameters specific to cool (C3) grass
europe[["C3_gr_include"]] <- 1
europe[["C3_gr_sla"]] <- 32.4
europe[["C3_gr_gmin"]] <- 0.5
europe[["C3_gr_leaflong"]] <- 0.5
europe[["C3_gr_turnover_leaf"]] <- 1
europe[["C3_gr_turnover_root"]] <- 0.7
europe[["C3_gr_phengdd5ramp"]] <- 100
europe[["C3_gr_pstemp_min"]] <- -5
europe[["C3_gr_pstemp_low"]] <- 10
europe[["C3_gr_pstemp_high"]] <- 30
europe[["C3_gr_pstemp_max"]] <- 45
europe[["C3_gr_tcmin_surv"]] <- -1000
europe[["C3_gr_tcmin_est"]] <- -1000
europe[["C3_gr_tcmax_est"]] <- 1000
europe[["C3_gr_twmin_est"]] <- -1000
europe[["C3_gr_gdd5min_est"]] <- 0
europe[["C3_gr_parff_min"]] <- 2000000
europe[["C3_gr_fireresist"]] <- 0.5
europe[["C3_gr_intc"]] <- 0.01
europe[["C3_gr_drought_tolerance"]] <- 0.01
europe[["C3_gr_eps_iso"]] <- 0.
europe[["C3_gr_seas_iso"]] <- 1
europe[["C3_gr_eps_mon"]] <- 1.0
europe[["C3_gr_storfrac_mon"]] <- 0.5
  # Parameters specific to warm (C4) grass
europe[["C4_gr_include"]] <- 1
europe[["C4_gr_gmin"]] <- 0.5
europe[["C4_gr_leaflong"]] <- 0.5
europe[["C4_gr_turnover_leaf"]] <- 1
europe[["C4_gr_turnover_root"]] <- 0.7
europe[["C4_gr_phengdd5ramp"]] <- 100
europe[["C4_gr_pstemp_min"]] <- 6
europe[["C4_gr_pstemp_low"]] <- 20
europe[["C4_gr_pstemp_high"]] <- 45
europe[["C4_gr_pstemp_max"]] <- 55
europe[["C4_gr_tcmin_surv"]] <- 15.5
europe[["C4_gr_tcmin_est"]] <- 15.5
europe[["C4_gr_tcmax_est"]] <- 1000
europe[["C4_gr_twmin_est"]] <- -1000
europe[["C4_gr_gdd5min_est"]] <- 0
europe[["C4_gr_parff_min"]] <- 1000000
europe[["C4_gr_fireresist"]] <- 0.5
europe[["C4_gr_intc"]] <- 0.01
europe[["C4_gr_drought_tolerance"]] <- 0.01
europe[["C4_gr_eps_iso"]] <- 0.
europe[["C4_gr_seas_iso"]] <- 1
europe[["C4_gr_eps_mon"]] <- 1.0
europe[["C4_gr_storfrac_mon"]] <- 0.5
# Add to the list
parameterList.default[['europe']] <- europe

#----------------------------------------------------------------------------------------------#-
# Put the data in the package


devtools::use_data(parameterList.default, files.parameters, typelist.default, templates, internal = TRUE, overwrite = TRUE)

