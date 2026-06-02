################################################################################
# This R script is the working data for Peter Martin's EPOG-JM Master's Thesis #
####################################### SETUP ##################################
### ✓ LOAD PACKAGES ### ----
  # Load packages
for (i in c("utils",        #
            "grDevices",    #
            "tidyverse",    #
            "fredr",        # FRED dataset API            
            "patchwork",    # combining ggplots
            "urca",         # unit root tests           
            "lmtest",       # 'coeftest`, 'bp test'
            "forecast",     # `checkresiduals`, `auto.arima`
            "strucchange",  # `breakpoints` `sctest`
            "vars",         # VAR and Cointegration Functions
            "ARDL",         # ARDL and uecm
            "dynlm",        # 
            "tseries",      # jarque.bera.test
            "FinTS",        # ARCH test
            "gt",           # for exporting dataframes as tables
            "countrycode")   
     ){if (!require(i, character.only = TRUE)){
       install.packages(i)
       library(i, character.only = TRUE)
  }
}

### ✓ CLEAR WORK-SPACE ### ----
  # Clear environment
rm(list=ls(all=TRUE))

  # Clear plots
if(!is.null(dev.list())) dev.off()

  # Clear console
cat("\014")

### ✓ LOAD DATASETS ### ----
  # Get the working directory string
path <- getwd() # set R Project Working Directory Path

# Calling API Key from txt file
fredr_set_key(read_file("FRED_API.txt"))

  # Create Folders and Subfolders for Figures
if (dir.exists(file.path(path, "/Figures")) == FALSE) {
  print("No folder for Figures exists. Creating now in Working Directory...")
  dir.create(file.path(path, "Figures"))
  dir.create(file.path(path, "Figures", "(P)ACFs"))
  dir.create(file.path(path, "Figures", "Tables"))
  dir.create(file.path(path, "Figures", "Breaktests"))
  dir.create(file.path(path, "Figures", "Regressions"))
  dir.create(file.path(path, "Figures", "Country_Comp"))
}

 ##---------------------##
 ## Material Flow Datas ##
 ##---------------------##

 ## UNEP National 4+ Categories Material Flows for the United States, 1970-2024
  # Source: https://www.resourcepanel.org/global-material-flows-database
mfa_data_1 <- read.csv("mfa4_export.csv") %>% 
  dplyr::select(-c("Flow.name")) # removing redundant columns

 ## Material and Energy Flows in the United States of America, 1870 to 2005 (Gierlinger & Krausmann, 2012)
  # Source: https://boku.ac.at/wiso/isec/data-download
  # Reformatted excel file to CSV, converted units from 1000 Tonnes to Tonnes, and rearranged table according to UNEP file column conventions 
mfa_data_2 <- read.csv("Online_data_USA_material.csv")

 ## Maddison Project Database 2023: Population and Real GDP in 2011$ Geary-Khamis dollars, 1820 - 2022 (Bold & van Zanden, 2024)
  # Source: https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2023
  # Reformated excel file to CSV, filtered for US values, and years with both GDP per capita and population data
# gdp_data <- read.csv("mpd2023_web_USA.csv") %>% 
#   dplyr::select(-c("countrycode", "country", "region")) # removing redundant columns

 ##---------------------##
 ## Macroeconomic Datas ##
 ##---------------------##

if (file.exists("macro_data.csv")){
  print("The file 'macro_data.csv' already exists in working directory. Loading data...")
  macro_data <- read.csv("macro_data.csv")
  }else{
  print("The file 'macro_data.csv' does not exist in working directory. Calling data...")
 ## AMECO Database
  # Source: https://economy-finance.ec.europa.eu/economic-research-and-databases/economic-databases/ameco-database_en
  # Downloaded "AMECO - All zipped CSV files", all related datasets are from the "ameco0_csv" folder
  
  indicators <- c(
    # Constant prices -> 2020 base year
    pop = 'total',            # Total population (National accounts)                          [AMECO1, Code: USA.1.0.0.0.NPTD] 
    NFCF_r = "bln. US$",      # Net fixed capital formation at 2020 prices: total economy     [AMECO3, Code: USA.1.1.0.0.OINT] (I)
    # GCF_r = "bln. US$",       # Gross capital formation at 2020 prices: total economy         [AMECO3, Code: USA.1.1.0.0.OITT] 
    # GFCF_r = "bln. US$",      # Gross fixed capital formation at 2020 prices: total economy   [AMECO3, Code: USA.1.1.0.0.OIGT] 
    GDP_r = "bln. US$",       # Gross domestic product at 2020 reference levels               [AMECO6, Code: USA.1.1.0.0.OVGD]
    gOS = "bln. US$",         # Gross operating surplus: total economy                        [AMECO7, Code: USA.1.0.0.0.UOGD]
    Emp_Comp = "bln. US$",    # Compensation of employees: total economy                      [AMECO7, Code: USA.1.0.0.0.UWCD]
    RULC = "2020 = 100",      # Real unit labour costs                                        [AMECO7, Code: USA.3.1.0.0.QLCD]
    netK_r = "bln. US$",      # Net capital stock at 2020 prices: total economy               [AMECO8, Code: USA.1.0.0.0.OKND] (K)
    INT.lr = "%"              # Nominal long-term interest rates                              [AMECO13, Code: USA.1.1.0.0.ILN]
  )
  
  ameco_vars <- data.frame(AMECO1 = c("USA.1.0.0.0.NPTD", NA, NA),
                           AMECO3 = c("USA.1.1.0.0.OINT", NA, NA), # "USA.1.1.0.0.OITT","USA.1.1.0.0.OIGT"
                           AMECO6 = c("USA.1.1.0.0.OVGD", NA, NA),
                           AMECO7 = c("USA.1.0.0.0.UOGD", "USA.1.0.0.0.UWCD", "USA.3.1.0.0.QLCD"),
                           AMECO8 = c("USA.1.0.0.0.OKND", NA, NA),
                           AMECO13 = c("USA.1.1.0.0.ILN", NA, NA))
  macro_data <- NULL 
  for (i in names(ameco_vars)){
    print(i)
    for (j in ameco_vars[[i]]){
      macro_data <- bind_rows(macro_data,read_csv(paste0("ameco0_csv/",i,".CSV")) %>%
                                dplyr::select(-c("COUNTRY","SUB-CHAPTER", "TITLE", "UNIT")) %>%
                                filter(CODE == j)) %>%  dplyr::select(-c("CODE"))
    }
  } # Load Data
  
  macro_data <- suppressWarnings(as.data.frame(t(type.convert(macro_data)))) #Transpose data
  macro_data <- macro_data[rowSums(is.na(macro_data)) < ncol(macro_data), ] # remove NA row
  colnames(macro_data) <- names(indicators) # name columns
  for (i in names(indicators)){
    attr(macro_data[[i]], "label") <- indicators[[i]]}         # sub-names
  
  # Add year variable as first column
  macro_data$year <- as.numeric(rownames(macro_data))
  macro_data <- macro_data[,c("year",colnames(macro_data %>% dplyr::select(-"year")))]
  
  # Added/adjusted variables
  macro_data$pop <- macro_data$pop*1000                                         # (convert from 1000 persons to gross total)
  macro_data$K_acc <- macro_data$NFCF_r / macro_data$netK_r                     # Capital Accumulation Rate: NFCF_r / netK_r
  attr(macro_data[[10]], "label") <- "%"
  macro_data$p_share <- macro_data$gOS / (macro_data$gOS + macro_data$Emp_Comp) # Profit Share: gOS / (gOS + Emp_Comp)
  attr(macro_data[[11]], "label") <- "%"
  # Long-Term Interest Rates:     INT.lr
  # Real Unit Labour Cost:        RULC

 ## Federal Reserve Economic Data (FRED)
  # Source: https://fred.stlouisfed.org
  # Called from FRED API with 'fredr' R Package
  temp <- fredr(series_id = "JHDUSRGDPBR", # recession quarters, annual sums
                frequency = "a",
                aggregation_method = "sum") %>%
    mutate(year = year(ymd(.data[["date"]])),
           recessions = value) %>%
    dplyr::select(c("year","recessions"))
  
  # Yearly sum of Recession Quarters
  macro_data <- left_join(macro_data, temp, by = join_by(year))
  attr(macro_data[[12]], "label") <- "Recession Quarters"
  
  write.csv(macro_data,"macro_data.csv", row.names=FALSE) # Save dataset  
}
# Label Variables
indicators <- {c(
  # Constant prices -> 2020 base year
  year = "",
  pop = 'total',            # Total population (National accounts)                          [AMECO1, Code: USA.1.0.0.0.NPTD] 
  # GCF_r = "bln. US$",       # Gross capital formation at 2020 prices: total economy         [AMECO3, Code: USA.1.1.0.0.OITT] (I)
  # GFCF_r = "bln. US$",      # Gross fixed capital formation at 2020 prices: total economy   [AMECO3, Code: USA.1.1.0.0.OIGT] XXXX
  NFCF_r = "bln. US$",      # Net fixed capital formation at 2020 prices: total economy     [AMECO3, Code: USA.1.1.0.0.OINT] !!!! (I)
  GDP_r = "bln. US$",       # Gross domestic product at 2020 reference levels               [AMECO6, Code: USA.1.1.0.0.OVGD]
  gOS = "bln. US$",         # Gross operating surplus: total economy                        [AMECO7, Code: USA.1.0.0.0.UOGD]
  Emp_Comp = "bln. US$",    # Compensation of employees: total economy                      [AMECO7, Code: USA.1.0.0.0.UWCD]
  RULC = "2020 = 100",      # Real unit labour costs                                        [AMECO7, Code: USA.3.1.0.0.QLCD]
  netK_r = "bln. US$",      # Net capital stock at 2020 prices: total economy               [AMECO8, Code: USA.1.0.0.0.OKND] !!!! (K)
  INT.lr = "%",             # Nominal long-term interest rates                              [AMECO13, Code: USA.1.1.0.0.ILN]
  K_acc = "%",                        # Capital Accumulation Rate: NFCF_r / netK_r
  p_share = "%",                      # Profit Share: gOS / (gOS + Emp_Comp)
  recessions = "Recession Quarters"   # Total Recession Quarters in Given Year
)}
for (i in names(indicators)){
  attr(macro_data[[i]], "label") <- indicators[[i]]} # Variable Units

cat("\014")
### Save Graphs and Tables? ### ----
save_png = F # (T)rue or (F)alse [will print figures in plots and Viewer window]
cat("\014")
####################### CLEAN, MERGE, & TRANSPOSE DATASETS #####################
### ✓ CLEANING ### ----
  # Filtering to four primary material flows to harmonize with Gierlinger & Krausmann, 2012; removing redundant columns
data <-  mfa_data_1 %>% 
  filter(Country %in% c("United States of America")) %>% 
  dplyr :: select(-c("Country"))
### ✓ MERGING ### ----
  # Merging material flow data, overlap from 1970-2005 covered with only UNEP data as the most up-to-date
data <- data %>%
  left_join(mfa_data_2 %>% dplyr::select(-c("Country", "Flow.name", "Flow.unit")), by = c("Category", "Flow.code")) %>%
  dplyr::select(-c("Flow.unit",grep("^X\\d{4}.y$", colnames(.), value = TRUE)))

  # Isolating year column names
names(data) <- sub("\\.x$", "", names(data))
names(data) <- sub("^X", "", names(data))

  # Identify year columns (names that are only digits)
year_cols <- grep("^\\d{4}$", names(data), value = TRUE)

  # Sort them numerically
year_cols_sorted <- year_cols[order(as.numeric(year_cols))]

  # Reorder dataset
data <- data %>%
  dplyr::select(-all_of(year_cols), all_of(year_cols_sorted))

### ✓ TRANSPOSING ### ----
  # Transpose/Pivot
data <- data %>%
  pivot_longer(-c("Category", "Flow.code"),
               names_to = "year",
               values_to = "Value") %>%
  pivot_wider(names_from = "Flow.code",
              values_from = "Value") %>%
  dplyr::select(-c("DMI"))
data$year <- as.integer(data$year)

  # binary variable for later distinctions in graphs ???
data$data <- "full"
data$data[is.na(data$MF)] <- "composite"

### ✓ COMPLETING DATASET MISSING VALUES ### ----
 ## Missing Material Footprint Values in the Gierlinger et al. (2012) are calculated 
  # according to methodology outlined in the Appendix of Cahen-Fourot & Magalhães (2023)

  # Completing Physical Trade Balance (PTB): Imports - Exports
data$PTB <- data$IMP - data$EXP

  # Calculate Real Trade Balance (RTB): Raw Material Equivalent Imports (RME_IMP) - Raw Material Equivalent Exports (RME_EXP)
data$RTB <- data$RME_IMP - data$RME_EXP

  # Calculate RTB to PTB Ratios where possible (UNEP Data from 1970-2024)
data$RTB_to_PTB = data$RTB/data$PTB
  
  # Label Variables
for (i in c("DE","DMC","EXP","IMP","MF","PTB","RME_EXP","RME_IMP","RTB")){
  attr(data[[i]], "label") <- "(tonnes)"}

  # Calculate Median Ratios for each Category
data <- data %>% group_by(Category) %>% mutate(median_ratio = median(RTB_to_PTB, na.rm = TRUE))

 ## Calculate Missing RTB, and Material Footprint (MF) values ##
  # RTB #
data$RTB[is.na(data$MF)] <- 
  data$PTB[is.na(data$MF)] * data$median_ratio[is.na(data$MF)]

  # MF #
data$MF[is.na(data$MF)] <- 
  data$RTB[is.na(data$MF)] + data$DE[is.na(data$MF)]

  # Summing Material Flows
agg_data <- data %>%
  group_by(year) %>%
  summarize(across(c("DE", "IMP", "EXP", "PTB","DMC", "RTB", "MF"), sum, na.rm = TRUE))
agg_data$Category <- "Aggregate"

  # Label Variables
for (i in c("DE","DMC","EXP","IMP","MF","PTB","RTB")){
  attr(agg_data[[i]], "label") <- "(tonnes)"}

  # Merging Real GDP per capita and Population data
agg_data <- agg_data %>%
  left_join(macro_data, by = c("year"))

  # Calculating Material Intensity (MI) and Footprint Adjusted Material Intensity (FAMI)
agg_data$MI <- (agg_data$DMC*1000) / (agg_data$GDP_r*1e9)   # in kg/USD
agg_data$FAMI <- (agg_data$MF*1000) / (agg_data$GDP_r*1e9)  # in kg/USD

  # Label Variables
for (i in c("MI","FAMI")){
  attr(agg_data[[i]], "label") <- "(kg/US$)"}

# add aggregated data columns to data
data <- data %>% left_join(dplyr::select(agg_data,c(colnames(macro_data),
                                    "MI", "FAMI")),
                           by = c("year"))

# add aggregated data rows to data
data <- rbind(data, agg_data) %>% 
  mutate(MI = (DMC*1000)/(GDP_r*1e9),
         FAMI= (MF*1000)/(GDP_r*1e9))

# Material Efficieny
temp <- lag(data$MI)
data$MI_delta <- ((1/data$MI) - (1/temp))/(1/temp)
temp <- lag(data$FAMI)
data$FAMI_delta <- ((1/data$FAMI) - (1/temp))/(1/temp)


  # Relabel Variables
for (i in c("DE","DMC","EXP","IMP","MF","PTB","RTB")){
  attr(data[[i]], "label") <- "(tonnes)"}
for (i in c("MI","FAMI")){
  attr(data[[i]], "label") <- "(kg/US$)"}
for (i in names(indicators)){
  attr(data[[i]], "label") <- indicators[[i]]}
# for (i in c("gdp_real_cap","gfcf_real_cap")){
#   attr(data[[i]], "label") <- "(constant 2015 US$)"}

 ## Differences, or Indirect Trade Flows ##
  # == MF - DMC == (DE + RTB) - (DE + PTB) == RTB - PTB
  # Implies the increases/decreases in embedded material import (+) / exports (-)
  # INDIRECT MATERIAL FLOWS DUE TO TRADE
data$IMF <-data$MF -data$DMC

 ## adding Column for other IMF series based only on Gierlinger & Krausmann 2012
mfa_GK <-  mfa_data_2 %>% 
  dplyr::select(-c("Country", "Flow.name", "Flow.unit")) %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               values_to = "value") %>%
  mutate(year = as.integer(str_remove(year, "X"))) %>%
  pivot_wider(names_from = Flow.code, values_from = value) %>%
  mutate(PTB_gk = IMP - EXP,
         RTB_ratio = case_when(
           Category == "Biomass" ~ -1.926671,
           Category == "Fossil fuels" ~ 1.983076,
           Category == "Metal ores" ~ 4.444523,
           Category == "Non-metallic minerals" ~ 14.544664
         ),
         RTB_gk = PTB_gk * RTB_ratio,
         DMC_gk = DMC,
         MF_gk = DE + RTB_gk,
         IMF_gk = RTB_gk - PTB_gk) 
temp <- mfa_GK %>%
  group_by(year) %>%
  summarise(Category = "Aggregate",
            PTB_gk = sum(PTB_gk, na.rm = F),
            RTB_gk = sum(RTB_gk, na.rm = F),
            DMC_gk = sum(DMC_gk, na.rm = F),
            MF_gk = sum(MF_gk, na.rm = F),
            IMF_gk = sum(IMF_gk, na.rm = F),
            .groups = "drop")

mfa_GK <- bind_rows(mfa_GK,temp) %>%
  dplyr::select(c("Category","year", "PTB_gk","RTB_gk","DMC_gk","MF_gk","IMF_gk"))

data <- data %>% left_join(mfa_GK, by = c("Category", "year"))

### ✓ Study Data Base ### ----
  # Dataset for most analysis focusing on time period of 1970-2024, 
  # with variables across columns
study_data <- macro_data[macro_data$year >= 1970 & macro_data$year <= 2024,]
rownames(study_data) <- NULL
  # Material Category/Column Name List, will be used interchangeably in loops
cat_col <- c(bio = "Biomass",
             ffs = "Fossil fuels",
             met = "Metal ores",
             nmm = "Non-metallic minerals",
             agg = "Aggregate")
  # Relabel Variables
for (i in names(cat_col)){
  # IMF by category as individual columns
  study_data[paste0("IMF_",i)] <- data[data$Category==cat_col[[i]],]$IMF[101:155]
  attr(study_data[[paste0("IMF_",i)]],"label") <- "(tonnes)"
}
for (i in names(indicators)){
  attr(study_data[[i]], "label") <- indicators[[i]]}

acc_vars <- c(K_acc = "Rate of Fixed Capital Accumulation", 
              p_share = "Profit Share",
              INT.lr = "Long-term Interest Rate", 
              RULC = "Real Unit Labour Cost")

cat("\014")
################################ VISUAL ANALYSIS ###############################
### TABLE FUNCTIONS AND VARIABLES #### ----
table_names <- {c(ur_table1 = "Unit Root Test Results",
                  bp_table = "Proposed Bai-Perron Break Years",
                  ur_table2 = "Residual Unit Root Test Results",
                  resid_table = "Model Residual Tests",
                  regr_table = "Regression Coefficients and R-squared Adjusted",
                  cumulative_effects = "Cumulative Effects of Segmented Regression",
                  ur_table3 = "Macro Data Unit Root Test Results"
)} # Keeping track of tables for easier visualization

  # Convert variable coefficients to statistical significance markers
stat_sig <- function(input){
  if(input < 0.001){
    return("***")
  }else if(input < 0.01){
    return("**")
  }else if (input < 0.05){
    return("*")
  }else if(input < 0.1){
    return(".")
  }else{return("")}
}

  # Function to Run Multiple Unit Roots
ur_tests <- function(input,     # Timeseries data
                     tbl,     # Unit Root Table Dataframe
                     transform, # Timeseries Form (Differenced/Levels)
                     i)        # Material Category
{
  tbl[nrow(tbl) + 1, ]$Category <- i
  tbl[nrow(tbl), ]$model_spec <- transform
  if (grepl("Levels",transform)==TRUE){ # Set to trend models for levels series
    # Augmented Dickey-Fuller Test (ADF):
    test <- ur.df(input, type = "trend", selectlags = "BIC");
    # H0 = unit root (non-stationary)
    # For rejection: test stat must SMALLER than critical values
    # Reject? → stationary around trend
    ifelse (test@teststat[1,1] < # tau3 test statistic
              test@cval[1,2], # 5% critical value for tau3 test statistic
            tbl[nrow(tbl), ]$ADF <- "Stationary", # reject
            tbl[nrow(tbl), ]$ADF <- "Unit Root")  # fail to reject
    
    # Phillips and Perron Unit Root Test (PP):
    test <- ur.pp(input,  type = "Z-tau", model = "trend")
    # H0 = unit root (non-stationary)
    # For rejection: test stat must LESS than critical values
    # Reject? → stationary around trend
    ifelse (test@teststat < # test statistic
              test@cval[1,2], # 5% critical value for test statistic
            tbl[nrow(tbl), ]$PP <- "Stationary", # reject
            tbl[nrow(tbl), ]$PP <- "Unit Root")  # fail to reject
    
    # Elliott, Rothenberg and Stock Unit Root Test (DF-GLS):
    test <- ur.ers(input, type = "DF-GLS", model = "trend")
    # H0 = unit root (non-stationary);
    # For rejection: test stat must LESS than critical values
    # Reject? → stationary around trend
    ifelse (test@teststat < # test statistic
              test@cval[1,2], # 5% critical value for test statistic
            tbl[nrow(tbl), ]$DF_GLS <- "Stationary", # reject
            tbl[nrow(tbl), ]$DF_GLS <- "Unit Root")  # fail to reject
    
    # Kwiatkowski-Phillips-Schmidt-Shin (KPSS):
    test <- ur.kpss(input, type = "tau")
    # H0 = trend-stationary (reject => likely unit root).
    # For rejection: statistics must be GREATER than critical values
    # Reject? → NOT trend-stationary (i.e., likely unit root)
    ifelse (test@teststat < # test statistic
              test@cval[1,2], # 5% critical value for test statistic
            tbl[nrow(tbl), ]$KPSS <- "Unit Root",  # reject
            tbl[nrow(tbl), ]$KPSS <- "Stationary") # fail to reject
    
    # Zivot-Andrews Unit Root with Endogenous Structural Break
    test <- ur.za(input, model = "both")
    # H0: unit root with a single structural break
    # For rejection: test stat must SMALLER than critical values
    # Reject? → stationary around trend
    ifelse (test@teststat < # test statistic
              test@cval[2], # 5% critical value for test statistic
            tbl[nrow(tbl), ]$Zivot_Andrews <- "Stationary with Break", # reject
            tbl[nrow(tbl), ]$Zivot_Andrews <- "Unit Root")  # fail to reject
  }
  
  if (grepl("Differenced",transform)==TRUE | grepl("Residual",transform)==TRUE){ # Set to constant models for differenced series
    # Augmented Dickey-Fuller Test (ADF):
    test <- ur.df(input, type = "none", selectlags = "BIC");
    # H0 = unit root (non-stationary)
    # For rejection: test stat must SMALLER than critical values
    # Reject? → stationary around trend
    ifelse (test@teststat[1,1] < # tau3 test statistic
              test@cval[1,2], # 5% critical value for tau3 test statistic
            tbl[nrow(tbl), ]$ADF <- "Stationary", # reject
            tbl[nrow(tbl), ]$ADF <- "Unit Root")  # fail to reject
    
    # Phillips and Perron Unit Root Test (PP):
    test <- ur.pp(input,  type = "Z-tau", model = "constant")
    # H0 = unit root (non-stationary)
    # For rejection: test stat must LESS than critical values
    # Reject? → stationary around trend
    ifelse (test@teststat < # test statistic
              test@cval[1,2], # 5% critical value for test statistic
            tbl[nrow(tbl), ]$PP <- "Stationary", # reject
            tbl[nrow(tbl), ]$PP <- "Unit Root")  # fail to reject
    
    # Elliott, Rothenberg and Stock Unit Root Test (DF-GLS):
    test <- ur.ers(input, type = "DF-GLS", model = "constant")
    # H0 = unit root (non-stationary);
    # For rejection: test stat must LESS than critical values
    # Reject? → stationary around trend
    ifelse (test@teststat < # test statistic
              test@cval[1,2], # 5% critical value for test statistic
            tbl[nrow(tbl), ]$DF_GLS <- "Stationary", # reject
            tbl[nrow(tbl), ]$DF_GLS <- "Unit Root")  # fail to reject
    
    # Kwiatkowski-Phillips-Schmidt-Shin (KPSS):
    test <- ur.kpss(input, type = "mu")
    # H0 = trend-stationary (reject => likely unit root).
    # For rejection: statistics must be GREATER than critical values
    # Reject? → NOT trend-stationary (i.e., likely unit root)
    ifelse (test@teststat < # test statistic
              test@cval[1,2], # 5% critical value for test statistic
            tbl[nrow(tbl), ]$KPSS <- "Unit Root",  # reject
            tbl[nrow(tbl), ]$KPSS <- "Stationary") # fail to reject
    
    # Zivot-Andrews Unit Root with Endogenous Structural Break
    test <- ur.za(input, model = "intercept")
    # H0: unit root with a single structural break
    # For rejection: test stat must SMALLER than critical values
    # Reject? → stationary around trend
    ifelse (test@teststat < # test statistic
              test@cval[2], # 5% critical value for test statistic
            tbl[nrow(tbl), ]$Zivot_Andrews <- "Stationary with Break", # reject
            tbl[nrow(tbl), ]$Zivot_Andrews <- "Unit Root")  # fail to reject
  }
  
  tbl[nrow(tbl), ]$za_year <- time(input)[test@bpoint]
  
  return(tbl)
  }

  # Record Bai-Perron Break Years
bp_years <- function(tbl,         # Saving main values to table
                     t,           # Bai-Perron `breakpoints` model output
                     i,           # Material Category
                     model_spec)  # Model specification (Differnced/Trend)
{
  ci <- confint(t, breaks = length(t$breakpoints))
  
  tbl[nrow(tbl) + 1, ]$Category <- i
  tbl[nrow(tbl), ]$model_spec <- model_spec
  
  for (j in 1:length(t$breakpoints)){
  tbl[nrow(tbl), ][[paste0("break_",j)]] <-  breakdates(t)[j]
  tbl[nrow(tbl), ][[paste0("ci_",j)]] <- paste0("[",1969+ci[["confint"]][j,1],"-",1969+ci[["confint"]][j,3],"]")
  }
    # <- c(i,
    #                        model_spec,
    #                        breakdates(t)[1], #break_1
    #                        paste0(1969+ci[["confint"]][1,1],"-",1969+ci[["confint"]][1,3]),
    #                        breakdates(t)[2],
    #                        paste0(1969+ci[["confint"]][2,1],"-",1969+ci[["confint"]][2,3]),
    #                        breakdates(t)[3],
    #                        breakdates(t)[4],
    #                        breakdates(t)[5],
    #                        breakdates(t)[6],
    #                        breakdates(t)[7],
    #                        breakdates(t)[8],
    #                        breakdates(t)[9])
  return(tbl)
  }

  # Record Residual Tests
residual_tests <- function(resid,              # Model`residuals` output
                           tbl,                # Saving main values to table
                           i = NULL,           # Material Category
                           test_type = NULL,   # Plot Type
                           model_spec = NULL)  # Model specification
{#  Ljung-Box Testing: autocorrelation ("lack of fit")
  lb_4 <- Box.test(resid, lag = 4, type = "Ljung-Box") # Lag 4: suggested for regression residuals ~ ln(n) years
  lb_8 <- Box.test(resid, lag = 8, type = "Ljung-Box") # Lag 8: capture potential of business cycles ~ 8 years

  
  tbl[nrow(tbl) + 1, ] <- c(i,
                             test_type,
                             signif(lb_4$p.value, digits = 3),
                             stat_sig(lb_4$p.value),
                             signif(lb_8$p.value, digits = 3),
                             stat_sig(lb_8$p.value))
  return(tbl)}

  # Record Regression Coefficients
regr_coefs <- function(model,      # Regression Model
                       model_spec, # Model specification
                       i,          # Material Category
                       tbl)        # Saving main values to table
{
  test <- coeftest(model,
                   vcov = kernHAC(model,
                                  kernel    = "Bartlett",
                                  bandwidth = bwNeweyWest,
                                  prewhite  = FALSE)) # Extract Coefficients, accounting for heteroskedasticity and autocorrelation
  
  tbl[nrow(tbl) + 1, ] <- c(i,
                            model_spec,
                            format(test[1], scientific = TRUE),       # Initial Level (mean)
                            stat_sig(test[25]), #31
                            format(test[2], scientific = TRUE),           # Initial Trend (slope)
                            stat_sig(test[26]), #32
                            format(test[3], scientific = TRUE),       # 1981 - 1994: Post-Oil Crises/Pre-WTO
                            stat_sig(test[27]), #33
                            format(test[4], scientific = TRUE),     
                            stat_sig(test[28]), #34
                            # format(test[5], scientific = TRUE),       # 1995-1998: WTO Adjustment Period
                            # stat_sig(test[35]), #35
                            # format(test[6], scientific = TRUE),       #  [removed from regression]
                            # stat_sig(test[36]), #36
                            format(test[5], scientific = TRUE),       # 1999 - 2008: Post-WTO Transition Period
                            stat_sig(test[29]), #37
                            format(test[6], scientific = TRUE),     
                            stat_sig(test[30]), #38
                            format(test[7], scientific = TRUE),       # 2008 - : Post-Great Recession Period
                            stat_sig(test[31]), #39
                            format(test[8], scientific = TRUE),     
                            stat_sig(test[32]), #40
                            signif(summary(model)[["adj.r.squared"]], digits = 3))         # R-Squared, Adjusted (fit)
return(tbl)
}

### ✓ REUSED GGPLOT2 LAYERS AND PLOTTING FUNCTIONS ### ----
 ## Function to make data.frame of recession start and end dates, represented as quarters in decimals
find_recession_periods <- function(df, date_col = "date", recession_col = "value") {
  
  # Quarter month -> decimal mapping subfunction (for START dates)
  to_decimal_year_start <- function(date_str) {
    year  <- as.numeric(substr(date_str, 1, 4)) # saving year from string positions [1:4] as number
    month <- substr(date_str, 6, 7) # saving year from string positions [6:7]
    
    # quarter month -> decimal
    quarter_map_start <- c("01" = 0.00, "04" = 0.25, "07" = 0.50, "10" = 0.75)
    decimal_start <- quarter_map_start[month]
    
    # adding quarter decimal to year numerical
    year + decimal_start # Q4: e.g. 1986-10-01 -> 1986.75... 
  }
  
  # Quarter month -> decimal mapping subfunction (for END dates)
  to_decimal_year_end <- function(date_str) {
    year  <- as.numeric(substr(date_str, 1, 4)) # saving year from string positions [1:4] as number
    month <- substr(date_str, 6, 7) # saving year from string positions [6:7]
    
    # quarter month -> decimal
    quarter_map_end <- c("01" = 0.25, "04" = 0.50, "07" = 0.75, "10" = 1.00)
    decimal_end <- quarter_map_end[month]
    
    # adding quarter decimal to year numerical
    year + decimal_end # Q4: e.g. 1986-10-01 -> 1987.00... 
  }
  
  # Sort by date
  df <- df[order(df[[date_col]]), ]
  df$decimal_year_start <- to_decimal_year_start(df[[date_col]])
  df$decimal_year_end <- to_decimal_year_end(df[[date_col]])
  
  recession_periods <- list()
  in_recession <- FALSE
  start_quarter <- NULL
  
  # saving recession starts and ends as year.month decimal values
  for (i in seq_len(nrow(df))) {
    val <- df[[recession_col]][i]
    
    # check for first recession period when not in recession
    if (val == 1 && !in_recession) { 
      # Recession starts
      in_recession    <- TRUE
      start_quarter   <- df$decimal_year_start[i]}
    
    # check for no recession period when already in recession 
    else if (val == 0 && in_recession) {
      # Recession ends — previous row was the last recession quarter
      in_recession <- FALSE
      end_quarter  <- df$decimal_year_end[i - 1]
      recession_periods[[length(recession_periods) + 1]] <- 
        data.frame(start = start_quarter, end = end_quarter)
    }
  }
  
  # Handle recession that extends to end of data
  if (in_recession) {
    end_quarter <- df$decimal_year_end[nrow(df)]
    recession_periods[[length(recession_periods) + 1]] <- 
      data.frame(start = start_quarter, end = end_quarter)
  }
  
  do.call(rbind, recession_periods)
}
  
  # Calling FRED quarterly recession data, binary variable
temp <- fredr(series_id = "JHDUSRGDPBR",
            frequency = "q") %>% dplyr::select(c("date","value"))

  # Recession Highlights Layer
shaders <- {geom_rect(data = find_recession_periods(temp),
  aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, color = "Recession"),
  fill = "grey70", alpha = 0.4,
  inherit.aes = FALSE)}
  # Reused markers and labels
layers <- {list(a = scale_color_manual(name = NULL, values = c("Recession" = "lightgrey")),
               b = geom_hline(yintercept = 0, color = "black"),
               c = geom_vline(xintercept = 1995, linetype = "longdash"), # year of institutional change
               d = theme_classic())}

 ## ggplot2 function for aggregated and disaggregated plots
  # Outputs a 2x2 plot and 1x1 plot
plot_all_1_4 <- function(ttl,              # plot title
                         cpt,              # plot caption
                         file_id,          # unique file id 
                         shade = TRUE,     # add recession shaders legend
                         store = save_png) # save output to png.file? Otherwise print in plots
  {
  # Separate Aggregated Plot #
  if (store == TRUE){
    png(filename = paste0(path, file_id, "_Agg.png"), width = 10, height = 8, units = "in", res = 300)
  }
  print(Aggregate_plot #+ labs(#title = paste("Aggregated",ttl),
                      #caption = cpt)
        )
  if (store == TRUE){dev.off()}
    # Disaggregated Plots #
  if (store == TRUE){
  png(filename = paste0(path, file_id, "_Disagg.png"), width = 10, height = 8, units = "in", res = 300)
}
  if (shade == TRUE){
    if (file_id =="/Figures/IMF.full" | file_id =="/Figures/FAMI&MI"){
      print((`Biomass_plot` + `Fossil fuels_plot` + `Metal ores_plot` + `Non-metallic minerals_plot`) + 
              plot_annotation(#title = paste("Disaggregated",ttl),
                #caption = cpt,
                theme = theme(plot.title = element_text(hjust = 0.5))) +
              theme(legend.position=c(0.2,0.25)))
      }else if(ttl == "IMF, 1970 - 2024 (Country Comp)"){
        print((`Biomass_plot` + `Fossil fuels_plot` + `Metal ores_plot` + `Non-metallic minerals_plot`) + 
                plot_annotation(#title = paste("Disaggregated",ttl),
                  #caption = cpt,
                  theme = theme(plot.title = element_text(hjust = 0.5))) +
                theme(legend.position=c(0.1 ,0.85)))
    }else{
      print((`Biomass_plot` + `Fossil fuels_plot` + `Metal ores_plot` + `Non-metallic minerals_plot`) + 
              plot_annotation(#title = paste("Disaggregated",ttl),
                #caption = cpt,
                theme = theme(plot.title = element_text(hjust = 0.5))) +
              theme(legend.position=c(0.9,0.35)))}
  }
  if (shade == FALSE){
    print((`Biomass_plot` + `Fossil fuels_plot` + `Metal ores_plot` + `Non-metallic minerals_plot`) + 
            plot_annotation(#title = paste("Disaggregated",ttl),
                            #caption = cpt,
                            theme = theme(plot.title = element_text(hjust = 0.5))))
  }
  if (store == TRUE){dev.off()}
}

 ## ggplot2 function for aggregated and disaggregated plots
  # Outputs a 2x4 plot and 1x2 plot
plot_all_2_8 <- function(ts_dat,           # timeseries data
                         ttl = NULL,       # plot title
                         cpt = NULL,       # plot caption
                         file_id = NULL,   # unique file id
                         test_type = NULL, # plot type
                         model_spec = NULL,# test regression model
                         scale = 1e9,      # scaling for degrees of magnitude
                         exp = 1,          # exponential option for variance plotting
                         tbl1 = NULL,      # saving main test values table 1
                         tbl2 = NULL,      # saving auxiliary test values in different table (e.g., residual tests)
                         ts_new = NULL,    # new timeseries output (e.g., residuals)
                         store = save_png) # save output to png.file? Otherwise print in plots
{
  if (save_png == TRUE){
    png(filename = paste0(path, file_id, "_Disagg.png"), width = 10, height = 8, units = "in", res = 300)
    }
  par(mfrow = c(2,4))
  for (i in colnames(ts_dat)){
    # Plotting Aggregate on its own Figure
    if (i == "Aggregate"){
      if (save_png == TRUE){
        png(filename = paste0(path, file_id, "_Agg.png"), width = 10, height = 8, units = "in", res = 300)
        }
      par(mfrow = c(1,2))
      }
   
    # (P)ACF Plots
    if (test_type == "(p)acf"){ 
      acf(na.omit(ts_dat[,i]),  lag.max = 50, main = paste0(ttl,"(",i,")"))
      pacf(na.omit(ts_dat[,i]), lag.max = 50, main = paste0("P",ttl,"(",i,")"))
    }
    
    # Bai Perron Tests
    if (test_type == "Bai-Perron"){
      print(i) # Keep to track stage of loop if error occurs
 
      ## Linear Model Regression With Trend (Time)
      if (model_spec == "Trend"){
        model <- as.formula(paste0("ts_dat[,i] ~ time(ts_dat[,i])"))
      }
      
      ## Linear Model Regression Without Trend
      if (model_spec == "Differenced"){
        model <- as.formula(paste0("ts_dat[,i] ~ 1"))
      }
      
      test<-breakpoints(model, h = seg, breaks = brks, 
                        vcov = kernHAC(model,
                                       kernel    = "Bartlett",
                                       bandwidth = bwNeweyWest,
                                       prewhite  = FALSE))  # Bai-Perron Test on Regession
      ### Bai Perron Test Results
      # print(summary(lm(model)))
      # print(test)
      # print(breakdates(test)) 
      print(coef(test))
      bp <- length(test$breakpoints) # Optimal Number of Breakpoints
      
      ### Bai-Perron BIC and RSS Graph
      plot(summary(test)) 
      title(sub = paste0(i, " (",model_spec,"), h = ",seg, " ; ", bp,"/",brks, " breaks"))
      
      ### Plotting Regression Breakpoints and Confidence Intervals
      plot((ts_dat[,i]^exp)/scale, # Base plot
           xlab = "Year",
           ylab = "Tonnes of Material (Billions)",
           main = "Breaks and Confidence Intervals")
      title(sub = paste0(i, " (",model_spec,"), h = ",seg, " ; ", bp,"/",brks, " breaks"))
      lines(ts(fitted(lm(model))/scale, start = min(time(ts_dat)), frequency = 1), col = "red") # linear model fitted line (red solid)
      lines(fitted(test, breaks = bp)/scale, col = 4) # piecewise fitted lines (light blue solid)
      
      
      ### ignore confidence intervals out of bounds to avoid plotting error
      ci <- suppressWarnings(confint(test, breaks = bp))
      ci$confint <- suppressWarnings(ci$confint[complete.cases(ci$confint), , drop = FALSE])
      suppressWarnings(lines(ci)) # confidence intervals of break points (dashed black lines + red intervals on x-axis)
      suppressWarnings(print(ci))
      
      ### Save Break Year Results
      tbl1 <- bp_years(tbl1, test, i, model_spec)
      
      ### Save Residuals in Timeseries
      ts_new <- cbind(ts_new, ts(residuals(test), start = min(time(ts_dat)), frequency = 1))
      
      ### Save Residual Test Results
      tbl2 <- residual_tests(residuals(test), tbl2, i, test_type, model_spec)
    }
    
    if (i == "Non-metallic minerals" | i == "Aggregate"){
      if (save_png == TRUE){dev.off()}
    } # close dev plot saving window
  }
  
  if (!is.null(ts_new)){colnames(ts_new) = colnames(ts_dat)}
  
  if (!is.null(tbl1)){return(list(tbl1 = tbl1, tbl2 = tbl2, ts_new = ts_new))}
  }

### ✓ MF and DMC (1970-2024) ### ----
for(i in unique(data$Category)){
  temp <- data[data$Category == i,][101:155,]
  
  if (i != "Aggregate"){
    assign(paste0(i,"_plot"),
           ggplot() +
             shaders + # Recessions
             geom_line(data=temp,aes(x = year, y = (MF/1e9),color="MF"), linetype = 5) +
             geom_line(data=temp,aes(x = year, y = (DMC/1e9), color="DMC")) +
             scale_color_manual(name = NULL, values = c("MF" = "chartreuse4", "DMC" = "navy", "Recession" = "lightgrey")) +
             geom_vline(xintercept = 1995, color = "black", linetype = "longdash") + # year of institutional change
             annotate("text", x=1995, y=(0.9*(max(data[data$Category != "Aggregate",]$MF))/1e9), label = "WTO\n", colour="black", angle=90) + # Different label placement
             geom_hline(yintercept = 0, color = "black") +
             theme_classic() +
             theme(legend.position="none",
                   legend.background = element_rect(color = 1),
                   legend.key = element_rect(fill = NA, color = NULL)) +
             labs(x = "Year", y = "Tonnes of Material (Billions)", title = i) + 
             theme(plot.title = element_text(hjust = 0.5)) +
             ylim(NA, (max(data[data$Category != "Aggregate",]$MF))/1e9)) #Set y max limit to same for all disaggregated categories
  }else{
    assign(paste0(i,"_plot"),
           ggplot() +
             shaders + # Recessions
             geom_line(data=temp,aes(x = year, y = (MF/1e9),color="MF"), linetype = 5) +
             geom_line(data=temp,aes(x = year, y = (DMC/1e9), color="DMC")) +
             scale_color_manual(name = NULL, values = c("MF" = "chartreuse4", "DMC" = "navy", "Recession" = "lightgrey")) +
             geom_vline(xintercept = 1995, color = "black", linetype = "longdash") + # year of institutional change
             annotate("text", x=1995, y=(0.9*max((temp$MF/1e9))), label = "WTO\n", colour="black", angle=90) + # Different label placement
             geom_hline(yintercept = 0, color = "black") +
             theme_classic() +
             theme(legend.position=c(0.9, 0.25),
                   legend.background = element_rect(color = 1),
                   legend.key = element_rect(fill = NA, color = NULL)) +
             labs(x = "Year", y = "Tonnes of Material (Billions)", title = i) + 
             theme(plot.title = element_text(hjust = 0.5)))
  }
}
plot_all_1_4(ttl = "MF and DMC, 1970 - 2024",
             cpt = "Source: FRED (2026); UNEP (2025)",
             file_id = "/Figures/MF&DMC",
             store = save_png)
### ✓ MF per cap and DMC per cap (1970-2024) ### ----
mean.MF.FAMI <- data.frame(a = NA, b = NA, c = NA, d = NA, e = NA)
mean.MF.FAMI <- mean.MF.FAMI[rowSums(is.na(mean.MF.FAMI)) < ncol(mean.MF.FAMI), ] # remove NA row
colnames(mean.MF.FAMI) <-c("Category", "type","1970-1994","Overall", "1995-2024")

for(i in unique(data$Category)){
  temp <- data[data$Category == i,][101:155,]
  
  mean.MF.FAMI[nrow(mean.MF.FAMI) + 1,] <- c(i,"mean MF per capita (tonnes)", 
                                             round(mean(temp$MF[1:25]/temp$pop[1:25]), digits = 2),
                                             round(mean(temp$MF/temp$pop), digits = 2),
                                             round(mean(temp$MF[26:55]/temp$pop[26:55]), digits = 2))
  
  if (i != "Aggregate"){
    assign(paste0(i,"_plot"),
           ggplot() +
             shaders + # Recessions
             geom_line(data=temp,aes(x = year, y = (MF/pop),color="MF"), linetype = 5) +
             geom_line(data=temp,aes(x = year, y = (DMC/pop), color="DMC")) +
             scale_color_manual(name = NULL, values = c("MF" = "chartreuse4", "DMC" = "navy", "Recession" = "lightgrey")) +
             geom_vline(xintercept = 1995, color = "black", linetype = "longdash") + # year of institutional change
             annotate("text", x=1995, y=(0.9*(max(data[data$Category != "Aggregate",]$MF)/2.5e8)), label = "WTO\n", colour="black", angle=90) + # Different label placement
             geom_hline(yintercept = 0, color = "black") +
             theme_classic() +
             theme(legend.position="none",
                   legend.background = element_rect(color = 1),
                   legend.key = element_rect(fill = NA, color = NULL)) +
             labs(x = "Year", y = "Tonnes of Material per Capita", title = i) + 
             theme(plot.title = element_text(hjust = 0.5)) +
             ylim(NA, (max(data[data$Category != "Aggregate",]$MF)/2.5e8))) #Set y max limit to same for all disaggregated categories
  }else{
    assign(paste0(i,"_plot"),
           ggplot() +
             shaders + # Recessions
             geom_line(data=temp,aes(x = year, y = (MF/pop),color="MF"), linetype = 5) +
             geom_line(data=temp,aes(x = year, y = (DMC/pop), color="DMC")) +
             scale_color_manual(name = NULL, values = c("MF" = "chartreuse4", "DMC" = "navy", "Recession" = "lightgrey")) +
             geom_vline(xintercept = 1995, color = "black", linetype = "longdash") + # year of institutional change
             annotate("text", x=1995, y=(0.9*(max((temp$MF))/min((temp$pop)))), label = "WTO\n", colour="black", angle=90) + # Different label placement
             geom_hline(yintercept = 0, color = "black") +
             theme_classic() +
             theme(legend.position=c(0.9, 0.25),
                   legend.background = element_rect(color = 1),
                   legend.key = element_rect(fill = NA, color = NULL)) +
             labs(x = "Year", y = "Tonnes of Material per Capita", title = i) + 
             theme(plot.title = element_text(hjust = 0.5)))
  }
}

plot_all_1_4(ttl = "MF and DMC (per capita), 1970 - 2024",
             cpt = "Source: FRED (2026); UNEP (2025)",
             file_id = "/Figures/MF_pc&DMC_pc",
             store = save_png)

### ✓ PTB and RTB (1970-2024) ### ----
for(i in unique(data$Category)){
  temp <- data[data$Category == i,][101:155,]
  
  if (i != "Aggregate"){
    assign(paste0(i,"_plot"),
           ggplot() +
             shaders + # Recessions
             geom_line(data=temp,aes(x = year, y = (RTB/1e9),color=" RTB"), linetype = 5) +
             geom_line(data=temp,aes(x = year, y = (PTB/1e9), color=" PTB")) +
             scale_color_manual(name = NULL, values = c(" RTB" = "chartreuse4", " PTB" = "navy", "Recession" = "lightgrey")) +
             geom_vline(xintercept = 1995, color = "black", linetype = "longdash") + # year of institutional change
             annotate("text", x=1995, y=(0.9*(max(data[data$Category != "Aggregate",]$RTB))/1e9), label = "WTO\n", colour="black", angle=90) + # Different label placement
             geom_hline(yintercept = 0, color = "black") +
             theme_classic() +
             theme(legend.position="none",
                   legend.background = element_rect(color = 1),
                   legend.key = element_rect(fill = NA, color = NULL)) +
             labs(x = "Year", y = "Tonnes of Material (Billions)", title = i) + 
             theme(plot.title = element_text(hjust = 0.5)) +
             ylim(NA, (max(data[data$Category != "Aggregate",]$RTB))/1e9)) #Set y max limit to same for all disaggregated categories
  }else{
    assign(paste0(i,"_plot"),
           ggplot() +
             shaders + # Recessions
             geom_line(data=temp,aes(x = year, y = (RTB/1e9),color=" RTB"), linetype = 5) +
             geom_line(data=temp,aes(x = year, y = (PTB/1e9), color=" PTB")) +
             scale_color_manual(name = NULL, values = c(" RTB" = "chartreuse4", " PTB" = "navy", "Recession" = "lightgrey")) +
             geom_vline(xintercept = 1995, color = "black", linetype = "longdash") + # year of institutional change
             annotate("text", x=1995, y=(0.9*max((temp$RTB/1e9))), label = "WTO\n", colour="black", angle=90) + # Different label placement
             geom_hline(yintercept = 0, color = "black") +
             theme_classic() +
             theme(legend.position=c(0.9, 0.25),
                   legend.background = element_rect(color = 1),
                   legend.key = element_rect(fill = NA, color = NULL)) +
             labs(x = "Year", y = "Tonnes of Material (Billions)", title = i) + 
             theme(plot.title = element_text(hjust = 0.5)))
  }
}

plot_all_1_4(ttl = "RTB and PTB, 1970 - 2024",
             cpt = "Source: FRED (2026); UNEP (2025)",
             file_id = "/Figures/RTB&PTB",
             store = save_png)

### ✓ MI and FAMI (1970-2024) ### ----
for(i in unique(data$Category)){
  temp <- data[data$Category == i,][101:155,]
  
  mean.MF.FAMI[nrow(mean.MF.FAMI) + 1,] <- c(i,"mean FAMI (kilos per $USD, 2020 constant)", 
                                             round(mean(temp$FAMI[1:25]), digits = 2),
                                             round(mean(temp$FAMI), digits = 2),
                                             round(mean(temp$FAMI[26:55]), digits = 2))
  
  if (i != "Aggregate"){
    assign(paste0(i,"_plot"),
           ggplot() +
             shaders + # Recessions
             geom_line(data=temp,aes(x = year, y = (FAMI),color=" FAMI"), linetype = 5) +
             geom_line(data=temp,aes(x = year, y = (MI), color=" MI")) +
             scale_color_manual(name = NULL, values = c(" FAMI" = "chartreuse4", " MI" = "navy", "Recession" = "lightgrey")) +
             geom_vline(xintercept = 1995, color = "black", linetype = "longdash") + # year of institutional change
             annotate("text", x=1995, y=(0.9*(max(data[data$Category != "Aggregate",]$FAMI))), label = "WTO\n", colour="black", angle=90) + # Different label placement
             geom_hline(yintercept = 0, color = "black") +
             theme_classic() +
             theme(legend.position="none",
                   legend.background = element_rect(color = 1),
                   legend.key = element_rect(fill = NA, color = NULL)) +
             labs(x = "Year", y = "Kilos of Material per $USD of Output (2020 constant)", title = i) + 
             theme(plot.title = element_text(hjust = 0.5)) +
             ylim(NA, (max(data[data$Category != "Aggregate",]$FAMI))/1e9)) #Set y max limit to same for all disaggregated categories
  }else{
    assign(paste0(i,"_plot"),
           ggplot() +
             shaders + # Recessions
             geom_line(data=temp,aes(x = year, y = (FAMI),color=" FAMI"), linetype = 5) +
             geom_line(data=temp,aes(x = year, y = (MI), color=" MI")) +
             scale_color_manual(name = NULL, values = c(" FAMI" = "chartreuse4", " MI" = "navy", "Recession" = "lightgrey")) +
             geom_vline(xintercept = 1995, color = "black", linetype = "longdash") + # year of institutional change
             annotate("text", x=1995, y=(0.9*max((temp$FAMI))), label = "WTO\n", colour="black", angle=90) + # Different label placement
             geom_hline(yintercept = 0, color = "black") +
             theme_classic() +
             theme(legend.position=c(0.9, 0.15),
                   legend.background = element_rect(color = 1),
                   legend.key = element_rect(fill = NA, color = NULL)) +
             labs(x = "Year", y = "Kilos of Material per $USD of Output (2020 constant)", title = i) + 
             theme(plot.title = element_text(hjust = 0.5)))
  }
}

plot_all_1_4(ttl = "FAMI and MI, 1970 - 2024",
             cpt = "Source: FRED (2026); UNEP (2025)",
             file_id = "/Figures/FAMI&MI",
             store = save_png)

### ✓ Indirect Material Flows Study Period (1970-2024) ### ----
for (i in unique(data$Category)){
  temp <- data[data$Category == i,][101:155,]
  
  if (i != "Aggregate"){
    assign(paste0(i,"_plot"),
           ggplot(temp, aes(x = year, y = (IMF/1e9))) +
             shaders +
             layers +
             geom_line(color = "navy") +
             annotate("text", x=1995, y=(0.9*max(data[data$Category != "Aggregate",]$IMF)/1e9), label = "WTO\n", colour="black", angle=90) + # Different label placement
             labs(x = "Year", y = "Tonnes of Material (Billions)", title = i) +
             theme(legend.position = "none",
                   legend.background = element_rect(color = 1),
                   plot.title = element_text(hjust = 0.5)) +
             ylim(NA, (max(data[data$Category != "Aggregate",]$IMF))/1e9)) #Set y max limit to same for all disaggregated categories
  }else{
    assign(paste0(i,"_plot"),
           ggplot(temp, aes(x = year, y = (IMF/1e9))) +
             shaders +
             layers +
             geom_line(color = "navy") +
             annotate("text", x=1995, y=(0.9*max((temp$IMF/1e9))), label = "WTO\n", colour="black", angle=90) + # Different label placement 
             labs(x = "Year", y = "Tonnes of Material (Billions)", title = i) +
             theme(legend.position=c(0.9, 0.25),
                   legend.background = element_rect(color = 1),
                   plot.title = element_text(hjust = 0.5)))
  }
}

plot_all_1_4(ttl = "Indirect Trade Flows (Levels), 1970 - 2024",
             cpt = "Source: FRED (2026); UNEP (2025)",
             file_id = "/Figures/IMF",
             store = save_png)

### ✓ Indirect Trade Flows Full Series (1870-2024) ### ----
for (i in unique(data$Category)){
  temp <- data[data$Category == i,][1:155,]
  
  if (i != "Aggregate"){
    assign(paste0(i,"_plot"),
           ggplot() +
             # shaders + # Recessions
             layers +
             geom_line(data=temp, aes(x = year, y = IMF_gk/1e9, color = "G&K Only"),linetype = 6) +
             geom_line(data=temp, aes(x = year, y = (IMF/1e9), color = "Combined Data")) +
             scale_color_manual(name = NULL, values = c("G&K Only" = "firebrick", "Combined Data" = "navy")) +
             geom_line(color = "navy") +
             annotate("text", x=1995, y=(0.9*max(data[data$Category != "Aggregate",]$IMF)/1e9), label = "WTO\n", colour="black", angle=90) + # Different label placement
             geom_vline(xintercept = 1944, linetype = "longdash") + # Bretton Woods
             annotate("text", x=1944, y=(0.8*max(data[data$Category != "Aggregate",]$IMF)/1e9), label = "Bretton Woods\n", colour="black", angle=90) + # Different label placement
             geom_vline(xintercept = 1970, linetype = "longdash", colour = "goldenrod") + # Dataset Shift Year
             annotate("text", x=1970, y=(0.8*max(data[data$Category != "Aggregate",]$IMF)/1e9), label = "Dataset Transition\n", colour="black", angle=90) + # Dataset Transition
             theme_classic() +
             theme(legend.position="none",
                   legend.background = element_rect(color = 1),
                   legend.key = element_rect(fill = NA, color = NULL)) +
             labs(x = "Year", y = "Tonnes of Material (Billions)", title = i) +
             theme(plot.title = element_text(hjust = 0.5)) + 
             xlim(1870,2025) +
             ylim(NA, (max(data[data$Category != "Aggregate",]$IMF))/1e9)) #Set y max limit to same for all disaggregated categories
    }else{
      assign(paste0(i,"_plot"),
             ggplot() +
               # shaders + # Recessions
               layers +
               geom_line(data=temp, aes(x = year, y = IMF_gk/1e9, color = "G&K Only"),linetype = 6) +
               geom_line(data=temp, aes(x = year, y = (IMF/1e9), color = "Combined Data")) +
               scale_color_manual(name = NULL, values = c("G&K Only" = "firebrick", "Combined Data" = "navy")) +
               geom_line(color = "navy") +
               annotate("text", x=1995, y=(0.9*max((temp$IMF/1e9))), label = "WTO\n", colour="black", angle=90) + # Different label placement
               geom_vline(xintercept = 1944, linetype = "longdash") + # Bretton Woods
               annotate("text", x=1944, y=(0.85*max((temp$IMF/1e9))), label = "Bretton Woods\n", colour="black", angle=90) + # Different label placement
               geom_vline(xintercept = 1970, linetype = "longdash", colour = "goldenrod") + # Dataset Shift Year
               annotate("text", x=1970, y=(0.85*max((temp$IMF/1e9))), label = "Dataset Transition\n", colour="black", angle=90) + # Dataset Transition
               theme_classic() +
               theme(legend.position=c(0.9, 0.25),
                     legend.background = element_rect(color = 1),
                     legend.key = element_rect(fill = NA, color = NULL)) +
               labs(x = "Year", y = "Tonnes of Material (Billions)", title = i) +
               xlim(1870,2025) +
               theme(plot.title = element_text(hjust = 0.5)))
    }
  }

  # Ignore Warnings for this plot
suppressWarnings(plot_all_1_4(ttl = "Indirect Trade Flows (Levels), 1870 - 2024",
             cpt = "Source: Gierlinger & Krausmann, (2012); UNEP (2025); author's calculations",
             file_id = "/Figures/IMF.full",
             store = save_png))

cat("\014")
######################### TIMESERIES AND BREAK TESTING #########################
### ✓ Testing For Stationarity in Indirect Material Flows ### ----
 ## ✓ Timeseries Conversions; Visual Inpsection ## ----
  # ✓ Levels & Differenced # ----
  # Creating Levels Series & Plotting
data_ts <- NULL
for (i in names(cat_col)){
  data_ts <- cbind(data_ts,i=ts(study_data[[paste0("IMF_",i)]],
                                frequency = 1, start = 1970))
}
colnames(data_ts) = unname(cat_col)

  # Creating Differenced Series [Centered at Zero] & Plotting
data_ts.dif <- NULL
for (i in colnames(data_ts)){
  # Creating Series
  temp <- diff(data_ts[,i])-mean(diff(data_ts[,i]))
  data_ts.dif <- cbind(data_ts.dif, temp)
  
  #Plotting
  temp <- data.frame(year = time(temp),IMF_diff = temp)
  
  if (i != "Aggregate"){
    assign(paste0(i,"_plot"),
           ggplot(temp, aes(x = year, y = (IMF_diff/1e9))) +
             shaders +
             layers +
             geom_line(colour = "navy") +
             annotate("text", x=1995, y=(0.9*0.25), label = "WTO\n", colour="black", angle=90) + # Different label placement
             labs(x = "Year", y = "Tonnes of Material (Billions)", title = i) + 
             theme(legend.position = "none",
                   legend.background = element_rect(color = 1),
                   plot.title = element_text(hjust = 0.5)) +
             ylim(-0.25, 0.25)) #Set y limits to same for all disaggregated categories
  }else{
    assign(paste0(i,"_plot"),
           ggplot(temp, aes(x = year, y = (IMF_diff/1e9))) +
             shaders +
             layers +
             geom_line(colour = "navy") +
             annotate("text", x=1995, y=(0.9*max((temp$IMF_diff/1e9))), label = "WTO\n", colour="black", angle=90) + # Different label placement
             labs(x = "Year", y = "Tonnes of Material (Billions)", title = i) + 
             theme(legend.position=c(0.9, 0.25),
                   legend.background = element_rect(color = 1),
                   plot.title = element_text(hjust = 0.5)))
  }
}
colnames(data_ts.dif) = unname(cat_col)

plot_all_1_4(ttl = "Indirect Trade Flows (Differenced), 1970 - 2024",
             cpt = "Source: FRED (2026); UNEP (2025); author's calculations",
             file_id = "/Figures/IMF.dif",
             store = save_png)

 ## ✓ ACF / PACF ## ----
  # Create Subfolder for (P)ACF Graphs
if (dir.exists(file.path(paste0(path, "/Figures/(P)ACFs"))) == FALSE) {
  print("No subfolder for (P)ACF plots exists. Creating now in 'Figures' Folder...")
  dir.create(file.path(path, "Figures", "(P)ACFs"))
}
  # Levels
plot_all_2_8(ts_dat = data_ts,
             ttl = "ACF ", 
             file_id = "/Figures/(P)ACFs/IMF.(p)acf", 
             test_type = "(p)acf",
             store = save_png)

          # ACF - PACF # 
### Bio:  7 lag | 0 lag
### FFs: 14 lag | 0 lag
### Met: 16 lag | 0 lag
### NMM: 15 lag | 0 lag
### Agg: 16 lag | 0 lag

  # Differenced
plot_all_2_8(ts_dat = data_ts.dif,
             ttl = "ACF ", 
             file_id = "/Figures/(P)ACFs/IMF.dif.(p)acf", 
             test_type = "(p)acf",
             store = save_png)
          # ACF - PACF #
### Bio:  1 lag | 0 lag
### FFs:  0 lag | 0 lag
### Met:  2 lag | 0 lag
### NMM:  0 lag | 0 lag
### Agg:  2 lag | 1 lag

  # RESULTS: Levels and Differences
 ## All levels series exhibit long-term autocorrelation
 ## Immediate PACF dropoff => AR(1) processes
 ## Non-stationarity in levels mostly resolved with I(1) differencing

 ## ✓ Unit Root Testing ## ----
ur_table1 <- {data.frame(Category = character(),      # Material Category
                        model_spec = character(),    # model specification (e.g., Differenced/Trend)
                        ADF = character(),           # Augmented Dickey-Fuller Test
                        PP = character(),            # Phillips Perron Test
                        DF_GLS = character(),        # Dickey-Fuller Generalized Least Squares 
                        KPSS = character(),          # Kwiatkowski-Phillips-Schmidt-Shin Test
                        Zivot_Andrews = character(), # Zivot Andrews Test
                        za_year = numeric())        # Zivot Andrews Test Year
  } # Unit Root Test Table Template
ur_table2 <- ur_table1 # Secondary Unit Root Table for Later Residual Tests
ur_table3 <- ur_table1 # Tertiary Unit Root Table for Later Macro Data Analysis
  # Running Unit Root Tests on all Timeseries (Levels, Differenced)
for (i in colnames(data_ts)){
  #--------------------------UR Tests on Levels Series-------------------------#
  ur_table1 <- ur_tests(data_ts[,i],ur_table1,"Levels",i)
  # RESULTS: Unit Roots for all series
 ## Bio, Metals, Non-Metal, Aggregate => very not stationary (0/5 passed)
 ## FF => Not stationary (1/5)
  
  #-----------------------UR Tests on Differenced Series-----------------------#
  ur_table1 <- ur_tests(data_ts.dif[,i],ur_table1,"Differenced",i)
  # RESULTS: Stationary for all series
 ## Bio, Metals => uncertain (3/5 passed)
 ## FF, Non-Metal, Aggregate => Likely stationary (4/5)
}
 # CONCLUSION: 
 ## Series in Levels are Unit Root but Stationary with Differencing

### ✓ Structural Break Testing ### ----
  # ✓ Bai-Perron Tests # ----
  # Create Subfolder for Breaktests
if (dir.exists(file.path(paste0(path, "/Figures/Breaktests"))) == FALSE) {
  print("No subfolder for Breaktests exists. Creating now in 'Figures' Folder...")
  dir.create(file.path(path, "Figures", "Breaktests"))
}
bp_table <- {data.frame(Category = character(), # Material Category
                        model_spec= character(), # model specification (e.g., Differenced/Trend)
                        break_1 = numeric(),
                        ci_1 = character(),
                        break_2 = numeric(),
                        ci_2 = character(),
                        break_3 = numeric(),
                        ci_3 = character(),
                        break_4 = numeric(),
                        ci_4 = character(),
                        break_5 = numeric(), # h=0.15 generally doesn't produce more
                        ci_5 = character(),
                        break_6 = numeric(), # than 5 breaks, but if parameters changed
                        ci_6 = character(),
                        break_7 = numeric(), # can reach up to 8 breaks
                        ci_7 = character(),
                        break_8 = numeric(), 
                        ci_8 = character(),
                        break_9 = numeric(),
                        ci_9 = character()) # na columns will be later removed
  } # Reset Bai-Perron Test Table Template
resid_table <- {data.frame(Category = character(), # Material Category
                        model_spec = character(), # regression specification (e.g., Bai_Perron, Segmented Regression)
                        LB_lag4 = numeric(), # Lag 4: suggested for regression residuals ~ ln(n) years
                        lag4_sig = character(),
                        LB_lag8 = numeric(), # Lag 8: capture potential of business cycles ~ 8 years
                        lag8_sig = character())
  } # Reset Residual Table Template
  # Used to check autocorrelation/homoskedasticity of model/test residuals

  # Segment Length Trimming Parameter (h=0.15):
seg = 0.15 # segment size must be integer at least X% of the sample size (ideal: 0.1-0.2)
brks = 3 # max number of breaks in Bai-Perron Test                       (ideal: 3 <- based on LB Residual Testing)

  # ✓ Trend Structural Changes #
  # Breaks in the intercept and/or slope of trend
 ## Dates relevant, but cannot infer from coefficients because of I(1)
suppressWarnings(results <- plot_all_2_8(ts_dat = data_ts,
                         file_id = "/Figures/Breaktests/IMF.bp-chow.trend_lvl", 
                         test_type = "Bai-Perron",
                         model_spec = "Trend",
                         tbl1 = bp_table,
                         tbl2 = resid_table,
                         store = save_png))
bp_table <- results$tbl1
resid_table <- results$tbl2
data_ts.resid_bp <- results$ts_new

# Separate Tables for examining Ljung-box and ARCH Tests across different parameters
# assign(paste0("bp_table_",brks,"_",seg), bp_table[,colSums(is.na(bp_table))<nrow(bp_table)])
# assign(paste0("resid_table_",brks,"_",seg), resid_table)


  # ✓ Growth Structural Changes #
  # Detects breaks in the *change* of IMF rather than the level
  # Does rate of metabolic offshoring accelerate/deccelerate?
 ## Differenced since I(1)
suppressWarnings(results <- plot_all_2_8(ts_dat = data_ts.dif,
                        file_id = "/Figures/Breaktests/IMF.bp-chow.mean_diff",
                        test_type = "Bai-Perron",
                        model_spec = "Differenced",
                        tbl1 = bp_table,
                        tbl2 = resid_table,
                        store = save_png))

# bp_table <- results$tbl1
# resid_table <- results$tbl2
# data_ts.resid_bp.dif <- results$ts_new

  # Removing all NAs filled columns
bp_table <- bp_table[,colSums(is.na(bp_table))<nrow(bp_table)]

  # Transpose BP Table Across Diagonal for Better Fitting Format
bp_table <- as.data.frame(t(bp_table %>% dplyr::select(-c("model_spec", "Category")))) 
colnames(bp_table) <- unname(cat_col)
bp_table <- bp_table %>% rownames_to_column(var = "Category")

  # UR Tests on Residuals
for (i in colnames(data_ts.resid_bp)){
ur_table2 <- ur_tests(data_ts.resid_bp[,i],ur_table2,"Bai-Perron Residuals",i)
}

  # (P)ACF on Residuals
plot_all_2_8(ts_dat = data_ts.resid_bp,
             ttl = "ACF BP Residuals ", 
             file_id = "/Figures/(P)ACFs/IMF.resid_bp.(p)acf", 
             test_type = "(p)acf",
             store = save_png)

######## Interrupted Timeseries/Segmented Regression and Counterfactual ######## 
### ✓ Regression with Break Dates (1980, 1997/8, 2007/8) ### ----
if (dir.exists(file.path(paste0(path, "/Figures/Regressions"))) == FALSE) {
  print("No subfolder for Regressions plots exists. Creating now in 'Figures' Folder...")
  dir.create(file.path(path, "Figures", "Regressions"))
}

regr_table <- {data.frame(Category = character(),
                          model_spec = character(),
                          base_intercept = numeric(),   # Initial Level (mean)
                          sig1 = character(), 
                          base_slope = numeric(),       # Initial Trend (slope)
                          sig2 = character(),
                          Pre_WTO_intercept = numeric(),    # 1981 - 1994: Post-Oil Crises/Pre-WTO
                          sig3 = character(),
                          Pre_WTO_slope = numeric(),     
                          sig4 = character(),
                          # WTO_trans_intercept = numeric(),  # 1995-1998: WTO Adjustment Period
                          # sig5 = character(),
                          # WTO_trans_slope = numeric(),      #  [removed from regression]
                          # sig6 = character(),
                          Post_WTO_intercept = numeric(), # 1999 - 2008: Post-WTO Transition Period
                          sig5 = character(),
                          Post_WTO_slope = numeric(),     
                          sig6 = character(),
                          Post_GR_intercept = numeric(),    # 2009 - 2024: Post-Great Recession Period
                          sig7 = character(),
                          Post_GR_slope = numeric(),     
                          sig8 = character(),
                          r_square_adj = numeric())     # R-Squared, Adjusted (fit)
} # Regression Coefficient Table Template

## Adding dummy skip/slope variables for periods of interest as indicated by break tests
study_data <- study_data %>% mutate(
  Time = year-1970,                                            # 1970 - 1980: Oil Crises Period (Base)
  post_1980_skip = ifelse(year >= 1980, 1, 0),                 # 1981 - 1994: Post-Oil Crises/Pre-WTO Period
  post_1980_slope = ifelse(year >= 1980, year - 1979, 0),
  # WTO_transition_skip = ifelse(year >= 1995, 1, 0),            # 1995 - 1998: WTO Adjustment Period
  # WTO_transition_slope = ifelse(year >= 1995, year - 1994, 0), #  [removed from regression]
  post_1998_skip = ifelse(year >= 1998, 1, 0),                 # 1999 - 2008: Post-WTO Transition Period
  post_1998_slope = ifelse(year >= 1998, year - 1997, 0),
  post_2007_skip = ifelse(year >= 2007, 1, 0),                # 2009 - 2024: Post-Great Recession Period
  post_2007_slope = ifelse(year >= 2007, year - 2006, 0))      

 ## Interrupted Time Series Model: 
  # Baseline: y_t = beta_0 + beta_1*T ...
  # Dummy Components (Segmented Regression): ... + beta_2*X_t + beta_3*T*X_t ...
  # ... + ARIMA Components (unlikely to computate accurately due to limited sample size)
dummies <- c(
  "post_1980_skip","post_1980_slope",
  # "WTO_transition_skip","WTO_transition_slope", #  [removed from regression]
  "post_1998_skip","post_1998_slope",
  "post_2007_skip","post_2007_slope")

for (i in unname(cat_col)){
print(i)
test <- auto.arima(
  data_ts[,i],
  xreg = cbind(as.matrix(study_data %>% dplyr::select(dummies))),
  stepwise = FALSE,
  approximation = FALSE,
  seasonal      = FALSE
)
print(summary(test))
#checkresiduals(test)
}
  # Inital Interrupted Timeseries with Auto ARIMA Controlling for Breaks points converge
  # OLS behavior afterwards, likely due to sample size. Thus Segmented Regression alone 
  # without ARIMA component must suffice. Checking on Segmented Regression alone:

 ## Segmented Regression
data_ts.resid_sr <- NULL  # Residuals in tonnes
if (save_png == TRUE){
  png(filename = paste0(path, "/Figures/Regressions/IMF.Segm_Regr", "_Disagg.png"), width = 10, height = 8, units = "in", res = 300)
} # Prepping plots
par(mfrow = c(2,2))
for (i in names(cat_col)){

  #  [removed from regression] WTO_transition_skip + WTO_transition_slope +
  model <- lm(as.formula(paste0("IMF_",i," ~ Time +
  post_1980_skip + post_1980_slope +
  post_1998_skip + post_1998_slope +
  post_2007_skip + post_2007_slope")),
              data = study_data)

  # Saving Coefficients to Table
  regr_table <- regr_coefs(model,"Segmented ITS Regression", cat_col[[i]],regr_table)
  # print(coeftest(model, vcov = NeweyWest(model, lag = 4, prewhite = FALSE)))
  
  # Saving Regression Residuals to Time Series
  data_ts.resid_sr <- cbind(data_ts.resid_sr, ts(residuals(model), start = min(study_data$year), frequency = 1))
  
  # Plot Fitted Segmented Regression
    # Plotting Aggregate on its own Figure
    if (cat_col[[i]] == "Aggregate"){
      if (save_png == TRUE){
        png(filename = paste0(path, "/Figures/Regressions/IMF.Segm_Regr", "_Agg.png"), width = 10, height = 8, units = "in", res = 300)
      }
      par(mfrow = c(1,1))
    }
  
  suppressWarnings(plot(study_data$year,study_data[[paste0("IMF_",i)]]/1e9, type = "line", col = "navy",
       xlab = "Year",
       ylab = "Tonnes of Material (Billions)",
       main = cat_col[[i]]))
  lines(ts(fitted(model)/1e9, start = 1970, frequency = 1), col = "red")
  abline(v=1995, lty = "dashed")
  
  if (cat_col[[i]] == "Non-metallic minerals" | cat_col[[i]] == "Aggregate"){
    if (save_png == TRUE){dev.off()}
  }
}
colnames(data_ts.resid_sr) <- unname(cat_col) # rename columns

  # UR Tests on Residuals
  # & Residual Tests on Autocorrelation and Heteroskedasticity
for (i in colnames(data_ts.resid_sr)){
  ur_table2 <- ur_tests(data_ts.resid_sr[,i],ur_table2,"Segmented Regression Residuals",i)
  resid_table <- residual_tests(data_ts.resid_sr[,i], resid_table, i, "Segmented Regression Residuals", "Trend")
}

  # (P)ACF on Residuals
plot_all_2_8(ts_dat = data_ts.resid_sr,
             ttl = "ACF ", 
             file_id = "/Figures/(P)ACFs/IMF.resid_sr.(p)acf", 
             test_type = "(p)acf",
             store = save_png)

  # Transpose Regression Table Across Diagonal for Better Fitting Format
regr_table <- as.data.frame(t(regr_table %>% dplyr::select(-c("model_spec", "Category")))) 
colnames(regr_table) <- unname(cat_col)
regr_table <- regr_table %>% rownames_to_column(var = "Category")

  # Conclusion: Residuals of Segmented Regression demonstrate stationarity for all 
  # categories, and no autocorrelation except for Metal Ores
  # Counterfactual analysis will run pre-WTO on basic OLS forecasting

 ## Cumulative Effects
cumulative_effects <- {data.frame(Cat = character(),
                                 Bio = character(),
                                 FFs = character(),
                                 Met = character(),
                                 NMM = character(),
                                 Agg = character(),
                                 type = character())}
colnames(cumulative_effects) <-c ("Category", "Biomass", "Fossil fuels", "Metal ores", 
                                  "Non-metallic minerals", "Aggregate", "model_spec")
for (i in c("intercept", "slope")){
  temp <- regr_table[grepl(i, regr_table$Category), ] # slope and intercept column names
  
  temp[,c(unname(cat_col))] <- apply(temp[,c(unname(cat_col))], 2, cumsum) # summing rows
  
  temp[nrow(temp) + 1,
       sapply(temp, is.numeric)] <- temp %>%
    summarise(across(where(is.numeric), ~ .[3] - .[2]))
  
  temp[, sapply(temp, is.numeric)] <-temp[, sapply(temp, is.numeric)] %>% # reformatting numbers
    format(big.mark = ",", decimal.mark = ".", scientific = FALSE)
  
  temp$Category <- c("Baseline: 1970-1980", "Pre-WTO: 1981-1994", # [removed] "WTO-Trans: 1995-1998",
                     "Post-WTO: 1999-2009","Post-GR: 2010-2024", "WTO Transition Difference")
  
  if (i == "intercept"){temp$model_spec <- "Cumulative Levels (tonnes/year)"}
  if (i == "slope"){temp$model_spec <- "Cumulative Slope (tonnes/year^2)"}
  
  cumulative_effects<-bind_rows(cumulative_effects,temp)
}
### ✓ Counterfactual ### ----
 ## Plot Counterfactual Forecast
for (i in names(cat_col)){
  # Include the 1980 break to control for bias to the baseline slope estimate
  baseline_model <- lm(as.formula(paste0("IMF_",i," ~ Time ",
                                         "+ post_1980_skip + post_1980_slope")),
                       data = study_data %>% filter(year < 1995)) ## Pre-WTO Data
  
  # coeftest(baseline_model, vcov = NeweyWest(baseline_model, lag = 4, prewhite = FALSE))
  
  # Plot Counterfactual Forecast
  study_data <- study_data %>% mutate(
    #!!paste0("IMF_", i,"_ctfl") := predict(baseline_model, newdata = study_data), 
    
    # predict() with interval = "confidence" gives uncertainty on the mean
    !!paste0("IMF_",i,"_ctfl_ci") := 
      as.data.frame(predict(baseline_model, 
                        newdata = study_data,
                        interval = "confidence",
                        level = 0.95)),
    
    # predict() with interval = "prediction" gives uncertainty on new obs
    !!paste0("IMF_",i,"_ctfl_pred") := 
      as.data.frame(predict(baseline_model,
                        newdata = study_data,
                        interval = "prediction",
                        level = 0.95)) %>%
      rename(cf_pi_lo = lwr,
             cf_pi_hi = upr) %>%
      dplyr::select(cf_pi_lo, cf_pi_hi))
  
  if (cat_col[[i]] != "Aggregate"){
    assign(paste0(cat_col[[i]],"_plot"),
           ggplot(study_data, aes(x = year, y = .data[[paste0("IMF_",i)]] / 1e9)) +

             # Prediction interval — outer, lighter band
               geom_ribbon(aes(ymin = .data[[paste0("IMF_",i,"_ctfl_pred")]][['cf_pi_lo']] / 1e9,
                             ymax = .data[[paste0("IMF_",i,"_ctfl_pred")]][['cf_pi_hi']] / 1e9),
                         fill  = "goldenrod",
                         alpha = 0.30) +
             
             # Confidence interval on mean — inner, darker band
               geom_ribbon(aes(ymin = .data[[paste0("IMF_",i,"_ctfl_ci")]][['lwr']] / 1e9,
                             ymax = .data[[paste0("IMF_",i,"_ctfl_ci")]][['upr']] / 1e9),
                         fill  = "goldenrod",
                         alpha = 0.50) +
             
             # Counterfactual mean projection
               geom_line(aes(y = .data[[paste0("IMF_",i,"_ctfl_ci")]][['fit']] / 1e9),
                       colour = "firebrick", linetype = "dashed", linewidth = 0.8) +
               
             # Actual IMF plot
             layers +
             theme_gray() +
             geom_line(colour ="navy") +
             
               annotate("text", x=1995, y=(0.9*max(data[data$Category != "Aggregate",]$IMF)/1e9), label = "WTO\n", colour="black", angle=90) + # Different label placement
              labs(x = "Year", y = "Tonnes of Material (Billions)", title = cat_col[[i]]) + 
              ylim(NA, (max(data[data$Category != "Aggregate",]$IMF))/1e9)) #Set y max limit to same for all disaggregated categories
  }else{
    assign(paste0(cat_col[[i]],"_plot"),
           ggplot(study_data, aes(x = year,  y = .data[[paste0("IMF_",i)]] / 1e9)) +
            
             # Prediction interval — outer, lighter band
               geom_ribbon(aes(ymin = .data[[paste0("IMF_",i,"_ctfl_pred")]][['cf_pi_lo']] / 1e9,
                               ymax = .data[[paste0("IMF_",i,"_ctfl_pred")]][['cf_pi_hi']] / 1e9),
                           fill  = "goldenrod", alpha = 0.30) +
            
             # Confidence interval on mean — inner, darker band
               geom_ribbon(aes(ymin = .data[[paste0("IMF_",i,"_ctfl_ci")]][['lwr']] / 1e9,
                             ymax = .data[[paste0("IMF_",i,"_ctfl_ci")]][['upr']] / 1e9),
                         fill  = "goldenrod", alpha = 0.50) +
             
             # Counterfactual Mean projection
               geom_line(aes(y = .data[[paste0("IMF_",i,"_ctfl_ci")]][['fit']] / 1e9),
                       colour = "firebrick", linetype = "dashed", linewidth = 0.8) +
             
               
            # Actual IMF plot
             layers +
             theme_gray() +
             geom_line(colour ="navy") +
             
              annotate("text", x=1995, y=(0.9*max((study_data$IMF_agg/1e9))), label = "WTO\n", colour="black", angle=90) + # Different label placement
             
              labs(x = "Year", y = "Tonnes of Material (Billions)", title = cat_col[[i]]))
  }
}

suppressWarnings(plot_all_1_4(ttl = "IMF: Actual vs Pre-WTO Counterfactual",
             cpt = "UNEP (2025), author's calculations",
             file_id = "/Figures/Regressions/IMF.ctf",
             shade = FALSE,
             store = save_png))

 ## Total transition effect 
  # Levels
test <- study_data[study_data$year == 1999,]$IMF_agg -
study_data[study_data$year == 1999,]$IMF_agg_ctfl_ci[["fit"]]

cat("Estimated WTO transition Aggregate effect at 1999 (Mt):",
    round(test / 1e6, 2), "\n")
### ✓ NAFTA, Chinese WTO Ascension, and other Economies: Confounding Factors? ### ----
if (dir.exists(file.path(paste0(path, "/Figures/Country_Comp"))) == FALSE) {
  print("No subfolder for Country Comparison plots exists. Creating now in 'Figures' Folder...")
  dir.create(file.path(path, "Figures", "Country_Comp"))
}
### Separate Download from UNEP:
 ## Countries: Brazil, Canada, China, India, Mexico, USA, Vietnam
  # Comparison Groups
nation_groups <- data.frame(a = c("US","CA","MX"), # NAFTA: Canada, Mexico
                            b = c("US","CN","SA"), # Developing WTO Ascension: China (2001), Vietnam (2012)
                            c = c("US","BR","IN"), # Developing WTO -> GATT: Brazil, India
                            d = c("US","JP","DE"))  # Developed Economies
 ## Categories: Biomass, Fossil fuels, Metal ores, Non-metallic minerals
data_multi <- mfa_data_1 %>%
  filter(Flow.code == "MF" | Flow.code == "DMC")

# Reshape to long
data_multi <- data_multi %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               values_to = "value") %>%
  mutate(year = as.integer(str_remove(year, "X")),
         Country = countrycode(Country, "country.name", "iso2c")) # shorten country name

# Indirect Material Flow variable
temp <- data_multi %>%
  dplyr::select(Country, Category, Flow.code, year, value) %>%
  pivot_wider(names_from = Flow.code, values_from = value) %>%
  mutate(Flow.code = "IMF",
         value = MF - DMC) %>%
  dplyr::select(Country, Category, Flow.code, year, value)

#  Combine
data_multi <- bind_rows(data_multi %>% 
                          dplyr::select(Country, Category, Flow.code, year, value),
                        temp)

# Aggregate category
temp <- data_multi %>%
  group_by(Country, Flow.code, year) %>%
  summarise(Category = "agg",
            value = sum(value, na.rm = F),
            .groups = "drop")

data_multi <- bind_rows(data_multi, temp)

# Shorten category names 
data_multi <- data_multi %>%
  mutate(Category = case_when(
    str_to_lower(Category) == "biomass" ~ "bio",
    str_to_lower(Category) == "fossil fuels" ~ "ffs",
    str_to_lower(Category) %in% c("metal ores", "metals") ~ "met",
    str_to_lower(Category) %in% c("non-metallic minerals", "non-metallic") ~ "nmm",
    TRUE ~ Category))

# Build final column names
data_multi <- data_multi %>%
  mutate(name = paste(Country, Category, Flow.code, sep = "_")) %>%
  dplyr::select(year, name, value) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  arrange(year)

country_colors <- c(
  "US" = "navy",
  "CA" = "firebrick",
  "MX" = "chartreuse4",
  "CN" = "firebrick",
  "SA" = "chartreuse4",
  "BR" = "firebrick",
  "IN" = "chartreuse4",
  "JP" = "firebrick",
  "DE" = "chartreuse4"
)
make_color_scale <- function(series_names, palette = country_colors) {
  codes  <- substr(series_names, 1, 2)          # pull "US", "CN", etc.
  colors <- palette[codes]                       # look up colors by code
  setNames(colors, series_names)                 # name by full series string
}

data_multi <- data_multi %>% mutate(
  Time = year-1970,                                            # 1970 - 1917: Oil Crises Period (Base)
  post_1980_skip = ifelse(year >= 1980, 1, 0),                 # 1988 - 1994: Post-Oil Crises/Pre-WTO Period
  post_1980_slope = ifelse(year >= 1980, year - 1979, 0))      

#Plot all at once
for (j in nation_groups){
  for (i in names(cat_col)){
    temp <- data_multi %>%
      dplyr::select(year, contains(paste0(j,"_",i,"_IMF"))) %>%
      pivot_longer(   # reshape for plotting
        cols = -year,
        names_to = "series",
        values_to = "value")

    if (cat_col[[i]] != "Aggregate"){
      temp$series<-strtrim(temp$series,2)
      col_scale <- make_color_scale(unique(temp$series))
        if (j[2] == "CN"){
          assign(paste0(cat_col[[i]],"_plot"),
                                 ggplot() +
                                   shaders +
                                   geom_hline(yintercept = 0, color = "black") +
                                   geom_line(data =temp, aes(x = year, y = value/1e9, color = series)) +
                                   scale_color_manual(name = NULL, values = c(col_scale, "Recession" = "lightgrey"), breaks = names(col_scale)) +
                                   labs(title = paste0(cat_col[[i]]),
                                        x = "Year",
                                        y = "Billions of Tonnes of Material") +
                                   geom_vline(xintercept = 1995, linetype = "longdash") + # WTO: 1995
                                   annotate("text", x=1995, y=(0.9*(max(temp$value)/1e9)), label = "WTO\n", colour="black", angle=90) + # Different label placement
                                   geom_vline(xintercept = 2001, linetype = "longdash", colour="firebrick") + # CN -> WTO: Dec 2001
                                   # annotate("text", x=2001, y=(0.9*(max(temp$value)/1e9)), label = "CN -> WTO\n", colour="black", angle=90) + # Different label placement
                                   geom_vline(xintercept = 2005, linetype = "longdash", colour="chartreuse4") + # SA -> WTO: Dec 2005
                                   # annotate("text", x=2005, y=(0.9*(max(temp$value)/1e9)), label = "SA -> WTO\n", colour="black", angle=90) + # Different label placement
                                   theme_classic() +
                                   theme(legend.position="none",
                                         legend.background = element_rect(color = 1),
                                         legend.key = element_rect(fill = NA, color = NULL)))
          }else{assign(paste0(cat_col[[i]],"_plot"),
                       ggplot() +
                         shaders +
                         geom_hline(yintercept = 0, color = "black") +
                         geom_line(data = temp, aes(x = year, y = value/1e9, color = series)) +
                         scale_color_manual(name = NULL, values = c(col_scale, "Recession" = "lightgrey"), breaks = names(col_scale)) +
                         labs(title = paste0(cat_col[[i]]),
                              x = "Year",
                              y = "Billions of Tonnes of Material") +
                         geom_vline(xintercept = 1995, linetype = "longdash") + # NAFTA: Jan 1994
                         annotate("text", x=1995, y=(0.9*(max(temp$value)/1e9)), label = "WTO\n", colour="black", angle=90) + # Different label placement
                         
                         theme_classic() +
                         theme(legend.position="none",
                               legend.background = element_rect(color = 1),
                               legend.key = element_rect(fill = NA, color = NULL)))}
    }else{
      temp$series<-strtrim(temp$series,2)
      col_scale <- make_color_scale(unique(temp$series))
      if (j[2] == "CN"){assign(paste0(cat_col[[i]],"_plot"),
                               ggplot() +
                                 shaders +
                                 geom_hline(yintercept = 0, color = "black") +
                                 geom_line(data =temp, aes(x = year, y = value/1e9, color = series)) +
                                 scale_color_manual(name = NULL, values = c(col_scale, "Recession" = "lightgrey"), breaks = names(col_scale)) +
                                 labs(title = paste0(cat_col[[i]]),
                                      x = "Year",
                                      y = "Billions of Tonnes of Material") +
                                 geom_vline(xintercept = 1995, linetype = "longdash") + # WTO: 1995
                                 annotate("text", x=1995, y=(0.96*(max(temp$value)/1e9)), label = "WTO\n", colour="black", angle=90) + # Different label placement
                                 geom_vline(xintercept = 2001, linetype = "longdash", colour="firebrick") + # CN -> WTO: Dec 2001
                                 annotate("text", x=2001, y=(0.96*(max(temp$value)/1e9)), label = "CN -> WTO\n", colour="black", angle=90) + # Different label placement
                                 geom_vline(xintercept = 2005, linetype = "longdash", colour="chartreuse4" ) + # SA -> WTO: Dec 2005
                                 annotate("text", x=2005, y=(0.96*(max(temp$value)/1e9)), label = "SA -> WTO\n", colour="black", angle=90) + # Different label placement
                                 theme_classic() +
                                 theme(legend.position=c(0.1, 0.84),
                                       legend.background = element_rect(color = 1),
                                       legend.key = element_rect(fill = NA, color = NULL)))
      }else{assign(paste0(cat_col[[i]],"_plot"),
                   ggplot() +
                     shaders +
                     geom_hline(yintercept = 0, color = "black") +
                     geom_line(data = temp, aes(x = year, y = value/1e9, color = series)) +
                     scale_color_manual(name = NULL, values = c(col_scale, "Recession" = "lightgrey"), breaks = names(col_scale)) +
                     labs(title = paste0(cat_col[[i]]),
                          x = "Year",
                          y = "Billions of Tonnes of Material") +
                     geom_vline(xintercept = 1995, linetype = "longdash") + # NAFTA: Jan 1994
                     annotate("text", x=1995, y=(0.9*(max(temp$value)/1e9)), label = "WTO\n", colour="black", angle=90) + # Different label placement
                     theme_classic() +
                     theme(legend.position=c(0.1, 0.84),
                           legend.background = element_rect(color = 1),
                           legend.key = element_rect(fill = NA, color = NULL)))}
    }
  }
  plot_all_1_4(ttl = "IMF, 1970 - 2024 (Country Comp)",
               cpt = "Source: FRED (2026); UNEP (2025)",
               file_id = paste0("/Figures/Country_Comp/", j[1],"_", j[2],"_", j[3],".IMF"),
               store = save_png)
}

cat("\014")
####################### Relation to Capital Accumulation #######################
### ✓ Variable Correlations ### ----
data_ARDL <- study_data %>%
  dplyr::select(names(acc_vars))%>%
  cbind(data_ts.resid_sr[,"Aggregate"]/1e9) %>% # scaled by 1e9 to match variance magnitude (billions of tonnes)
  drop_na()
colnames(data_ARDL)[5] <- "IMF_agg_resid"

# saving correlation matrix to table
temp <- as.data.frame(round(cor(data_ARDL),3))
temp <- cbind(variable = rownames(temp), temp)
table <- gt(temp)
for (i in seq_len(nrow(temp))){
  table <- table %>%
    tab_style(
      style = cell_fill(color = "grey"),
      locations = cells_body(
        columns = i + 1,  # +1 because first column is 'variable'
        rows = i
      )
    )
}
table<-{table %>%
  cols_align(align = "right", columns = "variable") %>%
  opt_stylize(style = 3) %>%
  # tab_header( title = "Variable Correlation Table") %>%
  opt_align_table_header(align = "left") %>%
  opt_table_outline(style = "solid", width = px(3), color = "#D3D3D3") %>%
  tab_options(column_labels.background.color = "navy") %>%
  tab_style(
    style = cell_borders(sides = c("left","right"),
                         color = "#D3D3D3"),
    locations = cells_body()) %>%
  data_color(columns = "variable",
             rows = everything(),
             color = "darkgrey")}

if (save_png == T){
  gtsave(table, filename = paste0(path,"/Figures/Tables/","corr_table.png"))
}else{table}

### ✓ UR Testing ### ----
  # Plot in levels
if (save_png == TRUE){
  png(filename = paste0(path, "/Figures/", "Macro_vars.png"), width = 10, height = 8, units = "in", res = 300)
}
par(mfrow = c(2,2))
for (i in names(acc_vars)){
  if (i == "K_acc"){
    plot(study_data$year, study_data[[i]]*100, type = "l",
         main = acc_vars[[i]], xlab="Year", ylab="Percentage (%)")
    grid()}
  if (i == "p_share"){
    plot(study_data$year, study_data[[i]]*100, type = "l", 
         main = acc_vars[[i]], xlab="Year", ylab="Percentage (%)")
    grid()}
  if (i == "INT.lr"){
    plot(study_data$year, study_data[[i]], type = "l", 
         main = acc_vars[[i]], xlab="Year", ylab="Percentage (%)")
    grid()}
  if (i == "RULC"){
    plot(study_data$year, study_data[[i]], type = "l", 
         main = acc_vars[[i]], xlab="Year", ylab="Index (2020 = 100)")
    grid()}
} 
if (save_png == TRUE){dev.off()}
par(mfrow = c(1,1))

  # UR Tests
for (i in names(acc_vars)){
  ur_table3 <- ur_tests(ts(study_data[[i]], frequency = 1, start = 1970),ur_table3,"Levels",i)
  ur_table3 <- ur_tests(diff(ts(study_data[[i]], frequency = 1, start = 1970)),ur_table3,"Differenced",i)
}

### ✓ ARDL bounds test ### ----
  # Tests whether a long-run relationship exists between IMF residuals capital acc. proxies
  # H0: no long-run relationship (bounds test F-statistic below lower bound)
  # Rejection at upper bound: long-run relationship confirmed regardles of integration order
test <- lapply(setNames(names(acc_vars), names(acc_vars)), function(v) {
  
  df <- data_ARDL %>%
    dplyr::select("IMF_agg_resid", all_of(v)) %>%
    drop_na()
  
  # Automatic lag selection (max 5 lags)
  ardl_fit <- auto_ardl(
    as.formula(paste(v, "~ IMF_agg_resid")),
    data    = df,
    max_order = 5,
    selection = "BIC"
  )
  
  # Bounds F-test for level relationship
  bounds <- bounds_f_test(ardl_fit$best_model,
                          case = 3,
                          alpha = 0.05)   # case 3: unrestricted intercept
  # no trend — most common for macroeconomic relationships
  
  list(
    variable = v,
    ardl_order = ardl_fit$best_order,
    F_stat = round(bounds$statistic[[1]], 3),
    lower_I0 = round(bounds$parameters[[1]], 3),
    upper_I1 = round(bounds$parameters[[2]], 3),
    result = case_when(
      bounds$statistic[[1]] > bounds$parameters[[2]]
      ~ "Long-run relationship confirmed",
      bounds$statistic[[1]] < bounds$parameters[[1]]
      ~ "No long-run relationship",
      TRUE
      ~ "Inconclusive (between bounds)"),
    model = list(ardl_fit$best_model)
  )
})

  # Summary table
bounds_summary <- bind_rows(lapply(test, function(r) {
  tibble(
    Variable = r$variable,
    ARDL_order = paste(r$ardl_order, collapse = ","),
    F_stat  = r$F_stat,
    Lower_I0 = r$lower_I0,
    Upper_I1 = r$upper_I1,
    Result = r$result
  )
}))
print(bounds_summary)
### RESULT: K_acc has long Long-run relationship confirmed in ARDL(2,0)


  # Retrieve the ARDL model object for K_acc from bounds_results
K_acc.ardl <- test[["K_acc"]]$model[[1]]

  # Long-run multiplier: the equilibrium effect of a unit change in 
  # IMF residuals on K_acc, after all dynamic adjustment is complete

  # + positive => higher IMF associated with higher accumulation
  # - negative => higher IMF associated with lower accumulation
  # Magnitude: interpreted as the long-run level relationship
lr_coef <- multipliers(K_acc.ardl, type = "lr")

### ✓ Error correction model (ECM) - Long-run Relationship ### ----
 ## ect (error correction term)
  # ect = -0.3, then 30% of any deviation from
  # the long-run equilibrium between IMF and K_acc is corrected within one year
  # Expected range: between -1 and 0
  # Outside this range: explosive or non-convergent — recheck specification
K_acc.ecm <- recm(K_acc.ardl, case = 3)
test <- coeftest(K_acc.ecm, vcov = kernHAC(K_acc.ecm,
                                   kernel = "Bartlett",
                                   bandwidth = bwNeweyWest,
                                   prewhite = FALSE))

 ## Diagnostic checks on the ECM ----
  # The long-run relationship is only valid if the ECM residuals are well-behaved
ECM_checks <- data.frame(Test = character(),
                         type = character(),
                        `p>0.5` = character(),
                         Result = character())
colnames(ECM_checks) <- c("Test", "type", "IF (p > 0.5)", "Result")

  # Serial Correlation
ECM_checks[nrow(ECM_checks)+1,] <- c("Breusch-Godfrey Test", "unmodified ECM", "No Serial Correlation",
                                    round(bgtest(K_acc.ecm, order = 4)[["p.value"]],4))
  # General Heteroskedasticity
ECM_checks[nrow(ECM_checks)+1,] <- c("Breusch-Pagan Test", "unmodified ECM", "Homoskedastic residuals",
                                     round(bptest(K_acc.ecm)[["p.value"]],4))
  # ARCH Test for conditional heteroskedasticity
ECM_checks[nrow(ECM_checks)+1,] <- c("ARCH Test", "unmodified ECM", "No Conditional Heteroskedasticity",
                                     round(ArchTest(residuals(K_acc.ecm), lags = 4)[["p.value"]],4))
  # Jarque-Bera test for normality 
ECM_checks[nrow(ECM_checks)+1,] <- c("Jarque-Bera Test", "unmodified ECM", "Residuals Normally Distributed",
                                     round(jarque.bera.test(residuals(K_acc.ecm))[["p.value"]],4))

### !!! GENERAL HETERSKEDASTICITY CAUSED BY VARIANCE BREAK CENTERED AT 2009 !!! ### ----
plot(index(K_acc.ecm)+1969,residuals(K_acc.ecm)^2, type = "l", 
     main = "Squared ECM Residuals — Variance Pattern")
abline(v = 2009, col = "darkgreen", lty = 2)

### Manually constructing ECM ### ----
 ## Manually Estimate ARDL(2,0) ##
# summary(K_acc.ardl)
  # Extract coefficients from level equation
K_acc.ardl_coefs <- data.frame("phi1" = coef(K_acc.ardl)[["L(K_acc, 1)"]],
                               "phi2" = coef(K_acc.ardl)[["L(K_acc, 2)"]],
                               "beta0" = coef(K_acc.ardl)[["IMF_agg_resid"]], # contemporaneous only — order 0
                               "const" = coef(K_acc.ardl)[["(Intercept)"]])

 ## Compute ECM parameters manually
 # speed of adjustment
K_acc.ardl_coefs$lambda <- K_acc.ardl_coefs$phi1 + K_acc.ardl_coefs$phi2 - 1

 # SR K_acc dynamics
K_acc.ardl_coefs$gamma1 <- -K_acc.ardl_coefs$phi1

 # long-run multiplier
K_acc.ardl_coefs$theta <- K_acc.ardl_coefs$beta0 / abs(K_acc.ardl_coefs$lambda)

 ## Reconstruct ECT term manually
  # ECT = K_acc_{t-1} + theta * resid_{t-1}
  # In restricted ECM, ECT enters as single term multiplied by lambda
data_ECM <- data_ARDL 

data_ECM$K_acc_lag1 <- lag(data_ECM$K_acc, 1)                 # L(K_acc, 1) 1971:2024
data_ECM$IMF_agg_resid_lag1 <- lag(data_ECM$IMF_agg_resid, 1) # L(IMF_agg_resid, 1) 1971:2024

# data_ECM$ECT <- data_ECM$K_acc_lag1 + (K_acc.ardl_coefs$theta*data_ECM$IMF_agg_resid_lag1)
data_ECM$ECT <- K_acc.ecm[["data"]]$ect[1:55]

 ## Manually estimate the restricted ECM via dynlm
data_ECM$K_acc.dif <- data_ECM$K_acc - data_ECM$K_acc_lag1      # d(K_acc)
data_ECM$K_acc.dif_lag1 <- lag(data_ECM$K_acc.dif, 1)       # L(d(K_acc), 1),

  # This exactly replicates recm(ardl_model, case = 3) for ARDL(2,0)
test <- dynlm(K_acc.dif ~ K_acc.dif_lag1 + ECT, data = data_ECM)

 ## Verify against recm() output [K_acc.ecm]
  # Coefficients should match exactly
coef(K_acc.ecm)
coef(test)

 ## Step dummy (level shift) + slope dummy (trend shift) + HAC SEs
data_ECM$year <- index(data_ECM) + 1969
data_ECM$D2009 <- as.numeric(data_ECM$year >= 2009)
data_ECM$S2009 <- (data_ECM$year - 2009) * as.numeric(data_ECM$year >= 2009)

### SETUP: ECM with break dummies + HAC robust covariance ----
ECM_break <- dynlm(
  K_acc.dif ~ K_acc_lag1 + IMF_agg_resid_lag1 + K_acc.dif_lag1 + D2009 + S2009,
  data = data_ECM
)
 ## Diagnostic checks on the ECM (Again) ----
# Serial Correlation
ECM_checks[nrow(ECM_checks)+1,] <- c("Breusch-Godfrey Test", "dummy + HAC", "No Serial Correlation",
                                     round(bgtest(ECM_break, order = 4)[["p.value"]],4))
# General Heteroskedasticity
ECM_checks[nrow(ECM_checks)+1,] <- c("Breusch-Pagan Test", "dummy + HAC", "Homoskedastic residuals",
                                     round(bptest(ECM_break)[["p.value"]],4))
# ARCH Test for conditional heteroskedasticity
ECM_checks[nrow(ECM_checks)+1,] <- c("ARCH Test", "dummy + HAC", "No Conditional Heteroskedasticity",
                                     round(ArchTest(residuals(ECM_break), lags = 4)[["p.value"]],4))
# Jarque-Bera test for normality 
ECM_checks[nrow(ECM_checks)+1,] <- c("Jarque-Bera Test", "dummy + HAC", "Residuals Normally Distributed",
                                     round(jarque.bera.test(residuals(ECM_break))[["p.value"]],4))

# CUSUM stability: checks whether the long-run relationship
# is stable over the sample or breaks down at some point
plot(efp(ECM_break$terms, data = model.frame(ECM_break), type = "OLS-CUSUM"),
     main = "CUSUM stability: K_acc ECM",
     ylab = "Cumulative sum")

 ## Granger Causality Testing ## ----
  # IMF Residuals on K_acc, short and long run
sr_terms <- grep("IMF_agg_resid", names(coef(ECM_break)), value = TRUE)
lr_terms <- c("K_acc_lag1", "IMF_agg_resid_lag1")

sr_wald  <- waldtest(ECM_break, terms = sr_terms, vcov = vcovHAC(ECM_break))
ect_test <- coeftest(ECM_break, vcov = vcovHAC(ECM_break))["K_acc_lag1", ]

granger_table <- data.frame(
  Test = c("Short-run Granger",
           "ECT coefficient"),
  `test Statistic` = c(sr_wald$F[2],
                ect_test["t value"]),
  p_value = c(sr_wald$`Pr(>F)`[2],
              ect_test["Pr(>|t|)"]),
  Conclusion = c(
    ifelse(sr_wald$`Pr(>F)`[2] < 0.05, "SR causality confirmed", "No SR causality"),
    ifelse(ect_test["Pr(>|t|)"] < 0.05, "LR causality confirmed", "No LR causality")))

## key quantities for Error Correction Term (ect)
ECM_stats <- data.frame (Statistic = c("Long-run coefficient (θ)",
                                      "Standard error",
                                      "Error correction term",
                                      "t-statistic",
                                      "p-value",
                                      "Implied adjustment speed"),
                        Value = c(round(K_acc.ardl_coefs$theta, 4),
                                  round(multipliers(K_acc.ardl, type = "lr")[2,3], 4), # @["IMF_agg_resid","Std. Error"]
                                  round(ect_test[[1]], 4),
                                  round(ect_test[[3]], 3),
                                  signif(ect_test[[4]], 3),
                                  paste(round(abs(ect_test[[1]])*100, 2),
                                        "% of disequilibrium corrected per year"))
)

# ── Mean adjustment lag from ECT coefficient
cat("Mean adjustment lag:", round(-1 / ect_test[[1]], 1), "years\n")
# Half-life of disequilibrium: time for 50% correction
cat("Half-life of disequilibrium:", round(log(0.5) / log(1 + ect_test[[1]]), 1), "years\n")


  # Test short-run Granger in p_share, INT.lr, and RULC
data_ECM$p_share_lag1 <- lag(data_ECM$p_share, 1)           # L(p_share, 1) 1971:2024
data_ECM$INT.lr_lag1 <- lag(data_ECM$INT.lr, 1)             # L(INT.lr, 1) 1971:2024
data_ECM$RULC_lag1 <- lag(data_ECM$RULC, 1)                 # L(RULC, 1) 1971:2024

data_ECM$p_share.dif <- data_ECM$p_share - data_ECM$p_share_lag1      # d(p_share)
data_ECM$INT.lr.dif <- data_ECM$INT.lr - data_ECM$INT.lr_lag1      # d(INT.lr)
data_ECM$RULC.dif <- data_ECM$RULC - data_ECM$RULC_lag1      # d(p_share)
data_ECM$IMF_agg_resid.dif <- data_ECM$IMF_agg_resid - data_ECM$IMF_agg_resid_lag1 # d(IMF_agg_resid)

  # Bivariate VAR in differences for each non-cointegrated pair
run_sr_granger <- function(dep_var, data, p = 2) {
  
  var_mat <- data %>%
    dplyr::select(IMF_agg_resid.dif, all_of(dep_var)) %>%
    drop_na() %>%
    as.matrix()
  
  var_mod <- VAR(var_mat, p = p, type = "const")
  
  # IMF → accumulation variable
  fwd <- causality(var_mod, cause = "IMF_agg_resid.dif")$Granger
  # Reverse
  rev <- causality(var_mod, cause = dep_var)$Granger
  
  tibble(
    Variable       = dep_var,
    `Forward Causality p-value`= round(fwd$p.value, 4),
    `Reverse Causality p-value` = round(rev$p.value, 4),
    `Forward Causality: IMF -> var`  = fwd$p.value < 0.05,
    `Reverse Causality: var -> IMF`  = rev$p.value < 0.05
  )
}

sr_granger_results <- bind_rows(lapply(
  c("p_share.dif", "INT.lr.dif", "RULC.dif"),
  run_sr_granger,
  data = data_ECM
))

################################################################################
#### CREATE TABLES #### ----
  # Create Subfolder for Tables
if (dir.exists(file.path(paste0(path, "/Figures/Tables"))) == FALSE) {
  print("No subfolder for Tables exists. Creating now in 'Figures' Folder...")
  dir.create(file.path(path, "Figures", "Tables"))
}

# Save or display all tables #
for (i in names(table_names)){
    table <- gt(get(i), groupname_col = "model_spec") %>%
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = ""
    ) %>%
    cols_align(align = "center", columns = c(colnames(get(i)))) %>%
    cols_align(align = "right", columns = "Category") %>%
    opt_stylize(style = 3) %>%
    tab_style(
      style = cell_borders(sides = c("left","right"),
                           color = "#D3D3D3"),
      locations = cells_body()) %>% 
    # tab_style(style = cell_text(size = px(16),
    #                             font = "Times New Roman"),
    #           locations = cells_body()) %>%
    opt_table_outline(style = "solid", width = px(3), color = "#D3D3D3") %>%
    tab_options(column_labels.background.color = "navy") %>%
    # tab_footnote(footnote = "All results reported at minimum 5% significance level") %>%
    # tab_source_note(source_note = "Source: authors' own calculations") %>%
    # tab_header( title = table_names[[i]]) %>%
    # tab_options(
    #   table.font.names = "Times New Roman",
    #   table.font.size = px(16)) %>%
    opt_align_table_header(align = "left")
  
  if (save_png == T){
    gtsave(table, filename = paste0(path,"/Figures/Tables/",i,".png"), expand = 10)
  }else{print(table)}
}
################################################################################
#### OTHER TABLES, Not Automated ####
 ### MFA Breakdown ### ----
table <- {data.frame(Name = c("Domestic Extraction", 
                              "Imports", "Exports", "Physical Trade Balance", "Domestic Material Consumption", "Material Intensity",
                              "Raw Material Equivalent of Imports","Raw Material Equivalent of Exports", "Raw Trade Balance", "Material Footprint","Footprint-Adjusted Material Intensity",
                              "Indirect Material Flow"),
                     Acronym = c("DE",
                                 "I","E","PTB","DMC","MI",
                                "RME_imp","RME_exp","RTB","MF","FAMI",
                                "IMF"),
                    Formula = c("",
                                "","","= I - E", "= DE + PTB", "= DMC / GDP",
                                "","","= RME_imp - RME_exp", "= DE + RTB", "= MF / GDP",
                                "= RTB - PTB (or MF - DMC)"),
                    Unit = c("tonnes per year",
                             "tonnes per year","tonnes per year","tonnes per year","tonnes per year","kg per $USD",
                             "tonnes per year","tonnes per year","tonnes per year","tonnes per year","kg per $USD",
                             "tonnes per year"),
                    Description = c("Raw materials from national sources",
                                    "Direct raw material flows from the Rest of the World",
                                    "Direct raw material flows to the Rest of the World",
                                    "Difference between imports and exports",
                                    "Total raw materials directly utilized in economic activity",
                                    "Efficiency of material use relative to GDP",
                                    "Direct and indirect upstream material requirements embodied in imports",
                                    "Direct and indirect upstream material requirements embodied in exports",
                                    "Difference between import and exports, including indirect embodied flows",
                                    "Sum of direct and embodied raw material consumption",
                                    "Efficiency of material use corrected for MF",
                                    "The flow of embedded materials implied by a difference in the raw and physical trade balances"))} %>%
  gt() %>%
  # cols_align(align = "right", columns = c("Acronym")) %>%
  cols_align(align = "center", columns = c("Acronym","Formula","Unit")) %>%
  cols_width(Name ~ px(200),
             Formula ~ px(125),
             Unit ~ px(125),
             Description ~ px(400)) %>%
  opt_stylize(style = 3) %>%
  tab_style(
    style = cell_borders(sides = c("left","right"),
                         color = "#D3D3D3"),
    locations = cells_body()) %>% 
  # tab_style(style = cell_text(size = px(16),
  #                             font = "Times New Roman"),
  #           locations = cells_body()) %>%
  opt_table_outline(style = "solid", width = px(3), color = "#D3D3D3") %>%
  tab_options(column_labels.background.color = "navy") %>%
  # tab_footnote(footnote = "All results reported at minimum 5% significance level") %>%
  # tab_source_note(source_note = "Source: authors' own calculations") %>%
  # tab_header( title = "Material Flow Analysis Indicators") %>%
  # tab_options(
  #   table.font.names = "Times New Roman",
  #   table.font.size = px(16)) %>%
  opt_align_table_header(align = "left")
table
gtsave(table, filename = paste0(path,"/Figures/Tables/","MFA_indicators",".png"), expand = 10)

 ### Mean FAMI and Per capita MF ###----
table <- mean.MF.FAMI %>%
  gt(groupname_col = "type") %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_width(everything() ~ px(155)) %>%
  opt_stylize(style = 3) %>%
  tab_style(
    style = cell_borders(sides = c("left","right"),
                         color = "#D3D3D3"),
    locations = cells_body()) %>% 
  opt_table_outline(style = "solid", width = px(3), color = "#D3D3D3") %>%
  tab_options(column_labels.background.color = "navy")
table
gtsave(table, filename = paste0(path,"/Figures/Tables/","mean_MF&FAMI",".png"), expand = 10)

 ### UR Test Specifications ### ----
table <- {data.frame(test = c("ADF:`ur.df()`", "ADF:`ur.df()`",
                              "PP:`ur.pp()`", "PP:`ur.pp()`",
                              "KPSS:`ur.kpss()`",
                              "ADF-GLS:`ur.ers()`", "ADF-GLS:`ur.ers()`",
                              "ZA:`ur.za()`"),
                     Setting = c("type =", "selectlags =",
                                 "type =", "model =",
                                 "type =",
                                 "type =", "model =",
                                 "model ="),
                     Levels = c("trend", "BIC",
                                          "Z-tau", "trend",
                                          "tau",
                                          "DF-GLS", "trend",
                                          "both"),
                     `Differenced/Residuals` = c("none", "BIC",
                                     "Z-tau", "constant",
                                     "mu",
                                     "DF-GLS", "constant",
                                     "intercept"))}
colnames(table) <- c("test","Setting","Levels","Differenced/ Residuals")
table <- table %>%
  gt(groupname_col = "test") %>%
  cols_align(align = "right", columns = "Setting") %>%
  cols_align(align = "center", columns = c("Levels", "test", "Differenced/ Residuals")) %>%
  cols_width(everything() ~ px(125)) %>%
  opt_stylize(style = 3) %>%
  tab_style(
    style = cell_borders(sides = c("left","right"),
                         color = "#D3D3D3"),
    locations = cells_body()) %>% 
  opt_table_outline(style = "solid", width = px(3), color = "#D3D3D3") %>%
  tab_options(column_labels.background.color = "navy")
table
gtsave(table, filename = paste0(path,"/Figures/Tables/","ur_test_specs",".png"), expand = 10)
 
 ### ARDL Bound Test Summary ### ----
table <- bounds_summary %>%
  gt() %>%
  cols_align(align = "center", everything()) %>%
  cols_width(everything() ~ px(100)) %>%
  cols_width(columns = "Result" ~ px(250)) %>%
  opt_stylize(style = 3) %>%
  tab_style(
    style = cell_borders(sides = c("left","right"),
                         color = "#D3D3D3"),
    locations = cells_body()) %>% 
  opt_table_outline(style = "solid", width = px(3), color = "#D3D3D3") %>%
  tab_options(column_labels.background.color = "navy")
table
gtsave(table, filename = paste0(path,"/Figures/Tables/","ARDL_bounds_test",".png"), expand = 10)

 ### ECM Checks ### ----
table <- ECM_checks %>%
  gt(groupname_col = "type") %>%
  cols_align(align = "right", columns = "Test") %>%
  cols_align(align = "center", columns = c("IF (p > 0.5)", "Result")) %>%
  cols_width(columns = -c("Result") ~ px(200)) %>%
  cols_width(columns = c("Result") ~ px(100)) %>%
  opt_stylize(style = 3) %>%
  tab_style(
    style = cell_borders(sides = c("left","right"),
                         color = "#D3D3D3"),
    locations = cells_body()) %>% 
  opt_table_outline(style = "solid", width = px(3), color = "#D3D3D3") %>%
  tab_options(column_labels.background.color = "navy")
table
gtsave(table, filename = paste0(path,"/Figures/Tables/","ECM_checks",".png"), expand = 10)

 ### ECM Stats ### ----
table <- ECM_stats %>%
  gt() %>%
  cols_align(align = "center", everything()) %>%
  cols_align(align = "right", columns = "Statistic") %>%
  cols_width(columns = "Statistic" ~ px(150)) %>%
  cols_width(columns = "Value" ~ px(250)) %>%
  opt_stylize(style = 3) %>%
  tab_style(
    style = cell_borders(sides = c("left","right"),
                         color = "#D3D3D3"),
    locations = cells_body()) %>% 
  opt_table_outline(style = "solid", width = px(3), color = "#D3D3D3") %>%
  tab_options(column_labels.background.color = "navy")
table
gtsave(table, filename = paste0(path,"/Figures/Tables/","ECM_stats",".png"), expand = 10)

 ### Granger Test Results: K_acc ### ----
table <- granger_table  %>%
  gt() %>%
  cols_align(align = "center", everything()) %>%
  cols_align(align = "right", columns = "Test") %>%
  cols_width(everything() ~ px(200)) %>%
  opt_stylize(style = 3) %>%
  tab_style(
    style = cell_borders(sides = c("left","right"),
                         color = "#D3D3D3"),
    locations = cells_body()) %>% 
  opt_table_outline(style = "solid", width = px(3), color = "#D3D3D3") %>%
  tab_options(column_labels.background.color = "navy")
table
gtsave(table, filename = paste0(path,"/Figures/Tables/","Granger_Results",".png"), expand = 10)

 ### SR Granger Test Results: p_share, INT.lr, and RULC ### ----
table <- sr_granger_results  %>%
  gt() %>%
  cols_align(align = "center", everything()) %>%
  cols_align(align = "right", columns = "Variable") %>%
  cols_width(everything() ~ px(150)) %>%
  opt_stylize(style = 3) %>%
  tab_style(
    style = cell_borders(sides = c("left","right"),
                         color = "#D3D3D3"),
    locations = cells_body()) %>% 
  opt_table_outline(style = "solid", width = px(3), color = "#D3D3D3") %>%
  tab_options(column_labels.background.color = "navy")
table
gtsave(table, filename = paste0(path,"/Figures/Tables/","Granger_Results_SR",".png"), expand = 10)

