##============================================================================##
####                           SPECIFY ANY CONSTANTS                        ####
##============================================================================##

SEED  <- 36; # Chosen for personal reasons
CORES <- parallel::detectCores()


#theme_set(theme_gray())

##============================================================================##
####                           BASIC FUNCTIONS                              ####
##============================================================================##

demean <- function(x) {x - mean(x)}
standardise <- function(x) { demean(x)/sd(x)}
zeros  <- function(x) {sum(x == '1')}
ones   <- function(x) {sum(x == '7')}
halves <- function(x) {sum(x == '4')}

##============================================================================##
####                           LOAD LIRARIES AND DATA                       ####
##============================================================================##


require(data.table)     # Personal preference since SQL-like
require(ggplot2)        # For EDA
require(bayesplot)      # For convergence
require(loo)            # For cross-validation
require(VIM)            # For missing data
require(dplyr)          # Some quick manipulations. 
require(brms)           # for making model. 
require(cowplot)        # grid plots, makes some other stuff look nice
require(xtable)         # Tables for LaTeX. 


## THEME SET
theme_set(theme_cowplot())
theme_update(legend.position = 'top')
#ggplot2::theme_update(plot.title = ggplot2::element_text(size = 10))




##============================================================================##
####                            LOAD DATA                                   ####
##============================================================================##

## Set path for data, and state what counts as missing values

DATAPATH      <- 'Data/Copy of Abx_drug_issues_final version.xlsx'
MISSING      <- c("NA", '', ' ')
data.full <- readxl::read_excel(path = DATAPATH, sheet = 1, na = MISSING)
data.full <- setDT(data.full)


## Rename columns
colnames(data.full) <- c('Name', 'Strength', 'Route', 'TRAK', 'Week', 
                         'DDD.Total', 'DDDp1kOBD', 'AvgAge', 'Class', 
                         'Proportion')

str(data.full)

### INSTANTLY DROP STRENGTH, AS USELESS. 

data.full$Strength <- NULL

### Shorten class names

classmap <- list()
for (classname in sort(unique(data.full$Class))){
  classmap[[classname]] <- classname
}

unique(classmap)
classmap[[1]]  <- 'Aminoglycoside'
classmap[[2]]  <- 'Anti-TB'
classmap[[3]]  <- 'BL - Broad Pen'
classmap[[4]]  <- 'BL - Carbapenem'
classmap[[5]]  <- 'BL - Cephalosporin'
classmap[[6]]  <- 'BL - Extend Pen'
classmap[[7]]  <- 'BL - Monobacatam'
classmap[[8]]  <- 'BL - Narrow Pen'
classmap[[9]]  <- 'Chloramphenicol'
classmap[[10]] <- 'Glyco/Lipopeptitdes'
classmap[[11]] <- 'Macrol/Lincosamide'
classmap[[12]] <- 'Nitroimidazole'
classmap[[13]] <- 'Other'
classmap[[14]] <- 'Oxalidinone'
classmap[[15]] <- 'Polymixin'
classmap[[16]] <- 'Quinolones'
classmap[[17]] <- 'Tetracycline'
classmap[[18]] <- 'Trime./Sulphur'

classtransform <- function(classname) {classmap[[classname]]}

data.full[, `:=`(Class = vapply(Class, 
                                classtransform, 
                                FUN.VALUE = character(1)))]


##============================================================================##
####                     VISUALISE THE MISSINGNESS                          ####
##============================================================================##

## Keep path here for easy alteration. 

SAVEPATH <- 'Figures/1_Missing.png'



aggr(data.full, cex.axis = 1, oma = c(7, 5, 6, 3), numbers = TRUE, sortVars = T)
title('Variable Missingness')
myplot <- recordPlot()

png(filename = SAVEPATH, width = 800, 500)
myplot
dev.off()

### We'll take the complete cases

data.cc <- na.omit(data.full)
aggr(data.cc, cex.axis = 1, oma = c(7, 5, 6, 3), numbers = TRUE, sortVars = T)



##============================================================================##
####                     INITIAL STAGE FEATURE CHANGES                      ####
##============================================================================##

# As string to maintain data table structure
unique(data.cc[, 'Week'])[order(Week)]

data.cc[, `:=`(Name  = as.factor(Name), 
               Route = as.factor(Route), 
               TRAK  = as.factor(TRAK), 
               Class = as.factor(Class), 
               Week  = factor(as.numeric(substring(Week, 5)), ordered = TRUE))]



### Tables to see if group differences. 

mytabulate <- function(dt, group){
  # get mean and count of each group. 
  
  dt[, .("MeanProportion" = vapply(.SD, mean, numeric(1)), 
         "SDProportion" = vapply(.SD, sd, numeric(1)),
         N = .N), 
     by = group, 
     .SDcols = 'Proportion']
}


t.week  <- mytabulate(data.cc, 'Week')[order(Week)]
t.route <- mytabulate(data.cc, 'Route')[order(Route)]
t.trak  <- mytabulate(data.cc, 'TRAK')[order(TRAK)]
t.drug  <- mytabulate(data.cc, 'Name')[order(Name)]
t.class <- mytabulate(data.cc, 'Class')[order(Class)]




##============================================================================##
####                  COMMENTS AND TRANSFORMATIONS                          ####
##============================================================================##

### Class: Visualise later
### Route: Check out

tempdata <- readxl::read_xlsx('Data/Copy of Abx_drug_issues_final version.xlsx',
                              sheet = 2)
paste(tempdata[4, 3])
rm(tempdata)

## I: Inhalation
## O: Oral
## P: Injection/Intraveneous. 



data.cc[, `:=`(Route = (ifelse(Route %in% c("I", "O"), "Non-Invasive", 
                        ifelse(Route %in% c("P"), "Invasive", Route))))]

data.cc[, `:=`(Route = as.factor(Route))]



