
# ipak function: install and load multiple R packages----

# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak = function(pkg){
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


# usage----

ipak(c("tidyverse", "haven", "readxl", "lubridate", "data.table", "modelr", "broom", "leaflet", "stargazer", "AER", "ggmap", "geosphere", "Hmisc"))

#devtools::install_github("rstudio/sparklyr")

#tidyverse_update()

# personal theme settings----

per_theme_set = theme_gray(base_size = 20, base_family = "") +
  theme(line = element_line(linewidth = 1.5), 
        text = element_text(family = NULL, size = NULL, face = "plain", colour = "black"),
        legend.key = element_rect(fill = "white"), 
        legend.key.width = unit(5, "line"), 
        legend.key.height = unit(1, "cm"),
        legend.text = element_text(family = NULL,size = 15, face = "plain"),
        legend.title = element_blank())

#### cormat function ####

cormat <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## truncate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 3))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

#### standard deviation function ####

stdev = function(x)
{ 
  x = x[!is.na(x)]
  return(round(sqrt(sum((x-mean(x))^2)/length(x)), 4))   
}