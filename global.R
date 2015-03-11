# shiny
library(shiny)
library(shinydashboard)

# data manipulation
library(reshape)
library(stringr)
library(R.utils)

# graphics 
library(ggplot2)
library(scales)

# max file size 
options(shiny.maxRequestSize = 9*1024^2)

# ggplot settings
stat.color <- c("#0A71B4", "#61636B", "#13235B", "#E36929", "#C9D30F", "#66B8DC")
theme_set(theme_bw(base_size = 18))
