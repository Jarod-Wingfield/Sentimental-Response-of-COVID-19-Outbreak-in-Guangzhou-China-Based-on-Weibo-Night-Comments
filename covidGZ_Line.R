# clear the working environment
rm(list=ls())

# set up the working directory
setwd("F:/BIGDATA_R/Covid_project")
getwd()
# prepare packages
library(pacman)
p_load("dplyr","tidytext","tidyr","tidyverse", "ggplot2",
       "lubridate","readxl","plyr","dygraphs","xts")

covid <- read_excel("covid_Guangzhou.xls")

# Then you can create the xts necessary to use dygraph
NLCC <- xts(x = covid$New_local_confirmed_cases, order.by = covid$Time)
NLAI <- xts(x = covid$New_local_asymptomatic_infections, order.by = covid$Time)

presAnnotation <- function(dygraph, x, text) {
  dygraph %>%
    dyAnnotation(x, text, attachAtBottom = FALSE, width = 110)
}
# Finally the plot
p <- dygraph(cbind(NLCC,NLAI),main = "Covid-19 in Guangzhou China") %>%
  dyCSS("dygraph.css") %>%
  dyAxis("y", label = "Amount(Person)") %>%
  dySeries("NLCC", label = "New local confirmed cases") %>%
  dySeries("NLAI", label = "New local asymptomatic infections") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE,width=450) %>%
  dyOptions(drawPoints = TRUE, pointSize = 2,labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.4, drawGrid = FALSE,colors = RColorBrewer::brewer.pal(2, "Set2")) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE) %>%
  dyShading(from = "2022-10-17", to = "2022-10-27") %>%
  dyShading(from = "2022-11-04", to = "2022-11-08", color = "#CFE6E6") %>%
  dyShading(from = "2022-11-10", to = "2022-11-17", color = "#CCEBD6") %>%
  presAnnotation("2022-10-22", text = "flat period") %>%
  presAnnotation("2022-11-06", text = "Soaring phase Ⅰ") %>%
  presAnnotation("2022-11-14", text = "Soaring phase Ⅱ")

p
