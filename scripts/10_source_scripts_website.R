#filename: 10_source_scripts_website

#to specify options for rendering the slides, specifically
#The reason I'm using this is because I need to be able
#to write the function with more control to ignore the 
#_site.yml in the "docs" folder

library(here)
getwd()
library(rmarkdown)
#render the normal rmarkdown stuff
rmarkdown::render(here("docs", "index.Rmd"))
rmarkdown::render(here("docs", "atl-bike-gsynth.Rmd"))

#render the slides
rmarkdown::render(here(
  "docs",
  "xaringan-practice.Rmd"), 
  "xaringan::moon_reader")

