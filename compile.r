# load packages for this script
if(!require(pacman)){install.packages("pacman")}
p_load(rmarkdown, bookdown)

# execute the analysis
source("Week 3/main.r")


# render the report
render(input = "Week 3/report.rmd", output_format = "pdf_document")

# open the report
system('open "Week 3/report.pdf"')
