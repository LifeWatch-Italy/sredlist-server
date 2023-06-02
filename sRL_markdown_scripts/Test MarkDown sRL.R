
library(knitr) ; library(rmarkdown) ; library(flextable) ; library(png) ; library(grid) ; library(magick)
scientific_name="Nesoenas mayeri"

# SERVER SHOULD BE CALLED BEFORE

tryCatch({
  render("sRL_markdown_scripts/Test MarkDown sRL.Rmd", 
         output_format="all", 
         output_file=paste0("sRedList_report_", sub(" ", "_", sRL_decode(scientific_name)), ".html"), 
         output_dir=paste0(sub(" ", "_", sRL_decode(scientific_name)), "_sRedList"),
         envir=parent.frame(),
         knit_root_dir="C:/Users/Victor/Documents/sRedList/Platform/InProgress/sredlist-server-develop"
         )
}, error=function(e){"Error in creating Markdown report"})



