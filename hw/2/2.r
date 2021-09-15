if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)
library(tidyverse)
this_dir <-  function() {
    this_dir <- commandArgs() %>%
      tibble::enframe(name = NULL) %>%
      tidyr::separate(fill = "right", col = value, sep = "=",
                      into = c("key", "value")) %>%
      dplyr::filter(key == "--file") %>%
      dplyr::pull(value)
    if (length(this_dir) == 0)
      this_dir <- rstudioapi::getSourceEditorContext()$path
    return(dirname(this_dir))
}
setwd(this_dir())
rmarkdown::render(paste("Paul Kogan-HW", 2, ".rmd", sep = ""))

# jpeg("plot.jpeg")
# plot
#invisible(dev.off())
# ![]("plot.jpeg")

#tinytex::install_tinytex()
#tinytex::tlmgr_update()