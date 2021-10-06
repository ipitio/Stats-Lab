if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, rmarkdown, scriptName)
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
name <- paste0(str_replace_all(str_extract(
               scriptName::current_filename(), regex("[^\\/]+\\.r", T)),
               "~\\+~", " "), "md")
rmarkdown::render(name)
file.rename(list.files(pattern = "*l-K*")[1],
            str_replace_all(name, regex("rmd", T), "pdf"))