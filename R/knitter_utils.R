
knit_rmarkdown <- function(input_file){
  ROOT <- rprojroot::find_rstudio_root_file()
  
  filename <- paste0(
    gsub( '.{4}$', '', basename(input_file) ),
    '.html'
  )
  dir_vec <- strsplit(dirname(input_file), split = '/')[[1]]
  dir_vec[1] <- 'html'
  output_file <- do.call(
    file.path,
    as.list(c(ROOT, dir_vec, filename))
  )
  
  rmarkdown::render(
    input = input_file,
    output_file = output_file
  )
}