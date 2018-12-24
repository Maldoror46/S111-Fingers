library(rmarkdown)

for (i in 1:3) {
  render("demos/03-params.Rmd", 
    output_file = paste0("03-params-", i, ".html"),
    params = list(num = i, date = Sys.Date())
  )
}