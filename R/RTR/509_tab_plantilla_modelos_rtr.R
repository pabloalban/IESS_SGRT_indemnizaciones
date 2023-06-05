#Función para exportar modelos de stargazer en latex------------------------------------------------
star_tex_write <- function(..., starlist = NULL, file, headers = FALSE, append = FALSE) {
  
  ##Get all of the output in a list
  starlist <- c(list(...), starlist)
  
  ##remove the information on the stargazer package author
  starlist <- lapply(starlist, function(x) x[!grepl("% Table created by", x)])
  
  top.header <- paste0("%% ", file, "\n",
                       "\\documentclass[a4paper,12pt]{article}",
                       "\n",
                       "\\usepackage{caption}\n",
                       "\\usepackage[spanish,es-tabla]{babel}\n", 
                       "\\usepackage{siunitx}\n",
                       "\\usepackage{rotating}\n",
                       "\\usepackage{natbib}\n",
                       "\\usepackage[flushleft]{threeparttable}",
                       "\\usepackage[a4paper, hmargin=0.5in,vmargin=0.5in]{geometry}",
                       "\\begin{document}",
                       "\n")
  
  bottom.header <- c("\n \\end{document}")
  
  if (headers == TRUE) write(top.header, file)
  
  if (!headers & !append) {
    ##Empty the text file
    write("", file)
  }
  
  ##Write the stargazer output to the file
  for (i in seq_along(starlist)) {
    
    write(paste0("%Tex File: ", getwd(), "/", file), file, append = TRUE)
    write(starlist[[i]], file, append = TRUE)
    write("\n\n", file, append = TRUE)
  }
  
  if (headers) {
    write(bottom.header, file, append = TRUE)
  }
  
  return(invisible(NULL))
}