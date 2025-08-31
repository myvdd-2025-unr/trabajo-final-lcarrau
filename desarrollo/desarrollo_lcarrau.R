
# Desarrollo del trabajo final

# Libraries
library(readr)

# Importación de los datos crudos
books <- read_delim("datos_crudos/books.csv", delim = "|", escape_double = FALSE, trim_ws = TRUE)

