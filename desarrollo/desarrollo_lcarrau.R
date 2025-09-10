
# Desarrollo del trabajo final

# Libraries
library(readr)
library(dplyr) #para la función filter()

# Importación de los datos crudos
books <- read_delim("datos_crudos/books.csv", delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Eliminar registros incompletos
books_filtrado <- books %>%
  filter(author != "Author not available" 
         & description != "No description available" 
         & genres != "['Subject not available']"
         & genres != "['Coloring books']"
         & average_rating != 0.00
         & num_pages != 0
         & rating_count > 10
         ) %>% 
  filter(!first_publish_year %in% c("Year not available", "Na", "AN")) %>%
  mutate(first_publish_year = as.numeric(first_publish_year))

# Conservar un único registro por cada book_id
books_unicos <- books_filtrado %>%
  distinct(book_id, .keep_all = TRUE)

# Exportar en un archivo .txt
write.table(
  x = books_unicos,
  file = "books.txt",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE
  )

