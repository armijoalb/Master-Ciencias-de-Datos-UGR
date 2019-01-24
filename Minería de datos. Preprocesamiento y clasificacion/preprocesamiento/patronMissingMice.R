# Este paquete puede usarse para imputar valores perdidos en
# variables de todo tipo
library(mice)

# carga de datos disponibles directamente en el paquete mice
datos <- airquality

# se obtiene el patron de aparicion de datos perdidos
patron <- mice::md.pattern(x=datos)

# se muestra el patron
print(patron)
