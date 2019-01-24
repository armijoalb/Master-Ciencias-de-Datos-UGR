library(ggplot2)
library(tidyverse)

# se usa el conjunto de datos llamado mpg. La forma de obtener
# ayuda sobre el conjunto de datos es la indicada a continuacion
?mpg

# se realiza un grafico simple para ver la relacion entre las
# variables hwy y displ
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))

# vemos el efecto de la variable clase mediante una propiedad
# estetica de los puntos
ggplot(data = mpg) +  
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# podemos usar el tamaño del punto para representar los diferentes 
# tipos de coches. Esta sentencia genera un aviso, porque no es 
# buena idea usar el tamaño para variables discretas cuyos dominios 
# no estan ordenados
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# uso de transparencia para representar los tipos de coches
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# igual con la forma de los puntos
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# se usa facet_wrap para subdividir los datos por la variable clase
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~class, nrow = 2)

# subdivision de datos mediante dos variables
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(cyl ~ drv)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)


# representacion usando geom_smooth
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

# uso de geom_smooth, clasificando los datos de acuerdo a la
# variable clase
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = class))

# uso de geom_smooth, pero visualizando tambien los puntos
# que representan a los datos
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color=class)) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype=class))

# otro ejemplo de visualizacion con varios tipos de geometria
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# forma de evitar la repeticion: mapping sobre ggplot
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()

# usando mappings locales y globales
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color=class)) +
  geom_smooth(mapping = aes(linetype = class))

# diferentes conjuntos de datos para cada capa
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = TRUE)

# se obtiene un diagrama de barras para ver la calidad del
# corte de los datos en el conjunto
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x=cut))

# se podría haber obtenido el gráfico de otra forma diferente
ggplot(data = diamonds) +
  stat_count(mapping = aes(x=cut))

# se modifica el comportamiento de stat para mostrar proporciones
# y no conteo
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x=cut, y = ..prop.., group=1))

# se enriquece el grafico con mas informacion sobre las calidades
# de corte
ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x=cut, y=depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = mean
  )

# se muestra informacion sobre clarity en las barras
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x=cut, fill=clarity))

# uso de jitter
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), 
             position = "jitter")

# solapamiento de etiquetas en el eje X
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

# para evitar el solapamiento se giran los ejes
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +      
  geom_boxplot() +
  coord_flip()
  

