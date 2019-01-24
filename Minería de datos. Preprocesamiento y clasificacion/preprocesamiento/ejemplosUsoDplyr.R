library(nycflights13)
library(dplyr)
library(ggplot2)
library(Lahman)

# se muestra informacion de resumen del conjunto de datos
# (de tipo tibble)
flights

# filtrado para vuelos del 1 de enero: tambien genera un
# tibble
unoEnero <- filter(flights, month == 1, day == 1)

# forma habitual de hacerlo hasta ahora
unoEneroOtro <- flights[(flights$month == 1 & flights$day == 1), ]

# filtrado con expresiones logicas
eneroFebrero <- filter(flights, month == 1 | month == 2)

# igual con operadores >, <
retrasosLlegadas <- filter(flights, arr_delay > 120)
retrasosLlegadasSalidas <- filter(flights, arr_delay > 120 & dep_delay > 120)

# se pueden especificar varios valores de interes en un vector
eneroFebreroMarzo <- filter(flights, month %in% c(1,2,3))

# se pueden ordenar las filas de acuerdo al valor de las
# variables seleccionadas
origenDestino <- arrange(flights, origin, dest)

# seleccion de columnas especificas con select
algunasColumnas <- select(flights, year, month, day, origin, dest)

# puede seleccionarse un determinado rango de columnas
seleccionRango1 <- select(flights, year:dest)

# tambien evitar todos las columnas en un rango determinado
seleccionRango2 <- select(flights,-(year:day))

# posibilidad de uso de funciones auxiliares para seleccion
# de variables
seleccion3 <- select(flights, starts_with("a"))
seleccion4 <- select(flights, ends_with("time"))
seleccion4 <- select(flights, contains("d"))

# la variable tailnum (a la derecha) ahora se llamara tailnumber.
# Las variables que no se indican en rename mantienen su nombre
renamed <- rename(flights, tailnumber=tailnum)

# uso de everything en combinacion con select
reordenada <- select(flights, time_hour, air_time, everything())

# uso de combinación de criterios para seleccion de variables
versionNueva <- select(flights, year:day, ends_with("delay"), distance, air_time)

# se agregan las nuevas variables mediante mutate
versionNueva <- mutate(versionNueva, gain=arr_delay-dep_delay, speed=distance/air_time*60)

# si solo se quieren mantener las nuevas variables, debe usarse transmute
versionNueva <- transmute(versionNueva, gain=arr_delay-dep_delay, speed=distance/air_time*60)

# agrupamiento mediante group_by: se agrupan los datos por año, mes y dia
porDia <- group_by(flights, year, month, day)

# una vez agrupados se obtiene un resumen de los datos. El valor de delay
# se obtiene mediante la media de los valores de la variable dep_delay de cada
# instancia
resumen <- summarize(porDia, delay=mean(dep_delay, na.rm=TRUE))

# posibilidad de agrupar operaciones mediante pipas
#paso 1: agrupamiento de instancias por destino
porDestino <- group_by(flights,dest)

#paso 2: se determina el retraso por destino
retraso <- summarize(porDestino, count=n(), dist=mean(distance, na.rm=TRUE),
                     delay=mean(arr_delay, na.rm=TRUE))

#paso 3: filtrado de instancias: nos quedamos con aquellas en que 
# al menos hay 20 instancias y el destino no es HNL
filtrado <- filter(retraso, count > 20, dest != "HNL")

# se representan los datos
ggplot(data=filtrado, mapping=aes(x=dist,y=delay)) + 
  geom_point(aes(size=count), alpha=1/3) +
  geom_smooth(se=FALSE)

# Estas operaciones se puden encadenar mediante pipas
resultado <- flights %>%
            group_by(dest) %>%
            summarize(count=n(), dist=mean(distance, na.rm=TRUE),
                                 delay=mean(arr_delay, na.rm=TRUE)) %>%
            filter(count > 20, dest != "HNL")

# se pinta el resultado para ver que coincide
ggplot(data=resultado, mapping=aes(x=dist,y=delay)) + 
  geom_point(aes(size=count), alpha=1/3) +
  geom_smooth(se=FALSE)

# se ontienen los vuelos no cancelados
noCancelados <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  group_by(year, month, day)

# se ontienen los retraso
retrasos <- noCancelados %>%
  group_by(tailnum) %>%
  summarize(delay=mean(arr_delay))
            
# se muestran
ggplot(data=retrasos, mapping=aes(x=delay)) + 
  geom_freqpoly(binwidth=10)

# incluso puede trabajarse directamante con el objetivo de realizar
# la representacion
flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  group_by(year, month, day) %>%
  group_by(tailnum) %>%
  summarize(delay=mean(arr_delay)) %>%
  ggplot(mapping=aes(x=delay)) + 
       geom_freqpoly(binwidth=10)


# es decir, hay algunos vuelos con mas de 300 minutos de retraso. Lo ideal
# sin embargo, al ser mas informativo, es ver cuantos vuelos sufren cada
# retraso
retrasos <- noCancelados %>%
  group_by(tailnum) %>%
  summarize(delay=mean(arr_delay, na.rm=TRUE),
            n=n())

# se representan ahora
ggplot(data=retrasos, mapping=aes(x=n,y=delay)) + 
  geom_point(alpha=1/10)

# tambien pueden eliminarse aquellos vuelos con pocas observaciones, para
# eliminar ruido
retrasos %>%
  filter(n > 25) %>%
  ggplot(mapping=aes(x=n,y=delay)) +
  geom_point(alpha=1/5)
            
# se usa conjunto de datos de bateadores: es un data.frame
class(Lahman::Batting)

# se transforma a tibble
bateo <- as_tibble(Lahman::Batting)

# hay correlacion positiva entre la habilidad del bateador (h:hits) y el
# numero de oportunidades par batear (AB)
bateadores <- bateo %>%
  group_by(playerID) %>%
  summarize(golpeos=sum(H, na.rm=TRUE)/sum(AB, na.rm=TRUE),
            posibilidades=sum(AB,na.rm=TRUE))

# filtramos para quedarnos con aquellos bateadores que tuvieron al
# menos 100 posibilidades de bateo
bateadores %>%
  filter(posibilidades > 100) %>%
  ggplot(mapping=aes(x=posibilidades,y=golpeos)) +
  geom_point() +
  geom_smooth(se=FALSE)

# se reordena el conjunto de datos: desc indica orden descendente. Si no
# se pone nada es ascendente
ordenados <- bateadores %>%
  arrange(desc(golpeos))
