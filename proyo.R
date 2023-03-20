install.packages("readr")

library(readr)

path_lui<- "luisvgsales.csv"
gapminder <- read.csv(path_lui)

print(gapminder)

gapminder[1,]
#Proyecto
#1
#1.1 crear vectores con los titulos de la dataset
nombre<-c("Rank","Name","Platform","Year","Genre","Publisher","NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales")

print(nombre)
aga<-c(gapminder$Global_Sales[1:13])
mi_df <- data.frame(
  "Ranking" = c(gapminder$Rank[1:13]), 
  "Nombre" = c(gapminder$Name[1:13]), 
  "Plataforma" = c(gapminder$Platform[1:13]),
  "Year" = c(gapminder$Year[1:13]),
  "Publisher"=c(gapminder$Genre[1:13]),
  "NA_SALES"=c(gapminder$NA_Sales[1:13]),
  "EU_SALES"= c(gapminder$EU_Sales[1:13]),
  "JP_SALES"=c(gapminder$Global_Sales[1:13])
)

precio<-c(gapminder$NA_Sales)
#1.2 crear vector numerico con el precio final de los videojuegos
precio_final<-c(gapminder$Global_Sales)
print(precio_final)

#1.3 Crear una condición lógica de precios bajos (ejemplo precios menores a 10.99)
newdata <- subset(precio_final, precio_final < 10.99)

#1.4 Sumar 5 al vector creado
precio_final+5

#1.5 dividir la puntuación entre 2

precio_final/2

#1.6 Calcular la media, moda, max, min de los datos de tipo numérico (verificar con Class)

#maximo
max(precio_final)
class(max(precio_final))

#minimo
min(precio_final)
class(min(precio_final))

#media
mean(precio_final)
class(mean(precio_final))

#moda
get_mode <- function(f) {
  uf <- unique(f)
  tab <- tabulate(match(f, uf))
  uf[tab == max(tab)]
}
get_mode(precio_final)
class(get_mode(precio_final))

#1.7 crear dataframe de 13 col con la base de datos y guardarlo en una nueva variable

mi_df <- data.frame(
  "Ranking" = c(gapminder$Rank[1:13]), 
  "Nombre" = c(gapminder$Name[1:13]), 
  "Plataforma" = c(gapminder$Platform[1:13]),
  "Year" = c(gapminder$Year[1:13]),
  "Publisher"=c(gapminder$Genre[1:13]),
  "NA_SALES"=c(gapminder$NA_Sales[1:13]),
  "EU_SALES"= c(gapminder$EU_Sales[1:13]),
  "JP_SALES"= c(gapminder$JP_Sales[1:13]),
  "OTHER_SALES"= c(gapminder$Other_Sales[1:13]),
  "GLOBAL_SALES"=c(gapminder$Global_Sales[1:13]),
  "Anio" = c(gapminder$Year[1:13]),
  "Platform" = c(gapminder$Platform[1:13]),
  "SALES_UE"=c(gapminder$EU_Sales[1:13])
)

mi_df



#1.8 Agregar filas y columnas a la matriz 

#creamos una variable para agregarlo en columna
portero<-gapminder$Global_Sales*2

portero

#agregar columna
caro <- cbind(gapminder[, c(1, 10)], portero)
caro

#agregar fila

#Para ello se crea una variable llamada bu
bu<-c(999999,"Mi pobre angelito","Disney")

caro1 <-rbind(caro,bu)

caro1[16599,]

#1.9 Agregar columna de "1"

caro1 <- cbind(caro1, 1)

caro1


#1.10 Agregar fila con los datos de un videojuego de tu preferencia

gapminder
mi_game<-c(12314231,"Mob Psycho 100","PS4",2018,"Capcom",12,16,18,21,22,33)

juego_final <-rbind(gapminder,mi_game)

juego_final

juego_final[16599,]

#1.11 Eliminar filas y columnas de la matriz

#eliminar columnas 

juego_final<- juego_final[, -c(11)]
juego_final

#eliminar filas
juego_final<- juego_final[-c(11:100),]

juego_final

#1.12 Seleccionar los elementos de la matriz

juego_final[1:16597,1:10]


#1.13 Convertir la matriz en data frame y asignar nombres a las columnas

dato_convertido_matriz<-as.data.frame(juego_final[1:16597,1:10])
dato_convertido_matriz

#Cambio de nombre a las columnas
colnames(dato_convertido_matriz) <- c("Ranking", "Nombre", "Plataforma","Año","Genero","PRECIO_USA","PRECIO_JAPAN","PRECIO_EUROPE","OTROS","GLOBAL")
dato_convertido_matriz


#1.14 Acceder a los datos del dataframe
print(dato_convertido_matriz[3,'Genero'])

print(dato_convertido_matriz[8:12,3:5])
#1.15 Cambiar nombre de dataframe

names(dato_convertido_matriz)<-c('Nam','Du','tre','Four','Five','Six','Sev','Ocho','Nine','Ten')

dato_convertido_matriz

#1.16 Seleccionar un elemento del dataframe
dato_convertido_matriz[3,'Du']

#2
#2.1 Importar datos desde Excel y Ordenar los datos con la funcion
#order() de preferencia para la variable Price_final

ordenar_menor<-order(gapminder$Global_Sales,decreasing = FALSE) 

gapminder[ordenar_menor,]

#2.2 Mostrar el dataframe ordenado de forma ascendente y descendente

ascendente<-order(gapminder$Global_Sales,decreasing = FALSE)
descendente <-order(gapminder$Global_Sales,decreasing = TRUE)

#dataframe ascendente:

gapminder[ascendente,]

#dataframe descendente:

gapminder[descendente,]

#2.3 Calcular el resumen estadistico de los datos con la funcion que corresponde

summary(gapminder)

#2.4 Realizar las graficas
plot(gapminder$Year ,gapminder$Global_Sales)
hist(gapminder$NA_Sales,col="darkolivegreen1")

#3 Programacion

#3.1 Implementar una funcion para la multiplicacion de 2 vectores(xy) 
# y probar con valores

multiplica_vectores<- function(x, y) {
  resultado <- x * y
  return(resultado)
}

#probando con valores
ai=c(1,2,3)

bic=c(50,100,9)

multiplica_vectores(ai,bic)


#3.2 Implementar una funcion que muestre el resultado de la ecuacion 
#de Bhaskara y probar con valores

ecuacion_Bhaskara<- function(a, b,c) {
  if(a ==0){
    return("a tiene que ser distinto de cero")
  }
  else{
    x1=(-b+sqrt((b*b)-(4*a*c)))/(2*a)
    x2=(-b-sqrt((b*b)-(4*a*c)))/(2*a)
    
    if(is.nan(x1) || is.nan(x2))
    {
      return("Valores imaginarios")
    }else{
      apo=c(x1,x2)
      return(apo)}
    
  }
}

#tomando un ejemplo el de la ecuacion x**2-2x+1
ecuacion_Bhaskara(1,2,1)


#3.3 Se quiere conocer la media muestral de n observaciones obtenidas 
# independiente de una distribucion normal con media=0 y varianza=1
#3.3.1 Realizar una simulacion, luego calcular las estadisticas descriptivas
#aplicando la funcion que corresponde y graficas



