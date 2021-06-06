# Creamos las bases de datos 
data <- data.frame( control1 = c(85.1, 85.02, 87.08, 85.24, 
                                 85.87, 85.31, 86.44, 85.22,
                                 86.87, 86.31, 87.19, 85.86, 
                                 85.57, 85.70, 85.29, 85.47,
                                 86.77, 86.86, 87.72, 85.75), 
                     control2 = c(87.45, 89.02, 89.25, 88.23,
                                 88.97, 87.51, 89.15, 88.89,
                                 89.20, 88.08, 88.83, 89.85,
                                 88.51, 89.09, 87.77, 89.72,
                                 87.85, 89.11, 89.96, 90.76)) 
data2 <- data.frame( control3 = c(4.47, 4.35, 4.31, 4.33, 4.31,
                                 4.27, 4.21, 4.28, 4.25, 4.21,
                                 4.20, 4.27, 4.27, 4.31, 4.32,
                                 4.25, 4.24, 4.21, 4.21, 4.38,
                                 4.11, 4.28, 4.20))
data3 <- data.frame( calcio   = c(10.0, 10.1, 10.1, 9.9 , 10.0,
                                  10.1, 10.2, 10.1, 10.0, 10.1,
                                  10.0, 10.0, 10.0, 10.0, 9.9 ,
                                  9.9 , 10.1, 10.1, 10.1, 10.1),
                     enero    = c(10.0, 10.0, 10.1, 10.2, 10.2,
                                  10.0, 10.1, 9.9 , 10.1, 10.1,
                                  10.0, 10.1, 10.1, 9.9 , 10.0,
                                  10.0, 10.1, 10.0, 9.9 , 10.1),
                     febrero  = c(10.1, 10.1, 10.0, 10.1, 10.1,
                                  10.0, 10.0, 9.9 , 10.1, 10.1,
                                  10.0, 10.1, 10.0, 10.0, 9.9 ,
                                  9.9 , 10.1, 10.1, 10.1, 10.0),
                     marzo    = c(10.0, 10.2, 10.0, 9.9 , 10.1,
                                  10.1, 9.9 , 10.0, 10.0, 10.1,
                                  10.0, 10.0, 10.1, 10.1, 10.3,
                                  10.2, 10.3, 10.3, 10.2, 10.3),
                     abril    = c(10.1, 9.9 , 10.0, 10.1, 10.0,
                                  10.0, 10.0, 10.0, 9.9 , 10.1,
                                  10.1, 9.9 , 10.0, 9.9 , 9.9 ,
                                  9.8 , 9.7 , 9.7 , 9.6 , 9.5),
                     mayo     = c(10.1, 10.1, 10.1, 10.0, 9.9 ,
                                  10.1, 10.0, 10.0, 10.0, 9.9 ,
                                  9.6 , 10.0, 9.9 , 10.3, 9.8 ,
                                  9.7 , 10.4, 9.5 , 10.4, 10.5))

# Optenemos la media, desviación estándar y CV para ambos controles
mean(data$control1)
mean(data$control2)
mean(data2$control3)
mean(data3$calcio)

sd(data$control1)
sd(data$control2)
sd(data2$control3)
sd(data3$calcio)

coef_var <- function(x, na.rm = FALSE) {
    sd(x, na.rm=na.rm) / mean(x, na.rm=na.rm)
}

coef_var(x=data$control1, na.rm=T) * 100
coef_var(x=data$control2, na.rm=T) * 100
coef_var(x=data2$control3, na.rm=T) * 100
coef_var(x=data3$calcio, na.rm=T) * 100

# Creamos la gráfica para el control 1
plot(data$control1,
     type="o",
     xlab = "Días",
     ylab = "Valores", 
     pch = 21,
     bg = "black",
     main = "Control 1",
     axes = FALSE,
     bty = "7",
     ylim = c(83, 89),
     xlim = c(0, 21),
)

# Añadimos las etiquetas de los ticks 
axis(1, at = 0:21, labels = 0:21,)
axis(2, at = 83:89, labels = 83:89, las = 1)

# Agregamos las líneas de desviación al gráfico
lines <- {
    abline(mean(na.omit(data$control1)),0,col="blue")
    abline(mean(na.omit(data$control1))+sd(na.omit(data$control1)),0,col="green")
    abline(mean(na.omit(data$control1))-sd(na.omit(data$control1)),0,col="green")
    
    abline(mean(na.omit(data$control1))+2*sd(na.omit(data$control1)),0,col="yellow")
    abline(mean(na.omit(data$control1))-2*sd(na.omit(data$control1)),0,col="yellow")
    
    abline(mean(na.omit(data$control1))+3*sd(na.omit(data$control1)),0,col="red")
    abline(mean(na.omit(data$control1))-3*sd(na.omit(data$control1)),0,col="red")
}

# Creamos el gráfico para el control 2
plot(data$control2,
     type="o",
     xlab = "Días",
     ylab = "Valores", 
     pch = 21,
     bg = "black",
     main = "Control 2",
     axes = FALSE,
     bty = "7",
     ylim = c(86, 92),
     xlim = c(0, 21),
)
# Añadimos las etiquetas de los ticks
axis(1, at = 0:21, labels = 0:21,)
axis(2, at = 86:92, labels = 86:92, las = 1)

# Agregamos las líneas de desviación para el gráfico
lines <- {
    abline(mean(na.omit(data$control2)),0,col="blue")
    abline(mean(na.omit(data$control2))+sd(na.omit(data$control2)),0,col="green")
    abline(mean(na.omit(data$control2))-sd(na.omit(data$control2)),0,col="green")
    
    abline(mean(na.omit(data$control2))+2*sd(na.omit(data$control2)),0,col="yellow")
    abline(mean(na.omit(data$control2))-2*sd(na.omit(data$control2)),0,col="yellow")
    
    abline(mean(na.omit(data$control2))+3*sd(na.omit(data$control2)),0,col="red")
    abline(mean(na.omit(data$control2))-3*sd(na.omit(data$control2)),0,col="red")
}


# Creamos el gráfico para el control 3
plot(data2$control3,
     type="o",
     xlab = "Días",
     ylab = "Valores", 
     pch = 21,
     bg = "black",
     main = "Control 3",
     #axes = FALSE,
     bty = "n",
     ylim = c(4, 4.6),
     xlim = c(0, 24),
)
# Añadimos las etiquetas de los ticks
axis(1, at = 0:24, labels = 0:24,)
#axis(2, at = 4:4.6, labels = 4:4.6, las = 1)

# Agregamos las líneas de desviación para el gráfico
lines <- {
    abline(mean(na.omit(data2$control3)),0,col="blue")
    abline(mean(na.omit(data2$control3))+sd(na.omit(data2$control3)),0,col="green")
    abline(mean(na.omit(data2$control3))-sd(na.omit(data2$control3)),0,col="green")
    
    abline(mean(na.omit(data2$control3))+2*sd(na.omit(data2$control3)),0,col="yellow")
    abline(mean(na.omit(data2$control3))-2*sd(na.omit(data2$control3)),0,col="yellow")
    
    abline(mean(na.omit(data2$control3))+3*sd(na.omit(data2$control3)),0,col="red")
    abline(mean(na.omit(data2$control3))-3*sd(na.omit(data2$control3)),0,col="red")
}

