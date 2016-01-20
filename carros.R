carros <- read.csv("revista_motor_colombia.csv", stringsAsFactors = F)

p <- ggplot(carros[carros$ano_revista == 2011,], aes(mes_revista, valor)) + geom_point() +
  geom_smooth() + xlab("Mes") + ylab("Precio Revista") +
  ggtitle("Chevrolet Aveo GT Emotion año 2011")

p1 <- ggplot(carros[carros$ano_revista == 2012,], aes(mes_revista, valor)) + geom_point() +
  geom_smooth() + xlab("Mes") + ylab("Precio Revista") +
  ggtitle("Chevrolet Aveo GT Emotion año 2012")

p2 <- ggplot(carros[carros$ano_revista == 2013,], aes(mes_revista, valor)) + geom_point() +
  geom_smooth() + xlab("Mes") + ylab("Precio Revista") +
  ggtitle("Chevrolet Aveo GT Emotion año 2013")

p3 <- ggplot(carros[carros$ano_revista == 2014,], aes(mes_revista, valor)) + geom_point() +
  geom_smooth() + xlab("Mes") + ylab("Precio Revista") +
  ggtitle("Chevrolet Aveo GT Emotion año 2014")

p4 <- ggplot(carros[carros$ano_revista == 2015,], aes(mes_revista, valor)) + geom_point() +
  geom_smooth() + xlab("Mes") + ylab("Precio Revista") +
  ggtitle("Chevrolet Aveo GT Emotion año 2015")

multiplot(p, p1, p2, p3, p4, cols=1)