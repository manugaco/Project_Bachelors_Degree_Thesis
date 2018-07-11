

#ICC preview

cis_index <- read_excel("Datasets/icc.xlsx")

#Libraries

library("lubridate")
library("scales")

cis_index$Date <- as.Date(as.POSIXct(cis_index$Date, format = "%Y/%M/%D"))

#Plotting the index

library("ggplot2")

iccplot <- ggplot(data = cis_index, aes(x = Date)) +
  geom_line(aes(y = ICC, colour = "ICC")) +
  scale_colour_manual("", 
                      breaks = c("ICC"),
                      values = c("black")) +
  xlab("Serie Temporal") +
  scale_y_continuous("Index", limits = c(15,135),
                     breaks = seq(15, 135, by =15)) +
  scale_x_date("Serie Temporal", breaks = date_breaks("24 months")) +
  labs(title="Índice de Confianza del Consumidor") +
  theme_light()

iccplot

iccplot1 <- ggplot(data = cis_index, aes(x = Date)) +
  geom_line(aes(y = ICC, colour = "ICC")) +
  geom_line(aes(y = ISA, colour = "ISA")) +
  geom_line(aes(y = IEE, colour = "IEE")) +
  scale_colour_manual("", 
                      breaks = c("ICC", "ISA", "IEE"),
                      values = c("black", "red", "blue")) +
  scale_y_continuous("Index", limits = c(15,135),
                     breaks = seq(15, 135, by =15)) + 
  scale_x_date("Serie Temporal", breaks = date_breaks("24 months")) +
  labs(title="Índices del CIS") +
  theme_light()

iccplot1

