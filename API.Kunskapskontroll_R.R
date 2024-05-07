# Statistikbasens API
# Syfte: Hämta extern data

# Ladda in nödvändiga bibliotek
library(pxweb)


min_data <- interactive_pxweb()

# PXWEB query 
pxweb_query_list <- 
  list("Region"=c("12"),
       "Agarkategori"=c("000","010","020","030","040","050","060"),
       "ContentsCode"=c("TK1001AB"),
       "Tid"=c("2014","2015","2016","2017","2018","2019","2020","2021","2022","2023"))

# Ladda ner data
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/TK/TK1001/TK1001A/PersBilarA",
            query = pxweb_query_list)

# Konvertera data till en dataframe 
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

View(px_data_frame)
str(px_data_frame)
px_data_frame$år <- as.numeric(px_data_frame$år)
px_data_frame$ägarkategori <- as.factor(px_data_frame$ägarkategori)

library(ggplot2)

# Filtrera data för de specifika ägarkategorierna
filtered_data <- px_data_frame %>%
  filter(ägarkategori %in% c("totalt", "män", "kvinnor", "juridiker"))

# Skapa plot med endast de valda ägarkategorierna
ggplot(filtered_data, aes(x = år, y = `Personbilar i trafik`, color = ägarkategori)) +
  geom_point(size = 4) +   
  geom_line(aes(group = ägarkategori)) + 
  labs(x = "År", y = "Antal personbilar i trafik", title = "Antal personbilar i trafik i Skåne") +
  scale_color_manual(values = c("totalt" = "red", "män" = "blue", "kvinnor" = "green", "juridiker" = "orange")) +   
  theme_minimal()   
