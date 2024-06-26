install.packages(c("ggplot2","dplyr","reshape2","scales","tidyr","scales"))     #Gerekli Paketler yüklenir  
library(ggplot2)                  #Veri Görselleştirmede Kullanılır
library(dplyr)                    #Veri Manipülasyonunda Kullanılır
library(reshape2)                 #Veri Seti Manipülasyonunda Kullanılır
library(scales)                   #Grafiklerde Yüzdelik Gösterimlerinde Kullanılır
library(tidyr)

# Enerji Verisi Yüklenir 
Energy_consumption_by_source_and_country <- read.csv("Energy_consumption_by_source_and_country.csv",check.names=FALSE)   

#Ana Veriye Zarar Vermemek için Kopyası Oluşturulur ve Sütunların Adları Türkçeleştirilir
veri <- Energy_consumption_by_source_and_country    
colnames(veri) <- c("Varlık","Kod","Yıl","Diğer Yenilenebilirler","Biyoyakıt","Güneş","Rüzgar","Hidrolik","Nükleer","Doğalgaz","Kömür","Petrol")

#Enerji Kaynakları Toplanarak Toplam Tüketim Sütunu Oluşturulur
veri$`Toplam Tüketim` = rowSums(veri[4:12],na.rm = TRUE)  

#Enerji Kaynaklarının Toplam Tüketime Oranını Veren Sütun Oluşturulur
veri <- veri %>%
  mutate(across(`Diğer Yenilenebilirler`:`Petrol`, 
                ~ .x / `Toplam Tüketim` * 100, 
                .names = '{col} Yüzdesi'))

# Baz Alınacak Yıl Olan 2012 ile Son Verinin Bulunduğu 2022 Arasında İleride Yapılacak Kıyaslamak İçin Ayrı Veri Setleri Oluşturulur
veri_2012 <- veri %>% filter(Yıl == 2012)      # 2012 (baz yılı) yılındaki değerleri alınması
veri_2022 <- veri %>% filter(Yıl == 2022)      # 2022  yılındaki değerleri alınması

# 2012-2022 Yılları Arasında Tüm Varlıklarda Toplam Tüketimin Değişimi bulunur
toplam_tüketim_büyüme_verisi <- veri_2012 %>%
  select(Varlık, `Toplam Tüketim 2012` = `Toplam Tüketim`) %>%
  inner_join(veri_2022 %>% select(Varlık, `Toplam Tüketim 2022` = `Toplam Tüketim`), by = "Varlık") %>%
  mutate(`Toplam Tüketimdeki Yüzdelik Büyüme` = ((`Toplam Tüketim 2022` - `Toplam Tüketim 2012`) / `Toplam Tüketim 2012`) * 100)

toplam_tüketim_büyüme_verisi <- toplam_tüketim_büyüme_verisi %>%
  filter(!is.na(`Toplam Tüketimdeki Yüzdelik Büyüme`))

# Sadece Dünyadaki Değişimleri Görmek İçin Filtrelenir
dünya_verisi <- veri %>% filter(Varlık == "World")

# Dünyadaki Enerji Tüketimindeki Yüzdelik Değişimler
ggplot(dünya_verisi) +
  geom_line(aes(x = Yıl, y = `Diğer Yenilenebilirler Yüzdesi`, color = "Diğer Yen."), linewidth = 0.8) +
  geom_line(aes(x = Yıl, y = `Biyoyakıt Yüzdesi`, color = "Biyoyakıt"), linewidth = 0.8) +
  geom_line(aes(x = Yıl, y = `Güneş Yüzdesi`, color = "Güneş"), linewidth = 0.8) +
  geom_line(aes(x = Yıl, y = `Rüzgar Yüzdesi`, color = "Rüzgar"), linewidth = 0.8) +
  geom_line(aes(x = Yıl, y = `Hidrolik Yüzdesi`, color = "Hidrolik"), linewidth = 0.8) +
  geom_line(aes(x = Yıl, y = `Nükleer Yüzdesi`, color = "Nükleer"), linewidth = 0.8) +
  geom_line(aes(x = Yıl, y = `Doğalgaz Yüzdesi`, color = "Doğalgaz"), linewidth = 0.8) +
  geom_line(aes(x = Yıl, y = `Kömür Yüzdesi`, color = "Kömür"), linewidth = 0.8) +
  geom_line(aes(x = Yıl, y = `Petrol Yüzdesi`, color = "Petrol"), linewidth = 0.8) +
  scale_x_continuous(breaks = seq(min(dünya_verisi$Yıl), max(dünya_verisi$Yıl), by = 5)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = seq(0, max(dünya_verisi$`Toplam Tüketim`), by = 5)) +
  labs(title = "Yıllara Göre Enerji Tüketimindeki Yüzdelik Pay",
       x = "Yıl",
       y = "Yüzde",
       color = "Enerji Kaynağı") +
  theme_light(base_family = "Arial", base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"),
    axis.title.x = element_text(face = "bold", size = 12, color = "darkblue"),
    axis.title.y = element_text(face = "bold", size = 12, color = "darkblue"),
    axis.text = element_text(size = 10, color = "darkblue"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    legend.position = "right"
  ) +
  scale_color_manual(values = c("Biyoyakıt" = "chocolate1", "Güneş" = "yellow", "Rüzgar" = "cyan", "Hidrolik" = "blue",
                                "Nükleer" = "green", "Doğalgaz" = "darkturquoise", "Petrol" = "darkolivegreen", "Kömür" = "black", "Diğer Yen." = "darkred"))

# Dünyadaki Enerji Tüketimdeki Sayısal Değişimler
ggplot(dünya_verisi) +
  geom_line(aes(x = Yıl, y = `Diğer Yenilenebilirler`, color = "Diğer Yen."), size = 0.8) +
  geom_line(aes(x = Yıl, y = `Biyoyakıt`, color = "Biyoyakıt"), size = 0.8) +
  geom_line(aes(x = Yıl, y = `Güneş`, color = "Güneş"), size = 0.8) +
  geom_line(aes(x = Yıl, y = `Rüzgar`, color = "Rüzgar"), size = 0.8) +
  geom_line(aes(x = Yıl, y = `Hidrolik`, color = "Hidrolik"), size = 0.8) +
  geom_line(aes(x = Yıl, y = `Nükleer`, color = "Nükleer"), size = 0.8) +
  geom_line(aes(x = Yıl, y = `Doğalgaz`, color = "Doğalgaz"), size = 0.8) +
  geom_line(aes(x = Yıl, y = `Kömür`, color = "Kömür"), size = 0.8) +
  geom_line(aes(x = Yıl, y = `Petrol`, color = "Petrol"), size = 0.8) +
  scale_x_continuous(breaks = seq(min(dünya_verisi$Yıl), max(dünya_verisi$Yıl), by = 5)) +
  scale_y_continuous(breaks = seq(0, max(dünya_verisi$`Toplam Tüketim`), by = 5000), labels = scales::comma) +
  labs(title = "Yıllara Göre Enerji Tüketimi",
       x = "Yıl",
       y = "Enerji Tüketimi (TWh)",
       color = "Enerji Kaynağı") +
  theme_light(base_family = "Arial", base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"),
    axis.title.x = element_text(face = "bold", size = 12, color = "darkblue"),
    axis.title.y = element_text(face = "bold", size = 12, color = "darkblue"),
    axis.text = element_text(size = 10, color = "darkblue"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    legend.position = "right"
  ) +
  scale_color_manual(values = c("Biyoyakıt" = "chocolate1", "Güneş" = "yellow", "Rüzgar" = "cyan", "Hidrolik" = "blue",
                                "Nükleer" = "green", "Doğalgaz" = "darkturquoise", "Petrol" = "darkolivegreen", "Kömür" = "black", "Diğer Yen." = "darkred"))

# İlgili Yıllardaki Veriler Filtrelenir
seçilmiş_yıllar <- c(1970, 1980, 1990, 2000, 2010, 2020)
filtrelenmiş_veri <- veri %>% filter(Yıl %in% seçilmiş_yıllar)

# OECD ve Non-OECD Verileri Ayrı Ayrı Toplanır
veri_özet <- filtrelenmiş_veri %>%
  group_by(Yıl, Varlık) %>%
  summarize(Total_Consumption = sum(`Toplam Tüketim`), .groups = 'drop') %>%
  pivot_wider(names_from = Varlık, values_from = Total_Consumption) %>%
  mutate(OECD_Percentage = 100 * (`OECD (EI)` / (`OECD (EI)` + `Non-OECD (EI)`)),
         NonOECD_Percentage = 100 * (`Non-OECD (EI)` / (`OECD (EI)` + `Non-OECD (EI)`)))

# Veri Uzun Forma Çevrilir ve Varlık Sütunu Güncellenir
veri_uzun <- veri_özet %>%
  select(Yıl, OECD_Percentage, NonOECD_Percentage) %>%
  pivot_longer(cols = c(OECD_Percentage, NonOECD_Percentage), names_to = "Varlık", values_to = "Percentage") %>%
  mutate(Varlık = recode(Varlık, "OECD_Percentage" = "OECD", "NonOECD_Percentage" = "OECD Dışı"))

# OECD ve OECD Dışı Ülkelerin Kıyaslandığı Yığılmış Çubuk Grafikleri
# Stacked bar chart oluşturma
ggplot(veri_uzun, aes(x = factor(Yıl), y = Percentage, fill = Varlık)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Percentage, 1)), position = position_stack(vjust = 0.5), color = "white", size = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Yıllara Göre OECD ve Non-OECD Enerji Tüketim Oranları",
       x = "Yıl",
       y = "Yüzde",
       fill = "Varlık") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"),
    axis.title.x = element_text(face = "bold", size = 12, color = "darkblue"),
    axis.title.y = element_text(face = "bold", size = 12, color = "darkblue"),
    axis.text = element_text(size = 10, color = "darkblue"),
    legend.position = "right"  
  ) +
  scale_fill_manual(values = c("OECD" = "deepskyblue2", "OECD Dışı" = "deeppink3"))  


# Tüm Ülkelerdeki Toplam Tüketimindeki Büyümeyi Bulmak Yerine Dünyadaki Enerji Kaynalarının Büyümelerini Ölçebileceğimiz Yeni Veri Seti hazırlanır
data_2012 <- veri %>% filter(Yıl == 2012)
data_2022 <- veri %>% filter(Yıl == 2022)

# İlgili Sütunlar Seçilir
data_2012 <- data_2012 %>% select(Varlık, `Diğer Yenilenebilirler`, Biyoyakıt, Güneş, Rüzgar, Hidrolik, Nükleer, Doğalgaz, Kömür, Petrol, `Toplam Tüketim`)
data_2022 <- data_2022 %>% select(Varlık, `Diğer Yenilenebilirler`, Biyoyakıt, Güneş, Rüzgar, Hidrolik, Nükleer, Doğalgaz, Kömür, Petrol, `Toplam Tüketim`)
# Sütunların Adları Değiştirilir
colnames(data_2012) <- paste0(colnames(data_2012), " 2012")
colnames(data_2022) <- paste0(colnames(data_2022), " 2022")

# 2012 ve 2022 Verileri Birleştirilir
merged_data <- inner_join(data_2012, data_2022, by = c("Varlık 2012" = "Varlık 2022"))

# Yüzde Büyüme Hesaplanır
büyüme_verisi <- merged_data %>%
  mutate(across(ends_with(" 2022"), ~ (. - get(sub("2022", "2012", cur_column()))) / get(sub("2022", "2012", cur_column())) * 100, .names = "{col} Büyüme"))

# Dünya Filtrelenir Böylece Dünyadaki Enerji Kaynaklarından Hangilerinin 2012-2022 Arasında En Çok Büyüdüğü Bulunur
dünya_büyüme <- büyüme_verisi %>% filter(`Varlık 2012` == "World")

# Eritme İşlemi Öncesinde İstenmen Sütunlar Atılır ve Eritilerek Tablonun Şeklide Değiştirilir
dünya_büyüme <- dünya_büyüme[,-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)]
colnames(dünya_büyüme) <- c("Diğer Yenilenebilirler","Biyoyakıt","Güneş","Rüzgar","Hidrolik","Nükleer","Doğalgaz","Kömür","Petrol","Toplam Tüketim")
dünya_büyüme <- melt(dünya_büyüme)
dünya_büyüme_sıralı <- dünya_büyüme[order(dünya_büyüme$value),]

# Yüzdelik İşareti Eklendiğinde 100 İle Çarpılmasını Engelleyecek Bir Fonksiyon Yazılır
percent_labels <- function(x) {
  paste0(round(x, 2), "%")
}

# 2012-2022 Arası Tüketimi En Çok Büyüyen Enerji Kaynaklarının Çubuk Grafiği Çizdirilir
ggplot(dünya_büyüme_sıralı, aes(x = reorder(variable, +value), y = value)) +
  coord_flip() +
  geom_bar(stat = "identity", aes(fill = value), color = "black", size = 0.1) +  
  geom_text(aes(label = round(value, 2)), hjust = 1.1, color = "white", size = 3.5, fontface = "bold") +  
  labs(x = "Enerji Kaynağı", y = "Büyüme Yüzdesi", title = "2012-2022 Arası Enerji Kaynaklarının Değişimi (Log10)") +
  theme_light(base_family = "Arial", base_size = 15) + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0, size = 15, color = "darkblue"),  
    axis.title.x = element_text(face = "bold", size = 12, color = "darkblue"),
    axis.title.y = element_text(face = "bold", size = 12, color = "darkblue"),
    axis.text = element_text(size = 10, color = "darkblue"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey80"), 
    panel.grid.minor = element_line(color = "grey90"),
    legend.position = "none"  # Remove legend
  ) +
  scale_fill_gradient(low = "cyan3", high = "darkblue") +
  scale_y_log10(labels = percent_labels)


# GDP'nin Olduğu CSV Dosyası Çağrılır
gdp_veri <- read.csv("GDP by Country 1999-2022.csv", check.names = FALSE)

# Veri Geniş Formattan Uzun Formata Dönüştürülür
data_long <- gdp_veri %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "GDP")

# GDP Sütunu Sayısal Veri Türüne Dönüştürülür
data_long <- data_long %>%
  mutate(GDP = as.numeric(gsub(",", "", GDP)))

# NA Değerlerinin Kaynağını Kontrol Etme
na_values <- data_long %>%
  filter(is.na(GDP))

# 2012 ve 2022 Yıllarındaki GSYİH Verileri Filtrelenir ve 0 Olan Değerler Atılır
gdp_2012 <- data_long %>% filter(Year == "2012")
gdp_2012 <- gdp_2012 %>% filter(GDP != 0)
gdp_2022 <- data_long %>% filter(Year == "2022")
gdp_2022 <- gdp_2022 %>% filter(GDP != 0)


# 2012 ve 2022 Verilerini Birleştirilip GSYİH Değişimi Hesaplanır
gdp_büyüme_verisi <- gdp_2012 %>%
  select(Country, GDP_2012 = GDP) %>%
  inner_join(gdp_2022 %>% select(Country, GDP_2022 = GDP), by = "Country") %>%
  mutate(`GSYİH Değişimi` = ((GDP_2022 - GDP_2012) / GDP_2012) * 100)

colnames(gdp_büyüme_verisi) <- c("Varlık","GSYİH 2012", "GSYİH 2022", "GSYİH Değişimi")

combined_data <- gdp_büyüme_verisi %>%
  inner_join(toplam_tüketim_büyüme_verisi, by = "Varlık")

# Bu İki İlişkinin İncelenmesi İçin Korelasyon Katsayısı Bulunur
correlation_coefficient <- cor(combined_data$`GSYİH Değişimi`, combined_data$`Toplam Tüketimdeki Yüzdelik Büyüme`, use = "complete.obs")

# Bu İlişkinin Saçılım Grafiği Çizdirilir
ggplot(combined_data, aes(x = `GSYİH Değişimi`, y = `Toplam Tüketimdeki Yüzdelik Büyüme`)) +
  geom_point(size = 2.5, shape = 21, fill = "blue", color = "black", alpha = 0.7) +
  geom_smooth(method = "lm", col = "red", se = FALSE, linetype = "dashed", size = 1) +
  labs(title = "GSYİH Değişimi - Toplam Enerji Tüketim Değişimi Arasındaki Korelasyon",
       x = "GSYİH Büyümesi",
       y = "Toplam Tüketim Büyümesi") +
  theme_minimal(base_family = "Arial", base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "darkblue"),
    axis.title.x = element_text(face = "bold", size = 12, color = "darkblue"),
    axis.title.y = element_text(face = "bold", size = 12, color = "darkblue"),
    axis.text = element_text(size = 10, color = "black"),
    panel.grid.major = element_line(color = "azure"),  #
    panel.grid.minor = element_line(color = "grey"),
    plot.background = element_rect(fill = "azure"),
    legend.position = "none"  # Remove legend
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1), breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), breaks = scales::pretty_breaks(n = 10)) +
  annotate("text", x = Inf, y = -Inf, label = paste("Korelasyon:", round(correlation_coefficient, 2)), 
           hjust = 1.1, vjust = -0.5, size = 5, color = "brown2")


veri[5:22] <- veri[5:22] %>% replace(is.na(.), 0)

# Doğalgaz Harici Fosil Kaynak Oranı  Hesaplanır
veri$`Gaz Hariç Fosil` = rowSums(veri[21:22], na.rm = TRUE)

seçilmiş_ülkeler <- c("World","Asia","Iran","Saudi Arabia","Indonesia","Mexico","France","United Kingdom","Turkey","China", "United States", "European Union (27)", "India", "Russia", "Japan", "Canada", "Brazil", "South Korea", "Germany")
ülkelere_göre_filtrelenmiş_veri <- veri %>% filter(Varlık %in% seçilmiş_ülkeler)

# En Çok Değişimin Hangi Ülkelerde Olduğu Hesaplanır
change_summary <- ülkelere_göre_filtrelenmiş_veri %>%
  group_by(Varlık) %>%
  summarise(total_change = sum(abs(`Gaz Hariç Fosil`), na.rm = TRUE)) %>%
  arrange(desc(total_change))

# Sıralama Varlık Sütununa Uygulanır
ülkelere_göre_filtrelenmiş_veri$Varlık <- factor(ülkelere_göre_filtrelenmiş_veri$Varlık, levels = change_summary$Varlık)

# Doğalgaz Harici Fosil Kaynak Oranına Göre Isı Haritası Oluşturulur
ggplot(ülkelere_göre_filtrelenmiş_veri, aes(x = Yıl, y = Varlık, fill =`Gaz Hariç Fosil`)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "black", na.value = "white") +
  labs(title = "Yıllara Göre Doğalgaz Harici Fosil Kaynak Oranının Değişimi",
       x = "Yıl",
       y = "Ülke",
       fill = "Doğalgaz Hariç 
Fosil Kaynak Oranı") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

