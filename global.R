rm(list = ls())
options(scipen = 999)

library(readxl);library(tidyverse)

master <- read_excel("./anadolu_yakasi.xlsx") %>% 
  filter(
    grepl("Net M2", detay),
    grepl("Bina Ya��", detay),
    grepl("Bulundu�u Kat", detay),
    grepl("Kat Say�s�", detay),
    grepl("Is�nma Tipi", detay),
    grepl("Yak�t Tipi", detay),
    grepl("Krediye Uygunluk", detay),
    grepl("Yap�n�n Durumu", detay),
    !grepl("Belirtilmemi�", detay)
  ) %>% 
  mutate(
    net_m2 = word(gsub(".*m2 (.+) m2.*", "\\1", detay), 3),
    bina_yasi = word(gsub(".*Bina Ya�� (.+) Is�nma Tipi.*", "\\1", detay),1,1),
    isinma_tipi = gsub(".*Is�nma Tipi (.+) Kat Say�s�.*", "\\1", detay),
    krediye_uygunluk = gsub(".*Krediye Uygunluk (.+) E�ya Durumu.*", "\\1", detay),
    bulundugu_kat = gsub(".*Bulundu�u Kat (.+) Bina Ya��.*", "\\1", detay),
    banyo_sayisi = ifelse(grepl("Yap� Tipi",detay), gsub(".*Banyo Say�s� (.+) Yap� Tipi.*", "\\1", detay),
                          ifelse(!grepl("Yap� Tipi",detay), gsub(".*Banyo Say�s� (.+) Yap�n�n Durumu.*", "\\1", detay), "fck"))
  ) %>% 
  mutate(
    net_m2 = as.numeric(net_m2),
    bina_yasi = as.numeric(ifelse(bina_yasi == "S�f�r", 0, bina_yasi))
  ) %>% 
  filter(isinma_tipi != "Is�tma Yok") %>% 
  filter(krediye_uygunluk != "Bilinmiyor") %>% 
  mutate(bulundugu_kat = gsub("�\u0087","�",bulundugu_kat)) #baz� harfler d�zeltme isteyebilir

df <- master %>% 
  select(ilce, fiyat, net_m2, bina_yasi, isinma_tipi, krediye_uygunluk, bulundugu_kat, banyo_sayisi) %>% 
  mutate(
    bulundugu_kat_grup = ifelse(grepl("Kot 1|Kot 2|Kot 3|Bodrum|Bodrum ve Zemin|Yar� Bodrum", bulundugu_kat), "1",
                                ifelse(grepl("Bah�e Kat�|Giri� Kat�|Y�ksek Giri�|Zemin", bulundugu_kat), "2",
                                       ifelse(grepl("�at� Kat�|Teras Kat�|En �st Kat|Villa Kat�", bulundugu_kat), "3", "4")))) %>% 
  mutate(
    krediye_uygunluk = ifelse(krediye_uygunluk == "Uygun", "1", "0")
  )

######################################################################################################################

df_isinma <- df %>% 
  select(fiyat, isinma_tipi) %>% 
  mutate(isinma_tipi_fac = factor(isinma_tipi)) %>% 
  mutate(isinma_tipi_fac = as.numeric(isinma_tipi_fac)) %>% 
  group_by(isinma_tipi,isinma_tipi_fac) %>% 
  summarise(
    medyan_fiyat = median(fiyat)
  )

k_isinma <- kmeans(df_isinma[,c(2,3)], centers = 4, iter.max = 1000, nstart = 50)
df_isinma$isinma_tipi_grup <- k_isinma$cluster

df <- df %>% 
  left_join(df_isinma[,c(1,4)], by = "isinma_tipi")

######################################################################################################################

df_gelir <- read_excel("gelir.xlsx")
df_fiyat <- df %>% group_by(ilce) %>% summarise(medyan_fiyat = median(fiyat))
df_gelir_fiyat <- merge(df_gelir, df_fiyat, by = "ilce")

k_gelir <- kmeans(df_gelir_fiyat[,c(2,3)], centers = 3, iter.max = 1000, nstart = 50)
df_gelir_fiyat$gelir_grup <- k_gelir$cluster

df <- df %>% 
  left_join(df_gelir_fiyat[,c(1,4)], by = "ilce")

openxlsx::write.xlsx(df, "master.xlsx")
