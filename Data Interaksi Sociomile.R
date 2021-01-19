library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(Hmisc)
library(readxl)
library(lubridate)
require(scales)
library(reshape2)

#1. Load Data
InteraksiSociomile<- read_csv("C:/Users/Umum/Documents/rays/kerja/import/data interaksi sociomile.csv")
glimpse(InteraksiSociomile)

#2. Ubah Judul
colnames(InteraksiSociomile)[colnames(InteraksiSociomile)=="date_time"]="Tanggal"
colnames(InteraksiSociomile)[colnames(InteraksiSociomile)=="category"]="Kategori.Interaksi"
colnames(InteraksiSociomile)[colnames(InteraksiSociomile)=="channel"]="Saluran.Interaksi"
glimpse(InteraksiSociomile)

#3. Formatting Tanggal
InteraksiSociomile$Tanggal <- as.Date(InteraksiSociomile$Tanggal)
glimpse(InteraksiSociomile)

#4. Grouping berdasarkan npwz-tanggal terakhir-resensi
data.interaksi <- InteraksiSociomile %>%
  group_by  (npwz) %>%
  summarise (Interaksi.Terakhir = max(Tanggal),
             Resensi.Interaksi  = as.double(difftime("2021-01-17",
                                                     as.Date(Interaksi.Terakhir, 
                                                             origin="2021-01-01"),
                                                     units = "day")))
glimpse(data.interaksi)
#
#3. Join Data
Data.Interaksi <- left_join(InteraksiSociomile,
                            data.interaksi, by = ("npwz"))
glimpse(Data.Interaksi)             

#4. Hilangkan atribut nama, email dan handphone
Data.Interaksi <- select(Data.Interaksi, -nama, -email, -handphone, -Tanggal)            
glimpse(Data.Interaksi)