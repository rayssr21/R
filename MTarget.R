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

#1. Loading Data
muzaki.mtarget <- 
  read_csv("C:/Users/Umum/Documents/rays/kerja/import/Data Email Outbound (MTarget).csv")

#2. Cek struktur data
glimpse(muzaki.mtarget)
head(muzaki.mtarget)
tail(muzaki.mtarget)
n_distinct(muzaki.mtarget$npwz)
distinct(muzaki.mtarget, jenis)
summary(muzaki.mtarget$last_sent)
count(muzaki.mtarget, jml_klik)

#3. Pilih Atribut penting
muzaki.mtarget <- select(muzaki.mtarget, 
                         npwz,
                         #email,
                         #gender,
                         first_sent,
                         last_sent,
                         jml_open,
                         jml_klik,
                         jml_sent)
glimpse(muzaki.mtarget)

#4. Ubah nama kolom
colnames(muzaki.mtarget)[colnames(muzaki.mtarget)=="NPWZ"]="npwz"
colnames(muzaki.mtarget)[colnames(muzaki.mtarget)=="EMAIL"]="Email"
colnames(muzaki.mtarget)[colnames(muzaki.mtarget)=="gender"]="Gender"
colnames(muzaki.mtarget)[colnames(muzaki.mtarget)=="first_sent"]="First.Sent.Email"
colnames(muzaki.mtarget)[colnames(muzaki.mtarget)=="last_sent"]="Last.Sent.Email"
colnames(muzaki.mtarget)[colnames(muzaki.mtarget)=="jml_open"]="Jumlah.Open.Email"
colnames(muzaki.mtarget)[colnames(muzaki.mtarget)=="jml_klik"]="Jumlah.Klik.Email"
colnames(muzaki.mtarget)[colnames(muzaki.mtarget)=="jml_sent"]="Jumlah.Sent.Email"
glimpse(muzaki.mtarget)

#5. Mengubah format data
muzaki.mtarget$npwz<- as.numeric(muzaki.mtarget$npwz)
muzaki.mtarget$First.Sent.Email  <- as.Date(muzaki.mtarget$First.Sent.Email)
muzaki.mtarget$Last.Sent.Email   <- as.Date(muzaki.mtarget$Last.Sent.Email)
muzaki.mtarget$Jumlah.Open.Email <- as.numeric(muzaki.mtarget$Jumlah.Open.Email)
muzaki.mtarget$Jumlah.Klik.Email <- as.numeric(muzaki.mtarget$Jumlah.Klik.Email)
muzaki.mtarget$Jumlah.Sent.Email <- as.numeric(muzaki.mtarget$Jumlah.Sent.Email)
glimpse(muzaki.mtarget)

#6. Mengubah Null/NA menjadi 0
muzaki.mtarget$Jumlah.Open.Email [is.na(muzaki.mtarget$Jumlah.Open.Email)] <- 0
muzaki.mtarget$Jumlah.Klik.Email [is.na(muzaki.mtarget$Jumlah.Klik.Email)] <- 0 
muzaki.mtarget$Jumlah.Sent.Email [is.na(muzaki.mtarget$Jumlah.Sent.Email)] <- 0 
glimpse(muzaki.mtarget)
head(muzaki.mtarget)
n_distinct(muzaki.mtarget$npwz)

#7. Menambah Kolom OpenRate
muzaki.mtarget <- muzaki.mtarget %>%
  mutate(open.rate = Jumlah.Open.Email/Jumlah.Sent.Email*100) #didata tdk ada jumlah sent
glimpse(muzaki.mtarget)
summary(muzaki.mtarget$open.rate)


#8. Join dengan data transaksi
muzaki.mtarget.transaksi <- left_join(muzaki.mtarget,
                                      RFM.Pareto, by = ("npwz"))
#
glimpse(muzaki.mtarget.transaksi)
n_distinct(muzaki.mtarget.transaksi$npwz)
head(muzaki.mtarget.transaksi)

#9. Segmentasi Muzaki Berdasarkan Open.Rate
summary(muzaki.mtarget$open.rate)
muzaki.mtarget$Segment_Openrate <- NA
muzaki.mtarget$Segment_Openrate [which(muzaki.mtarget$open.rate >= 40.541) ] <- "40% up"
muzaki.mtarget$Segment_Openrate [which(muzaki.mtarget$open.rate >= 15.789  &
                                         muzaki.mtarget$open.rate <  40.541 )] <- "15-40%"
muzaki.mtarget$Segment_Openrate [which(muzaki.mtarget$open.rate >=  2.381  &
                                         muzaki.mtarget$open.rate <  15.789 )] <- "2-15%"
muzaki.mtarget$Segment_Openrate [which(muzaki.mtarget$open.rate <   2.381 )] <- "2% down"
#
count(muzaki.mtarget, Segment_Openrate)
#

#10. Segmentasi Muzaki Berdasarkan Sent.Rate
summary(muzaki.mtarget$Jumlah.Sent.Email)
muzaki.mtarget$Segment_Jumlahsent <- NA
muzaki.mtarget$Segment_Jumlahsent [which(muzaki.mtarget$Jumlah.Sent.Email >= 41.00)] <- "40xUP"
muzaki.mtarget$Segment_Jumlahsent [which(muzaki.mtarget$Jumlah.Sent.Email >= 20.00 &
                                           muzaki.mtarget$Jumlah.Sent.Email < 41.00)]<- "20-40x" 
muzaki.mtarget$Segment_Jumlahsent [which(muzaki.mtarget$Jumlah.Sent.Email >= 10.00 &
                                           muzaki.mtarget$Jumlah.Sent.Email < 21.00)]<- "10-20x" 
muzaki.mtarget$Segment_Jumlahsent [which(muzaki.mtarget$Jumlah.Sent.Email <= 10.00)] <- "10xDown"
#
count(muzaki.mtarget, Segment_Jumlahsent)

################################################################################            

#11. Membuat Tabel Data
muzaki.mtarget.tabel <- data.frame(muzaki.mtarget)
DT::datatable(head(muzaki.mtarget.tabel, 110000),
              filter = 'top',
              rownames = FALSE,
              options = list(
                pagelength = 20))
glimpse(muzaki.mtarget.tabel)
################################################################################

#12. Eksplorasi Visual
muzaki.mtarget.tabel %>%
  group_by(Segment_Openrate,
           Segment_Jumlahsent) %>%
  summarise(n=n()) %>%
  ggplot(.,
         aes(x = Segment_Openrate,
             y = Segment_Jumlahsent,
             color = n,
             size  =n)) +
  geom_point(shape = 19) + 
  scale_color_gradient(low  = "#EAF2F8", 
                       high = "#154360") + 
  theme_bw(base_size = 8) + 
  scale_size(range = c(3, 20)) +
  theme(legend.position = "bottom")


muzaki.mtarget.tabel %>%
  count(Segment_Openrate,
        Segment_Jumlahsent) %>%
  ungroup() %>%
  mutate(jum_muzaki = n) %>%
  ggplot(.,
         aes(x=Segment_Openrate,
             y=Segment_Jumlahsent,
             fill=jum_muzaki,
             label=(jum_muzaki))) +
  geom_tile(color = "white") + 
  scale_fill_gradient(low = "#fed98e", high = "#993404") + 
  geom_text() + theme_bw(base_size = 8) +
  theme(legend.position = "bottom")

################################################################################

# Reporting MTarget

#1. Loading Data
MTarget <- read_excel("Data Reporting Mtarget 2711.xlsx")
glimpse(MTarget)

#2. Perbaiki Judul
colnames(MTarget)
colnames(MTarget)[colnames(MTarget)=="Email Subject"]="Judul"
colnames(MTarget)[colnames(MTarget)=="Published Date"]="Tanggal"


#3. Edit tanggal, hari
MTarget$Tanggal <- as.Date(MTarget$Tanggal)
summary(MTarget$Tanggal)

#4. Ganti Format ke numeric
MTarget$Recipient <- as.numeric(MTarget$Recipient)
MTarget$Sent <- as.numeric(MTarget$Sent)
MTarget$Opened <- as.numeric(MTarget$Opened)
MTarget$Clicked <- as.numeric(MTarget$Clicked)
MTarget$Bounce <- as.numeric(MTarget$Bounce)
MTarget$Unsubscribe <- as.numeric(MTarget$Unsubscribe)


#5. Grouping
MTarget %>%
  filter(Sent >= 100 &
           Tanggal >= "2020-11-01",
         Tanggal <= "2020-11-30") %>%
  group_by(Judul) %>%
  summarise(Jumlah_Sent = sum(Sent),
            Jumlah_Open = sum(Opened),
            Jumlah_Klik = sum(Clicked),
            Open_Rate   = Jumlah_Open/Jumlah_Sent*100,
            Click_Rate  = sum(Clicked)/sum(Opened)*100) %>%
  arrange(desc(Open_Rate))

#6. Eksplorasi
MTarget %>%
  filter(Sent >= 700 &
           Tanggal >= "2020-01-01") %>%
  group_by(Hari) %>%
  summarise(sum(Opened)) %>%
  arrange

MTarget %>%
  filter(Sent >= 700 &
           Tanggal >= "2020-10-01") %>%
  summarise(Jumlah_Sent = mean(Sent),
            Jumlah_Open = mean(Opened),
            Jumlah_Klik = mean(Clicked),
            Open_Rate   = Jumlah_Open/Jumlah_Sent*100,
            Click_Rate  = sum(Clicked)/sum(Opened)*100) %>%
  arrange()  
################################################################################
eksplor <- muzaki.mtarget.transaksi %>%
  filter(Jumlah.Open.Email == 0 &
           Donasi_Terakhir <= "2019-12-31" &
           Pareto == "Top20%") 
glimpse(eksplor)
eksplor <- data.frame(eksplor)
write.csv(eksplor, file = "muzaki non aktif.csv")
#
glimpse(eksplor)
n_distinct(eksplor$npwz)
summary(eksplor$Donasi_Terakhir)
distinct(eksplor, Donasi_Terakhir)
sum(eksplor$Monetari)
median(eksplor$Monetari)
count(eksplor, Pareto)