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
options(scipen = 999)

# Load Data Transaksi updated
datatransaksi.2015_2020 <-read_csv("C:/Users/Umum/Documents/rays/kerja/import/data transaksi.csv")
glimpse(datatransaksi.2015_2020)
summary(datatransaksi.2015_2020$tgl)
sapply(datatransaksi.2015_2020, function(x) sum(is.na(x)))
n_distinct(datatransaksi.2015_2020$npwz)

# Model RFM

#1. Panggil data
data_rfm <- datatransaksi.2015_2020 %>%
  filter(!npwz == 317110010030204 &
           !divisi == "Divisi Unit Pengumpul Zakat (UPZ)" &
           !divisi == "Divisi Korporasi" &
           !divisi == "Divisi Humas dan Protokoler*" &
           !divisi == "Marketing dan Komunikasi" &
           !divisi == "Bidang Koordinasi OPZ" &
           bill >= median(bill) &
           tgl >= "2019-01-01")
glimpse(data_rfm)
n_distinct(data_rfm$npwz)

#2. Melakukan grouping pada tabel RFM
data_rfm.group <- data_rfm %>%
  group_by  (npwz) %>%
  summarise (Tgl.Donasi.Terakhir = max(tgl),
             Resensi.Donasi      = as.double(difftime("2021-01-17",
                                                      as.Date(Tgl.Donasi.Terakhir, 
                                                              origin="2021-01-01"),
                                                      units = "day")),
             Frekuensi.Donasi    = n_distinct(no_bukti),
             Monetari.Donasi     = sum(bill),
             One.Time.Donasi     = ifelse(Frekuensi.Donasi == 1, "One Timer", "More Than One"),
             Rata2.Donasi        = Monetari.Donasi/Frekuensi.Donasi,
             Jenis.Donasi        = last(jenis_donasi),
             Nama.Divisi         = last(divisi),
             Channel.Donasi      = last(via))
glimpse(data_rfm.group)
tail(data_rfm.group, n=30)
summary(data_rfm.group)
n_distinct(data_rfm.group$npwz)
head(data_rfm.group)
sapply(data_rfm.group, function(x) sum(is.na(x)))


#3. Membuat Pareto
D.RFM.G.Pareto        <- data_rfm.group[order(-data_rfm.group$Monetari.Donasi),]
pareto.cutoff         <- 0.8 * sum(D.RFM.G.Pareto$Monetari.Donasi)
D.RFM.G.Pareto$Pareto <- ifelse(cumsum(D.RFM.G.Pareto$Monetari.Donasi) <= pareto.cutoff, "Top20%", "Bottom80%")
D.RFM.G.Pareto$Pareto <- factor(D.RFM.G.Pareto$Pareto, levels = c("Top20%", "Bottom80%"), ordered = TRUE)
levels(D.RFM.G.Pareto$Pareto)
remove(pareto.cutoff)
RFM.Pareto            <- D.RFM.G.Pareto[order(D.RFM.G.Pareto$npwz),] 

head(RFM.Pareto, n=20)
glimpse(RFM.Pareto)
#
count(RFM.Pareto, Pareto)
#

#4. Menambah "Ranking RFM" dan "RFM Score"
RFM.Pareto.Rank <- RFM.Pareto %>%
  mutate_at(.funs             = funs(rank = ntile(., n = 5)), 
            .vars             = vars(Frekuensi.Donasi, Monetari.Donasi)) %>%
  mutate(Resensi.Donasi_rank  = ntile(desc(Resensi.Donasi), 5)) %>%
  mutate(RFM.Score            = as.integer(paste0(Resensi.Donasi_rank, 
                                                  Frekuensi.Donasi_rank, 
                                                  Monetari.Donasi_rank)))
#
head(RFM.Pareto.Rank)
glimpse(RFM.Pareto.Rank)
sapply(RFM.Pareto.Rank, function(x) sum(is.na(x)))
summary(RFM.Pareto.Rank)
#

#6. Join dengan Data Reach MTarget
# Loading data MTarget (Pastikan di script MTarget.R sudah di-run)
glimpse(muzaki.mtarget)
# Join data RFM.Pareto Rank vs muzaki.mtarget
RFM.Pareto.Mtarget <- left_join(RFM.Pareto.Rank,
                                muzaki.mtarget, by = ("npwz"))
glimpse(RFM.Pareto.Mtarget)
n_distinct(RFM.Pareto.Mtarget$npwz)
sapply(RFM.Pareto.Mtarget, function(x) sum(is.na(x)))
summary(RFM.Pareto.Mtarget)
head(RFM.Pareto.Mtarget)


#7. Join dengan Data Interaksi Sociomile
# Loading data Interaksi Sociomile (Pastikan di script Data Interaksi Sociomile.R sudah di-run)
glimpse(Data.Interaksi)
RFM.Pareto.Mtarget.Int <- left_join(RFM.Pareto.Mtarget,
                                    Data.Interaksi, by = ("npwz"))
glimpse(RFM.Pareto.Mtarget.Int)
#


#8. Join dengan Data Registrasi Muzaki Corner
glimpse(Data_Muzaki_Corner)
RFM.Pareto.Mtarget.Int.MC <- left_join(RFM.Pareto.Mtarget.Int,
                                       Data_Muzaki_Corner, by = ("npwz"))
glimpse(RFM.Pareto.Mtarget.Int.MC)
n_distinct(RFM.Pareto.Mtarget.Int.MC$npwz)


#9. Membuat Tabel
RFM.Tabel <- data.frame(RFM.Pareto.Mtarget.Int.MC)
glimpse(RFM.Tabel)


#10. Scoring
# R-Score versi 1
summary(RFM.Tabel$Resensi.Donasi)
RFM.Tabel$Segment_Resensi <- NA
RFM.Tabel$Segment_Resensi[which(RFM.Tabel$Resensi.Donasi >= 192.7083)]  <-"Frozen"
RFM.Tabel$Segment_Resensi[which(RFM.Tabel$Resensi.Donasi >= 183.7083 &
                                  RFM.Tabel$Resensi.Donasi <  192.7083)]  <-"Cold"
RFM.Tabel$Segment_Resensi[which(RFM.Tabel$Resensi.Donasi >= 80.7083  &
                                  RFM.Tabel$Resensi.do <  183.7083)]  <-"Warm"
RFM.Tabel$Segment_Resensi[which(RFM.Tabel$Resensi.Donasi <  80.7083)]  <-"Hot"
#
count(RFM.Tabel, Segment_Resensi)
#
# R-Score versi 2
summary(RFM.Tabel$Resensi.Donasi_rank)
RFM.Tabel$Segment_Resensi <- NA
RFM.Tabel$Segment_Resensi[which(RFM.Tabel$Resensi.Donasi_rank <=   2)]  <-"Cold"
RFM.Tabel$Segment_Resensi[which(RFM.Tabel$Resensi.Donasi_rank ==   3)]  <-"Warm"
RFM.Tabel$Segment_Resensi[which(RFM.Tabel$Resensi.Donasi_rank >=   4)]  <-"Hot"

#
count(RFM.Tabel, Segment_Resensi)
#

# F-Score
summary(RFM.Tabel$Frekuensi)
RFM.Tabel$Segment_Frekuensi <- NA
RFM.Tabel$Segment_Frekuensi[which(RFM.Tabel$Frekuensi == 1.000)] <- "Sekali"
RFM.Tabel$Segment_Frekuensi[which(RFM.Tabel$Frekuensi == 2.000)] <- "Dua Kali"
RFM.Tabel$Segment_Frekuensi[which(RFM.Tabel$Frekuensi > 2.000)] <- "Sering"
#
count(RFM.Tabel, Segment_Frekuensi)
#
# F-Score Versi 2
summary(RFM.Tabel$Frekuensi.Donasi_rank)
RFM.Tabel$Segment_Frekuensi <- NA
RFM.Tabel$Segment_Frekuensi[which(RFM.Tabel$Frekuensi.Donasi_rank <= 2)] <- "Jarang"
RFM.Tabel$Segment_Frekuensi[which(RFM.Tabel$Frekuensi.Donasi_rank == 3)] <- "Middle"
RFM.Tabel$Segment_Frekuensi[which(RFM.Tabel$Frekuensi.Donasi_rank >= 4)] <- "Sering"
#
count(RFM.Pareto.Rank, Segment_Frekuensi)
#

# M-Score
options(scipen = 999)
summary(RFM.Tabel$Monetari)
RFM.Tabel$Segment_Monetari <- NA
RFM.Tabel$Segment_Monetari[which(RFM.Tabel$Monetari <  162500) ] <- "<160rb"
RFM.Tabel$Segment_Monetari[which(RFM.Tabel$Monetari >= 162500  &
                                   RFM.Tabel$Monetari <  365000 ) ] <- "160-370rb"
RFM.Tabel$Segment_Monetari[which(RFM.Tabel$Monetari >= 365000   &
                                   RFM.Tabel$Monetari <  1382000)] <- "370-1400rb"
RFM.Tabel$Segment_Monetari[which(RFM.Tabel$Monetari >= 1382000)] <- ">1400rb"
#
count(RFM.Tabel, Segment_Monetari)
glimpse(RFM.Tabel)
#
# M-Score Versi-2
options(scipen = 999)
summary(RFM.Tabel$Monetari.Donasi_rank)
RFM.Tabel$Segment_Monetari <- NA
RFM.Tabel$Segment_Monetari[which(RFM.Tabel$Monetari.Donasi_rank <= 2)] <- "Jarang"
RFM.Tabel$Segment_Monetari[which(RFM.Tabel$Monetari.Donasi_rank == 3)] <- "Middle"
RFM.Tabel$Segment_Monetari[which(RFM.Tabel$Monetari.Donasi_rank >= 4)] <- "Sering"

############
############
write.csv(RFM.Tabel, file = "Tabel Muzaki.csv")
DT::datatable (head(RFM.Tabel, 59950),
               filter   = 'top',
               rownames = FALSE,
               options  = list(pagelength = 10))

# Eksplorasi

RFM.Tabel %>%
  group_by(Pareto) %>%
  summarise(sum(Monetari.Donasi),
            n_distinct(npwz))
#

# Plot & Bar Chart
#1.
RFM.Tabel %>%
  ggplot(aes(x = Pareto)) +
  geom_bar(color = "white", fill = "#993404") +
  theme_bw(base_size = 8)

#2.
RFM.Tabel %>%
  count(Pareto) %>%
  mutate(Persentase = n/sum(n)) %>%
  ggplot(aes(x=Pareto, y=Persentase)) +
  geom_bar(stat = "identity", color = "white", fill = "#993404") +
  geom_text(aes(label = n), vjust = -0.25) +
  scale_y_continuous(labels = percent, breaks = seq(0, 0.90, by = 0.2), limits = c(0, 0.90)) +
  labs(title = "Proportion of Pareto Muzaki",
       y = "% of Muzaki",
       x = "Pareto")
#3.
RFM.Tabel %>%
  ggplot(aes(x=RFM_score,
             y=Monetari,
             color = factor(Monetari_rank))) +
  geom_jitter() +
  scale_color_brewer() +
  scale_y_sqrt(labels = dollar) +
  theme_bw(base_size = 8) +
  theme(legend.position = "bottom")

#4.
RFM.Tabel %>%
  group_by(Segment.Resensi.Donasi,
           Segment.Frekuensi.Donasi) %>%
  summarise(n=n(),
            avg_giving = sum(Monetari.Donasi)) %>%
  ggplot(.,
         aes(x=Segment.Resensi.Donasi,
             y=Segment.Frekuensi.Donasi,
             fill=n)) +
  geom_tile(color = "white") + 
  scale_fill_gradient(low = "#fed98e", 
                      high = "#993404") + 
  theme_bw(base_size = 8) +
  theme(legend.position = "bottom")

#5.
RFM.Tabel %>%
  group_by(Segment.Resensi.Donasi,
           Segment.Frekuensi.Donasi) %>%
  summarise(n=n(),
            Avg.ZIS=median(Monetari.Donasi)) %>%
  ggplot(.,
         aes(x=Segment.Resensi.Donasi,
             y=Segment.Frekuensi.Donasi,
             color=Avg.ZIS,
             size=n)) +
  geom_point(shape = 19) + 
  scale_color_gradient(low  = "#EAF2F8", 
                       high = "#154360") + 
  theme_bw(base_size = 8) + 
  scale_size(range = c(3, 20)) +
  theme(legend.position = "bottom")

RFM.Tabel %>%
  group_by(Segment.Openrate.Email,
           Segment.Resensi.Donasi) %>%
  summarise(n=n(),
            Avg.Monetari=median(Monetari.Donasi)) %>%
  ggplot(.,
         aes(x=Segment.Openrate.Email,
             y=Segment.Resensi.Donasi,
             color=Avg.Monetari,
             size=n)) +
  geom_point(shape = 19) + 
  scale_color_gradient(low  = "#EAF2F8", 
                       high = "#154360") + 
  theme_bw(base_size = 8) + 
  scale_size(range = c(3, 20)) +
  theme(legend.position = "bottom")
# Jika 

#6. Melihat jumlah muzaki terbanyak ada di kelompok mana
RFM.Tabel %>%
  count(Segment.Resensi.Donasi,
        Segment.Frekuensi.Donasi) %>%
  ungroup() %>%
  mutate(persen_muzaki = n/sum(n)) %>%
  ggplot(.,
         aes(x=Segment.Resensi.Donasi,
             y=Segment.Frekuensi.Donasi,
             fill=persen_muzaki,
             label=percent(persen_muzaki))) +
  geom_tile(color = "white") + 
  scale_fill_gradient(low = "#fed98e", high = "#993404") + 
  geom_text() + theme_bw(base_size = 8) +
  theme(legend.position = "bottom")

#7. Melihat jumlah muzaki terbanyak
RFM.Tabel %>%
  count(Segment.Resensi.Donasi,
        Segment.Frekuensi.Donasi) %>%
  ungroup() %>%
  mutate(jum_muzaki = n) %>%
  ggplot(.,
         aes(x=Segment.Resensi.Donasi,
             y=Segment.Frekuensi.Donasi,
             fill=jum_muzaki,
             label=(jum_muzaki))) +
  geom_tile(color = "white") + 
  scale_fill_gradient(low = "#fed98e", high = "#993404") + 
  geom_text() + theme_bw(base_size = 8) +
  theme(legend.position = "bottom")
# >> kelompok terbesar adalah yang baru2 ini kembali berzakat walau jarang sekali

################################################################################