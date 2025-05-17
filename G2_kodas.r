
#Reikiamų bibliotekų įkelimas
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(sf)
library(patchwork)
library(ggcorrplot)
library(MASS)
library(car)
library(lmtest)
library(lm.beta)
library(GGally)
library(ggpubr)
library(sandwich)

#Duomenų nuskaitymas iš failų
bvp_bendras_apsk <- read_excel("bvp_bendras_apskrities.xlsx", sheet = "Sheet1")%>%
  pivot_longer(cols = c("2019", "2020", "2021", "2022", "2023"), 
               names_to = "Metai", 
               values_to = "BVP")

darbo_uzmokestis_apsk <- read_excel("darbo_uzmokestis_2018_2024.xlsx", sheet = "Sheet3")%>%
  pivot_longer(cols = c("2019", "2020", "2021", "2022", "2023"), 
               names_to = "Metai", 
               values_to = "Darbo uzmokestis")

nedarbas <- read_excel("nedarbas.xlsx", sheet = "Sheet1")%>%
  pivot_longer(cols = c("2019", "2020", "2021", "2022", "2023"), 
               names_to = "Metai", 
               values_to = "Nedarbas")

soc_duomenys <- bvp_bendras_apsk %>%
  full_join(darbo_uzmokestis_apsk, by = c("Apskritis", "Metai")) %>%
  full_join(nedarbas, by = c("Apskritis", "Metai")) %>%
  rename("metai"=Metai)%>%
  mutate(metai = dplyr::recode(metai,
                        "2019" = "2019_2020",
                        "2020" = "2020_2021",
                        "2021" = "2021_2022",
                        "2022" = "2022_2023",
                        "2023" = "2023_2024"))



VBE_2019_2020_vidurkiai <- read_excel("VBE_2019_2020_vidurkiai.xlsx",sheet = "2019_2020")%>%
  filter(str_detect(mokykla, "gimnazija$"))%>%
  dplyr::select(mokykla,`Lietuvių kalba ir literatūra`,Matematika, `Užsienio kalba (anglų)`,Geografija, Biologija)%>%
  mutate(metai="2019_2020")

VBE_2020_2021_vidurkiai <- read_excel("VBE_2020_2021_vidurkiai.xlsx",sheet = "2020_2021")%>%
  filter(str_detect(mokykla, "gimnazija$"))%>%
  dplyr::select(mokykla,`Lietuvių kalba ir literatūra`,Matematika, `Užsienio kalba (anglų)`,Geografija, Biologija)%>%
  mutate(metai="2020_2021")

VBE_2021_2022_vidurkiai <- read_excel("VBE_2021_2022_vidurkiai.xlsx", sheet = "2021_2022")%>%
  filter(str_detect(mokykla, "gimnazija$"))%>%
  dplyr::select(mokykla,`Lietuvių kalba ir literatūra`,Matematika, `Užsienio kalba (anglų)`,Geografija, Biologija)%>%
  mutate(metai="2021_2022")

VBE_2022_2023_vidurkiai <- read_excel("VBE_2022_2023_vidurkiai.xlsx",sheet = "2022_2023")%>%
  filter(str_detect(mokykla, "gimnazija$"))%>%
  dplyr::select(mokykla,`Lietuvių kalba ir literatūra`,Matematika, `Užsienio kalba (anglų)`,Geografija, Biologija)%>%
  mutate(metai="2022_2023")

VBE_2023_2024_vidurkiai <- read_excel("VBE_2023_2024_vidurkiai.xlsx",sheet = "2023_2024")%>%
  filter(str_detect(mokykla, "gimnazija$"))%>%
  dplyr::select(mokykla,`Lietuvių kalba ir literatūra`,Matematika, `Užsienio kalba (anglų)`,Geografija, Biologija)%>%
  mutate(metai="2023_2024")


VBE_visu <- bind_rows(VBE_2019_2020_vidurkiai, 
                       VBE_2020_2021_vidurkiai, 
                       VBE_2021_2022_vidurkiai, 
                       VBE_2022_2023_vidurkiai, 
                       VBE_2023_2024_vidurkiai) %>%
  arrange(mokykla, metai)


mokiniu_kiekis_2019_2020 <- read_excel("mokinių_kiekis.xlsx", sheet = "2019_2020", col_types = c("text", "numeric")) %>%
  filter(str_detect(Pavadinimas, "gimnazija$")) %>%
  rename(kiekis = `2019_2020`)%>%
  mutate(Metai = "2019_2020")

mokiniu_kiekis_2020_2021 <- read_excel("mokinių_kiekis.xlsx", sheet = "2020_2021", col_types = c("text", "numeric")) %>%
  filter(str_detect(Pavadinimas, "gimnazija$")) %>%
  rename(kiekis = `2020_2021`)%>%
  mutate(Metai = "2020_2021")

mokiniu_kiekis_2021_2022 <- read_excel("mokinių_kiekis.xlsx", sheet = "2021_2022", col_types = c("text", "numeric")) %>%
  filter(str_detect(Pavadinimas, "gimnazija$")) %>%
  rename(kiekis = `2021_2022`)%>%
  mutate(Metai = "2021_2022")

mokiniu_kiekis_2022_2023 <- read_excel("mokinių_kiekis.xlsx", sheet = "2022_2023", col_types = c("text", "numeric")) %>%
  filter(str_detect(Pavadinimas, "gimnazija$")) %>%
  rename(kiekis = `2022_2023`)%>%
  mutate(Metai = "2022_2023")

mokiniu_kiekis_2023_2024 <- read_excel("mokinių_kiekis.xlsx", sheet = "2023_2024", col_types = c("text", "numeric")) %>%
  filter(str_detect(Pavadinimas, "gimnazija$")) %>%
  rename(kiekis = `2023_2024`)%>%
  mutate(Metai = "2023_2024")

mokiniu_kiekis <- bind_rows(
  mokiniu_kiekis_2019_2020,
  mokiniu_kiekis_2020_2021,
  mokiniu_kiekis_2021_2022,
  mokiniu_kiekis_2022_2023,
  mokiniu_kiekis_2023_2024) %>%
  arrange(Pavadinimas,Metai) %>%
  rename("mokykla" = Pavadinimas,
         "metai"=Metai)


pedagogai <- read_excel("877Pedagogų dalis.xlsx",sheet = "Sheet1")%>%
  mutate(rodiklis=ifelse(`Darbo stažas` %in% c("15-19 metų", "20-24 metų", "25 ir daugiau"), `Asmenų skaičius`, 0))
  
pedagogu_skaicius<- pedagogai%>%
  filter(`Mokslo metai` %in% c("2019-2020","2020-2021","2021-2022","2022-2023","2023-2024"),
         str_detect(`Pd Institucijos pavadinimas`, "gimnazija$")) %>%
  group_by(`Pd Institucijos pavadinimas`,`Mokslo metai`)%>%
  summarise(bendras=sum(`Asmenų skaičius`),
            stazas_skaic=sum(rodiklis),
            dalis_mok=stazas_skaic/bendras)%>%
  mutate(`Mokslo metai` = str_replace(`Mokslo metai`, "-", "_"))%>%
  rename("mokykla" = `Pd Institucijos pavadinimas`,
         "metai"=`Mokslo metai`) 

mok_sk_vienam_mokyt<-pedagogu_skaicius%>%
  full_join(mokiniu_kiekis, by=c("mokykla","metai")) %>%
  rename("mokytojai"=bendras,
         "mokiniai"=kiekis) %>%
  mutate(dalis=ceiling(mokiniai/mokytojai))



mokyklos_apsk <- read_excel("mokyklos.xlsx")%>%
  rename("mokykla"=Mokykla)

mokinių_kiekis_klases <- read_excel("mokinių_kiekis_klases 2.xlsx",sheet = "klases")%>%
  pivot_longer(cols = c("2019_2020","2020_2021","2021_2022","2022_2023","2023_2024"), 
               names_to = "metai", 
               values_to = "klasiu_sk")


finansai_2019_2020 <- read_excel("finansavimas.xlsx", sheet = "2019_2020_fin", col_types = c("numeric", "text")) %>% mutate(Metai = "2019_2020") %>% filter(str_detect(Mokykla, "gimnazija$"))
finansai_2020_2021 <- read_excel("finansavimas.xlsx", sheet = "2020_2021_fin", col_types = c("numeric", "text")) %>% mutate(Metai = "2020_2021") %>% filter(str_detect(Mokykla, "gimnazija$"))
finansai_2021_2022 <- read_excel("finansavimas.xlsx", sheet = "2021_2022_fin", col_types = c("numeric", "text")) %>% mutate(Metai = "2021_2022") %>% filter(str_detect(Mokykla, "gimnazija$"))
finansai_2022_2023 <- read_excel("finansavimas.xlsx", sheet = "2022_2023_fin", col_types = c("numeric", "text")) %>% mutate(Metai = "2022_2023") %>% filter(str_detect(Mokykla, "gimnazija$"))
finansai_2023_2024 <- read_excel("finansavimas.xlsx", sheet = "2023_2024_fin", col_types = c("numeric", "text")) %>% mutate(Metai = "2023_2024") %>% filter(str_detect(Mokykla, "gimnazija$"))

finansai_bendrai <- bind_rows(
  finansai_2019_2020,
  finansai_2020_2021,
  finansai_2021_2022,
  finansai_2022_2023,
  finansai_2023_2024) %>%
  arrange(Mokykla, Metai) %>%
  rename("mokykla"=Mokykla,
         "metai"=Metai,
         "krepselio_dydis"=Suma)



#duomenų apjungimas

bendri_duomenys <- VBE_visu%>%
  left_join(mok_sk_vienam_mokyt, by=c("mokykla","metai"))%>%  
  full_join(finansai_bendrai,by=c("mokykla","metai"))%>%
  full_join(mokinių_kiekis_klases,by=c("mokykla","metai"))%>%
  full_join(mokyklos_apsk, by="mokykla")%>%
  full_join(soc_duomenys, by=c("Apskritis","metai"))%>%
  mutate(vid_klases_dydis=ceiling(mokiniai/klasiu_sk))

bendri_duomenys_galutiniai <- bendri_duomenys %>%
  group_by(mokykla) %>%
  filter(n() == 5) %>%  
  filter(!any(if_any(everything(), is.na))) %>% 
  ungroup()

length(unique(bendri_duomenys_galutiniai$mokykla)) #120 mokyklu
#=======================================================================================================================
#           PRADINĖ ANALIZĖ 
#=======================================================================================================================
bendri_duomenys_galutiniai %>%
  group_by(metai) %>%
  summarise(
    med_Lietuviu = median(`Lietuvių kalba ir literatūra`, na.rm = TRUE),
    med_Matematika = median(Matematika, na.rm = TRUE),
    med_Anglu = median(`Užsienio kalba (anglų)`, na.rm = TRUE),
    med_Geografija = median(Geografija, na.rm = TRUE),
    med_Biologija = median(Biologija, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -metai, names_to = "dalykas", values_to = "med_reiksm")%>%
  ggplot(., aes(x = as.factor(metai), y = med_reiksm, color = dalykas, group = dalykas)) +
  geom_line(linewidth = 1.2) +  
  geom_point(size = 3) +  
  scale_color_manual(
    values = c("#D55E00", "#0072B2", "#009E73", "#F0E442", "#CC79A7"),
    labels = c(
      "med_Lietuviu" = "Lietuvių kalba",
      "med_Matematika" = "Matematika",
      "med_Anglu" = "Anglų kalba",
      "med_Geografija" = "Geografija",
      "med_Biologija" = "Biologija"
    )
  ) +
  scale_y_continuous(limits = c(15, 80), breaks = seq(15, 80)) +  
  labs(
    title = "VBE egzaminų rezultatų medianos pagal metus",
    x = "Mokslo metai",
    y = "Rezultatų mediana",
    color = "Dalykas"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "gray90"),  
    panel.grid.minor = element_blank(),  
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14), 
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )


pavadinimai<- c(  
  `Lietuvių kalba ir literatūra`="Lietuvių kalba\n ir literatūra",
  Matematika="Matematika",
  `Užsienio kalba (anglų)`="Užsienio kalba \n (anglų)",
  Geografija="Geografija",
  Biologija="Biologija")

bendri_duomenys_galutiniai %>%
  dplyr::select(`Lietuvių kalba ir literatūra`, Matematika, `Užsienio kalba (anglų)`, Geografija, Biologija) %>%
  ggpairs(columnLabels = pavadinimai,
          lower = list(continuous = wrap("smooth", method = "lm", se = FALSE, color = "blue")),  
          diag = list(continuous = wrap("densityDiag", fill = "lightblue", alpha = 0.5)),  
          upper = list(continuous = wrap("cor", size = 6, color = "black"))) +
  theme_minimal(base_size = 14) +  
  theme(axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.text.x = element_text(size = 14),  
        axis.text.y = element_text(size = 14)) +
  labs(title = "Egzaminų rezultatų koreliacijos matrica")




VBE_visu %>%
  pivot_longer(cols = c(`Lietuvių kalba ir literatūra`, Matematika, `Užsienio kalba (anglų)`, Geografija, Biologija),
               names_to = "dalykas", values_to = "rezultatai")%>%
  ggplot(., aes(x = as.factor(metai), y = rezultatai)) +
  geom_boxplot(aes(fill = metai),na.rm = TRUE, outlier.size = 1, outlier.alpha = 0.5, color = "black") +
  facet_wrap(~dalykas, scales = "free_y", nrow=1) +  
  theme_minimal(base_size = 16) +
  labs(
    title = "VBE egzaminų rezultatai pagal metus",
    x = "Mokslo metai",
    y = "Rezultatai") +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14), 
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold",),
    legend.position = "none")







#žemėlapis pagal egzaminų vidurkius apskrityje 
Regionai <- st_read("gadm41_LTU_shp (3)/gadm41_LTU_1.shp")

zemelapis <- bendri_duomenys_galutiniai %>%
  group_by(Apskritis) %>%
  summarise(
    LT = mean(`Lietuvių kalba ir literatūra`),
    MAT = mean(Matematika),
    BIO = mean(Biologija),
    GEO = mean(Geografija),
    ANGL = mean(`Užsienio kalba (anglų)`)
  ) %>%
  as_tibble() %>%
  mutate(Apskritis = str_replace_all(Apskritis, c(
    "Klaipėdos apskritis" = "Klaipedos",
    "Marijampolės apskritis" = "Marijampoles",
    "Panevėžio apskritis" = "Panevezio",
    "Šiaulių apskritis" = "Šiauliai",
    "Tauragės apskritis" = "Taurages",
    "Telšių apskritis" = "Telšiai",
    "Vilniaus apskritis" = "Vilniaus",
    "Alytaus apskritis" = "Alytaus",
    "Utenos apskritis" = "Utenos",
    "Kauno apskritis" = "Kauno"
  )))

zemelapis_sf <- zemelapis %>%
  left_join(Regionai, by = c("Apskritis" = "NAME_1"))

summary(zemelapis_sf)

if (!inherits(zemelapis_sf, "sf")) {
  zemelapis_sf <- st_as_sf(zemelapis_sf)
}



p1 <- ggplot(zemelapis_sf) +
  geom_sf(aes(fill = MAT), color = "white", size = 0.2) +  
  scale_fill_viridis_c(option = "plasma", begin = 0, end = 1, 
                       limits = c(25, 35), breaks = seq(25, 35, 2))+
  theme_minimal(base_size = 15) +  
  theme(
    axis.text = element_blank(),  
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    panel.grid = element_blank(), 
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  
    legend.position = "right",   
    legend.title = element_text(size = 12, face = "bold"),  
    legend.text = element_text(size = 10)
  ) +
  labs(title = "Matematika", fill="Matematika")

p2 <- ggplot(zemelapis_sf) +
  geom_sf(aes(fill = LT), color = "white", size = 0.2) +  
  scale_fill_viridis_c(option = "plasma", begin = 0, end = 1,
                       limits = c(40, 52), breaks = seq(40, 52, 2)) + 
  theme_minimal(base_size = 15) +  
  theme(
    axis.text = element_blank(),  
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    panel.grid = element_blank(), 
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  
    legend.position = "right",   
    legend.title = element_text(size = 12, face = "bold"),  
    legend.text = element_text(size = 10) 
  ) +
  labs(title = "Lietuvių k.", fill="Lietuvių k.")

p3 <- ggplot(zemelapis_sf) +
  geom_sf(aes(fill = ANGL), color = "white", size = 0.2) +  
  scale_fill_viridis_c(option = "plasma", begin = 0, end = 1,
                       limits = c(58, 70), breaks = seq(58, 70, 2)) + 
  theme_minimal(base_size = 15) +  
  theme(
    axis.text = element_blank(),  
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    panel.grid = element_blank(), 
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  
    legend.position = "right",   
    legend.title = element_text(size = 12, face = "bold"),  
    legend.text = element_text(size = 10) 
  ) +
  labs(title = "Anglų k.", fill="Anglu k.")

p4 <- ggplot(zemelapis_sf) +
  geom_sf(aes(fill = BIO), color = "white", size = 0.2) +  
  scale_fill_viridis_c(option = "plasma", begin = 0, end = 1,
                       limits = c(46, 54), breaks = seq(46, 54, 2))+
  theme_minimal(base_size = 15) +  
  theme(
    axis.text = element_blank(),  
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    panel.grid = element_blank(), 
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  
    legend.position = "right",   
    legend.title = element_text(size = 12, face = "bold"),  
    legend.text = element_text(size = 10) 
  ) +
  labs(title = "Biologija", fill="Biologija")

p5 <- ggplot(zemelapis_sf) +
  geom_sf(aes(fill = GEO), color = "white", size = 0.2) +  
  scale_fill_viridis_c(option = "plasma", begin = 0, end = 1,
                       limits = c(42, 58), breaks = seq(42, 58, 2))+
  theme_minimal(base_size = 15) +  
  theme(
    axis.text = element_blank(),  
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    panel.grid = element_blank(), 
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  
    legend.position = "right",   
    legend.title = element_text(size = 12, face = "bold"),  
    legend.text = element_text(size = 10) 
  ) +
  labs(title = "Geografija", fill="Geografija")

graf1 <- ((p1 | p2)/(p3|p4|p5))
print(graf1) 




pav<-c(
  dalis_mok = "Mokytojų dalis \n kūrių stažas >15 m.", 
  dalis = "Mokinių skaičius \n vienam mokytojui",
  krepselio_dydis = "Krepšelio dydis", 
  BVP = "BVP", 
  Nedarbas = "Nedarbas", 
  `Darbo uzmokestis` = "Darbo užmokestis"
)

bendri_duomenys_galutiniai %>%
  dplyr::select(dalis_mok,dalis,krepselio_dydis,BVP,Nedarbas,`Darbo uzmokestis`) %>%
  ggpairs(., 
          lower = list(continuous = wrap("points",color = "blue")),  
          diag = list(continuous = wrap("densityDiag", fill = "lightblue", alpha = 0.5)),  
          upper = list(continuous = wrap("cor", size = 5, color = "black")),
          columnLabels = pav) + 
  theme_minimal(base_size = 14) +
  theme( axis.title.x = element_text(size = 14),
         axis.title.y = element_text(size = 14), 
         plot.title = element_text(size = 18, hjust = 0.5),
         axis.text.x = element_text(size = 12),
         axis.text.y = element_text(size = 12))



#######################################################################################
#Krepselio dydis ----------------
pl1<-ggplot(bendri_duomenys_galutiniai, aes(x=krepselio_dydis, y=Matematika)) + 
  geom_point()+
  labs(
    title = "Matematika",
    x = "Krepšelio dydis (€)",
    y = "Matematikos VBE balas",
    color = "Mokykla"
  ) +
  geom_point(alpha = 0.7, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80", size = 0.5),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16)  
  )

pl2<-ggplot(bendri_duomenys_galutiniai, aes(x=krepselio_dydis, y=`Lietuvių kalba ir literatūra`)) + 
  geom_point()+
  labs(
    title = "Lietuvių k.",
    x = "Krepšelio dydis (€)",
    y = "Lietuvių k. VBE balas",
    color = "Mokykla"
  ) +
  geom_point(alpha = 0.7, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80", size = 0.5),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16))

pl3<-ggplot(bendri_duomenys_galutiniai, aes(x=krepselio_dydis, y=Biologija)) + 
  geom_point()+
  labs(
    title = "Biologija",
    x = "Krepšelio dydis (€)",
    y = "Biologijos VBE balas",
    color = "Mokykla"
  ) +
  geom_point(alpha = 0.7, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80", size = 0.5),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16))

pl4<-ggplot(bendri_duomenys_galutiniai, aes(x=krepselio_dydis, y=`Užsienio kalba (anglų)`)) + 
  geom_point()+
  labs(
    title = "Anglų k.",
    x = "Krepšelio dydis (€)",
    y = "Anglų k. VBE balas",
    color = "Mokykla"
  ) +
  geom_point(alpha = 0.7, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80", size = 0.5),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16))

pl5<-ggplot(bendri_duomenys_galutiniai, aes(x=krepselio_dydis, y=Geografija)) + 
  geom_point()+
  labs(
    title = "Geografija",
    x = "Krepšelio dydis (€)",
    y = "Geografijos VBE balas",
    color = "Mokykla"
  ) +
  geom_point(alpha = 0.7, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80", size = 0.5),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16))

graf2<-((pl1|pl2)/(pl3|pl4|pl5))  +
  plot_annotation(
    title = "Egzaminų rezultatų ir krepšelio dydžio sąryšis",
    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)))
print(graf2)

#Vid alga -----------------------------------------
pl1.1<-ggplot(bendri_duomenys_galutiniai, aes(x=`Darbo uzmokestis`, y=Matematika)) + 
  geom_point()+
  labs(
    title = "Matematika",
    x = "Vidutinė alga (€)",
    y = "Matematikos VBE balas",
    color = "Mokykla"
  ) +
  geom_point(alpha = 0.7, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80", size = 0.5),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16)  
  )

pl2.1<-ggplot(bendri_duomenys_galutiniai, aes(x=`Darbo uzmokestis`, y=`Lietuvių kalba ir literatūra`)) + 
  geom_point()+
  labs(
    title = "Lietuvių k.",
    x = "Vidutinė alga (€)",
    y = "Lietuvių k. VBE balas",
    color = "Mokykla"
  ) +
  geom_point(alpha = 0.7, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80", size = 0.5),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16))

pl3.1<-ggplot(bendri_duomenys_galutiniai, aes(x=`Darbo uzmokestis`, y=Biologija)) + 
  geom_point()+
  labs(
    title = "Biologija",
    x = "Vidutinė alga (€)",
    y = "Biologijos VBE balas",
    color = "Mokykla"
  ) +
  geom_point(alpha = 0.7, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80", size = 0.5),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16))

pl4.1<-ggplot(bendri_duomenys_galutiniai, aes(x=`Darbo uzmokestis`, y=`Užsienio kalba (anglų)`)) + 
  geom_point()+
  labs(
    title = "Anglų k.",
    x = "Vidutinė alga (€)",
    y = "Anglų k. VBE balas",
    color = "Mokykla"
  ) +
  geom_point(alpha = 0.7, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80", size = 0.5),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16))

pl5.1<-ggplot(bendri_duomenys_galutiniai, aes(x=`Darbo uzmokestis`, y=Geografija)) + 
  geom_point()+
  labs(
    title = "Geografija",
    x = "Vidutinė alga (€)",
    y = "Geografijos VBE balas",
    color = "Mokykla"
  ) +
  geom_point(alpha = 0.7, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80", size = 0.5),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16))

graf2.1<-((pl1.1|pl2.1)/(pl3.1|pl4.1|pl5.1)) +
  plot_annotation(
    title = "Egzaminų rezultatų ir vidutinio darbo užmokesčio sąryšis",
    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)))
print(graf2.1)

#vidutinis_klases_mokiniu_kiekis_-----------------------------------------------------------

pl_1.1<-ggplot(bendri_duomenys_galutiniai, aes(x=vid_klases_dydis, y=Matematika)) + 
  geom_point()+
  labs(
    title = "Matematika",
    x = "Vidutinis kalsės dydis",
    y = "Matematikos VBE balas",
    color = "Mokykla"
  ) +
  geom_point(alpha = 0.7, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80", size = 0.5),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16)  
  )

pl_2.1<-ggplot(bendri_duomenys_galutiniai, aes(x=vid_klases_dydis, y=`Lietuvių kalba ir literatūra`)) + 
  geom_point()+
  labs(
    title = "Lietuvių k.",
    x = "Vidutinis kalsės dydis",
    y = "Lietuvių k. VBE balas",
    color = "Mokykla"
  ) +
  geom_point(alpha = 0.7, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80", size = 0.5),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16))

pl_3.1<-ggplot(bendri_duomenys_galutiniai, aes(x=vid_klases_dydis, y=Biologija)) + 
  geom_point()+
  labs(
    title = "Biologija",
    x = "Vidutinis kalsės dydis",
    y = "Biologijos VBE balas",
    color = "Mokykla"
  ) +
  geom_point(alpha = 0.7, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80", size = 0.5),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16))

pl_4.1<-ggplot(bendri_duomenys_galutiniai, aes(x=vid_klases_dydis, y=`Užsienio kalba (anglų)`)) + 
  geom_point()+
  labs(
    title = "Anglų k.",
    x = "Vidutinis kalsės dydis",
    y = "Anglų k. VBE balas",
    color = "Mokykla"
  ) +
  geom_point(alpha = 0.7, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80", size = 0.5),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16))

pl_5.1<-ggplot(bendri_duomenys_galutiniai, aes(x=vid_klases_dydis, y=Geografija)) + 
  geom_point()+
  labs(
    title = "Geografija",
    x = "Vidutinis kalsės dydis",
    y = "Geografijos VBE balas",
    color = "Mokykla"
  ) +
  geom_point(alpha = 0.7, size = 1.2) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +  
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray80", size = 0.5),  
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16))

graf3.1<-((pl_1.1|pl_2.1)/(pl_3.1|pl_4.1|pl_5.1))+
  plot_annotation(
    title = "Egzaminų rezultatų ir vidutinio klasės dydžio sąsajos",
    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)))
print(graf3.1)

#koreliaciju matricos --------------------------------------------------------------------
koreliacijai <- bendri_duomenys_galutiniai %>%
  select(`Darbo uzmokestis`,BVP,Nedarbas,dalis_mok,dalis,vid_klases_dydis, krepselio_dydis, `Lietuvių kalba ir literatūra`, Matematika, 
         `Užsienio kalba (anglų)`, Geografija, Biologija)

cor_matrix <- cor(koreliacijai, use = "pairwise.complete.obs")

uzvard <- c("Atlyginimas", "BVP", "Nedarbas", "Mokytojų dalis \n (stažas>15m.)", "Mokinių skč. \n vienam mokyt.", 
                  "Vid. klasės dydis","Krepšelio dydis","Lietuvių k.", "Matematika", "Anglų k.", "Geografija", "Biologija")

rownames(cor_matrix) <- uzvard
colnames(cor_matrix) <- uzvard

nauja_tvarka <- c("Matematika", "Anglų k.", "Lietuvių k.", "Biologija", "Geografija",
                  "Atlyginimas", "BVP", "Nedarbas", 
                  "Mokytojų dalis \n (stažas>15m.)", "Mokinių skč. \n vienam mokyt.", 
                  "Vid. klasės dydis", "Krepšelio dydis")
cor_matrix <- cor_matrix[nauja_tvarka, nauja_tvarka]

ggcorrplot(cor_matrix, 
           lab = TRUE, 
           hc.order = TRUE,
           type = "lower", 
           colors = c("#B2182B", "white", "#2166AC"),  
           outline.color = "gray50",  
           tl.cex = 12,  
           lab_size = 5,  
           title = "Koreliacijų matrica",
           legend.title = "Koreliacijos \n koeficientas") +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5), 
    legend.position = "right", 
    legend.text = element_text(size = 12),  
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 14)
  )




p1 <- ggplot(bendri_duomenys_galutiniai, aes(x = `Lietuvių kalba ir literatūra`)) +
  geom_histogram(fill = "skyblue", color = "white", bins = 20) +
  labs(title = "Lietuvių kalbos ir literatūros rezultatai", x = "Balai", y = "Mokinių skaičius")

p2 <- ggplot(bendri_duomenys_galutiniai, aes(x = Matematika)) +
  geom_histogram(fill = "lightgreen", color = "white", bins = 20) +
  labs(title = "Matematikos rezultatai", x = "Balai", y = "Mokinių skaičius")

p3 <- ggplot(bendri_duomenys_galutiniai, aes(x = `Užsienio kalba (anglų)`)) +
  geom_histogram(fill = "lightcoral", color = "white", bins = 20) +
  labs(title = "Anglų kalbos rezultatai", x = "Balai", y = "Mokinių skaičius")

p4 <- ggplot(bendri_duomenys_galutiniai, aes(x = Geografija)) +
  geom_histogram(fill = "wheat", color = "white", bins = 20) +
  labs(title = "Geografijos rezultatai", x = "Balai", y = "Mokinių skaičius")

p5 <- ggplot(bendri_duomenys_galutiniai, aes(x = Biologija)) +
  geom_histogram(fill = "orchid", color = "white", bins = 20) +
  labs(title = "Biologijos rezultatai", x = "Balai", y = "Mokinių skaičius")

p6 <- ggplot(bendri_duomenys_galutiniai, aes(x = mokiniai)) +
  geom_histogram(fill = "gold", color = "white", bins = 20) +
  labs(title = "Mokinių skaičius", x = "Skaičius", y = "Dažnis")

p7 <- ggplot(bendri_duomenys_galutiniai, aes(x = klasiu_sk)) +
  geom_histogram(fill = "turquoise", color = "white", bins = 20) +
  labs(title = "Klasių skaičius", x = "Skaičius", y = "Dažnis")

(p1 | p2 | p3) / (p4 | p5 | p6) / p7


##########################################################
#Mokyklos palyginimas
##########################################################

palyginimas_mokyklos<-bendri_duomenys_galutiniai %>%
  mutate(mokykla = str_remove(mokykla, "\\s+gimnazija.*$"))%>%
  group_by(mokykla) %>%
  summarise(across(
    c(`Lietuvių kalba ir literatūra`, Matematika, `Užsienio kalba (anglų)`, Geografija, Biologija),
    ~ mean(.x, na.rm = TRUE),
    .names = "vid_{.col}"
  ))



bendras_stilius <- theme(
  plot.title = element_text(size = 17, face = "bold"),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 13),
)

top_mokyklos <- unique(c(
  palyginimas_mokyklos %>% arrange(desc(vid_Matematika)) %>% slice(1:10) %>% pull(mokykla),
  palyginimas_mokyklos %>% arrange(desc(`vid_Lietuvių kalba ir literatūra`)) %>% slice(1:10) %>% pull(mokykla),
  palyginimas_mokyklos %>% arrange(desc(`vid_Užsienio kalba (anglų)`)) %>% slice(1:10) %>% pull(mokykla),
  palyginimas_mokyklos %>% arrange(desc(vid_Geografija)) %>% slice(1:10) %>% pull(mokykla),
  palyginimas_mokyklos %>% arrange(desc(vid_Biologija)) %>% slice(1:10) %>% pull(mokykla)
))

library(RColorBrewer)

spalvos <- brewer.pal(n = max(12, length(top_mokyklos)), name = "Set3")
spalvų_vardai <- setNames(spalvos[1:length(top_mokyklos)], top_mokyklos)

scale_fill_manual(values = spalvų_vardai)


# 1. Matematika
p1 <- palyginimas_mokyklos %>%
  arrange(desc(vid_Matematika)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(mokykla, vid_Matematika), y = vid_Matematika, fill = mokykla)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = spalvų_vardai) +
  labs(title = "Top 10 mokyklų pagal \n matematikos vidurkį", x = "Mokykla", y = "Matematikos vidurkis") +
  bendras_stilius

# 2. Lietuvių kalba
p2 <- palyginimas_mokyklos %>%
  arrange(desc(`vid_Lietuvių kalba ir literatūra`)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(mokykla, `vid_Lietuvių kalba ir literatūra`), y = `vid_Lietuvių kalba ir literatūra`, fill = mokykla)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = spalvų_vardai) +
  labs(title = "Top 10 mokyklų pagal \n lietuvių kalbos vidurkį", x = "Mokykla", y = "Lietuvių kalbos vidurkis") +
  bendras_stilius

# 3. Anglų kalba
p3 <- palyginimas_mokyklos %>%
  arrange(desc(`vid_Užsienio kalba (anglų)`)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(mokykla, `vid_Užsienio kalba (anglų)`), y = `vid_Užsienio kalba (anglų)`, fill = mokykla)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = spalvų_vardai) +
  labs(title = "Top 10 mokyklų pagal \n anglų kalbos vidurkį", x = "Mokykla", y = "Anglų kalbos vidurkis") +
  bendras_stilius

# 4. Geografija
p4 <- palyginimas_mokyklos %>%
  arrange(desc(vid_Geografija)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(mokykla, vid_Geografija), y = vid_Geografija, fill = mokykla)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = spalvų_vardai) +
  labs(title = "Top 10 mokyklų pagal \n geografijos vidurkį", x = "Mokykla", y = "Geografijos vidurkis") +
  bendras_stilius

# 5. Biologija
p5 <- palyginimas_mokyklos %>%
  arrange(desc(vid_Biologija)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(mokykla, vid_Biologija), y = vid_Biologija, fill = mokykla)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = spalvų_vardai) +
  labs(title = "Top 10 mokyklų pagal \n biologijos vidurkį", x = "Mokykla", y = "Biologijos vidurkis") +
  bendras_stilius

# Sudėlioti viską į vieną vaizdą
(p1 | p2 | p3) 
(p4 | p5)


# KLASTERIZAVIMAS -------------------------------------------
klasterizavimui<-bendri_duomenys_galutiniai%>%
  group_by(mokykla)%>%
  summarise(LT=mean(`Lietuvių kalba ir literatūra`),
            MAT=mean(Matematika),
            GEO=mean(Geografija),
            BIO=mean(Biologija),
            ANGL=mean(`Užsienio kalba (anglų)`),
            dalis_mok=mean(dalis_mok),
            dalis=mean(dalis),
            krepselis=mean(krepselio_dydis),
            klases_dydis=mean(vid_klases_dydis))

#alkunės metodas
wss <- numeric(10)
for (k in 1:10) {
  wss[k] <- sum(kmeans(scale(klasterizavimui[,2:10]), centers = k, nstart = 10)$withinss)
}
data.frame(
  Klasteriai = 1:10,
  WSS = wss
)%>%
  ggplot(., aes(x = Klasteriai, y = WSS)) +
  geom_line(color = "#2C3E50", linewidth = 1) +
  geom_point(color = "#E74C3C", size = 3) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Alkūnės metodas: optimalaus klasterių skaičiaus nustatymas",
    x = "Klasterių skaičius",
    y = "Vidinė klasterių dispersija (WSS)"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )


kNNdistplot(klasterizavimui, k =  20)
abline(h = 0.3, lty = 2)



set.seed(123)  
kmeans_result <- kmeans(scale(klasterizavimui[,2:10]), centers =4) 
klasterizavimui$cluster <- as.factor(kmeans_result$cluster)

#klasterių statistika 
klasteriu_analize<-klasterizavimui%>%
  group_by(cluster)%>%
  summarise(LT=mean(LT),
            MAT=mean(MAT),
            GEO=mean(GEO),
            BIO=mean(BIO),
            ANGL=mean(ANGL),
            dalis_mok=mean(dalis_mok),
            dalis=mean(dalis),
            krepselis=mean(krepselis),
            klases_dydis=mean(klases_dydis))




#--------------------------------
m2019<-bendri_duomenys_galutiniai%>%filter(metai=="2019_2020")
m2020<-bendri_duomenys_galutiniai%>%filter(metai=="2020_2021")
m2021<-bendri_duomenys_galutiniai%>%filter(metai=="2021_2022")
m2022<-bendri_duomenys_galutiniai%>%filter(metai=="2022_2023")
m2023<-bendri_duomenys_galutiniai%>%filter(metai=="2023_2024")

skirstinio_patikrinimas<-function(lentele, metai){
  p1<-ggplot(lentele, aes(x=`Lietuvių kalba ir literatūra`))+
    geom_histogram(fill = "#66c2a5", color = "black", alpha = 0.7)+
    labs(y="Dažnis") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    )

  p2<-ggplot(lentele,aes(x=Matematika))+
   geom_histogram(fill = "#66c2a5", color = "black", alpha = 0.7)+
    labs(y="Dažnis")+
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    )

  p3<-ggplot(lentele, aes(x=Geografija))+
    geom_histogram(fill = "#66c2a5", color = "black", alpha = 0.7)+
    labs(y="Dažnis")+
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    )

  p4<-ggplot(lentele,aes(x=`Užsienio kalba (anglų)`))+
    geom_histogram(fill = "#66c2a5", color = "black", alpha = 0.7)+
    labs(y="Dažnis")+
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    )

  p5<-ggplot(lentele,aes(x=Biologija))+
    geom_histogram(fill = "#66c2a5", color = "black", alpha = 0.7)+
    labs(y="Dažnis")+
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12)
    )

  shapiro_results <- list()
  variables <- c("Lietuvių kalba ir literatūra", "Matematika", "Geografija", "Užsienio kalba (anglų)", "Biologija")
  
  for (var in variables) {
    test_result <- shapiro.test(lentele[[var]])  
    shapiro_results[[var]] <- test_result
  }
  
  cat("Shapiro-Wilk rezultatai ", metai, ":\n")
  for (var in names(shapiro_results)) {
    cat(paste0(var," p-reikšmė = ", round(shapiro_results[[var]]$p.value, 4)), "\n")
  }
  
  print((p1|p2)/(p3|p4|p5)+
          plot_annotation(
            title = paste0("Egzaminų rezultatų histogramos ",metai),
            theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))))
}

skirstinio_patikrinimas(m2019,"2019-2020")  #Matematika, geografija
skirstinio_patikrinimas(m2020,"2020-2021")  #Lietuviu, matematika
skirstinio_patikrinimas(m2021,"2021-2022")  #Lietuviu, matematika, anglu
skirstinio_patikrinimas(m2022,"2022-2023")  #Lietuviu, matematika, geografija
skirstinio_patikrinimas(m2023,"2023-2024")  #Matematika

#funkcija kuri nubraižo modelio liekanų standartizuotų, stjudentizuotų ir kuko atsumo reikšmes
isskirtys<-function(modelis){
  plt1<-ggplot(data = NULL, aes(x = seq_along(modelis$residuals), y = modelis$residuals)) +
    geom_point(color = "blue", shape = 16) +
    labs(
      title = "Liekamosios paklaidos pagal stebėjimus",
      x = "Indeksas",
      y = "Liekanos"
    ) +
    
    theme_classic() +
    theme(
      panel.grid.major = element_line(color = "gray", size = 0.5),  
      panel.grid = element_line(color = "gray", size = 0.5),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14), 
      plot.title = element_text(size = 18, hjust = 0.5),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12))
  
  
  plt2<-ggplot(data = NULL, aes(x = seq_along(cooks.distance(modelis)), y = cooks.distance(modelis))) +
    geom_point(color = "blue", shape = 16) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth=1) + 
    labs(
      title = "Cooks atsumo reikšmės pagal stebėjimus",
      x = "Indeksas",
      y = "Cook's atstumas"
    ) +
    theme_classic() +
    theme(
      panel.grid.major = element_line(color = "gray", size = 0.5),  
      panel.grid = element_line(color = "gray", size = 0.5),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14), 
      plot.title = element_text(size = 18, hjust = 0.5),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
  
  
  
  plt3<-ggplot(data = NULL, aes(x = seq_along(rstandard(modelis)), y = rstandard(modelis))) +
    geom_point(color = "blue", shape = 16) +
    geom_hline(yintercept = c(-3, 3), color = "red", linetype = "dashed", linewidth=1) + 
    labs(
      title = "Standartizuotos reikšmės pagal stebėjimus",
      x = "Indeksas",
      y = "Standartizuata reikšmė"
    ) +
    theme_classic() +
    theme(
      panel.grid.major = element_line(color = "gray", size = 0.5),  
      panel.grid = element_line(color = "gray", size = 0.5),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14), 
      plot.title = element_text(size = 18, hjust = 0.5),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
  
  
  
  plt4<-ggplot(data = NULL, aes(x = seq_along(rstudent(modelis)), y = rstudent(modelis))) +
    geom_point(color = "blue", shape = 16) +
    geom_hline(yintercept = c(-3, 3), color = "red", linetype = "dashed", linewidth=1) + 
    labs(
      title = "Stjudentizuotos reikšmės pagal stebėjimus",
      x = "Indeksas",
      y = "Stjudentizuota reikšmė"
    ) +
    theme_classic() +
    theme(
      panel.grid.major = element_line(color = "gray", linewidth = 0.5),  
      panel.grid = element_line(color = "gray", linewidth = 0.5),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14), 
      plot.title = element_text(size = 18, hjust = 0.5),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
  
  graf1 <- (plt1 | plt2) / (plt3 | plt4)
  print(graf1)
}

#funckija skirta nustatyti ar paklaidos homoskedastiškos, sklaidos diagrama tarp prognozuotų ir nustatytų reikšmių 
homoskedastiskumas<-function(model){
  plot_data <- data.frame(
    PritaikytosReikšmės = fitted(model),
    Likučiai = resid(model)
  )
  ggplot(plot_data, aes(x = PritaikytosReikšmės, y = Likučiai)) +
    geom_point(color = "steelblue") +
    geom_hline(yintercept = 0, color = "red") +
    labs(
      title = "Sklaidos diagrama",
      x = "Pritaikytos reikšmės (fitted values)",
      y = "Liekanos"
    ) +
    theme_classic(base_size = 14) +
    theme(
      panel.grid.major = element_line(color = "gray", linewidth = 0.5),  
      panel.grid = element_line(color = "gray", linewidth = 0.5),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14), 
      plot.title = element_text(size = 18, hjust = 0.5),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
}


####################################
#   REGRESIJOS
###################################

names(klasterizavimui)
names(bendri_duomenys_galutiniai)

bendri_duomenys_galutiniai_su_klasteriu <- bendri_duomenys_galutiniai %>%
  left_join(klasterizavimui[, c("mokykla", "cluster")], by = "mokykla") %>%
  rename(klasteris = cluster)


#Duomenų suskaidymas pagal atskirus klasterius 

# 2019–2020
m2019_klasteris1<-bendri_duomenys_galutiniai_su_klasteriu%>%filter(metai=="2019_2020"$ klasteris==1)
m2019_klasteris2<-bendri_duomenys_galutiniai_su_klasteriu%>%filter(metai=="2019_2020" & klasteris==2)
m2019_klasteris3<-bendri_duomenys_galutiniai_su_klasteriu%>%filter(metai=="2019_2020" & klasteris==3)
m2019_klasteris4<-bendri_duomenys_galutiniai_su_klasteriu%>%filter(metai=="2019_2020" & klasteris==4)


# 2020–2021
m2020_klasteris1 <- bendri_duomenys_galutiniai_su_klasteriu %>% filter(metai == "2020_2021" & klasteris == 1)
m2020_klasteris2 <- bendri_duomenys_galutiniai_su_klasteriu %>% filter(metai == "2020_2021" & klasteris == 2)
m2020_klasteris3 <- bendri_duomenys_galutiniai_su_klasteriu %>% filter(metai == "2020_2021" & klasteris == 3)
m2020_klasteris4 <- bendri_duomenys_galutiniai_su_klasteriu %>% filter(metai == "2020_2021" & klasteris == 4)

# 2021–2022
m2021_klasteris1 <- bendri_duomenys_galutiniai_su_klasteriu %>% filter(metai == "2021_2022" & klasteris == 1)
m2021_klasteris2 <- bendri_duomenys_galutiniai_su_klasteriu %>% filter(metai == "2021_2022" & klasteris == 2)
m2021_klasteris3 <- bendri_duomenys_galutiniai_su_klasteriu %>% filter(metai == "2021_2022" & klasteris == 3)
m2021_klasteris4 <- bendri_duomenys_galutiniai_su_klasteriu %>% filter(metai == "2021_2022" & klasteris == 4)

# 2022–2023
m2022_klasteris1 <- bendri_duomenys_galutiniai_su_klasteriu %>% filter(metai == "2022_2023" & klasteris == 1)
m2022_klasteris2 <- bendri_duomenys_galutiniai_su_klasteriu %>% filter(metai == "2022_2023" & klasteris == 2)
m2022_klasteris3 <- bendri_duomenys_galutiniai_su_klasteriu %>% filter(metai == "2022_2023" & klasteris == 3)
m2022_klasteris4 <- bendri_duomenys_galutiniai_su_klasteriu %>% filter(metai == "2022_2023" & klasteris == 4)

# 2023–2024
m2023_klasteris1 <- bendri_duomenys_galutiniai_su_klasteriu %>% filter(metai == "2023_2024" & klasteris == 1)
m2023_klasteris2 <- bendri_duomenys_galutiniai_su_klasteriu %>% filter(metai == "2023_2024" & klasteris == 2)
m2023_klasteris3 <- bendri_duomenys_galutiniai_su_klasteriu %>% filter(metai == "2023_2024" & klasteris == 3)
m2023_klasteris4 <- bendri_duomenys_galutiniai_su_klasteriu %>% filter(metai == "2023_2024" & klasteris == 4)


# 2019–2020
skirstinio_patikrinimas(m2019_klasteris1, "2019-2020 I-klasteris")
skirstinio_patikrinimas(m2019_klasteris2, "2019-2020 II-klasteris")
skirstinio_patikrinimas(m2019_klasteris3, "2019-2020 III-klasteris")  # geografija, biologija
skirstinio_patikrinimas(m2019_klasteris4, "2019-2020 IV-klasteris")

# 2020–2021
skirstinio_patikrinimas(m2020_klasteris1, "2020-2021 I-klasteris")
skirstinio_patikrinimas(m2020_klasteris2, "2020-2021 II-klasteris")
skirstinio_patikrinimas(m2020_klasteris3, "2020-2021 III-klasteris")#MATEMATIKA
skirstinio_patikrinimas(m2020_klasteris4, "2020-2021 IV-klasteris")

# 2021–2022
skirstinio_patikrinimas(m2021_klasteris1, "2021-2022 I-klasteris")
skirstinio_patikrinimas(m2021_klasteris2, "2021-2022 II-klasteris")#LIETUVIU
skirstinio_patikrinimas(m2021_klasteris3, "2021-2022 III-klasteris")#Lietuviu
skirstinio_patikrinimas(m2021_klasteris4, "2021-2022 IV-klasteris")

# 2022–2023
skirstinio_patikrinimas(m2022_klasteris1, "2022-2023 I-klasteris")
skirstinio_patikrinimas(m2022_klasteris2, "2022-2023 II-klasteris")
skirstinio_patikrinimas(m2022_klasteris3, "2022-2023 III-klasteris")#Lietuviu
skirstinio_patikrinimas(m2022_klasteris4, "2022-2023 IV-klasteris")

# 2023–2024
skirstinio_patikrinimas(m2023_klasteris1, "2023-2024 I-klasteris")
skirstinio_patikrinimas(m2023_klasteris2, "2023-2024 II-klasteris")#geografija
skirstinio_patikrinimas(m2023_klasteris3, "2023-2024 III-klasteris")
skirstinio_patikrinimas(m2023_klasteris4, "2023-2024 IV-klasteris")



#-------------------------------------
#Lietuviu kalba  2019-2020
#-------------------------------------
LT_2019_I<-lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija + Biologija+
              `Užsienio kalba (anglų)` +  dalis_mok + 
              dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
              vid_klases_dydis,
            data=m2019_klasteris1)
ggqqplot(LT_2019_I$residuals)

#H0: paklaidos turi normalųjį skirstinį
#HA: paklaidos neturi normalaus skirstinio

shapiro.test(LT_2019_I$residuals)  
#kagandi p-reikšmė>alhpa (0.05), nulinės hipotezės neatmetame, paklaidu normalumas TENKINAMAS

homoskedastiskumas(LT_2019_I)

#H0: paklaidų dispersijos lygios
#HA: paklaidos dispersijos skiriasi
bptest(LT_2019_I)
#kagandi p-reikšmė>alhpa (0.05), nulinės hipotezės neatmetame, dispersijos lygios

isskirtys(LT_2019_I)
vif(LT_2019_I)                #multikolinearumo tikrinimas
summary(LT_2019_I)            #tiesinio modelio rezultatai

LT_2019_I<-lm(`Lietuvių kalba ir literatūra` ~ Matematika +
                `Užsienio kalba (anglų)`,
              data=m2019_klasteris1)
summary(LT_2019_I)                                   
vif(LT_2019_I)                                      
print(coef(lm.beta(LT_2019_I),standardized=TRUE))   #modelio standartizuoti koeficientai 

####

LT_2019_III <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija + Biologija+
                    `Užsienio kalba (anglų)` +  dalis_mok + 
                    dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                    vid_klases_dydis,
                  data = m2019_klasteris3)
ggqqplot(LT_2019_III$residuals)
shapiro.test(LT_2019_III$residuals)
#paklaidu normalumas TENKINAMAS

homoskedastiskumas(LT_2019_III)
bptest(LT_2019_III)
#dispersijos nelygios

isskirtys(LT_2019_III)
vif(LT_2019_III)
summary(LT_2019_III)

LT_2019_III <- lm(`Lietuvių kalba ir literatūra` ~ Biologija+ `Darbo uzmokestis`,
                  data = m2019_klasteris3)
coeftest(LT_2019_III, vcov = vcovHC(LT_2019_III, type = "HC1"))
vif(LT_2019_III)
summary(LT_2019_III)
print(coef(lm.beta(LT_2019_III),standardized=TRUE))

####
LT_2019_IV <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija + Biologija+
                   `Užsienio kalba (anglų)` +  dalis_mok + 
                   dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                   vid_klases_dydis,
                 data = m2019_klasteris4)
ggqqplot(LT_2019_IV$residuals)
shapiro.test(LT_2019_IV$residuals)
#paklaidu normalumas TENKINAMAS

homoskedastiskumas(LT_2019_IV)
bptest(LT_2019_IV)
#dispersijos lygios

isskirtys(LT_2019_IV)
vif(LT_2019_IV)
summary(LT_2019_IV)


LT_2019_IV <- lm(`Lietuvių kalba ir literatūra` ~ Geografija + Biologija+
                   +  dalis_mok + 
                   + Nedarbas + 
                   vid_klases_dydis,
                 data = m2019_klasteris4)
summary(LT_2019_IV)
vif(LT_2019_IV)
print(coef(lm.beta(LT_2019_IV),standardized=TRUE))

#-------------------------------------
#Lietuviu kalba  2020-2021
#-------------------------------------
LT_2020_I <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija + Biologija+
                  `Užsienio kalba (anglų)` +  dalis_mok + 
                  dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                  vid_klases_dydis,
                data = m2020_klasteris1)
ggqqplot(LT_2020_I$residuals)
shapiro.test(LT_2020_I$residuals)
#paklaidu normalumas tenkinamas

homoskedastiskumas(LT_2020_I)
bptest(LT_2020_I)
#disperijso lygios

isskirtys(LT_2020_I)
vif(LT_2020_I)
summary(LT_2020_I)

LT_2020_I <- lm(`Lietuvių kalba ir literatūra` ~`Užsienio kalba (anglų)` + `Darbo uzmokestis`,
                data = m2020_klasteris1)
summary(LT_2020_I)
vif(LT_2020_I)
print(coef(lm.beta(LT_2020_I),standardized=TRUE))


##

LT_2020_III <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija + Biologija+
                    `Užsienio kalba (anglų)` +  dalis_mok + 
                    dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                    vid_klases_dydis,
                  data = m2020_klasteris3)
ggqqplot(LT_2020_III$residuals)
shapiro.test(LT_2020_III$residuals)
#normalumas tenkinamas

homoskedastiskumas(LT_2020_III)
bptest(LT_2020_III)
#disperijos lygios

isskirtys(LT_2020_III)
vif(LT_2020_III)
summary(LT_2020_III)

LT_2020_III <- lm(`Lietuvių kalba ir literatūra` ~ Matematika +
                    `Užsienio kalba (anglų)` +  dalis_mok,
                  data = m2020_klasteris3)
summary(LT_2020_III)
vif(LT_2020_III) 
print(coef(lm.beta(LT_2020_III),standardized=TRUE))

####

LT_2020_IV <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija + Biologija+
                   `Užsienio kalba (anglų)` +  dalis_mok + 
                   dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                   vid_klases_dydis,
                 data = m2020_klasteris4)
ggqqplot(LT_2020_IV$residuals)
shapiro.test(LT_2020_IV$residuals)
#paklaidu normalumas tenkinamas

homoskedastiskumas(LT_2020_IV)
bptest(LT_2020_IV)
#dispersijos lygios

vif(LT_2020_IV)
summary(LT_2020_IV)
isskirtys(LT_2020_IV)

LT_2020_IV <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + vid_klases_dydis,
                 data = m2020_klasteris4)
summary(LT_2020_IV)
vif(LT_2020_IV) 
print(coef(lm.beta(LT_2020_IV),standardized=TRUE))

#-------------------------------------
#Lietuviu kalba  2021-2022
#-------------------------------------
LT_2021_I <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija + Biologija+
                  `Užsienio kalba (anglų)` +  dalis_mok + 
                  dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                  vid_klases_dydis,
                data = m2021_klasteris1)
ggqqplot(LT_2021_I$residuals)
shapiro.test(LT_2021_I$residuals)
#paklaidos normalios

homoskedastiskumas(LT_2021_I)
bptest(LT_2021_I)
#dispersijos lygios

isskirtys(LT_2021_I)
vif(LT_2021_I)
summary(LT_2021_I)

LT_2021_I <- lm(`Lietuvių kalba ir literatūra` ~ Biologija + vid_klases_dydis,
                data = m2021_klasteris1)
summary(LT_2021_I)
vif(LT_2021_I)
print(coef(lm.beta(LT_2021_I),standardized=TRUE))

###

LT_2021_III <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija + Biologija+
                    `Užsienio kalba (anglų)` +  dalis_mok + 
                    dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                    vid_klases_dydis,
                  data = m2021_klasteris3)
ggqqplot(LT_2021_III$residuals)
shapiro.test(LT_2021_III$residuals)
#paklaidos Normaliosios

homoskedastiskumas(LT_2021_III)
bptest(LT_2021_III)
#dispersijos lygios

isskirtys(LT_2021_III)
vif(LT_2021_III)

summary(LT_2021_III)

LT_2021_III <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija + Biologija,
                  data = m2021_klasteris3)
summary(LT_2021_III)
vif(LT_2021_III)
print(coef(lm.beta(LT_2021_III),standardized=TRUE))

###

LT_2021_IV <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija + Biologija+
                    `Užsienio kalba (anglų)` +  dalis_mok + 
                    dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                    vid_klases_dydis,
                  data = m2021_klasteris4)
ggqqplot(LT_2021_IV$residuals)
shapiro.test(LT_2021_IV$residuals)
#paklaidos Normaliosios

homoskedastiskumas(LT_2021_IV)
bptest(LT_2021_IV)
#dispersijos lygios

isskirtys(LT_2021_IV)
vif(LT_2021_IV)

summary(LT_2021_IV)

LT_2021_IV <- lm(`Lietuvių kalba ir literatūra` ~ Matematika  + vid_klases_dydis,
                 data = m2021_klasteris4)
summary(LT_2021_IV)
vif(LT_2021_IV)
print(coef(lm.beta(LT_2021_IV),standardized=TRUE))

#-------------------------------------
#Lietuviu kalba  2022-2023
#-------------------------------------
LT_2022_I <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija + Biologija+
                  `Užsienio kalba (anglų)` +  dalis_mok + 
                  dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                  vid_klases_dydis,
                data = m2022_klasteris1)
ggqqplot(LT_2022_I$residuals)
shapiro.test(LT_2022_I$residuals)
#paklaidos normaliosios

homoskedastiskumas(LT_2022_I)
bptest(LT_2022_I)
#dispersijos lygios

isskirtys(LT_2022_I)
vif(LT_2022_I)
summary(LT_2022_I)

LT_2022_I <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + `Darbo uzmokestis`,
                data = m2022_klasteris1)
summary(LT_2022_I)
vif(LT_2022_I)
print(coef(lm.beta(LT_2022_I),standardized=TRUE))

###
LT_2022_III <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija + Biologija+
                    `Užsienio kalba (anglų)` +  dalis_mok + 
                    dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                    vid_klases_dydis,
                  data = m2022_klasteris3)
ggqqplot(LT_2022_III$residuals)
shapiro.test(LT_2022_III$residuals)
#paklaidos normaliosios

homoskedastiskumas(LT_2022_III)
bptest(LT_2022_III)
#dispersijos nera lygios

isskirtys(LT_2022_III)
vif(LT_2022_III)
summary(LT_2022_III)

LT_2022_III <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija+
                    `Užsienio kalba (anglų)` + `Darbo uzmokestis`,
                  data = m2022_klasteris3)
coeftest(LT_2022_III, vcov = vcovHC(LT_2022_III, type = "HC1"))
vif(LT_2022_III)
print(coef(lm.beta(LT_2022_III),standardized=TRUE))
summary(LT_2022_III)
###

LT_2022_IV <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija + Biologija+
                   `Užsienio kalba (anglų)` +  dalis_mok + 
                   dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                   vid_klases_dydis,
                 data = m2022_klasteris4)
ggqqplot(LT_2022_IV$residuals)
shapiro.test(LT_2022_IV$residuals)
#paklaidos normaliosios

homoskedastiskumas(LT_2022_IV)
bptest(LT_2022_IV)
#dispersijos lygios

isskirtys(LT_2022_IV)
vif(LT_2022_IV)
summary(LT_2022_IV)

LT_2022_IV <- lm(`Lietuvių kalba ir literatūra` ~`Užsienio kalba (anglų)` + Nedarbas,
                 data = m2022_klasteris4)
summary(LT_2022_IV)
print(coef(lm.beta(LT_2022_IV),standardized=TRUE))

#-------------------------------------
#Lietuviu kalba  2023-2024
#-------------------------------------

LT_2023_I <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija + Biologija+
                  `Užsienio kalba (anglų)` +  dalis_mok + 
                  dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                  vid_klases_dydis,
                data = m2023_klasteris1)
ggqqplot(LT_2023_I$residuals)
shapiro.test(LT_2023_I$residuals)
#paklaidos normaliosios

homoskedastiskumas(LT_2023_I)
bptest(LT_2023_I)
#dispersijos lygios

isskirtys(LT_2023_I)
vif(LT_2023_I)

summary(LT_2023_I)

LT_2023_I <- lm(`Lietuvių kalba ir literatūra` ~ `Užsienio kalba (anglų)`,
                data = m2023_klasteris1)
summary(LT_2023_I)
print(coef(lm.beta(LT_2023_I),standardized=TRUE))

###

LT_2023_III <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija + Biologija+
                    `Užsienio kalba (anglų)` +  dalis_mok + 
                    dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                    vid_klases_dydis,
                  data = m2023_klasteris3)
ggqqplot(LT_2023_III$residuals)
shapiro.test(LT_2023_III$residuals)
#paklaidos normalios

homoskedastiskumas(LT_2023_III)
bptest(LT_2023_III)
#dispersijos lygios

isskirtys(LT_2023_III)
vif(LT_2023_III)
summary(LT_2023_III)

LT_2023_III <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Biologija+
                    `Užsienio kalba (anglų)` +  dalis_mok,
                  data = m2023_klasteris3)
summary(LT_2023_III)
print(coef(lm.beta(LT_2023_III),standardized=TRUE))

####

LT_2023_IV <- lm(`Lietuvių kalba ir literatūra` ~ Matematika + Geografija + Biologija+
                   `Užsienio kalba (anglų)` +  dalis_mok + 
                   dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                   vid_klases_dydis,
                 data = m2023_klasteris4)
ggqqplot(LT_2023_IV$residuals)
shapiro.test(LT_2023_IV$residuals)
#paklaidos normalios

homoskedastiskumas(LT_2023_IV)
bptest(LT_2023_IV)
#dispersijos lygios

isskirtys(LT_2023_IV)

vif(LT_2023_IV)
summary(LT_2023_IV)

LT_2023_IV <- lm(`Lietuvių kalba ir literatūra` ~ Geografija + Biologija + vid_klases_dydis,
                 data = m2023_klasteris4)
summary(LT_2023_IV)
print(coef(lm.beta(LT_2023_IV),standardized=TRUE))


#-------------------------------------
# Matematika  2019-2020
#-------------------------------------
Matematika_2019_I <- lm(Matematika ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija+
                          `Užsienio kalba (anglų)` +  dalis_mok + 
                          dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                          vid_klases_dydis,
                        data = m2019_klasteris1)
ggqqplot(Matematika_2019_I$residuals)
shapiro.test(Matematika_2019_I$residuals)
#paklaidos noamrlios

homoskedastiskumas(Matematika_2019_I)
bptest(Matematika_2019_I)
#dispersijos lygios

isskirtys(Matematika_2019_I)
vif(Matematika_2019_I)
summary(Matematika_2019_I)

Matematika_2019_I <- lm(Matematika ~ `Lietuvių kalba ir literatūra`,
                        data = m2019_klasteris1)
summary(Matematika_2019_I)
vif(Matematika_2019_I)
print(coef(lm.beta(Matematika_2019_I),standardized=TRUE))

####
Matematika_2019_III <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija+
                            `Užsienio kalba (anglų)` +  dalis_mok + 
                            dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                            vid_klases_dydis,
                          data = m2019_klasteris3)
ggqqplot(Matematika_2019_III$residuals)
shapiro.test(Matematika_2019_III$residuals)
#paklaidos normalios

homoskedastiskumas(Matematika_2019_III)
bptest(Matematika_2019_III)
#dispersijos lygios

isskirtys(Matematika_2019_III)
vif(Matematika_2019_III)
summary(Matematika_2019_III)

Matematika_2019_III <- lm(`Matematika` ~ Geografija +
                            `Užsienio kalba (anglų)`  + Nedarbas,
                          data = m2019_klasteris3)
summary(Matematika_2019_III)
vif(Matematika_2019_III)
print(coef(lm.beta(Matematika_2019_III),standardized=TRUE))

####

Matematika_2019_IV <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija+
                           `Užsienio kalba (anglų)` +  dalis_mok + 
                           dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                           vid_klases_dydis,
                         data = m2019_klasteris4)
ggqqplot(Matematika_2019_IV$residuals)
shapiro.test(Matematika_2019_IV$residuals)
#paklaidos normalios

homoskedastiskumas(Matematika_2019_IV)
bptest(Matematika_2019_IV)
#dispersijos lygios

isskirtys(Matematika_2019_IV)
vif(Matematika_2019_IV)
summary(Matematika_2019_IV)

Matematika_2019_IV <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` +
                           `Užsienio kalba (anglų)` +  dalis_mok + 
                           dalis + krepselio_dydis  + Nedarbas,
                         data = m2019_klasteris4)

summary(Matematika_2019_IV)
vif(Matematika_2019_IV)
print(coef(lm.beta(Matematika_2019_IV),standardized=TRUE))

#-------------------------------------
#Matematika  2020-2021
#-------------------------------------
Matematika_2020_I <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija+
                          `Užsienio kalba (anglų)` +  dalis_mok + 
                          dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                          vid_klases_dydis,
                        data = m2020_klasteris1)
ggqqplot(Matematika_2020_I$residuals)
shapiro.test(Matematika_2020_I$residuals)
#paklaidos normalios

homoskedastiskumas(Matematika_2020_I)
bptest(Matematika_2020_I)
#dispersijos lygios

isskirtys(Matematika_2020_I)
vif(Matematika_2020_I)
summary(Matematika_2020_I)

Matematika_2020_I <- lm(`Matematika` ~ `Užsienio kalba (anglų)`,
                        data = m2020_klasteris1)
summary(Matematika_2020_I)
vif(Matematika_2020_I)
print(coef(lm.beta(Matematika_2020_I),standardized=TRUE))

####

Matematika_2020_III <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija+
                            `Užsienio kalba (anglų)` +  dalis_mok + 
                            dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                            vid_klases_dydis,
                          data = m2020_klasteris3)
ggqqplot(Matematika_2020_III$residuals)
shapiro.test(Matematika_2020_III$residuals)
#paklaidos normalios

homoskedastiskumas(Matematika_2020_III)
bptest(Matematika_2020_III)
#dispersijos lygios

isskirtys(Matematika_2020_III)
vif(Matematika_2020_III)
summary(Matematika_2020_III)

Matematika_2020_III <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Biologija+`Užsienio kalba (anglų)`,
                          data = m2020_klasteris3)
summary(Matematika_2020_III)
vif(Matematika_2020_III)
print(coef(lm.beta(Matematika_2020_III),standardized=TRUE))

####
Matematika_2020_IV <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija+
                           `Užsienio kalba (anglų)` +  dalis_mok + 
                           dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                           vid_klases_dydis,
                         data = m2020_klasteris4)
ggqqplot(Matematika_2020_IV$residuals)
shapiro.test(Matematika_2020_IV$residuals)
#paklaidos normalios

homoskedastiskumas(Matematika_2020_IV)
bptest(Matematika_2020_IV)
#dispersijos lygios

isskirtys(Matematika_2020_IV)
vif(Matematika_2020_IV)
summary(Matematika_2020_IV)

Matematika_2020_IV <- lm(`Matematika` ~ Biologija + krepselio_dydis,
                         data = m2020_klasteris4)
summary(Matematika_2020_IV)
vif(Matematika_2020_IV)
print(coef(lm.beta(Matematika_2020_IV),standardized=TRUE))

#-------------------------------------
#Matematika  2021-2022
#-------------------------------------

Matematika_2021_I <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija+
                          `Užsienio kalba (anglų)` +  dalis_mok + 
                          dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                          vid_klases_dydis,
                        data = m2021_klasteris1)
ggqqplot(Matematika_2021_I$residuals)
shapiro.test(Matematika_2021_I$residuals)
#paklaidos normalios

homoskedastiskumas(Matematika_2021_I)
bptest(Matematika_2021_I)
#dispersijos lygios

isskirtys(Matematika_2021_I)
vif(Matematika_2021_I)
summary(Matematika_2021_I)

Matematika_2021_I <- lm(`Matematika` ~ Geografija +  dalis_mok + BVP + vid_klases_dydis,
                        data = m2021_klasteris1)
summary(Matematika_2021_I)
vif(Matematika_2021_I)
print(coef(lm.beta(Matematika_2021_I),standardized=TRUE))

####

Matematika_2021_III <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija+
                            `Užsienio kalba (anglų)` +  dalis_mok + 
                            dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                            vid_klases_dydis,
                          data = m2021_klasteris3)
ggqqplot(Matematika_2021_III$residuals)
shapiro.test(Matematika_2021_III$residuals)
#paklaidos normalios

homoskedastiskumas(Matematika_2021_III)
bptest(Matematika_2021_III)
#dispersijos lygios

isskirtys(Matematika_2021_III)
vif(Matematika_2021_III)
summary(Matematika_2021_III)

Matematika_2021_III <- lm(`Matematika` ~ Biologija + `Užsienio kalba (anglų)` + BVP,
                          data = m2021_klasteris3)
summary(Matematika_2021_III)
vif(Matematika_2021_III)
print(coef(lm.beta(Matematika_2021_III),standardized=TRUE))

####
Matematika_2021_IV <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija+
                           `Užsienio kalba (anglų)` +  dalis_mok + 
                           dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                           vid_klases_dydis,
                         data = m2021_klasteris4)
ggqqplot(Matematika_2021_IV$residuals)
shapiro.test(Matematika_2021_IV$residuals)
#paklaidos normalios

homoskedastiskumas(Matematika_2021_IV)
bptest(Matematika_2021_IV)
#dispersijos lygios

isskirtys(Matematika_2021_IV)
vif(Matematika_2021_IV)
summary(Matematika_2021_IV)

Matematika_2021_IV <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra`,
                         data = m2021_klasteris4)
summary(Matematika_2021_IV)
vif(Matematika_2021_IV)
print(coef(lm.beta(Matematika_2021_IV),standardized=TRUE))

#-------------------------------------
#Matematika  2022-2023
#-------------------------------------

Matematika_2022_I <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija+
                          `Užsienio kalba (anglų)` +  dalis_mok + 
                          dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                          vid_klases_dydis,
                        data = m2022_klasteris1)
ggqqplot(Matematika_2022_I$residuals)
shapiro.test(Matematika_2022_I$residuals)
#paklaidos normaliosios

homoskedastiskumas(Matematika_2022_I)
bptest(Matematika_2022_I)
#dispersijos lygios

isskirtys(Matematika_2022_I)
vif(Matematika_2022_I)
summary(Matematika_2022_I)

Matematika_2022_I <- lm(`Matematika` ~
                          `Užsienio kalba (anglų)` + BVP + 
                          vid_klases_dydis,
                        data = m2022_klasteris1)
summary(Matematika_2022_I)
vif(Matematika_2022_I)
print(coef(lm.beta(Matematika_2022_I),standardized=TRUE))

####

Matematika_2022_III <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija+
                            `Užsienio kalba (anglų)` +  dalis_mok + 
                            dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                            vid_klases_dydis,
                          data = m2022_klasteris3)
ggqqplot(Matematika_2022_III$residuals)
shapiro.test(Matematika_2022_III$residuals)
#paklaidos normalios

homoskedastiskumas(Matematika_2022_III)
bptest(Matematika_2022_III)
#dispersijos lygios

isskirtys(Matematika_2022_III)
vif(Matematika_2022_III)
summary(Matematika_2022_III)

Matematika_2022_III <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Biologija + krepselio_dydis,
                          data = m2022_klasteris3)
summary(Matematika_2022_III)
vif(Matematika_2022_III)
print(coef(lm.beta(Matematika_2022_III),standardized=TRUE))

####

Matematika_2022_IV <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija+
                           `Užsienio kalba (anglų)` +  dalis_mok + 
                           dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                           vid_klases_dydis,
                         data = m2022_klasteris4)
ggqqplot(Matematika_2022_IV$residuals)
shapiro.test(Matematika_2022_IV$residuals)
#paklaidos normalios

homoskedastiskumas(Matematika_2022_IV)
bptest(Matematika_2022_IV)
#dispersijos lygios

isskirtys(Matematika_2022_IV)
vif(Matematika_2022_IV)
summary(Matematika_2022_IV)

Matematika_2022_IV <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Biologija+  dalis_mok  + 
                           vid_klases_dydis,
                         data = m2022_klasteris4)
summary(Matematika_2022_IV)
vif(Matematika_2022_IV)
print(coef(lm.beta(Matematika_2022_IV),standardized=TRUE))

#-------------------------------------
#Matematika  2023-2024
#-------------------------------------

Matematika_2023_I <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija+
                          `Užsienio kalba (anglų)` +  dalis_mok + 
                          dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                          vid_klases_dydis,
                        data = m2023_klasteris1)
ggqqplot(Matematika_2023_I$residuals)
shapiro.test(Matematika_2023_I$residuals)
#paklaidos normaliosios

homoskedastiskumas(Matematika_2023_I)
bptest(Matematika_2023_I)
#dispersijos lygios

isskirtys(Matematika_2023_I)
vif(Matematika_2023_I)
summary(Matematika_2023_I)

Matematika_2023_I <- lm(`Matematika` ~Biologija+
                          `Užsienio kalba (anglų)`,
                        data = m2023_klasteris1)
summary(Matematika_2023_I)
vif(Matematika_2023_I)
print(coef(lm.beta(Matematika_2023_I),standardized=TRUE))

####

Matematika_2023_III <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija+
                            `Užsienio kalba (anglų)` +  dalis_mok + 
                            dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                            vid_klases_dydis,
                          data = m2023_klasteris3)
ggqqplot(Matematika_2023_III$residuals)
shapiro.test(Matematika_2023_III$residuals)
#paklaidos normaliosios

homoskedastiskumas(Matematika_2023_III)
bptest(Matematika_2023_III)
#dispersijos nelygios

isskirtys(Matematika_2023_III)
vif(Matematika_2023_III)
summary(Matematika_2023_III)

Matematika_2023_III <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra`,
                          data = m2023_klasteris3)
summary(Matematika_2023_III)
coeftest(Matematika_2023_III, vcov = vcovHC(Matematika_2023_III, type = "HC1"))
print(lm.beta(Matematika_2023_III))

####

Matematika_2023_IV <- lm(`Matematika` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija+
                           `Užsienio kalba (anglų)` +  dalis_mok + 
                           dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                           vid_klases_dydis,
                         data = m2023_klasteris4)
ggqqplot(Matematika_2023_IV$residuals)
shapiro.test(Matematika_2023_IV$residuals)
#paklaidos normalios

homoskedastiskumas(Matematika_2023_IV)
bptest(Matematika_2023_IV)
#dispersijos lygios

isskirtys(Matematika_2023_IV)
vif(Matematika_2023_IV)
summary(Matematika_2023_IV)

Matematika_2023_IV <- lm(`Matematika` ~ `Užsienio kalba (anglų)`,
                         data = m2023_klasteris4)
summary(Matematika_2023_IV)
vif(Matematika_2023_IV)
print(coef(lm.beta(Matematika_2023_IV),standardized=TRUE))

#-------------------------------------
#Užsienio kalba (anglų) 2019–2020
#-------------------------------------
Uzsienio_kalba_anglu_2019_I <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija +
                                    `Matematika` +  dalis_mok + 
                                    dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                                    vid_klases_dydis,
                                  data = m2019_klasteris1)
ggqqplot(Uzsienio_kalba_anglu_2019_I$residuals)
shapiro.test(Uzsienio_kalba_anglu_2019_I$residuals)
#poaklaidos normalios

homoskedastiskumas(Uzsienio_kalba_anglu_2019_I)
bptest(Uzsienio_kalba_anglu_2019_I)
#dispersijos nelygios

isskirtys(Uzsienio_kalba_anglu_2019_I)
vif(Uzsienio_kalba_anglu_2019_I)
summary(Uzsienio_kalba_anglu_2019_I)

Uzsienio_kalba_anglu_2019_I <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` +  dalis_mok,
                                  data = m2019_klasteris1)
coeftest(Uzsienio_kalba_anglu_2019_I, vcov = vcovHC(Uzsienio_kalba_anglu_2019_I, type = "HC1"))
summary(Uzsienio_kalba_anglu_2019_I)$r.squared_adj
vif(Uzsienio_kalba_anglu_2019_I)
print(coef(lm.beta(Uzsienio_kalba_anglu_2019_I),standardized=TRUE))

####

Uzsienio_kalba_anglu_2019_III <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija +
                                      `Matematika` +  dalis_mok + 
                                      dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                                      vid_klases_dydis,
                                    data = m2019_klasteris3)
ggqqplot(Uzsienio_kalba_anglu_2019_III$residuals)
shapiro.test(Uzsienio_kalba_anglu_2019_III$residuals)
#paklaidos normalios

homoskedastiskumas(Uzsienio_kalba_anglu_2019_III)
bptest(Uzsienio_kalba_anglu_2019_III)
#dispersijos lygios

isskirtys(Uzsienio_kalba_anglu_2019_III)
vif(Uzsienio_kalba_anglu_2019_III)
summary(Uzsienio_kalba_anglu_2019_III)

Uzsienio_kalba_anglu_2019_III <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Biologija +
                                      `Matematika` + BVP,
                                    data = m2019_klasteris3)
summary(Uzsienio_kalba_anglu_2019_III)
vif(Uzsienio_kalba_anglu_2019_III)
print(coef(lm.beta(Uzsienio_kalba_anglu_2019_III),standardized=TRUE))

####

Uzsienio_kalba_anglu_2019_IV <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija +
                                     `Matematika` +  dalis_mok + 
                                     dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                                     vid_klases_dydis,
                                   data = m2019_klasteris4)
ggqqplot(Uzsienio_kalba_anglu_2019_IV$residuals)
shapiro.test(Uzsienio_kalba_anglu_2019_IV$residuals)
#paklaidos normalios

homoskedastiskumas(Uzsienio_kalba_anglu_2019_IV)
bptest(Uzsienio_kalba_anglu_2019_IV)
#dispersijos lygios

isskirtys(Uzsienio_kalba_anglu_2019_IV)
vif(Uzsienio_kalba_anglu_2019_IV)
summary(Uzsienio_kalba_anglu_2019_IV)

Uzsienio_kalba_anglu_2019_IV <- lm(`Užsienio kalba (anglų)` ~`Matematika`  + Nedarbas,
                                   data = m2019_klasteris4)
summary(Uzsienio_kalba_anglu_2019_IV)
vif(Uzsienio_kalba_anglu_2019_IV)
print(coef(lm.beta(Uzsienio_kalba_anglu_2019_IV),standardized=TRUE))

#-------------------------------------
#Užsienio kalba (anglų)   2020-2021
#-------------------------------------

Uzsienio_kalba_anglu_2020_I <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija +
                                    `Matematika` +  dalis_mok + 
                                    dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                                    vid_klases_dydis,
                                  data = m2020_klasteris1)
ggqqplot(Uzsienio_kalba_anglu_2020_I$residuals)
shapiro.test(Uzsienio_kalba_anglu_2020_I$residuals)
#paklaidos normalios

homoskedastiskumas(Uzsienio_kalba_anglu_2020_I)
bptest(Uzsienio_kalba_anglu_2020_I)
#dispersijos lygios

isskirtys(Uzsienio_kalba_anglu_2020_I)
vif(Uzsienio_kalba_anglu_2020_I)
summary(Uzsienio_kalba_anglu_2020_I)

Uzsienio_kalba_anglu_2020_I <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + `Darbo uzmokestis`,
                                  data = m2020_klasteris1)
summary(Uzsienio_kalba_anglu_2020_I)
vif(Uzsienio_kalba_anglu_2020_I)
print(coef(lm.beta(Uzsienio_kalba_anglu_2020_I),standardized=TRUE))

####

Uzsienio_kalba_anglu_2020_III <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija +
                                      `Matematika` +  dalis_mok + 
                                      dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                                      vid_klases_dydis,
                                    data = m2020_klasteris3)
ggqqplot(Uzsienio_kalba_anglu_2020_III$residuals)
shapiro.test(Uzsienio_kalba_anglu_2020_III$residuals)
#paklaidos normalios

homoskedastiskumas(Uzsienio_kalba_anglu_2020_III)
bptest(Uzsienio_kalba_anglu_2020_III)
#dispersijos lygios

isskirtys(Uzsienio_kalba_anglu_2020_III)
vif(Uzsienio_kalba_anglu_2020_III)
summary(Uzsienio_kalba_anglu_2020_III)

Uzsienio_kalba_anglu_2020_III <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija +
                                    dalis_mok + BVP,
                                    data = m2020_klasteris3)
summary(Uzsienio_kalba_anglu_2020_III)
vif(Uzsienio_kalba_anglu_2020_III)
print(coef(lm.beta(Uzsienio_kalba_anglu_2020_III),standardized=TRUE))

####

Uzsienio_kalba_anglu_2020_IV <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija +
                                     `Matematika` +  dalis_mok + 
                                     dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                                     vid_klases_dydis,
                                   data = m2020_klasteris4)
ggqqplot(Uzsienio_kalba_anglu_2020_IV$residuals)
shapiro.test(Uzsienio_kalba_anglu_2020_IV$residuals)
#paklaidos normalios

homoskedastiskumas(Uzsienio_kalba_anglu_2020_IV)
bptest(Uzsienio_kalba_anglu_2020_IV)
#dispersijos lygios

isskirtys(Uzsienio_kalba_anglu_2020_IV)
vif(Uzsienio_kalba_anglu_2020_IV)
summary(Uzsienio_kalba_anglu_2020_IV)

Uzsienio_kalba_anglu_2020_IV <- lm(`Užsienio kalba (anglų)` ~ `Matematika` + Nedarbas,
                                   data = m2020_klasteris4)
summary(Uzsienio_kalba_anglu_2020_IV)
vif(Uzsienio_kalba_anglu_2020_IV)
print(coef(lm.beta(Uzsienio_kalba_anglu_2020_IV),standardized=TRUE))

#-------------------------------------
#Užsienio kalba (anglų)   2021-2022
#-------------------------------------

Uzsienio_kalba_anglu_2021_I <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija +
                                    `Matematika` +  dalis_mok + 
                                    dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                                    vid_klases_dydis,
                                  data = m2021_klasteris1)
ggqqplot(Uzsienio_kalba_anglu_2021_I$residuals)
shapiro.test(Uzsienio_kalba_anglu_2021_I$residuals)
#paklaidos normalios

homoskedastiskumas(Uzsienio_kalba_anglu_2021_I)
bptest(Uzsienio_kalba_anglu_2021_I)
#dispersijos lygios

isskirtys(Uzsienio_kalba_anglu_2021_I)
vif(Uzsienio_kalba_anglu_2021_I)
summary(Uzsienio_kalba_anglu_2021_I)

Uzsienio_kalba_anglu_2021_I <- lm(`Užsienio kalba (anglų)` ~ `Matematika` +  dalis_mok,
                                  data = m2021_klasteris1)
summary(Uzsienio_kalba_anglu_2021_I)
vif(Uzsienio_kalba_anglu_2021_I)
print(coef(lm.beta(Uzsienio_kalba_anglu_2021_I),standardized=TRUE))

####

Uzsienio_kalba_anglu_2021_III <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija +
                                      `Matematika` +  dalis_mok + 
                                      dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                                      vid_klases_dydis,
                                    data = m2021_klasteris3)
ggqqplot(Uzsienio_kalba_anglu_2021_III$residuals)
shapiro.test(Uzsienio_kalba_anglu_2021_III$residuals)
#paklaidos normalios

homoskedastiskumas(Uzsienio_kalba_anglu_2021_III)
bptest(Uzsienio_kalba_anglu_2021_III)
#dispersijos nera lygios

isskirtys(Uzsienio_kalba_anglu_2021_III)
vif(Uzsienio_kalba_anglu_2021_III)

Uzsienio_kalba_anglu_2021_III <- lm(`Užsienio kalba (anglų)` ~ Geografija +
                                      `Matematika` +  dalis_mok + BVP,
                                    data = m2021_klasteris3)
coeftest(Uzsienio_kalba_anglu_2021_III, vcov = vcovHC(Uzsienio_kalba_anglu_2021_III, type = "HC1"))
summary(Uzsienio_kalba_anglu_2021_III)$r.squared
print(coef(lm.beta(Uzsienio_kalba_anglu_2021_III),standardized=TRUE))


####

Uzsienio_kalba_anglu_2021_IV <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija +
                                     `Matematika` +  dalis_mok + 
                                     dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                                     vid_klases_dydis,
                                   data = m2021_klasteris4)
ggqqplot(Uzsienio_kalba_anglu_2021_IV$residuals)
shapiro.test(Uzsienio_kalba_anglu_2021_IV$residuals)
#paklaidos normalios

homoskedastiskumas(Uzsienio_kalba_anglu_2021_IV)
bptest(Uzsienio_kalba_anglu_2021_IV)
#dispersijos lygios

isskirtys(Uzsienio_kalba_anglu_2021_IV)
vif(Uzsienio_kalba_anglu_2021_IV)
summary(Uzsienio_kalba_anglu_2021_IV)

Uzsienio_kalba_anglu_2021_IV <- lm(`Užsienio kalba (anglų)` ~ Geografija,
                                   data = m2021_klasteris4)
summary(Uzsienio_kalba_anglu_2021_IV)
vif(Uzsienio_kalba_anglu_2021_IV)
print(coef(lm.beta(Uzsienio_kalba_anglu_2021_IV),standardized=TRUE))

#-------------------------------------
#Užsienio kalba (anglų)   2022-2023
#-------------------------------------

Uzsienio_kalba_anglu_2022_I <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija +
                                    `Matematika` +  dalis_mok + 
                                    dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                                    vid_klases_dydis,
                                  data = m2022_klasteris1)
ggqqplot(Uzsienio_kalba_anglu_2022_I$residuals)
shapiro.test(Uzsienio_kalba_anglu_2022_I$residuals)
#paklaidos normalios

homoskedastiskumas(Uzsienio_kalba_anglu_2022_I)
bptest(Uzsienio_kalba_anglu_2022_I)
#dispersijos lygios

isskirtys(Uzsienio_kalba_anglu_2022_I)
vif(Uzsienio_kalba_anglu_2022_I)
summary(Uzsienio_kalba_anglu_2022_I)

Uzsienio_kalba_anglu_2022_I <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` +  dalis_mok,
                                  data = m2022_klasteris1)
summary(Uzsienio_kalba_anglu_2022_I)
vif(Uzsienio_kalba_anglu_2022_I)
print(coef(lm.beta(Uzsienio_kalba_anglu_2022_I),standardized=TRUE))

####

Uzsienio_kalba_anglu_2022_III <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija +
                                      `Matematika` +  dalis_mok + 
                                      dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                                      vid_klases_dydis,
                                    data = m2022_klasteris3)
ggqqplot(Uzsienio_kalba_anglu_2022_III$residuals)
shapiro.test(Uzsienio_kalba_anglu_2022_III$residuals)
#paklaidos normalios

homoskedastiskumas(Uzsienio_kalba_anglu_2022_III)
bptest(Uzsienio_kalba_anglu_2022_III)
#dispersijos lygios

isskirtys(Uzsienio_kalba_anglu_2022_III)
vif(Uzsienio_kalba_anglu_2022_III)
summary(Uzsienio_kalba_anglu_2022_III)

Uzsienio_kalba_anglu_2022_III <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Biologija +  dalis_mok + BVP,
                                    data = m2022_klasteris3)
summary(Uzsienio_kalba_anglu_2022_III)
vif(Uzsienio_kalba_anglu_2022_III)
print(coef(lm.beta(Uzsienio_kalba_anglu_2022_III),standardized=TRUE))

####

Uzsienio_kalba_anglu_2022_IV <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija +
                                     `Matematika` +  dalis_mok + 
                                     dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                                     vid_klases_dydis,
                                   data = m2022_klasteris4)
ggqqplot(Uzsienio_kalba_anglu_2022_IV$residuals)
shapiro.test(Uzsienio_kalba_anglu_2022_IV$residuals)
#paklaidos normalios

homoskedastiskumas(Uzsienio_kalba_anglu_2022_IV)
bptest(Uzsienio_kalba_anglu_2022_IV)
#disperijos lygios

isskirtys(Uzsienio_kalba_anglu_2022_IV)
vif(Uzsienio_kalba_anglu_2022_IV)
summary(Uzsienio_kalba_anglu_2022_IV)

Uzsienio_kalba_anglu_2022_IV <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra`+ Nedarbas,
                                   data = m2022_klasteris4)
summary(Uzsienio_kalba_anglu_2022_IV)
vif(Uzsienio_kalba_anglu_2022_IV)
print(coef(lm.beta(Uzsienio_kalba_anglu_2022_IV),standardized=TRUE))

#-------------------------------------
#Užsienio kalba (anglų)   2023-2024
#-------------------------------------


Uzsienio_kalba_anglu_2023_I <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija +
                                    `Matematika` +  dalis_mok + 
                                    dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                                    vid_klases_dydis,
                                  data = m2023_klasteris1)
ggqqplot(Uzsienio_kalba_anglu_2023_I$residuals)
shapiro.test(Uzsienio_kalba_anglu_2023_I$residuals)
#paklaidos normalios

homoskedastiskumas(Uzsienio_kalba_anglu_2023_I)
bptest(Uzsienio_kalba_anglu_2023_I)
#dispersijos lygios

isskirtys(Uzsienio_kalba_anglu_2023_I)
vif(Uzsienio_kalba_anglu_2023_I)
summary(Uzsienio_kalba_anglu_2023_I)

Uzsienio_kalba_anglu_2023_I <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` +`Matematika`,
                                  data = m2023_klasteris1)
summary(Uzsienio_kalba_anglu_2023_I)
vif(Uzsienio_kalba_anglu_2023_I)
print(coef(lm.beta(Uzsienio_kalba_anglu_2023_I),standardized=TRUE))

####

Uzsienio_kalba_anglu_2023_III <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija +
                                      `Matematika` +  dalis_mok + 
                                      dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                                      vid_klases_dydis,
                                    data = m2023_klasteris3)
ggqqplot(Uzsienio_kalba_anglu_2023_III$residuals)
shapiro.test(Uzsienio_kalba_anglu_2023_III$residuals)
#paklaidos normalios

homoskedastiskumas(Uzsienio_kalba_anglu_2023_III)
bptest(Uzsienio_kalba_anglu_2023_III)
#dispersijos lygios

isskirtys(Uzsienio_kalba_anglu_2023_III)
vif(Uzsienio_kalba_anglu_2023_III)
summary(Uzsienio_kalba_anglu_2023_III)

Uzsienio_kalba_anglu_2023_III <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Biologija +dalis_mok,
                                    data = m2023_klasteris3)
summary(Uzsienio_kalba_anglu_2023_III)
vif(Uzsienio_kalba_anglu_2023_III)
print(coef(lm.beta(Uzsienio_kalba_anglu_2023_III),standardized=TRUE))

####

Uzsienio_kalba_anglu_2023_IV <- lm(`Užsienio kalba (anglų)` ~ `Lietuvių kalba ir literatūra` + Geografija + Biologija +
                                     `Matematika` +  dalis_mok + 
                                     dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + 
                                     vid_klases_dydis,
                                   data = m2023_klasteris4)
ggqqplot(Uzsienio_kalba_anglu_2023_IV$residuals)
shapiro.test(Uzsienio_kalba_anglu_2023_IV$residuals)
#paklaidos normalios

homoskedastiskumas(Uzsienio_kalba_anglu_2023_IV)
bptest(Uzsienio_kalba_anglu_2023_IV)
#dispersijos lygios

isskirtys(Uzsienio_kalba_anglu_2023_IV)
vif(Uzsienio_kalba_anglu_2023_IV)
summary(Uzsienio_kalba_anglu_2023_IV)

Uzsienio_kalba_anglu_2023_IV <- lm(`Užsienio kalba (anglų)` ~ Geografija +
                                     `Matematika` + 
                                     dalis + BVP  + 
                                     vid_klases_dydis,
                                   data = m2023_klasteris4)
summary(Uzsienio_kalba_anglu_2023_IV)
vif(Uzsienio_kalba_anglu_2023_IV)
print(coef(lm.beta(Uzsienio_kalba_anglu_2023_IV),standardized=TRUE))

#-------------------------------------
#Geografija 2019–2020
#-------------------------------------
Geografija_2019_I <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + Biologija+ dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                        data = m2019_klasteris1)
ggqqplot(Geografija_2019_I$residuals)
shapiro.test(Geografija_2019_I$residuals) #normalumas nera tenkinamas

homoskedastiskumas(Geografija_2019_I)
bptest(Geografija_2019_I)#dispersijos lygios


isskirtys(Geografija_2019_I)
vif(Geografija_2019_I)
summary(Geografija_2019_I)

stepAIC(Geografija_2019_I)

Geografija_2019_I <- lm(`Geografija` ~   krepselio_dydis  ,
                        data = m2019_klasteris1)

summary(Geografija_2019_I)
vif(Geografija_2019_I)
print(coef(lm.beta(Geografija_2019_I),standardized=TRUE))





Geografija_2019_III <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + Biologija+ dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                          data = m2019_klasteris3)
ggqqplot(Geografija_2019_III$residuals)
shapiro.test(Geografija_2019_III$residuals) #normalumas nera tenkinamas

homoskedastiskumas(Geografija_2019_III)
bptest(Geografija_2019_III)#dispersijos lygios


isskirtys(Geografija_2019_III)
vif(Geografija_2019_III)
summary(Geografija_2019_III)

stepAIC(Geografija_2019_III)

Geografija_2019_III <- lm(`Geografija` ~  Matematika   ,
                          data = m2019_klasteris3)

summary(Geografija_2019_III)
vif(Geografija_2019_III)
print(coef(lm.beta(Geografija_2019_III),standardized=TRUE))







Geografija_2019_IV <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` +  dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                         data = m2019_klasteris4)
ggqqplot(Geografija_2019_IV$residuals)
shapiro.test(Geografija_2019_IV$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Geografija_2019_IV)
bptest(Geografija_2019_IV)#dispersijos lygios


isskirtys(Geografija_2019_IV)
vif(Geografija_2019_IV)
summary(Geografija_2019_IV)

stepAIC(Geografija_2019_IV)

Geografija_2019_IV <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra`        ,
                         data = m2019_klasteris4)

summary(Geografija_2019_IV)
vif(Geografija_2019_IV)
print(coef(lm.beta(Geografija_2019_IV),standardized=TRUE))




#-------------------------------------
#Geografija  2020-2021
#-------------------------------------
Geografija_2020_I <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` +  Biologija +dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                        data = m2020_klasteris1)
ggqqplot(Geografija_2020_I$residuals)
shapiro.test(Geografija_2020_I$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Geografija_2020_I)
bptest(Geografija_2020_I)#dispersijos lygios


isskirtys(Geografija_2020_I)
vif(Geografija_2020_I)
summary(Geografija_2020_I)

stepAIC(Geografija_2020_I)

Geografija_2020_I <- lm(`Geografija` ~  `Darbo uzmokestis`  ,
                        data = m2020_klasteris1)

summary(Geografija_2020_I)
vif(Geografija_2020_I)
print(coef(lm.beta(Geografija_2020_I),standardized=TRUE))







Geografija_2020_III <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + Biologija+ dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                          data = m2020_klasteris3)
ggqqplot(Geografija_2020_III$residuals)
shapiro.test(Geografija_2020_III$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Geografija_2020_III)
bptest(Geografija_2020_III)#dispersijos lygios


isskirtys(Geografija_2020_III)
vif(Geografija_2020_III)
summary(Geografija_2020_III)

stepAIC(Geografija_2020_III)

Geografija_2020_III <- lm(`Geografija` ~ `Užsienio kalba (anglų)` + vid_klases_dydis,
                          data = m2020_klasteris3)

summary(Geografija_2020_III)
vif(Geografija_2020_III)
print(coef(lm.beta(Geografija_2020_III),standardized=TRUE))













Geografija_2020_IV <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` +  dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                         data = m2020_klasteris4)
ggqqplot(Geografija_2020_IV$residuals)
shapiro.test(Geografija_2020_IV$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Geografija_2020_IV)
bptest(Geografija_2020_IV)#dispersijos lygios


isskirtys(Geografija_2020_IV)
vif(Geografija_2020_IV)
summary(Geografija_2020_IV)

stepAIC(Geografija_2020_IV)
Geografija_2020_IV <- lm(`Geografija` ~  Matematika      ,
                         data = m2020_klasteris4)

summary(Geografija_2020_IV)
vif(Geografija_2020_IV)
print(coef(lm.beta(Geografija_2020_IV),standardized=TRUE))





#-------------------------------------
#Geografija  2021-2022
#-------------------------------------


Geografija_2021_I <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` +  dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                        data = m2021_klasteris1)
ggqqplot(Geografija_2021_I$residuals)
shapiro.test(Geografija_2021_I$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Geografija_2021_I)
bptest(Geografija_2021_I)#dispersijos nelygios


isskirtys(Geografija_2021_I)
vif(Geografija_2021_I)
summary(Geografija_2021_I)

stepAIC(Geografija_2021_I)
Geografija_2021_I <- lm(`Geografija` ~ Matematika      ,
                        data = m2021_klasteris1)

summary(Geografija_2021_I)
vif(Geografija_2021_I)
print(coef(lm.beta(Geografija_2021_I),standardized=TRUE))






Geografija_2021_III <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + Biologija+ dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                          data = m2021_klasteris3)

ggqqplot(Geografija_2021_III$residuals)
shapiro.test(Geografija_2021_III$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Geografija_2021_III)
bptest(Geografija_2021_III)#dispersijos lgyios


isskirtys(Geografija_2021_III)
vif(Geografija_2021_III)
summary(Geografija_2021_III)

stepAIC(Geografija_2021_III)
Geografija_2021_III <- lm(`Geografija` ~ `Užsienio kalba (anglų)` + dalis_mok+ vid_klases_dydis,
                          data = m2021_klasteris3)

summary(Geografija_2021_III)
vif(Geografija_2021_III)
print(coef(lm.beta(Geografija_2021_III),standardized=TRUE))




Geografija_2021_IV <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + Biologija+ dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                         data = m2021_klasteris4)

ggqqplot(Geografija_2021_IV$residuals)
shapiro.test(Geografija_2021_IV$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Geografija_2021_IV)
bptest(Geografija_2021_IV)#dispersijos lgyios


isskirtys(Geografija_2021_IV)
vif(Geografija_2021_IV)
summary(Geografija_2021_IV)

stepAIC(Geografija_2021_IV)
Geografija_2021_IV <- lm(`Geografija` ~ `Užsienio kalba (anglų)`    ,
                         
                         data = m2021_klasteris4)

summary(Geografija_2021_IV)
vif(Geografija_2021_IV)
print(coef(lm.beta(Geografija_2021_IV),standardized=TRUE))




#------------------------------------
#Geografija  2022-2023
#-------------------------------------


Geografija_2022_I <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + Biologija+  dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                        data = m2022_klasteris1)


ggqqplot(Geografija_2022_I$residuals)
shapiro.test(Geografija_2022_I$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Geografija_2022_I)
bptest(Geografija_2022_I)#dispersijos lyygios


isskirtys(Geografija_2022_I)
vif(Geografija_2022_I)
summary(Geografija_2022_I)

stepAIC(Geografija_2022_I)
Geografija_2022_I <- lm(`Geografija` ~   Matematika   ,
                        
                        data = m2022_klasteris1)

summary(Geografija_2022_I)
vif(Geografija_2022_I)
print(coef(lm.beta(Geografija_2022_I),standardized=TRUE))







Geografija_2022_III <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` +  Biologija+dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                          data = m2022_klasteris3)
ggqqplot(Geografija_2022_III$residuals)
shapiro.test(Geografija_2022_III$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Geografija_2022_III)
bptest(Geografija_2022_III)#dispersijos lygios


isskirtys(Geografija_2022_III)
vif(Geografija_2022_III)
summary(Geografija_2022_III)

stepAIC(Geografija_2022_III)
Geografija_2022_III <- lm(`Geografija` ~  `Lietuvių kalba ir literatūra` +vid_klases_dydis,
                          
                          data = m2022_klasteris3)

summary(Geografija_2022_III)
vif(Geografija_2022_III)
print(coef(lm.beta(Geografija_2022_III),standardized=TRUE))






Geografija_2022_IV <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + Biologija+ dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                         data = m2022_klasteris4)
ggqqplot(Geografija_2022_IV$residuals)
shapiro.test(Geografija_2022_IV$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Geografija_2022_IV)
bptest(Geografija_2022_IV)#dispersijos lygios


isskirtys(Geografija_2022_IV)
vif(Geografija_2022_IV)
summary(Geografija_2022_IV)

stepAIC(Geografija_2022_IV)
Geografija_2022_IV <- lm(`Geografija` ~  Matematika     + vid_klases_dydis,
                         
                         data = m2022_klasteris4)

summary(Geografija_2022_IV)
vif(Geografija_2022_IV)
print(coef(lm.beta(Geografija_2022_IV),standardized=TRUE))





#-------------------------------------
#Geografija  2023-2024
#-------------------------------------
Geografija_2023_I <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` +  dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                        data = m2023_klasteris1)
ggqqplot(Geografija_2023_I$residuals)
shapiro.test(Geografija_2023_I$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Geografija_2023_I)
bptest(Geografija_2023_I)#dispersijos lygios


isskirtys(Geografija_2023_I)
vif(Geografija_2023_I)
summary(Geografija_2023_I)

stepAIC(Geografija_2023_I)
Geografija_2023_I <- lm(`Geografija` ~ Matematika    + `Darbo uzmokestis` +   vid_klases_dydis ,
                        data = m2023_klasteris1)

summary(Geografija_2023_I)
vif(Geografija_2023_I)
print(coef(lm.beta(Geografija_2023_I),standardized=TRUE))



Geografija_2023_III <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + Biologija+  dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                          data = m2023_klasteris3)
ggqqplot(Geografija_2023_III$residuals)
shapiro.test(Geografija_2023_III$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Geografija_2023_III)
bptest(Geografija_2023_III)#dispersijos lygios


isskirtys(Geografija_2023_III)
vif(Geografija_2023_III)
summary(Geografija_2023_III)

stepAIC(Geografija_2023_III)
Geografija_2023_III <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra`+  
                            dalis_mok,
                          data = m2023_klasteris3)

summary(Geografija_2023_III)
vif(Geografija_2023_III)

print(coef(lm.beta(Geografija_2023_III),standardized=TRUE))






Geografija_2023_IV <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + Biologija+  dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                         data = m2023_klasteris4)
ggqqplot(Geografija_2023_IV$residuals)
shapiro.test(Geografija_2023_IV$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Geografija_2023_IV)
bptest(Geografija_2023_IV)#dispersijos lygios


isskirtys(Geografija_2023_IV)
vif(Geografija_2023_IV)
summary(Geografija_2023_IV)

stepAIC(Geografija_2023_IV)
Geografija_2023_IV <- lm(`Geografija` ~ `Lietuvių kalba ir literatūra`  + BVP,
                         data = m2023_klasteris4)

summary(Geografija_2023_IV)
vif(Geografija_2023_IV)
print(coef(lm.beta(Geografija_2023_IV),standardized=TRUE))









#-------------------------------------
#Biologija 2019–2020
#-------------------------------------
Biologija_2019_I <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` +  dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                       data = m2019_klasteris1)
ggqqplot(Biologija_2019_I$residuals)
shapiro.test(Biologija_2019_I$residuals) #normalumas nera tenkinamas

homoskedastiskumas(Biologija_2019_I)
bptest(Biologija_2019_I)#dispersijos lygios


isskirtys(Biologija_2019_I)
vif(Biologija_2019_I)
summary(Biologija_2019_I)

stepAIC(Biologija_2019_I)
Biologija_2019_I <- lm(`Biologija` ~  Matematika ,
                       data = m2019_klasteris1)

summary(Biologija_2019_I)
vif(Biologija_2019_I)
print(coef(lm.beta(Biologija_2019_I),standardized=TRUE))






Biologija_2019_III <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + Geografija+ dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                         data = m2019_klasteris3)
ggqqplot(Biologija_2019_III$residuals)
shapiro.test(Biologija_2019_III$residuals) #normalumas nera tenkinamas

homoskedastiskumas(Biologija_2019_III)
bptest(Biologija_2019_III)#dispersijos lygios


isskirtys(Biologija_2019_III)
vif(Biologija_2019_III)
summary(Biologija_2019_III)

stepAIC(Biologija_2019_III)
Biologija_2019_III <- lm(`Biologija` ~  Matematika + dalis + krepselio_dydis ,
                         data = m2019_klasteris3)

summary(Biologija_2019_III)
vif(Biologija_2019_III)
print(coef(lm.beta(Biologija_2019_III),standardized=TRUE))








Biologija_2019_IV <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + Geografija+ dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                        data = m2019_klasteris4)
ggqqplot(Biologija_2019_IV$residuals)
shapiro.test(Biologija_2019_IV$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Biologija_2019_IV)
bptest(Biologija_2019_IV)#dispersijos lygios


isskirtys(Biologija_2019_IV)
vif(Biologija_2019_IV)
summary(Biologija_2019_IV)

stepAIC(Biologija_2019_IV)
Biologija_2019_IV <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra`  + Geografija+ dalis_mok    +  Nedarbas + vid_klases_dydis,
                        data = m2019_klasteris4)

summary(Biologija_2019_IV)
vif(Biologija_2019_IV)
print(coef(lm.beta(Biologija_2019_IV),standardized=TRUE))




#-------------------------------------
#Biologija  2020-2021
#-------------------------------------
Biologija_2020_I <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + Geografija+ dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                       data = m2020_klasteris1)
ggqqplot(Biologija_2020_I$residuals)
shapiro.test(Biologija_2020_I$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Biologija_2020_I)
bptest(Biologija_2020_I)#dispersijos lygios


isskirtys(Biologija_2020_I)
vif(Biologija_2020_I)
summary(Biologija_2020_I)

stepAIC(Biologija_2020_I)
Biologija_2020_I <- lm(`Biologija` ~       `Užsienio kalba (anglų)` + dalis_mok,
                       data = m2020_klasteris1)

summary(Biologija_2020_I)
vif(Biologija_2020_I)
print(coef(lm.beta(Biologija_2020_I),standardized=TRUE))



Biologija_2020_III <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + `Geografija`+ dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                         data = m2020_klasteris3)
ggqqplot(Biologija_2020_III$residuals)
shapiro.test(Biologija_2020_III$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Biologija_2020_III)
bptest(Biologija_2020_III)#dispersijos lygios


isskirtys(Biologija_2020_III)
vif(Biologija_2020_III)
summary(Biologija_2020_III)

stepAIC(Biologija_2020_III)
Biologija_2020_III <- lm(`Biologija` ~      Matematika +  dalis+krepselio_dydis   +   BVP      ,
                         data = m2020_klasteris3)

summary(Biologija_2020_III)
vif(Biologija_2020_III)
print(coef(lm.beta(Biologija_2020_III),standardized=TRUE))








Biologija_2020_IV <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + Geografija +dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                        data = m2020_klasteris4)
ggqqplot(Biologija_2020_IV$residuals)
shapiro.test(Biologija_2020_IV$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Biologija_2020_IV)
bptest(Biologija_2020_IV)#dispersijos lygios


isskirtys(Biologija_2020_IV)
vif(Biologija_2020_IV)
summary(Biologija_2020_IV)

stepAIC(Biologija_2020_IV)
Biologija_2020_IV <- lm(`Biologija` ~      Matematika     +  Geografija      + krepselio_dydis   ,
                        data = m2020_klasteris4)

summary(Biologija_2020_IV)
vif(Biologija_2020_IV)
print(coef(lm.beta(Biologija_2020_IV),standardized=TRUE))

#-------------------------------------
#Biologija  2021-2022
#-------------------------------------
Biologija_2021_I <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + `Geografija` +  dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                       data = m2021_klasteris1)
ggqqplot(Biologija_2021_I$residuals)
shapiro.test(Biologija_2021_I$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Biologija_2021_I)
bptest(Biologija_2021_I)#dispersijos lygios


isskirtys(Biologija_2021_I)
vif(Biologija_2021_I)
summary(Biologija_2021_I)

stepAIC(Biologija_2021_I)
Biologija_2021_I <- lm(`Biologija` ~    `Lietuvių kalba ir literatūra`   +                   Matematika      +          vid_klases_dydis     ,
                       data = m2021_klasteris1)

summary(Biologija_2021_I)
vif(Biologija_2021_I)
print(coef(lm.beta(Biologija_2021_I),standardized=TRUE))







Biologija_2021_III <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + `Geografija` +  dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                         data = m2021_klasteris3)
ggqqplot(Biologija_2021_III$residuals)
shapiro.test(Biologija_2021_III$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Biologija_2021_III)
bptest(Biologija_2021_III)#dispersijos nelygios


isskirtys(Biologija_2021_III)
vif(Biologija_2021_III)
summary(Biologija_2021_III)

stepAIC(Biologija_2021_III)
Biologija_2021_III <- lm(`Biologija` ~    `Lietuvių kalba ir literatūra`    +  Matematika         +         krepselio_dydis              ,
                         data = m2021_klasteris3)

summary(Biologija_2021_III)
vif(Biologija_2021_III)
print(coef(lm.beta(Biologija_2021_III),standardized=TRUE))






Biologija_2021_IV <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + `Geografija` + dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                        data = m2021_klasteris4)
ggqqplot(Biologija_2021_IV$residuals)
shapiro.test(Biologija_2021_IV$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Biologija_2021_IV)
bptest(Biologija_2021_IV)#dispersijos lygios


isskirtys(Biologija_2021_IV)
vif(Biologija_2021_IV)
summary(Biologija_2021_IV)

stepAIC(Biologija_2021_IV)
Biologija_2021_IV <- lm(`Biologija` ~   `Užsienio kalba (anglų)`       ,
                        data = m2021_klasteris4)

summary(Biologija_2021_IV)
vif(Biologija_2021_IV)
print(coef(lm.beta(Biologija_2021_IV),standardized=TRUE))












#-------------------------------------
#Biologija  2022-2023
#-------------------------------------

Biologija_2022_I <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + `Geografija`+  dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                       data = m2022_klasteris1)
ggqqplot(Biologija_2022_I$residuals)
shapiro.test(Biologija_2022_I$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Biologija_2022_I)
bptest(Biologija_2022_I)#dispersijos lygios


isskirtys(Biologija_2022_I)
vif(Biologija_2022_I)
summary(Biologija_2022_I)

stepAIC(Biologija_2022_I)
Biologija_2022_I <- lm(`Biologija` ~   dalis_mok          +     BVP     ,
                       data = m2022_klasteris1)

summary(Biologija_2022_I)
vif(Biologija_2022_I)
print(coef(lm.beta(Biologija_2022_I),standardized=TRUE))














Biologija_2022_III <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + `Geografija`+  dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                         data = m2022_klasteris3)
ggqqplot(Biologija_2022_III$residuals)
shapiro.test(Biologija_2022_III$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Biologija_2022_III)
bptest(Biologija_2022_III)#dispersijos lygios


isskirtys(Biologija_2022_III)
vif(Biologija_2022_III)
summary(Biologija_2022_III)

stepAIC(Biologija_2022_III)
Biologija_2022_III <- lm(`Biologija` ~  Matematika  +`Užsienio kalba (anglų)`       +    krepselio_dydis    ,
                         data = m2022_klasteris3)

summary(Biologija_2022_III)
vif(Biologija_2022_III)
print(coef(lm.beta(Biologija_2022_III),standardized=TRUE))








Biologija_2022_IV <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + `Geografija`+  dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                         data = m2022_klasteris4)

ggqqplot(Biologija_2022_IV$residuals)
shapiro.test(Biologija_2022_IV$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Biologija_2022_IV)
bptest(Biologija_2022_IV)#dispersijos lygios


isskirtys(Biologija_2022_IV)
vif(Biologija_2022_IV)
summary(Biologija_2022_IV)

stepAIC(Biologija_2022_IV)
Biologija_2022_IV <- lm(`Biologija` ~ Matematika + dalis_mok,
                        data = m2022_klasteris4)

summary(Biologija_2022_IV)
vif(Biologija_2022_IV)
print(coef(lm.beta(Biologija_2022_IV),standardized=TRUE))


#------------------------------------
#Biologija  2023-2024
#-------------------------------------
Biologija_2023_I <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + `Geografija`+   dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                       data = m2023_klasteris1)
ggqqplot(Biologija_2023_I$residuals)
shapiro.test(Biologija_2023_I$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Biologija_2023_I)
bptest(Biologija_2023_I)#dispersijos lygios


isskirtys(Biologija_2023_I)
vif(Biologija_2023_I)
summary(Biologija_2023_I)

stepAIC(Biologija_2023_I)
Biologija_2023_I <- lm(`Biologija` ~  Matematika  ,
                       data = m2023_klasteris1)

summary(Biologija_2023_I)
vif(Biologija_2023_I)
print(coef(lm.beta(Biologija_2023_I),standardized=TRUE))






Biologija_2023_III <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + `Geografija`+  dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                         data = m2023_klasteris3)
ggqqplot(Biologija_2023_III$residuals)
shapiro.test(Biologija_2023_III$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Biologija_2023_III)
bptest(Biologija_2023_III)#dispersijos lygios


isskirtys(Biologija_2023_III)
vif(Biologija_2023_III)
summary(Biologija_2023_III)

stepAIC(Biologija_2023_III)
Biologija_2023_III <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra`   +     `Užsienio kalba (anglų)`         +                  dalis      ,
                         data = m2023_klasteris3)

summary(Biologija_2023_III)
vif(Biologija_2023_III)
print(coef(lm.beta(Biologija_2023_III),standardized=TRUE))








Biologija_2023_IV <- lm(`Biologija` ~`Lietuvių kalba ir literatūra` + Matematika + `Užsienio kalba (anglų)` + `Geografija`+   dalis_mok + dalis + krepselio_dydis + BVP + `Darbo uzmokestis` + Nedarbas + vid_klases_dydis,
                        data = m2023_klasteris4)

ggqqplot(Biologija_2023_IV$residuals)
shapiro.test(Biologija_2023_IV$residuals) #normalumas yra tenkinamas

homoskedastiskumas(Biologija_2023_IV)
bptest(Biologija_2023_IV)#dispersijos lygios


isskirtys(Biologija_2023_IV)
vif(Biologija_2023_IV)
summary(Biologija_2023_IV)

stepAIC(Biologija_2023_IV)
Biologija_2023_IV <- lm(`Biologija` ~ `Lietuvių kalba ir literatūra`     + vid_klases_dydis,
                        data = m2023_klasteris4)

summary(Biologija_2023_IV)
vif(Biologija_2023_IV)
print(coef(lm.beta(Biologija_2023_IV),standardized=TRUE))


