library(readr) # tror ikke denne behøves
data <- read.csv("regnskaber_industri_transport_byg_5_25000_ansatte_anonym.csv", 
                 header = TRUE, sep = ";", fileEncoding = "ISO-8859-1")

sum(is.na(data)) # Se antallet af NAs
# 292481


#### Opgave 3 – Spørgeskema og Cumulative Link Models ####
##### Opgave 3.1 – Illustration af spørgsmålet #####
# Hent data fra filen ”regnskaber_industri_transport_byg_5_25000_ansatte_anonym” ind i R og lav en grafisk
# illustration af fordelingen af svarene på spørgsmålet: ”Hvordan ser du mulighederne for at låne penge til din
# virksomhed?”

# Se mulige svar
unique(data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.)
# Kan se at der er 2 forskellige: dårlig og dårlige                                                 \\b = ordstart og ordslutning, fast defineret ord
data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- gsub("\\bDårlig\\b","Dårlige",data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.)
unique(data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.)

# Frekvenstabel
data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- factor(
  data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.,
  levels = c("Meget dårlige", "Dårlige", "Neutrale", "Gode", "Meget gode"), 
  ordered = TRUE)
freq <- as.data.frame(table(data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.),decreasing = T)
colnames(freq) <- c("Svar", "Frekvens")

# ny df uden 'Ved ikke'
df <- subset(data, Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. != "Ved ikke")
# Går fra 4484 til 4433 -51 obs
df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- factor(
  df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.,
  levels = c("Meget dårlige", "Dårlige", "Neutrale", "Gode", "Meget gode"), 
  ordered = TRUE)
df$gruppering <- ifelse(df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. %in% c("Dårlige", "Meget dårlige"), 
                        "Negativ",
                        ifelse(df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. %in% c("Gode", "Meget gode"),
                               "Positiv",
                               "Neutral"))
freq <- as.data.frame(table(
  Svar = df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.,
  Gruppering = df$gruppering
))
colnames(freq) <- c("Svar","Gruppering" ,"Frekvens")
freq <- freq[freq$Frekvens > 0, ]
#freq$Svar <- factor(freq$Svar, levels = c("Meget gode", "Gode", "Neutrale", "Dårlige", "Meget dårlige"))


# Plot over frekvens
library(ggplot2)

# Beregn procenter 
freq$Procent <- round((freq$Frekvens / sum(freq$Frekvens)) * 100,1)

# Plot med procentangivelser på y-aksen
ggplot(freq, aes(x = Svar, y = Procent, fill = Gruppering)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "72% af virksomhederne er positivt stemt når det kommer til muligheden om at låne penge",
    x = "Svar",
    y = "Procent (%)"
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + # Sæt interval på y-aksen
  scale_fill_manual(
    values = c("Positiv" = "#A3E1CE",   # Farve til Positiv
               "Neutral" = "#FAB958",   # Farve til Neutral
               "Negativ" = "#002E6D")   # Farve til Negativ
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

##### Opgave 3.2 – Cumulative Link Models #####
# Lav en Cumulative Link Model for spørgsmålet ”Hvordan ser du mulighederne for at låne penge til din
# virksomhed?”. I må selv vurdere, hvilke forklarende variable I ønsker i jeres model. Vær opmærksom på, at I
# kan finde inspiration til forklarende variable i artiklerne ”Historisk nemt at låne penge” og ”Gode muligheder
# for at låne penge i hele landet”.
### BAUMS GULDKORN ###
#selv forklare de variable
#"Find ud af hvad vi vil have med"
#"start ved colonne a og bare kør derudaf for at finde ud af hvad vi skal bruge"
#" ELLER kig i artiklen og kopier"
#lav modellen
#kør tomodeller, find signifikant og tag kun insignifikant med hvis det skal bruges til analysen (fks geo-placering)
#grafisk forklare variablen i forhold til hvad er er svaret
# Vores valgte variabler:
# Y = Virksomhedernes vurdering på deres lånemuligheder

# X:
# 1. Soliditetsgraden (Soliditetsgraden er et pejlemærke på, hvor stærkt din virksomhed vil stå i en eventuel økonomisk krise. - dinero.dk ) 𝑆𝑜𝑙𝑖𝑑𝑖𝑡𝑒𝑡𝑠𝑔𝑟𝑎𝑑 = (𝐸𝑔𝑒𝑛𝑘𝑎𝑝𝑖𝑡𝑎𝑙 / 𝐵𝑎𝑙𝑎𝑛𝑐𝑒𝑛) × 100
# 2. Balancen (virksomheds pengebinding, i form af aktiver og passiver på en given dag, hvilket typisk vil være på årsafslutningsdatoen. - Billy.dk)
# 3. Etableringsdatoen
# 4. Branche
# 5. Egenkapital (beløbet tilhørende virksomhedens ejer(ere), når passiver er fratrukket fraaktiver. - dinero.dk)
# 6. Antal ansatte i virksomheden
# 7. Afkastningsgraden (viser hvordan en given virksomhed har formået at generere et overskud ud fra den kapital, der er blevet indskudt. - dinero.dk ) 𝐴𝑓𝑘𝑎𝑠𝑡𝑛𝑖𝑛𝑔𝑠𝑔𝑟𝑎𝑑 =(𝐵𝑟𝑢𝑡𝑡𝑜𝑟𝑒𝑠𝑢𝑙𝑡𝑎𝑡 / 𝑆𝑎𝑚𝑙𝑒𝑑𝑒 𝑎𝑘𝑡𝑖𝑣𝑒𝑟) × 100

# Som det første se på brancher
library(dkstat)
dst_search(string = "DB07", field = "text") # Documentation kan ikke findes via dkstat pakke



# Fundet manuelt
library(readxl)
branchekode <- read_excel("Dansk-Branchekode-2007-(DB07)-v3-2014.xlsx")

# Giv samme colname ift. senere merging
colnames(df)[colnames(df) == "Branchekode.primær"] <- "BRANCHEKODE"
#tilsæt 0 på de koder med kun 5 cifre, så de matcher med dem fra DST
#                       ^ start at srt, $ slut af str, det vil sige KUN dem med præcist 5 digits, ingen andre
df$BRANCHEKODE <- gsub("^([0-9]{5})$", "0\\1", df$BRANCHEKODE) 

#Merge dem
df <- merge(branchekode, df, by = "BRANCHEKODE") # Inner join, fordi branchekode skal findes i begge dataframes

df$GRP10KODE <- as.vector(df$GRP10KODE)
#> unique(df$GRP10KODE)
#[1]  1  2  3  4  5  7  8  9 10
branch_labels <- c(
  "1" = "Landbrug og fiskeri",
  "2" = "Industri og forsyning",
  "3" = "Bygge og anlæg",
  "4" = "Handel og transport",
  "5" = "Information og kommunikation",
  "7" = "Ejendomshandel",
  "8" = "Erhvervsservice",
  "9" = "Off. adm., undervisning, sundhed",
  "10" = "Kultur og fritid"
)

# kun med vores x-variabler
# Opret en tom dataframe med kolonnenavne
DFCLM <- data.frame(
  Svar = df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.,
  Balance = df$Balance.2020..1.000.kr.,
  Soliditetsgrad = df$Soliditetsgrad.2020....,
  AFKAST = df$Afkastningsgrad.2020....,
  RESULTAT = df$Primært.Resultat.2020..1.000.kr.,
  Ansatte = df$Antal.ansatte.Cvr.nr.,
  ALDER = df$Etableringsdato,
  ÅRSTAL = substr(df$Etableringsdato,7,10), # kun årstal
  Egenkapital = df$Egenkapital.2020..1.000.kr.,
  Branchekode = df$GRP10KODE
)
DFCLM$Branche <- branch_labels[DFCLM$Branchekode]

# tjek na
DFCLM[DFCLM == ""] <- NA # Lave tomme rækker om til NA
antal_na_rækker <- sum(apply(DFCLM, 1, function(x) any(is.na(x))))
print(antal_na_rækker)
# Kun 14 na, lad os se hvor de er
na_positioner <- which(is.na(DFCLM), arr.ind = TRUE) # arr.ind retunere positionsindeks (altså række og kolonne)
print(na_positioner)
DFCLM <- na.omit(DFCLM)
# obs 4433 -> 4419
# Datacleaning
str(DFCLM)

# Vurding
# Korrekt
# Balance
DFCLM$Balance <- as.numeric(DFCLM$Balance)
# Soliditetsgrad
# Som chr, grundet ','
DFCLM$Soliditetsgrad <- as.numeric(sub(",",".",DFCLM$Soliditetsgrad))
# AFKAST
# Som chr, grundet ','
DFCLM$AFKAST <- as.numeric(sub(",",".",DFCLM$AFKAST))
# RESULTAT
DFCLM$RESULTAT <- as.numeric(DFCLM$RESULTAT)
# Ansatte
# Korrekt som int, kan have decimal
# ALDER
# Som chr, og skal laves om til dage
DFCLM$ALDER <- as.Date(DFCLM$ALDER, format = "%d-%m-%Y")
DFCLM$ALDER <- as.numeric(difftime(DFCLM$ALDER, as.Date("2024-12-10"), units = "days"))
DFCLM$ALDER <- DFCLM$ALDER *-1
# ÅRSTAL
DFCLM$ÅRSTAL <- as.factor(DFCLM$ÅRSTAL)

# Egenkapital
DFCLM$Egenkapital <- as.numeric(DFCLM$Egenkapital)
# Branche
DFCLM$Branche <- as.factor(DFCLM$Branche)
# Branchekode
DFCLM$Branchekode <- as.factor(DFCLM$Branchekode)

DFCLM_Scaled <- data.frame(
  Svar = DFCLM$Svar,
  Soliditetsgrad = as.numeric(scale(DFCLM$Soliditetsgrad)),
  #Balance_log = as.numeric(log(DFCLM$Balance)), # Ingen negative balancer, så kan ignorer chance for log(0)
  Balance = as.numeric(scale(DFCLM$Balance)),
  AFKAST = as.numeric(scale(DFCLM$AFKAST)),
  RESULTAT = as.numeric(scale(DFCLM$RESULTAT)),
  Ansatte = as.numeric(scale(DFCLM$Ansatte)),
  Egenkapital = as.numeric(scale(DFCLM$Egenkapital)),
  ALDER = as.numeric(scale(DFCLM$ALDER))
)

# # Hvorfor log på bal?
# # Log-transform af Balance
# hist(scale(data$Balance.2020..1.000.kr.))
# hist(log(data$Balance.2020..1.000.kr.+1)) # +1 for at undgå log(0)






library(ordinal)
CLMBAUM <- clm(Svar ~ Soliditetsgrad + Balance + AFKAST, data = DFCLM_Scaled)
summary(CLMBAUM)

# Årstal kan ikke tages med, da det er en factor
CLM <- clm(Svar ~ Soliditetsgrad + Balance + AFKAST + Ansatte + ALDER + Egenkapital, data = DFCLM_Scaled)
summary(CLM)
# Cefficients viser en stigning fra 1 kategori til næste



DFCLM_Scaled_reordered <- DFCLM_Scaled
DFCLM_Scaled_reordered$Svar <- factor(
  DFCLM_Scaled_reordered$Svar,
  levels = c("Meget gode", "Gode", "Neutral", "Dårlige", "Meget dårlige"), # Ny rækkefølge
  ordered = TRUE)
levels(DFCLM_Scaled_reordered$Svar)
CLM_reordered <- clm(Svar ~ Soliditetsgrad + Balance + AFKAST + Ansatte + ALDER + Egenkapital, data = DFCLM_Scaled_reordered)
summary(CLM_reordered)
# Her er ansatte ikke signifikant


# Virker mærkeligt, at afkast ikke har indflydelse, vi kan prøve med åretsresultat
CLM1 <- clm(Svar ~ Soliditetsgrad + Balance + RESULTAT + Ansatte + ALDER + Egenkapital, data = DFCLM_Scaled)
summary(CLM1)

# Tjekke signifikans for en signifkans x og se, hvilke år er bedst
Balancedf <- data.frame(
  Svar = df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.,
  #Balance_2021 = df$Balance.2021..1.000.kr., Fjernes grundet mange NA
  Balance_2020 = df$Balance.2020..1.000.kr.,
  Balance_2019 = df$Balance.2019..1.000.kr.,
  Balance_2018 = df$Balance.2018..1.000.kr.,
  Balance_2017 = df$Balance.2017..1.000.kr.,
  Balance_2016 = df$Balance.2016..1.000.kr. )

Balancedf$Balance_2020 <- as.numeric(log(Balancedf$Balance_2020))
Balancedf$Balance_2019 <- as.numeric(log(Balancedf$Balance_2019))
Balancedf$Balance_2018 <- as.numeric(log(Balancedf$Balance_2018))
Balancedf$Balance_2017 <- as.numeric(log(Balancedf$Balance_2017))
Balancedf$Balance_2016 <- as.numeric(log(Balancedf$Balance_2016))

clm_Bal <- clm(Svar ~ Balance_2020 + Balance_2019 + Balance_2018 + Balance_2017 + Balance_2016, data = Balancedf)
summary(clm_Bal)


Egenkapitaldf <- data.frame(
  Svar = df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.,
  #Balance_2021 = df$Balance.2021..1.000.kr., Fjernes grundet mange NA
  Egenkapital_2020 = df$Egenkapital.2020..1.000.kr.,
  Egenkapital_2019 = df$Egenkapital.2019..1.000.kr.,
  Egenkapital_2018 = df$Egenkapital.2018..1.000.kr.,
  Egenkapital_2017 = df$Egenkapital.2017..1.000.kr.,
  Egenkapital_2016 = df$Egenkapital.2016..1.000.kr. )

Egenkapitaldf$Egenkapital_2020 <- as.numeric(scale(Egenkapitaldf$Egenkapital_2020))
Egenkapitaldf$Egenkapital_2019 <- as.numeric(scale(Egenkapitaldf$Egenkapital_2019))
Egenkapitaldf$Egenkapital_2018 <- as.numeric(scale(Egenkapitaldf$Egenkapital_2018))
Egenkapitaldf$Egenkapital_2017 <- as.numeric(scale(Egenkapitaldf$Egenkapital_2017))
Egenkapitaldf$Egenkapital_2016 <- as.numeric(scale(Egenkapitaldf$Egenkapital_2016))

clm_Egen <- clm(Svar ~ Egenkapital_2020 + Egenkapital_2019 + Egenkapital_2018 + Egenkapital_2017 + Egenkapital_2016, data = Egenkapitaldf)
summary(clm_Egen)
# Kun 2020 i begge
# Overens med Baum artikel

##### Opgave 3.3 – Illustration af forklarende variable #####
# Lav en grafisk illustration af den ene af jeres forklarende variable og spørgsmålet 
# ”Hvordan ser du mulighederne for at låne penge til din virksomhed?”. 
# (hint: se figur på side 6 i artiklen: ”Gode muligheder for at låne penge i hele landet”)
branch_tabel <- as.data.frame(sort(table(df$GRP10KODE),decreasing=T))

branch_tabel$Brancher <- branch_labels[branch_tabel$Var1]

colnames(branch_tabel) <- c("GRP10KODE", "Antal Svar", "Branche")

# Beregn procent for hver branche
branch_tabel$Procent <- round((branch_tabel$`Antal Svar` / sum(branch_tabel$`Antal Svar`)) * 100, 1)

# Plot med procenter
ggplot(branch_tabel, aes(x = reorder(Branche, -Procent), y = Procent, fill = Branche)) + # reorder tager fra procent descending
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "98,9% af alle svar, kommer fra 3 brancher",
    x = "Branche",
    y = "Procent (%)"
  ) +
  scale_fill_manual(values = c("#002E6D", "#FBB040", "#6310F4", "#808996", "#A3DFD0", "#B892F9", "#FAC7F1", "#B4C6EB", "#77CBDA")
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + # Interval for y-aksen
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none" # Fjern legend, da brancher allerede står på x-aksen
  )

# Dataframe for kun signifikante:
Dataframe <- data.frame(
  Svar = DFCLM$Svar,
  Soliditetsgrad = DFCLM$Soliditetsgrad,
  Balance = DFCLM$Balance,
  RESULTAT = DFCLM$RESULTAT,
  AFKAST = DFCLM$AFKAST,
  ALDER = DFCLM$ALDER,
  Egenkapital = DFCLM$Egenkapital,
  Branche = DFCLM$Branche,
  Branchekode = DFCLM$Branchekode
)
Dataframe$gruppering <- ifelse(Dataframe$Svar %in% c("Dårlige", "Meget dårlige"), 
                               "Negative - Dårlige / Meget Dårlige",
                               ifelse(Dataframe$Svar %in% c("Gode", "Meget gode"),
                                      "Positive - Gode / Meget gode",
                                      "Neutral"))


###### Soliditetesgrad ######
SoliDF <- aggregate(Dataframe$Soliditetsgrad, list(Dataframe$gruppering), FUN=mean) # Skal være list
colnames(SoliDF) <- c("Group", "Soliditetsgrad_GNS")

ggplot(SoliDF, aes(x = Group, y = Soliditetsgrad_GNS, fill = Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#002E6D", "#FAB958", "#A3E1CE")) + # Tilpas farver
  labs(
    title = "Virksomheder med en højere soliditetsgrad ser mere positivt på deres lånemuligheder",
    x = "Vurdering",
    y = "Gennemsnitlig soliditetsgrad"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5), # Centreret og fed overskrift
    panel.background = element_blank(),    # Transparent panel
    plot.background = element_blank(),     # Transparent plot baggrund
    legend.background = element_blank()    # Transparent legend baggrund
  )

# Gem plottet med transparent baggrund
ggsave("soliditetsgrad_plot.png", bg = "transparent", width = 12, height = 6, dpi = 300)


###### Balance ######
DFCLM$gruppering <- ifelse(DFCLM$Svar %in% c("Dårlige", "Meget dårlige"), 
                           "Negative - Dårlige / Meget Dårlige",
                           ifelse(DFCLM$Svar %in% c("Gode", "Meget gode"),
                                  "Positive - Gode / Meget gode",
                                  "Neutral"))
DFCLM$Balance_kategori <- cut(
  DFCLM$Balance,
  breaks = c(0, 1000, 10000, 100000, Inf), # Intervallerne er nu i 1.000 DKK
  labels = c("0-1M", "1M-10M", "10M-100M", ">100M"), # Opdaterede labels
  include.lowest = TRUE)



BalaDF <- as.data.frame(table(DFCLM$Balance_kategori, DFCLM$gruppering))
colnames(BalaDF) <- c("Balance_Interval", "Gruppe", "Antal_Svar")
BalaDF$Procent <- round((BalaDF$Antal_Svar / sum(BalaDF$Antal_Svar)) * 100, 1)

ggplot(BalaDF, aes(x = Balance_Interval, y = Procent, fill = Gruppe)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#002E6D", "#FAB958", "#A3E1CE")) + # Farver til grupper
  labs(
    title = "Ingen virksomheder med Balance >100M mener andet end positivt om lånemuligheder",
    x = "Balance Intervaller (DKK)",
    y = "Procent (%)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5), # Centreret overskrift
    panel.background = element_blank(),    # Transparent panel baggrund
    plot.background = element_blank(),     # Transparent plot baggrund
    legend.background = element_blank()    # Transparent legend baggrund
  )

# Tilføj en ny kolonne for log-transformeret Balance
DFCLM$Log_Balance <- log(DFCLM$Balance + 1) # +1 for at undgå log(0)

# Beregn gennemsnitlig log(Balance) for hver gruppe
LogBalDF <- aggregate(Log_Balance ~ gruppering, data = DFCLM, FUN = mean)
colnames(LogBalDF) <- c("Gruppe", "Log_Balance_GNS")

# Plot gennemsnitlig log(Balance)
ggplot(LogBalDF, aes(x = Gruppe, y = Log_Balance_GNS, fill = Gruppe)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("#A3E1CE","#FAB958","#002E6D")) +
  labs(
    title = "En højere log(balance), gør virksomhederne mere positive",
    x = "Svargruppe",
    y = "log(Balance)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5),
    panel.background = element_blank(),    # Transparent panel baggrund
    plot.background = element_blank(),     # Transparent plot baggrund
    legend.background = element_blank()    # Transparent legend baggrund
  )

# Gem plottet med transparent baggrund
ggsave("log_balance_plot.png", bg = "transparent", width = 12, height = 8, dpi = 300)


# Gem plottet som PNG med transparent baggrund
ggsave("balance_plot.png", bg = "transparent", width = 16, height = 10, dpi = 300)



###### Afkastningsgrad ######
AfkaDF <- aggregate(Dataframe$AFKAST, list(Dataframe$gruppering), FUN=mean) # Skal være list
colnames(AfkaDF) <- c("Group", "AFKAST_GNS")

ggplot(AfkaDF, aes(x = Group, y = AFKAST_GNS, fill = Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#002E6D", "#FAB958", "#A3E1CE")) + # Tilpas farver
  labs(
    title = "Ingen logisk sammenhæng ved Afkastningsgrad",
    x = "Vurdering",
    y = "Gennemsnitlig Afkastningsgrad"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5), # Centreret overskrift
    panel.background = element_blank(),    # Transparent panel baggrund
    plot.background = element_blank(),     # Transparent plot baggrund
    legend.background = element_blank()    # Transparent legend baggrund
  )

# Gem plottet som PNG med transparent baggrund
ggsave("afkastningsgrad_plot.png", bg = "transparent", width = 12, height = 9, dpi = 300)


###### Resultat ######

DFCLM$RESULTAT_kategori <- cut(
  DFCLM$RESULTAT,
  breaks = c(-Inf, 0, 1000, 10000, Inf), # Tilføj et negativt interval
  labels = c("<0", "0-1M", "1M-10M", ">10M"), # Opdaterede labels
  include.lowest = TRUE
)

# Beregn antal svar og procent for hver RESULTAT-kategori
ResultDF <- as.data.frame(table(DFCLM$RESULTAT_kategori, DFCLM$gruppering))
colnames(ResultDF) <- c("Resultat_Interval", "Gruppe", "Antal_Svar")

# Beregn procenter
ResultDF$Procent <- round((ResultDF$Antal_Svar / sum(ResultDF$Antal_Svar)) * 100, 1)

# Plot resultat som procent
ggplot(ResultDF, aes(x = Resultat_Interval, y = Procent, fill = Gruppe)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#002E6D", "#FAB958", "#A3E1CE")) + # Farver til grupper
  labs(
    title = "Højere resultat, giver bedre låne muligheder",
    x = "Resultat",
    y = "Procent (%)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5) # Centreret overskrift
  )

###### Alder ######
# Opret ALDER-intervaller (i dage)
DFCLM$ALDER_kategori <- cut(
  DFCLM$ALDER,
  breaks = c(365, 7300, 14600, Inf), # Nye intervaller: 1-20 år, 20-40 år, >40 år
  labels = c("1-20 år", "20-40 år", ">40 år"), # Labels til intervaller
  include.lowest = TRUE
)

# Beregn antal svar og procent for hver ALDER-kategori
AlderDF <- as.data.frame(table(DFCLM$ALDER_kategori, DFCLM$gruppering))
colnames(AlderDF) <- c("Alder_Interval", "Gruppe", "Antal_Svar")

# Beregn procenter
AlderDF$Procent <- round((AlderDF$Antal_Svar / sum(AlderDF$Antal_Svar)) * 100, 1)




DFCLM$EGENKAPITAL_kategori <- cut(
  DFCLM$Egenkapital,
  breaks = c(0, 10000, 1000000, Inf), # Samlet de sidste to intervaller
  labels = c("0 - 10.000", "10.000 - 1.000.000", ">1.000.000"), # Opdaterede labels
  include.lowest = TRUE
)
# Plot ALDER som procent
ggplot(AlderDF, aes(x = Alder_Interval, y = Procent, fill = Gruppe)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#002E6D", "#FAB958", "#A3E1CE")) + # Farver til grupper
  labs(
    title = "Differencen mellem neutral og positive bliver størrere med alderen på virksomheden",
    x = "Aldersgruppe",
    y = "Procent (%)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5), # Centreret overskrift
    panel.background = element_blank(),    # Transparent panel baggrund
    plot.background = element_blank(),     # Transparent plot baggrund
    legend.background = element_blank()    # Transparent legend baggrund
  )

# Gem plottet som PNG med transparent baggrund
ggsave("alder_plot.png", bg = "transparent", width = 16, height = 12, dpi = 300)


EgenkapitalDF <- as.data.frame(table(DFCLM$EGENKAPITAL_kategori, DFCLM$gruppering))
colnames(EgenkapitalDF) <- c("Egenkapital_Interval", "Gruppe", "Antal_Svar")

# Beregn procenter
EgenkapitalDF$Procent <- round((EgenkapitalDF$Antal_Svar / sum(EgenkapitalDF$Antal_Svar)) * 100, 1)

# Plot EGENKAPITAL som procent
ggplot(EgenkapitalDF, aes(x = Egenkapital_Interval, y = Procent, fill = Gruppe)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#002E6D", "#FAB958", "#A3E1CE")) + # Farver til grupper
  labs(
    title = "Størrere egenkapital resultere i markant bedre lånemuligheder",
    x = "Egenkapital",
    y = "Procent (%)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5) # Centreret overskrift
  )


#### Kommentar/Live codning fra eksmane ####
# Præsentationslink
# https://www.canva.com/design/DAGZlIBk3IY/2EuRKh8xFDjh_dIb-QD1Fg/edit?utm_content=DAGZlIBk3IY&utm_campaign=designshare&utm_medium=link2&utm_source=sharebutton


# Se om der er correlation mellem Alder og Log_Balance
# Dette er gjort, for at lave om på det plot, opdelt ved Alder i Canva
ALderDF1 <- aggregate(DFCLM$ALDER, list(DFCLM$gruppering), FUN=mean)
ALderDF1$x <- ALderDF1$x / 365

DFCLM$AlderDage <- round(DFCLM$ALDER / 365,2)
cor <- cor(log(DFCLM$AlderDage), DFCLM$Log_Balance)



