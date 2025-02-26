#### Toto je skript pro nalosov�n� pojm� pro test z v�zkumn�ch ot�zek

## Encoding: windows-1250
## Vytvo�il: 2021-10-04 FrK
## Upravil:  2021-10-14 FrK

## NOTES:
#



# Hlavi�ka ----------------------------------------------------------------

# Vy�i�t�n� pam�ti
rm(list = ls())

# Package
library(dplyr)
library(tibble)
library(tidyr)
library(readxl)
library(writexl)

# Moje vlastn� funkce
source("D:/ownCloud2/MujSkript.R")

# Vektor s pojmy a dal�� pot�ebn� prom�nn� --------------------------------

pojmy = c("pohlav�", "v�k", "vzd�l�n�", "p��jem", "po�et d�t�", "po�et osob v dom�cnosti",
          "po�et knih v dom�c� knihovn�",
          "d�lka sp�nku", "zdravotn� stav", "spokojenost se �ivotem",

          "m�ra samostatnosti", "znalost soc. slu�eb", "intenzita u��v�n� soc. slu�eb",
          "frekvence u��v�n� soc. slu�eb", "spokojenost se soc. slu�bami", "z�jem o politiku",
          "vlastnictv� mobiln�ho telefonu", "vlastnictv� TV p�ij�ma�e", "intenzita sledov�n� TV",
          "intenzita u��v�n� soci�ln�ch s�t�",

          "uplatn�n� vlastn� v�le p�i �e�en� situace",
          "p�edsudky", "lidsk� pr�va", "osobn� svobody",
          "znalost pr�vn�ch norem",
          "znalost vlastn�ch pr�v",
          "p�ij�m�n� dar�", "d�v�n� dar�",
          "podm�nky poskytov�n� slu�by",
          "mo�nosti poskytov�n� slu�by",

          "osobn� c�le", "informovanost", "porozum�n� obsahu a ��elu smlouvy",
          "forma st�nosti", "zp�sob vy��zen� st�nosti",
          "informovanost zam�stnanc� o mo�nostech st�nost�",
          "evidence st�nost�", "pro�et�en� vy�izov�n� st�nost�",
          "p��le�itost vyu��t b�n� dostupn� ve�ejn� slu�by",
          "zprost�edkov�n� slu�eb",

          "kontakty s p�irozen�m soci�ln�m prost�ed�m",
          "vztahy s p�irozen�m soci�ln�m prost�ed�m",
          "po�et zam�stnanc�", "po�et �vazk� zam�stnanc�",
          "kvalifika�n� po�adavky na zam�stnance",
          "osobnostn� p�edpoklady zam�stnanc�",
          "za�kolov�n� nov�ch zam�stnanc�", "kapacita slu�by",
          "pravideln� hodnocen� zam�stnanc�",
          "program dal��ho vzd�l�v�n� zam�stnanc�",

          "syst�m finan�n�ho oce�ov�n� zam�stnanc�",
          "syst�m mor�ln�ho oce�ov�n� zam�stnanc�",
          "podpora zam�stnanc� od nez�visl�ho odborn�ka",
          "m�sto poskytov�n� slu�by",
          "�as poskytov�n� slu�by",
          "materi�ln� podm�nky slu�by", "technick� podm�nky slu�by",
          "hygienick� podm�nky slu�by",
          "d�stojn� prost�ed�", "nouzov� situace",

          "havarijn� situace", "�e�en� situace",
          "sezn�men� zam�stnanc� s �e�en�m nouzov� situace",
          "sezn�men� klient� s �e�en�m nouzov� situace",
          "sezn�men� klient� s �e�en�m havarijn� situace",
          "kvalita slu�by", "posl�n� slu�by", "c�le slu�by",
          "odcizen�", "soci�ln� struktura",

          "soci�ln� akt�r", "b�da", "blahobyt", "masa",
          "cirkulace elit", "marginalizace", "dav", "d�dictv�",
          "disent", "diskriminace",

          "distance", "degradace", "diferenciace", "dynamika", "elita", "gang",
          "soci�ln� hierarchie", "status", "statusov� inkonzistence", "instituce",

          "inteligence", "kari�ra", "kasta", "klan", "kmen", "kolektiv", "klientelismus",
          "kolektiv", "komunita", "krystalizace statusu",

          "meritokracie", "soci�ln� mobilita", "soci�ln� nerovnost(i)", "organizace",
          "privilegium", "p��buzenstv�", "rodina", "soci�ln� s�",
          "skupina", "sousedstv�",

          "spole�enstv�", "spole�nost", "spolek", "spravedlnost", "stabilita", "st�t",
          "politick� strana", "stratifikace", "t��da", "adaptace",

          "agrese", "altruismus", "alokace", "askeze", "aspirace", "atraktivita",
          "autorita", "stereotyp", "dar", "d�stojnost",

          "d�v�ra", "dysfunkce", "egoismus", "euforie", "existence", "exkluze", "funkce",
          "harmonie", "hodnota", "hra",

          "charisma", "chov�n�", "individualismus", "indoktrinace", "infiltrace",
          "internalizace", "jedn�n�", "kapit�l", "kulturn� kapit�l", "ekonomick� kapit�l",

          "soci�ln� kapit�l", "spolupr�ce", "sout�", "kompetence", "konflikt", "role",
          "konflikt rol�", "konformita", "konsensus (shoda)", "kontrola",

          "konvence", "kritika", "korupce", "loajalita", "motivace", "mravnost",
          "mor�lka", "n�sil�", "norma", "nad�je",

          "odm�na", "trest", "odpov�dnost", "organizace", "otev�enost", "orientace",
          "ovliv�ov�n�", "pam�", "poslu�nost", "postoj",

          "potenci�l", "pot�eba", "pov�st", "povinnost", "praxe", "preference", "presti�",
          "prost�ed�", "p��telstv�", "p�edsudek",

          "racionalita", "reciprocita", "regulace", "revoluce", "rezignace", "rovnov�ha",
          "rozhodov�n�", "��d", "sankce", "socializace",

          "solidarita", "strategie", "svoboda", "syst�m", "tabu", "tolerance", "tradice",
          "soci�ln� u�en�", "vlastnost", "vztah", "z�jem"
          )  # Seznam pojm�

length(pojmy)  # kontrola po�tu pojm�
nchar(pojmy)  # kontrola d�lky pojm�
nchar(pojmy) %>% max()



# Vytvo�en� seznam� studuj�c�ch -------------------------------------------

# sud� t�den
sud� = read_xls("seznamMTV1.xls") %>% select(1:3, email) %>%
  add_case(read_xls("seznamMV1Y-sudy.xls") %>% select(1:3, email))

# lich� t�den
lich� = read_xls("seznamMV1.xls") %>% select(1:3, email) %>%
  add_case(read_xls("seznamMV1Y-lichy.xls") %>% select(1:3, email))



# Vylosov�n� --------------------------------------------------------------

# Aby bylo losov�n� 'reproducible', mus�me zafixovat random seed:
set.seed(20211005)  # V�dy kdy� d�l�m test, d�m sem datum, kdy je test spu�t�n

# NOTE: Mus�me ud�lat nejd��v sud� a potom lich� t�den.

## Sud� t�den
# P�edem si vytvo��me pr�zdnou matici pro losovan� pojmy
los = matrix(rep(".", 11 * nrow(sud�)), ncol = 11) %>% as_tibble()

# A cyklem to napln�me:
for (i in 1:nrow(sud�)) {
  vx = sample(pojmy, 10)
  for (j in 1:10) {
    los[i, (j+1)] = vx[j]
  }
  los[i, 1] = paste0(vx, collapse = ", ")
}
names(los) = c("c4s", paste0("P", 1:10))

# Spoj�me 'administrativn� data' s pojmy:
df = tibble(sud�, los)

# Ulo�en� losova�ky

write_xlsx(df, "test01_2021-10-05.xlsx")



## Lich� t�den
# Aby bylo losov�n� 'reproducible', mus�me zafixovat random seed:
set.seed(20211012)  # V�dy kdy� d�l�m test, d�m sem datum, kdy je test spu�t�n

# NOTE: Mus�me ud�lat nejd��v sud� a potom lich� t�den.

# P�edem si vytvo��me pr�zdnou matici pro losovan� pojmy
los = matrix(rep(".", 11 * nrow(lich�)), ncol = 11) %>% as_tibble()

# A cyklem to napln�me:
for (i in 1:nrow(lich�)) {
  vx = sample(pojmy, 10)
  for (j in 1:10) {
    los[i, (j+1)] = vx[j]
  }
  los[i, 1] = paste0(vx, collapse = ", ")
}
names(los) = c("c4s", paste0("P", 1:10))

# Spoj�me 'administrativn� data' s pojmy:
df = tibble(lich�, los)

# Ulo�en� losova�ky
write_xlsx(df, "test01_2021-10-12.xlsx")



# P��prava odpov�d� na bodov�n� -------------------------------------------

# Na�ten� odpov�d�
df = read_xlsx("01OtazkyMv1_2021-10-13.xlsx") %>%

  # Ponech�n� identifika�n�ho e-mailu a v�cn�ch prom�nn�ch
  select(-c(1:10, 12:33)) %>%

  # P�ejmenov�n� v�cn�ch prom�nn�ch, aby se s nimi dob�e d�l pracovalo
  prejmenuj(2:7, c("Pojem.1", "V�zkumn� ot�zka.1", "Pojem.2", "V�zkumn� ot�zka.2", "Pojem.3", "V�zkumn� ot�zka.3")) %>%

  # Pokud n�kdo nevyplnil ani prvn� v�zkumnou ot�zku, jeho data odstran�me
  filter(!is.na(`V�zkumn� ot�zka.1`)) %>%

  # P�evedeme data do dlouh� formy a odd�l�me v n�zvu typ �daje od jeho po�ad�
  pivot_longer(2:7) %>% separate(name, into = c("Typ", "Po�ad�"), sep = "\\.") %>%

  # Nyn� jsou pojem a ot�zka na r�zn�ch ��dc�ch, t�mto je d�me na spole�n� ��dek
  pivot_wider(id_cols = c(email, Typ, Po�ad�), names_from = Typ) %>%

  # Vytvo��me si nov� pr�zdn� prom�nn�, kam ru�n� dop�u body (Body) a p��padn� koment�� (Koment)
  mutate(Body = NA_real_, Koment = NA_character_)


# Ulo�en� zpracovan�ch odpov�d� k bodov�n�
write_xlsx(df, "01OtazkyMV1_kBodovani.xlsx")

## NOTE: Soubor s odpov��mi p�ipraven� k bodov�n� je t�eba nyn� ru�n� v Excelu otev��t,
##       ru�n� obodovat ka�dou ot�zku, okomentovat ot�zky a ulo�it soubor pod nov�m jm�nem:
##       '01OtazkyMV1_obodovano.xlsx'.



# Na�ten� opraven�ch v�sledk� a p��prava dopisu ---------------------------------------------

# Na�ten� zpracovan�ch odpov�d� k bodov�n�
podklad = read_xlsx("01OtazkyMV1_obodovano.xlsx") %>%

  # P��prava shrnut� hodnocen� jednotliv�ch ot�zek
  group_by(email) %>%
  mutate(
    Celkem = sum(Body),
    Text = paste0(
      "Jako pojem �. ", Po�ad� ," jste zvolil/a: \n'", Pojem,
      "', \n\nten jste pou�il/a ve v�zkumn� ot�zce takto: \n'", `V�zkumn� ot�zka`,
      "' \n\nZa tuto v�zkumnou ot�zku m�te ", Body, " bod/u/�.\n\n",
      "M�j koment�� k t�to v�zkumn� ot�zce je:\n", Koment),
    Dopis = paste0(Text, collapse = "\n----------------------------------------------------\n"))

# P��prava fin�ln�ch dopis�
dopisy = podklad %>% select(email, Celkem, Dopis) %>% unique() %>%
  mutate(Dopis = paste0(
    "Mil�/� studuj�c�, \n\n\n\nzas�l�m V�m vyhodnocen� prvn�ho pr�b�n�ho testu ",
    "ze cvi�en�/semin��e p�edm�tu KSA-MVT1/KSS-MV1/KSS-MV1Y, a to testu na formulaci v�zkumn�ch ot�zek. ",
    "Pokud v��i n�mu m�te v�hrady, nev�hejte mne kontaktovat.\n\n",
    "SHRNUT� TESTU:\n Celkem z testu m�te: ",
    Celkem, " bod�/bodu/body.",
    "\n\n ******************* R E K A P I T U L A C E   T E S T U *******************\n\n",
    Dopis,
    "\n\n *******************  \n\n\n\nS �ctou,\nFranti�ek"))



# Ulo�en� dopis� a v�sledk� -----------------------------------------------

# Dopisy
write_xlsx(dopisy, "dopisy.xlsx")

# V�sledky
dopisy %>% select(-Dopis) %>%
  write_xlsx("01OtazkyMV1_Vysledky_2021-10-15.xlsx")


# ### KONEC ### -----------------------------------------------------------



