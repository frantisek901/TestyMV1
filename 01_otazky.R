#### Toto je skript pro nalosování pojmù pro test z výzkumných otázek

## Encoding: windows-1250
## Vytvoøil: 2021-10-04 FrK
## Upravil:  2021-10-14 FrK

## NOTES:
#



# Hlavièka ----------------------------------------------------------------

# Vyèištìní pamìti
rm(list = ls())

# Package
library(dplyr)
library(tibble)
library(tidyr)
library(readxl)
library(writexl)

# Moje vlastní funkce
source("D:/ownCloud2/MujSkript.R")

# Vektor s pojmy a další potøebné promìnné --------------------------------

pojmy = c("pohlaví", "vìk", "vzdìlání", "pøíjem", "poèet dìtí", "poèet osob v domácnosti",
          "poèet knih v domácí knihovnì",
          "délka spánku", "zdravotní stav", "spokojenost se životem",

          "míra samostatnosti", "znalost soc. služeb", "intenzita užívání soc. služeb",
          "frekvence užívání soc. služeb", "spokojenost se soc. službami", "zájem o politiku",
          "vlastnictví mobilního telefonu", "vlastnictví TV pøijímaèe", "intenzita sledování TV",
          "intenzita užívání sociálních sítí",

          "uplatnìní vlastní vùle pøi øešení situace",
          "pøedsudky", "lidská práva", "osobní svobody",
          "znalost právních norem",
          "znalost vlastních práv",
          "pøijímání darù", "dávání darù",
          "podmínky poskytování služby",
          "možnosti poskytování služby",

          "osobní cíle", "informovanost", "porozumìní obsahu a úèelu smlouvy",
          "forma stížnosti", "zpùsob vyøízení stížnosti",
          "informovanost zamìstnancù o možnostech stížností",
          "evidence stížností", "prošetøení vyøizování stížností",
          "pøíležitost využít bìžnì dostupné veøejné služby",
          "zprostøedkování služeb",

          "kontakty s pøirozeným sociálním prostøedím",
          "vztahy s pøirozeným sociálním prostøedím",
          "poèet zamìstnancù", "poèet úvazkù zamìstnancù",
          "kvalifikaèní požadavky na zamìstnance",
          "osobnostní pøedpoklady zamìstnancù",
          "zaškolování nových zamìstnancù", "kapacita služby",
          "pravidelné hodnocení zamìstnancù",
          "program dalšího vzdìlávání zamìstnancù",

          "systém finanèního oceòování zamìstnancù",
          "systém morálního oceòování zamìstnancù",
          "podpora zamìstnancù od nezávislého odborníka",
          "místo poskytování služby",
          "èas poskytování služby",
          "materiální podmínky služby", "technické podmínky služby",
          "hygienické podmínky služby",
          "dùstojné prostøedí", "nouzová situace",

          "havarijní situace", "øešení situace",
          "seznámení zamìstnancù s øešením nouzové situace",
          "seznámení klientù s øešením nouzové situace",
          "seznámení klientù s øešením havarijní situace",
          "kvalita služby", "poslání služby", "cíle služby",
          "odcizení", "sociální struktura",

          "sociální aktér", "bída", "blahobyt", "masa",
          "cirkulace elit", "marginalizace", "dav", "dìdictví",
          "disent", "diskriminace",

          "distance", "degradace", "diferenciace", "dynamika", "elita", "gang",
          "sociální hierarchie", "status", "statusová inkonzistence", "instituce",

          "inteligence", "kariéra", "kasta", "klan", "kmen", "kolektiv", "klientelismus",
          "kolektiv", "komunita", "krystalizace statusu",

          "meritokracie", "sociální mobilita", "sociální nerovnost(i)", "organizace",
          "privilegium", "pøíbuzenství", "rodina", "sociální sí",
          "skupina", "sousedství",

          "spoleèenství", "spoleènost", "spolek", "spravedlnost", "stabilita", "stát",
          "politická strana", "stratifikace", "tøída", "adaptace",

          "agrese", "altruismus", "alokace", "askeze", "aspirace", "atraktivita",
          "autorita", "stereotyp", "dar", "dùstojnost",

          "dùvìra", "dysfunkce", "egoismus", "euforie", "existence", "exkluze", "funkce",
          "harmonie", "hodnota", "hra",

          "charisma", "chování", "individualismus", "indoktrinace", "infiltrace",
          "internalizace", "jednání", "kapitál", "kulturní kapitál", "ekonomický kapitál",

          "sociální kapitál", "spolupráce", "soutìž", "kompetence", "konflikt", "role",
          "konflikt rolí", "konformita", "konsensus (shoda)", "kontrola",

          "konvence", "kritika", "korupce", "loajalita", "motivace", "mravnost",
          "morálka", "násilí", "norma", "nadìje",

          "odmìna", "trest", "odpovìdnost", "organizace", "otevøenost", "orientace",
          "ovlivòování", "pamì", "poslušnost", "postoj",

          "potenciál", "potøeba", "povìst", "povinnost", "praxe", "preference", "prestiž",
          "prostøedí", "pøátelství", "pøedsudek",

          "racionalita", "reciprocita", "regulace", "revoluce", "rezignace", "rovnováha",
          "rozhodování", "øád", "sankce", "socializace",

          "solidarita", "strategie", "svoboda", "systém", "tabu", "tolerance", "tradice",
          "sociální uèení", "vlastnost", "vztah", "zájem"
          )  # Seznam pojmù

length(pojmy)  # kontrola poètu pojmù
nchar(pojmy)  # kontrola délky pojmù
nchar(pojmy) %>% max()



# Vytvoøení seznamù studujících -------------------------------------------

# sudý týden
sudý = read_xls("seznamMTV1.xls") %>% select(1:3, email) %>%
  add_case(read_xls("seznamMV1Y-sudy.xls") %>% select(1:3, email))

# lichý týden
lichý = read_xls("seznamMV1.xls") %>% select(1:3, email) %>%
  add_case(read_xls("seznamMV1Y-lichy.xls") %>% select(1:3, email))



# Vylosování --------------------------------------------------------------

# Aby bylo losování 'reproducible', musíme zafixovat random seed:
set.seed(20211005)  # Vždy když dìlám test, dám sem datum, kdy je test spuštìn

# NOTE: Musíme udìlat nejdøív sudý a potom lichý týden.

## Sudý týden
# Pøedem si vytvoøíme prázdnou matici pro losované pojmy
los = matrix(rep(".", 11 * nrow(sudý)), ncol = 11) %>% as_tibble()

# A cyklem to naplníme:
for (i in 1:nrow(sudý)) {
  vx = sample(pojmy, 10)
  for (j in 1:10) {
    los[i, (j+1)] = vx[j]
  }
  los[i, 1] = paste0(vx, collapse = ", ")
}
names(los) = c("c4s", paste0("P", 1:10))

# Spojíme 'administrativní data' s pojmy:
df = tibble(sudý, los)

# Uložení losovaèky

write_xlsx(df, "test01_2021-10-05.xlsx")



## Lichý týden
# Aby bylo losování 'reproducible', musíme zafixovat random seed:
set.seed(20211012)  # Vždy když dìlám test, dám sem datum, kdy je test spuštìn

# NOTE: Musíme udìlat nejdøív sudý a potom lichý týden.

# Pøedem si vytvoøíme prázdnou matici pro losované pojmy
los = matrix(rep(".", 11 * nrow(lichý)), ncol = 11) %>% as_tibble()

# A cyklem to naplníme:
for (i in 1:nrow(lichý)) {
  vx = sample(pojmy, 10)
  for (j in 1:10) {
    los[i, (j+1)] = vx[j]
  }
  los[i, 1] = paste0(vx, collapse = ", ")
}
names(los) = c("c4s", paste0("P", 1:10))

# Spojíme 'administrativní data' s pojmy:
df = tibble(lichý, los)

# Uložení losovaèky
write_xlsx(df, "test01_2021-10-12.xlsx")



# Pøíprava odpovìdí na bodování -------------------------------------------

# Naètení odpovìdí
df = read_xlsx("01OtazkyMv1_2021-10-13.xlsx") %>%

  # Ponechání identifikaèního e-mailu a vìcných promìnných
  select(-c(1:10, 12:33)) %>%

  # Pøejmenování vìcných promìnných, aby se s nimi dobøe dál pracovalo
  prejmenuj(2:7, c("Pojem.1", "Výzkumná otázka.1", "Pojem.2", "Výzkumná otázka.2", "Pojem.3", "Výzkumná otázka.3")) %>%

  # Pokud nìkdo nevyplnil ani první výzkumnou otázku, jeho data odstraníme
  filter(!is.na(`Výzkumná otázka.1`)) %>%

  # Pøevedeme data do dlouhé formy a oddìlíme v názvu typ údaje od jeho poøadí
  pivot_longer(2:7) %>% separate(name, into = c("Typ", "Poøadí"), sep = "\\.") %>%

  # Nyní jsou pojem a otázka na rùzných øádcích, tímto je dáme na spoleèný øádek
  pivot_wider(id_cols = c(email, Typ, Poøadí), names_from = Typ) %>%

  # Vytvoøíme si nové prázdné promìnné, kam ruènì dopíšu body (Body) a pøípadný komentáø (Koment)
  mutate(Body = NA_real_, Koment = NA_character_)


# Uložení zpracovaných odpovìdí k bodování
write_xlsx(df, "01OtazkyMV1_kBodovani.xlsx")

## NOTE: Soubor s odpovìïmi pøipravený k bodování je tøeba nyní ruènì v Excelu otevøít,
##       ruènì obodovat každou otázku, okomentovat otázky a uložit soubor pod novým jménem:
##       '01OtazkyMV1_obodovano.xlsx'.



# Naètení opravených výsledkù a pøíprava dopisu ---------------------------------------------

# Naètení zpracovaných odpovìdí k bodování
podklad = read_xlsx("01OtazkyMV1_obodovano.xlsx") %>%

  # Pøíprava shrnutí hodnocení jednotlivých otázek
  group_by(email) %>%
  mutate(
    Celkem = sum(Body),
    Text = paste0(
      "Jako pojem è. ", Poøadí ," jste zvolil/a: \n'", Pojem,
      "', \n\nten jste použil/a ve výzkumné otázce takto: \n'", `Výzkumná otázka`,
      "' \n\nZa tuto výzkumnou otázku máte ", Body, " bod/u/ù.\n\n",
      "Mùj komentáø k této výzkumné otázce je:\n", Koment),
    Dopis = paste0(Text, collapse = "\n----------------------------------------------------\n"))

# Pøíprava finálních dopisù
dopisy = podklad %>% select(email, Celkem, Dopis) %>% unique() %>%
  mutate(Dopis = paste0(
    "Milá/ý studující, \n\n\n\nzasílám Vám vyhodnocení prvního prùbìžného testu ",
    "ze cvièení/semináøe pøedmìtu KSA-MVT1/KSS-MV1/KSS-MV1Y, a to testu na formulaci výzkumných otázek. ",
    "Pokud vùèi nìmu máte výhrady, neváhejte mne kontaktovat.\n\n",
    "SHRNUTÍ TESTU:\n Celkem z testu máte: ",
    Celkem, " bodù/bodu/body.",
    "\n\n ******************* R E K A P I T U L A C E   T E S T U *******************\n\n",
    Dopis,
    "\n\n *******************  \n\n\n\nS úctou,\nFrantišek"))



# Uložení dopisù ----------------------------------------------------------

write_xlsx(dopisy, "dopisy.xlsx")



# ### KONEC ### -----------------------------------------------------------



