#### Tento skript slouží ke zpracování výsledkù z testù na KSS/PV1


## FrK vytvoøil 2021-04-05
## FrK upravil  2021-04-05
## encoding: windows-1250

# Head
rm(list=ls())

## Packages
library(dplyr)
library(readr)
library(readxl)
library(writexl)
library(tidyr)
library(ggplot2)
library(tibble)


### Naètení odpovìdí z jednoho souboru
raw = read_xlsx("jednaHypotezaJednaStrana.xlsx")
odp = raw[,c(12, 31:50)]
names(odp)[seq(2, 20, 2)] = paste0("get", 1:10) 
names(odp)[seq(1, 21, 2)] = c("email", paste0("hyp", 1:10)) 
odp

### Naètení odpovìdí z druhého souboru
raw2 = read_xlsx("vseNaJedneStrane.xlsx")
odp2 = raw2[,c(12, 21, 31:35, 37:41)]
names(odp2)[3:12] = paste0("hyp", 1:10) 
names(odp2)[1:2] = c("email", "get1") 
odp2


### Spojení dat
odpAll = bind_rows(odp, odp2) %>% select(email, contains("get"), contains("hyp")) %>% 
  add_column(bod1 = 0, .after = "hyp1") %>% 
  add_column(bod2 = 0, .after = "hyp2") %>% 
  add_column(bod3 = 0, .after = "hyp3") %>% 
  add_column(bod4 = 0, .after = "hyp4") %>% 
  add_column(bod5 = 0, .after = "hyp5") %>% 
  add_column(bod6 = 0, .after = "hyp6") %>% 
  add_column(bod7 = 0, .after = "hyp7") %>% 
  add_column(bod8 = 0, .after = "hyp8") %>% 
  add_column(bod9 = 0, .after = "hyp9") %>% 
  add_column(bod10 = 0, .after = "hyp10") 


### Uložení do souboru k obodování
write_xlsx(odpAll, "kBodovani.xlsx")



### Naètení obodovaných výsledkù
vys = read_xlsx("obodovano.xlsx") %>% select(email, contains("bod"), contains("hyp")) 
names(vys) = c("email", paste0("bod.", 1:10), paste0("hyp.", 1:10))
vys = vys %>%  mutate_at(vars(contains("bod")), ~as.character(.)) %>% 
  pivot_longer(cols = 2:21) %>%   # Abychom nedìlali vše 10x, reshapnu otázku, øešení a odpovìdi. Nejdøív vše najednou... 
  separate(col = name, into = c("name", "poradi")) %>%  # Teï oddìlíme promìnnou a poøadové èíslo
  pivot_wider(id_cols = c(email, poradi)) %>%  # Teï koneènì vydìlíme tøi promìnné: otázku, odpovìï a øešení
  mutate(bod = as.numeric(bod)) %>%   # Teï urèím pomocí funkce 'bod()' body za každou otázku
  group_by(email) %>% mutate(
    celkem = ifelse(poradi == 1, sum(bod), NA),
    koment = paste0(
      "Hypotézu è. ", poradi," jste formuloval/a takto: \n''", hyp, 
      "'' \n\ntato hypotéza je ", ifelse(bod == 1, "správná", "chybná"),
      # " oznaèil/a ", ifelse(nchar(prevod(odp)) == 1, "odpovìï ", "odpovìdi "), prevod(odp),
      # ".\nSprávnì ", ifelse(nchar(prevod(UP)) == 1, "je odpovìï ", "jsou odpovìdi "), prevod(UP),
      ", tudíž za ní máte ", ifelse(bod == 1, "1 bod.", "0 bodù.")),
    zaklad = paste0(koment, collapse = "\n----------------------------------------------------\n")) 

dopisy = vys %>% filter(!is.na(celkem)) %>% select(email, celkem, zaklad) %>% 
  mutate(zaklad = paste0(
    "Milá/ý studující, \n\n\n\nzasílám Vám automatické vyhodnocení testu z KSS/PV1. Pokud vùèi nìmu máte výhrady, neváhejte mne kontaktovat. ", 
    "Mìjte prosím na pamìti má dvì základní pravidla pro revizi hodnocení testù:\n",
    "1) budeme se bavit pouze o Vašem testu, pokud chcete jako argument použít srovnání s testem spolužáka/èky, tak jen pouze pokud chcete oba dva spoleènì revidovat hodnocení, \n",
    "2) pokud pøi revizi narazím na to, že jsem Vám dal omylem více bodù, tak o nì pøijdete, tj. revize pùsobí na obì strany, oba druhy omylù opravíme. ",
    "\n\n******************* S H R N U T Í   T E S T U *******************\n\nTest pro držitele/ku emailu: ",
    email, ". \nCelkem z testu máte: ",
    celkem, " bodù/body. \n\n\n\nS úctou,\nFrantišek",
    "\n\n********* P O D R O B N Á   R E K A P I T U L A C E   T E S T U *********\n\n",
    zaklad
  ))
# SUPER! Máme to vyhodnocené a máme i dopisy hotové!



## Odesílání personalizovaných výsledkù:
library(emayili)
library(dplyr)
library(magrittr)

for (d in 1:nrow(dopisy)) {
  email <- envelope() %>% 
    from("kalvas@kss.zcu.cz") %>% 
    to(dopisy$email[d]) %>% 
    subject(qp_encode("Automaticky vyhodnocené výsledky testu ze semináøù KSS/PV1")) %>% 
    text(qp_encode(dopisy$zaklad[d]))
  
  # print(email, details = T)
  
  smtp <- server(host = "smtp.zcu.cz",
                 port = 465,
                 username = "kalvas",
                 password = "Dingo933")
  
  smtp(email, verbose = TRUE)  
}



