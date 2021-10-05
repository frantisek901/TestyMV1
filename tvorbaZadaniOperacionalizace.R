## Skript pro vygenerování zadání studujícím na PV1

## FrK vytvoøil 2021-03-13
## FrK upravil  2021-05-11
## encoding: windows-1250


# Cílem je vybrat unikátních 40/50 pojmù a rozdìlit je do ètveøic/pìtic,
# a pak z každé ètveøice/pìtice udìlat jeden string, který se uloží do jednoho
# políèka matice, která bude mít tolik øádek, kolik je studentù a 10 sloupcù 
# (pro každé zadání jeden). A nakonec pùjde o to, pøipojit k nim sloupec s e-maily a
# celou tuto matici exportovat jako excelový soubor, který pak pùjde naèíst do C4S.
rm(list = ls())

### Package
library(dplyr)
library(writexl)
library(readxl)


### Vstupní objekty
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
          "podmínky poskytování sociální služby", 
          "možnosti poskytování sociální služby", 
          
          "informovanost o sociální službì", 
          "požadavky na soc. službu", 
          "odmítnutí poskytnutí soc. služby", 
          "uzavøení smlouvy o poskytování soc. služby",
          "porozumìní obsahu a úèelu smlouvy", 
          "sjednání rozsahu poskytování soc. služby",
          "sjednání prùbìhu poskytování soc. služby ", 
          "osobní cíle", "poslání soc. služby",
          "plánování sociální služby",
          
          "hodnocení naplnìní cílù soc. služby", 
          "dokumentace o poskytování sociální služby",
          "stížnosti na kvalitu soc. služby", 
          "stížnosti na zpùsob poskytování soc. služby",
          "informovanost o možnosti stížnosti", 
          "forma stížnosti", "zpùsob vyøízení stížnosti",
          "informovanost zamìstnancù o možnostech stížností",
          "evidence stížností",
          "prošetøení vyøizování stížností",
          
          "pøíležitost využít bìžnì dostupné veøejné služby",
          "zprostøedkování služeb", 
          "kontakty s pøirozeným sociálním prostøedím",
          "vztahy s pøirozeným sociálním prostøedím",
          "poèet zamìstnancù", "poèet úvazkù zamìstnancù",
          "kvalifikaèní požadavky na zamìstnance",
          "osobnostní pøedpoklady zamìstnancù",
          "zaškolování nových zamìstnancù", "kapacita sociální služby",
          
          "pravidelné hodnocení zamìstnancù", 
          "program dalšího vzdìlávání zamìstnancù", 
          "systém finanèního oceòování zamìstnancù",
          "systém morálního oceòování zamìstnancù",
          "podpora zamìstnancù od nezávislého odborníka",
          "místo poskytování soc. služby",
          "èas poskytování soc. služby",
          "materiální podmínky soc. služby", "technické podmínky soc. služby", 
          "hygienické podmínky soc. služby",
          
          "dùstojné prostøedí", "nouzová situace", "havarijní situace",
          "øešení situace", 
          "seznámení zamìstnancù s øešením nouzové situace",
          "seznámení klientù s øešením nouzové situace",
          "seznámení klientù s øešením havarijní situace",
          "kvalita sociální služby", "poslání soc. služby", "cíle soc. služby")  # Seznam pojmù
length(pojmy)  # kontrola poètu pojmù
nchar(pojmy)  # kontrola délky pojmù
nchar(pojmy) %>% max()
#maily = read_xls("getStudentiByPredmet-2021-03-16-6-35.xls") %>%  # Emaily studentù plus mùj
maily = read_xls("OpravyPV1.xls") %>%  
  select(email) %>% as.matrix() %>% as.vector() %>% c("kalvas@kss.zcu.cz", .)  
studs = length(maily)  # Poèet studujících

# Matice, která bude obsahovat zadání pro každého studenta, 
# 10 ètveøic/pìtic, v každé buòce jedna, tedy,
# matice má tolik øádek, kolik studentù, a 10 sloupcù jako 10 hypotéz.
zadaniTen = matrix(rep("0", studs * 10), ncol = 10)  
delky = matrix(rep(0, studs * 10), ncol = 10)  # Matice pro kontrolu délek stringù v 'zadaniTen'

# Matice, která obsahuje jedno zadání pro každého studenta, tj. 20 pojmù najednou,
# matice má tolik øádek, kolik studentù a 1 sloupec.
zadaniOne = matrix(rep("0", studs), ncol = 1)


### Cykly, které vygenerují matici se zadáním
for (s in 1:studs) {  # cyklus, který iteruje pøes všechny studující
  m = pojmy %>% sample(., 40, replace = F) %>% 
    matrix(., ncol = 4, byrow = F)  # Vylosování pojmù a jejich rozdìlení do 10 ètveøic/pìtic
  for(z in 1:10){  # cyklus, který iteruje pøes 10 zadání hypotéz u konkrétního studujícího 's'
    radek = m[z,]  # Vybereme pøíslušný øádek z matice s výbìrem pojmù
    zadaniTen[s, z] =  # Vytvoøíme string vèetnì html znaèek a uložíme do matice 'zadani'
      paste0("Operacionalizace ", z, ": ", paste0(paste0("<u>", radek[1:4], "</u>"), collapse = "; "))
  }
  v = pojmy %>% sample(., 20, replace = F)
  zadaniOne[s, ] = paste0(paste0("<u>", v, "</u>"), collapse = "; ")
}
delky = nchar(zadaniTen)
max(delky)
zadaniOne %>% nchar() %>% max()

### Pøipojení e-mailù
zadaniTen = cbind(maily, zadaniTen) 
colnames(zadaniTen) = paste0("V", 1:ncol(zadaniTen))
zadaniOne = cbind(maily, zadaniOne)
colnames(zadaniOne) = paste0("V", 1:ncol(zadaniOne))


### Export do excelu
dat = zadaniTen %>% as_tibble() 
names(dat) = c("email", paste0("oper", 1:10))
write_xlsx(dat, "TenOperOprava.xlsx")

dat = zadaniOne %>% as_tibble() 
names(dat) = c("email", "zadání")
write_xlsx(dat, "OneOperOprava.xlsx")


# dat = zadaniTen %>% as_tibble() 
# names(dat) = c("email", paste0("hyp", 1:10))
# write_xlsx(dat[1,], "TenTestOper.xlsx")
# 
# dat = zadaniOne %>% as_tibble() 
# names(dat) = c("email", "zadání")
# write_xlsx(dat[1,], "OneTestOper.xlsx")



