#### Skript pro vygenerování uživatelských dat pro obsah druhého testu ze semináøù ZS1(Y) na C4S

## Encoding: windows-1250
## Vytvoøil: 2021-03-30 FrK
## Upravil:  2021-05-05 FrK


# Head
rm(list=ls())

## Packages
library(dplyr)
library(readr)
library(readxl)
library(writexl)
library(tidyr)


## Naètení e-mailù:
maily = read_excel("UvodDoSociologie-Vsichni.xlsx") %>% filter(nazevSp != "Antropologie") %>% 
  select(email) %>%  # Naèteme jen maily
  filter(!is.na(email)) %>% distinct() %>%  as.matrix() %>% as.vector()  # Vyhodíme duplicity a pøevedeme na vektor


## Ruèní vytvoøení listù s texty otázek a odpovìdí, strukturovat to budeme podle:
## Textu, otázky, správné varianty, nesprávné varianty
t1 = list(  # List s otázkami a odpovìïmi na první text "SLUT!"
  "q01" = list(
    "Q01. O èem pojednává text Leory Tanenbaum 'Slut!'?",
    "a" = c("1" = "'Sexual double standard'", 
            "0" = "Magii", 
            "0" = "Míru mezi národy", "0" = "Váleèné zónì")),
  "q02" = list(
    "Q02. Proè bychom mìli odmítnou 'sexual doble standard', o kterém píše Leora Tanenbaum v textu 'Slut!'?",
    "a" = c("1" = "Pøedsudky jsou obecnì špatné", "1" = "'Sexual doble standard' nièí sexualitu dívek", 
            "1" = "Negativní dopady na další život dívek oznaèených jako 'slut'",
            "0" = "'Sexual doble standard' je sociální inženýrství", 
            "0" = "'Sexual doble standard' je pøíliš feministický"))
 
)



t2 = list(  # List s otázkami a odpovìïmi na druhý text
  "q05" = list(
    "Q05. O èem pojednává text Marka Snydera 'When Belif Creates Reality'?",
    "a" = c("0" = "O náboženství", "0" = "O iluzi humanismu", 
            "1" = "Jak první dojem ovlivòuje sociální interakci",
            "1" = "Jak naše dojmy pøedcházející interakci ovlivòují naše dojmy z interakce samotné", 
            "0" = "O magii"))
)

  

t3 = list(  # List s otázkami a odpovìïmi na první text
  "q08" = list(
    "Q08. Jakým zpùsobem pomáhají technologie podkopávat moc státu? (Johna Pollock 'How Egyptian and Tunisian youth hacked the Arab Spring')",
    "a" = c("1" = "Sdílí vtipy o režimu", 
            "1" = "Umožòuje udržovat anonymitu tìch, kteøí sdílí informace",
            "1" = "Sdílí informace, které by režim rád zatajil",
            "1" = "Sdílí informace o brutalitì zásahù moci", 
            "1" = "Sdílí organizaèní informace (zhruba kdy a kde se bude demonstrovat)",
            "1" = "Propojuje proti režimu naladìné lidi, které by režim rád držel oddìlené a bez kontaktu")),
  "q06" = list(
    "Q06. Jakou roli mìly technologie ve svržení Ben Aliho v Tunisu? (Johna Pollock 'How Egyptian and Tunisian youth hacked the Arab Spring')",
    "a" = c("1" = "Sdílení vtipy o režimu", 
            "1" = "Umožnily udržovat anonymitu tìch, kteøí sdílí informace",
            "1" = "Sdílení informací, které by režim rád zatajil",
            "1" = "Sdílení informací o brutalitì zásahù moci", 
            "1" = "Sdílení organizaèních informací (zhruba kdy a kde se bude demonstrovat)",
            "1" = "Propojení proti režimu naladìných lidí, které by režim rád držel oddìlené a bez kontaktu",
            "1" = "Návody jak vytvoøit Molotovùv koktejl atp.", 
            "1" = "Sociální sítì sloužily jako záznam, kde se co dìje",
            "1" = "Sdílení nìkterých videí odradilo policii (videa ukazující, že støety dopadly špatnì i pro policii)"))
  
)

# OK 1. Proè je dùležité zkoumat, zda je náš konceptuální systém založený na metaforách? Jaké to má dùsledky, pokud takový je? 
# OK 2. Èím se liší konvenèní a nové/imaginativní metafory? 
# OK Jakých pìt aspektù má termín “reverberation”? 
# no... 3. Jak se autoøi staví k myšlence, že existuje absolutní a nepodmínìná objektivní pravda? Znamená to, že subjektivita a objektivita jsou pouze naše jediné volby? 
# no... 4. Co znamená, že je pravda relativní vzhledem k pochopení?
  

t4 = list(  # List s otázkami a odpovìïmi na první text
  "q10" = list(
    "Q10. Jaký se vìøilo, že je vztah mezi tøídou managerù (“managerial class”) a vyšší tøídou (“upper class”)? (William Domhoff: 'Who rules America: Power and Politics')",
    "a" = c("1" = "Vìøilo se, že jsou oddìlené.",
            "0" = "Vìøilo se, že manageøi bezvýhradnì poslouchají vyšší tøídu.",
            "0" = "Vìøilo se, že manažeøi nenápadnì øídí vyšší tøídu (tj. jsou krkem, který hýbe hlavou).",
            "0" = "Vìøilo se, že jsou obì tøídy pevnì propojené.")),
  "q03" = list(
    "Q03. Jakým zpùsobem je udržován vztah mezi tøídou managerù (“managerial class”) a vyšší tøídou (“upper class”)? (William Domhoff: 'Who rules America: Power and Politics')",
    "a" = c("1" = "Vyšší tøída se podílí na øízení.",
            "1" = "Vyšší tøída a manažeøi vytváøí jednu propojenou tøídu.",
            "1" = "Budoucí manažeøi studují soukromé školy spolu s dìtmi z vyšší tøídy.",
            "1" = "Ze strany budoucích manažerù jde o anticipovanou socializaci.")),
  "q11" = list(
    "Q11. Co legitimizovala víra, že tøída managerù (“managerial class”) je oddìlená od vyšší tøídy (“upper class”)? (William Domhoff: 'Who rules America: Power and Politics')?",
    "a" = c("0" = "Tato víra degradovala managery",
            "1" = "Tato víra zakrývala, jak mocná tøída vlastníkù je.", 
            "0" = "Tato víra degradovala vyšší tøídu.",
            "0" = "Tato víra nemìla žádnou funkci."))
)


# 1) Jaký je vliv izolace na socializaci dítìte? Je nìjak podmínìný? Jaké dùležité èasové hranice text ilustruje?
# 2) Co byste oznaèili jako hlavní rozdíly ve výsledcích Isabellu a Annu ústavnì socializovat a jaké hlavní rozdíly vidíte v jejich osudech pøed nalezením a ústavní socializací? Souvisí? 
# OK 3) Je Anna vhodný pøípad pro studium dopadu deprivace na socializaci? Proè ano, proè ne?
  

t5 = list(  # List s otázkami a odpovìïmi na první text
  "q12" = list(
    "Q12. Jaké jsou základní charakteristiky totálních institucí? (Jan Keller: Totální instituce)",
    "a" = c("1" = "Ovlivòují prakticky veškerý život svých klientù/chovancù.", 
            "1" = "Slouží zároveò jako bydlištì i pracovištì.",
            "1" = "Klienti/chovanci jsou odøíznuti na delší dobu od vnìjší spoleènosti.",
            "1" = "Klienti/chovanci vedou formálnì spravovaný zpùsob života.",
            "0" = "Jsou urèené <u>POUZE</u> pro duševnì nemocné.",
            "0" = "Jsou urèené <u>POUZE</u> pro trestance.",
            "0" = "Jsou urèené <u>POUZE</u> pro kontemplující mnichy.")),
  "q13" = list(
    "Q13. Jaké tøi základní typy totálních institucí rozlišuje Keller a jakých pìt typù Goffman? (Jan Keller: Totální instituce)",
    "a" = c("1" = "Keller uvádí: nápravné, vojenské a léèebné instituce.",
            "1" = "Goffmann uvádí: péèe o ty, kteøí to nedokáží; péèe o nebezpeèné; ochrana pøed nebezpeènými; realizace technických záležitostí; stáhnutí do samoty.",
            "0" = "Keller uvádí: ochrana pøed nebezpeènými; realizace technických záležitostí; stáhnutí do samoty.",
            "0" = "Keller uvádí: péèe o nebezpeèné; ochrana pøed nebezpeènými; realizace technických záležitostí.",
            "0" = "Goffman uvádí: patriotické, ležérní, nápravné, vojenské a léèebné instituce."))
)

# Na 10/3: Romero, Mary: “Life as a Maids Daughter.” Pp.: 102-113 In Newman, David M. A Jodi O’Brien. 2004. Sociology. Exploring the architecture of everyday life. Readings. Thousand Oaks: Pine Forge Press.
  # 1) O jakých rùzných typech hranic text pojednává a jak spolu tyto typy souvisí?
  # 2) Jaké statusy Teresa vystøídá? Jak se Teresa uèila pravidlùm v jednotlivých místech? Jakou roli v tom procesu hrála její matka a jakou zamìstnavatelé matky?
  # 3) Jak fungují sociální hranice, které Teresa zakouší?
  # 4) Jak se Teresa nauèila anglicky a jak si tento jazyk vybrala? V èem byly pro Teresu traumatické oslavy Díkùvzdání?
  

t6 = list(  # List s otázkami a odpovìïmi na první text
  "q14" = list( # Ve znìní otázky Q14 byla chyba -- rovnou za to dát lidem jeden bod...
    "Q14. Jak rozumíte paradoxu všemocné byrokracie a bezmocného úøedníka? (Jank Keller: Byrokracie)",
    "a" = c("1" = "Úøedník je jen malou souèástkou obrovského mechanismu byrokracie.", 
            "1" = "Byrokracie je nevyhnutelnou souèástí systému moci.",
            "1" = "Úøedník si uvìdomuje, že je snadno nahraditelný.",
            "0" = "Úøedník se vymlouvá na systém a jen pøedstírá bezmocnost.",
            "0" = "Úøedník nechápe, jak funguje byrokracie, což vyvolává jeho bezmocnost.",
            "0" = "Úøedník je mocnìjší než byrokracie.",
            "1" = "Sytém moci se bez byrokracie neobejde.")),
  
  "q15" = list(
    "Q15. Kdy pracují efektivnìji odborníci a kdy úøedníci (Blau 1956)? (Jank Keller: Byrokracie) ",
    "a" = c("1" = "Odborníci jsou efektivnìjší v situacích nejistoty.",
            "1" = "Úøedníci jsou efektivnìjší pøi rutinním rozhodování o standardních situacích.",
            "0" = "Úøedníci jsou efektivnìjší v situacích nejistoty.",
            "0" = "Odborníci jsou efektivnìjší pøi rutinním rozhodování o standardních situacích."))
)

#Na 17/3: Bloom, Lisa. 2011. “How to Talk to Little Girls.” Huffington Post, June 22. http://www.huffingtonpost.com/lisa-bloom/how-to-talk-to-little-gir_b_882510.html  
# OK 1) Vùèi jakému stylu komunikace s malými dívkami se autorka ohrazuje? Proè?
#   2) Na jaké další ‘nástrahy’ nevhodné socializace dívek autorka se svou malou komunikaèní partnerkou narazí? Jak jim pak èelí?
#   3) Zkuste rekonstruovat mechanismus, jak nevhodná komunikace s malými dívkami vede nakonec k poruchám pøíjmu stravy a závislosti na estetické medicínì.


t7 = list(  # List s otázkami a odpovìïmi na první text
  "q16" = list(
    "Q16. Jak je nyní ve sportu urèováno pohlaví? (Jaime Schultz: Caster Semenya and the “question of too”: Sex testing in elite women's sport and the issue of advantage)",
    "a" = c("1" = "Testuje se pouze na základì pochybnosti pomìrnì komplexní procedurou.", 
            "1" = "Pohlaví se už u žen plošnì netestuje.",
            "1" = "Stále nepanuje definitivní konsensus jak urèit pohlaví.",
            "1" = "Ideální plošná metoda se poøád hledá.",
            "0" = "Podle používání unisexových hraèek, dívèích hraèek a chlapeckých hraèek.")),
  "q17" = list(
    "Q17. Jaké otázky navrhuje Schulz øešit jako lepší místo souèasné otázky, zda poèítat sportovce mezi ženy èi muže (“whether athlete ‘count as’ woman”)? (Jaime Schultz: Caster Semenya and the “question of too”: Sex testing in elite women's sport and the issue of advantage) ",
    "a" = c("0" = "Co zpùsobuje poruchy pøíjmu stravy?",
            "0" = "Co ovlivòuje pøíliš brzské používání líèidel a makeupu?",
            "0" = "Co zpùsobuje závislost na estetické medicínì?",
            "1" = "Jakou neférovou výhodu poskytuje pøíslušná genetická odlišnost?",
            "1" = "Pokud nìjaká vlastnost sportovce pøekraèuje stanovené limity, jak moc je má pøekroèit, aby šlo o neférovou výhodu?"))
)


# Na 17/3: Mead, George H. 1972 (1934). Mind, Self and Society. Chicago: University of Chicago Press. Pp. 173-178. 
# 1) Jak vnímáte Meadovo pojetí ‘me’, co vše je jeho souèástí? 
#   2) Jak se Vám ve svìtle takové koncepce ‘me’ jeví vztah našeho já a druhých lidí?
# OK  3) Jak vnímáte Meadovo pojetí ‘I’? Co ‘I’ vlastnì je?
# OK  4) Jak vnímáte vztah ‘I’ a ‘me’? 
  

t8 = list(  # List s otázkami a odpovìïmi na první text
  "q18" = list(
    "Q18. Jakou povahu a podstatu má gender podle autorek a také Butler, kterou citují? (Kristin L. Anderson, Debra Umberson: Gendering violence)",
    "a" = c("1" = "Gender je performativní", 
            "1" = "Tím, co dìláme, se hlásíme/jsme pøiøazováni k urèité genderové roli",
            "1" = "Mužem nebo ženou se stáváme tím, jak se chováme",
            "0" = "Gender je plán jak s v urèité situaci zachováme",
            "0" = "Gender je mechanismus, kde se rozhodneme, co opravdu udìláme"))
)


# Na 24/3:Goffman, Erving.  1999. Všichni hrajem divadlo. Sebeprezentace v každodenním životì. Pp.: 39-52. Praha: Nakladatelství Studia Ypsilon.
# OK  2) Proè nìkteøí jedinci ve svých pøedstaveních dìlají zámìrné chyby (napø. pravopis)? (s.42-45)
# OK 3) Co oznaèuje Goffman jako “tajnou spotøebu”? A jaké další druhy jednání se snažíme skrývat (Goffman uvádí ètyøi) (s. 45-47)
# no... 5) Co oznaèuje Goffman jako “segregaci obecenstva”? (s. 51) 
# 



t9 = list(  # List s otázkami a odpovìïmi na první text
  # "q23" = list(
  #   "Q23. Co oznaèuje E. Goffman jako “segregaci obecenstva”?",
  #   "a" = c("1" = "Snahu pøedvádìt svou roli pøed vhodným publikem.", 
  #           "0" = "Vùèi rozhovorùm o knihách s tìmito dívkami.",
  #           "0" = "Vùèi konverzacím o technice a matematice.",
  #           "1" = "Vùèi automatickému opomíjení jiných vlastností a aspektù, než je krása, u tìchto dívek.",
  #           "0" = "Vùèi pøedkládání unisexových hraèek a chlapeckých hraèek tìmto dívkám.")),
  "q07" = list(
    "Q07. O èem pojednává text Johna Pollocka 'How Egyptian and Tunisian youth hacked the Arab Spring'?",
    "a" = c("1" = "O roli technologií v prùbìhu Arabského jara", 
            "0" = "O dìtství vùdcù Arabského jara", "0" = "O detailním prùbìhu Arabského jara",
            "1" = "O vlivu technologií na revoluèní události"))
)





# Na 24/3: Becker, Howard S. „Becoming a Marihuana User.” Pp: 3-11 In Newman, David M. a Jodi O’Brien. 2004. Sociology. Exploring the architecture of everyday life. Readings. Thousand Oaks: Pine Forge Press. 
# 1) Co je tøeba se nauèit, aby se jedinec mohl stát kuøákem marihuany?
#   3) Jakou teorii Becker nabízí pro to, že se nìkdo stane uživatelem drog? Vùèi jaké teorii se vymezuje? Jak její odmítnutí zdùvodòuje?
# 



t10 = list(  # List s otázkami a odpovìïmi na první text
  "q09" = list(
    "Q09. O èem pojednává text Williama Domhoffa 'Who rules America: Power and Politics'?",
    "a" = c("1" = "O propojení tøídy manažerù (“managerial class”) a tøídy vlastníkù(“upper class”)", 
            "0" = "O metaforách umožòujících rozlišit mezi 'osobními obtížemi danými prostøedím' a 'veøejnými problémy sociální struktury'.",
            "0" = "O metaforách umožòujících pochopit historii a biografii a jejich vzájemný vztah ve spoleènosti.",
            "0" = "O tom, jak naše koncepty strukturují naše vnímání, jak pøistupujeme ke svìtu a druhým lidem."))
)






## Pøipravíme si matici pro vkládání
mx = matrix("blank", nrow = length(maily), ncol = 21)
colnames(mx) = c("email", paste0("UP", 1:10), paste0("ODP", 1:10))

## Upravíme 'mat' tak, že tam budou maily a uživatelská pole UP1--UP10, tj. jedenáct sloupeèkù
for (m in 1:length(maily)) {
  # Uložíme mail pøíslušného studenta
  mx[m, 1] = maily[m]
  
  # Vylosujeme k textùm otázky a k nim právì 4 odpovìdi
  for (i in 1:10) {
    eval(str2lang(paste0('x', i, ' = t', i, '[[names(t', i, ') %>% sample(1)]]')))  
    eval(str2lang(paste0('x',i,'$a = sample(x',i,'$a, 4)')))  # POZOR! v tuto chvíli tu není kontrola, jestli je alespoò jedna odpovìï pravdivá, zatím jsem to vyøešil tak, že u každé otázky jsou maximálnì 3 nepravdivé odpovìdi a neomezený nenulový poèet pravdivých, tím je zajištìno, že je pravdivá alespoò jedna ze 4 odpovìdí.
  }
  
  # Vylosované otázky s odpovìïmi zamícháme a máme data testu
  test = sample(list(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10), 10)
  
  # Z dat vytvoøíme obsah uživatelských polí a ta uložíme jako stringy do matice
  for (t in 1:length(test)){
    mx[m, (t+1)] = paste0(
      "<hr>", test[[t]][[1]], "<p>",
      "(1) ", test[[t]][[2]][[1]], "<p>",
      "(2) ", test[[t]][[2]][[2]], "<p>",
      "(3) ", test[[t]][[2]][[3]], "<p>",
      "(4) ", test[[t]][[2]][[4]])
    mx[m, (t+11)] = paste0(names(test[[t]][[2]]), collapse = "")
  }
}

# write_xlsx(as_tibble(mx), "TestSeminare02.xlsx")  # Radši to zakomentuju, abych si to nepøepsal...
