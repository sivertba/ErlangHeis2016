###Oppslagstavle
Sverre stresset at bugfiksing siste uka gir få totale poeng og kan gjøre mer skade. 
Vi ligger bra an med andre ord og kan ta livet litt med ro! :))
Husk det! :))
####Bugs
* SANNSYNLIGVIS FIKSET: state_monitor kræsjer hvis fsm ikke starter opp riktig og state er invalid, deretter kræsjer alt. Forårsaket av gammelt funksjonskall.
 * Scenario der dette skjedde: heisen er satt i PLS når vi starter opp, flippes til PC etter at state_monitor har kræsja, heisen kjører utfor

####Øvrige ting å gjøre
* Rydde kode obv; fjerne unødvendige funksjoner og kommentarer
* Hvis vi er litt ambisiøse: gjøre et forsøk på å stoppe heiser som kjører ut i gokk. Forslag:
  * få to_remove til å returnere en liste og en bool, {List, OrdersBeyond}. Hvis lista er tom og boolen er false, stopp


###Tanker rundt heis i Erlang

Det var dristig å prøve å lære seg ett helt nytt språk til ett større prosjekt, men ettersom hvordan det språket er bygd opp, og hvilke moduler man har innebygd kan man relativt enkelt agrumentere for å gi det det sjanse.

#####2016-02-11:
Trenger hjelp fra Kjetil med:
* Hvordan fungerer driveren
* Få til simulatoren
* Spørre dumme spørsmål om å koble til flere maskiner.

#####2016-02-18:
* Må Gjøre endringer i FSM modul
* Må se nærmere på driver
* Spørre om server / orderHandler

#####2016-02-25:
Får spurt Kjetil på fredag om:
* Distribuering ov ordreserver over datamaskiner
* Hvorfor Kjetil flusher i alle tilstander???
* Er det for å tømme for meldinger som kom tidligere (f.eks. mens vi stod med åpne dører) som vi skiter i?
* PLAUSIBILITY CHECKS??? Explain?
* Connection-modulen: den kobler seg til alt den tar imot, og sender ut eget nodenavn/id. Kjøres dette på tre pc'er samtidig på magisk vis?
* EPIPHANY: alt kjøres trippelt! Halleluja. Hva skjer da med konkurrerende ordredistributører?

#####2016-03-03:


#####2016-03-31:

#####2016-04-7:

#####2016-04-14:

#####2016-04-21:


