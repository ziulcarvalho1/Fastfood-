select 1
USE "c:\curso-ml\assignment-1\total-sp-01.dbf" EXCLUSIVE
select 2
use c:\curso-ml\assignment-1\dia-sem-me
dele all 
pack
select 1
go top
do while .not. eof()
 
 store desserts      to tdesserts
 store pizzas        to tpizzas
 store beverage      to tbeverage
 store cbmaker       to tcbmaker
 store combos        to tcombos
 store sfiha         to tsfiha
 store kit1          to tkit1 
 store kit2          to tkit2
 store snack         to tsnack
 store pastas        to tpastas
 store dishes        to tdishes 
 store promotion     to tpromotion
 store savory        to tsavory
 store salads        to tsalads
 store diasem        to tdiasem
 store mes           to tmes
  
 select 2
 go top
 locate for (diasem)=(tdiasem) .and. (mes)=(tmes)

 if .not. eof()
   replace  desserts      with desserts+ tdesserts
   replace  pizzas        with pizzas+tpizzas
   replace  beverage      with beverage+tbeverage
   replace  cbmaker       with cbmaker+tcbmaker
   replace  combos        with combos+tcombos
   replace  sfiha         with sfiha+tsfiha
   replace  kit1          with kit1+tkit1 
   replace  kit2          with kit2+tkit2
   replace  snack         with snack+tsnack
   replace  pastas        with pastas+tpastas
   replace  dishes        with dishes+tdishes 
   replace  promotion     with promotion+tpromotion
   replace  savory        with savory+tsavory
   replace  salads        with salads+tsalads
   replace  quant         with quant+1
  && soma
 else
   ? tdiasem,tmes
   &&wait
   append blank
   replace  desserts      with desserts+ tdesserts
   replace  pizzas        with pizzas+tpizzas
   replace  beverage      with beverage+tbeverage
   replace  cbmaker       with cbmaker+tcbmaker
   replace  combos        with combos+tcombos
   replace  sfiha         with sfiha+tsfiha
   replace kit1           with kit1+tkit1 
   replace kit2           with kit2+tkit2
   replace snack          with snack+tsnack
   replace pastas         with pastas+tpastas
   replace dishes         with dishes+tdishes 
   replace promotion      with promotion+tpromotion
   replace savory         with savory+tsavory
   replace  salads        with salads+tsalads
   replace diasem         with diasem+tdiasem
   replace quant          with 1
   replace diasem         with tdiasem
   replace mes            with tmes
 
 endif

 
 select 1
 skip
enddo
select 2
   replace all desserts      with desserts/quant
   replace all pizzas        with pizzas/quant
   replace all beverage      with beverage/quant
   replace all cbmaker       with cbmaker/quant
   replace all combos        with combos/quant
   replace all sfiha         with sfiha/quant
   replace all kit1           with kit1/quant 
   replace all kit2           with kit2/quant
   replace all snack          with snack/quant
   replace all pastas         with pastas/quant
   replace all dishes         with dishes/quant 
   replace all promotion      with promotion/quant
   replace all savory         with savory/quant
   replace all salads        with salads/quant

select 1
go top
do while .not. eof()
 store desserts      to tdesserts
 store pizzas        to tpizzas
 store beverage      to tbeverage
 store cbmaker       to tcbmaker
 store combos        to tcombos
 store sfiha         to tsfiha
 store kit1          to tkit1 
 store kit2          to tkit2
 store snack         to tsnack
 store pastas        to tpastas
 store dishes        to tdishes 
 store promotion     to tpromotion
 store savory        to tsavory
 store salads        to tsalads
 store diasem        to tdiasem
 store mes           to tmes
 select 2
  locate for (diasem)=(tdiasem) .and. (mes)=(tmes)

 if .not. eof()
    store desserts      to tdesserts
    store pizzas        to tpizzas
    store beverage      to tbeverage
    store cbmaker       to tcbmaker
    store combos        to tcombos
    store sfiha         to tsfiha
    store kit1          to tkit1 
    store kit2          to tkit2
    store snack         to tsnack
    store pastas        to tpastas
    store dishes        to tdishes 
    store promotion     to tpromotion
    store savory        to tsavory
    store salads        to tsalads
    select 1
   replace  dessert1       with tdesserts
   replace  pizzas1         with tpizzas
   replace  beverage1       with tbeverage
   replace  cbmaker1        with tcbmaker
   replace  combos1        with tcombos
   replace  sfiha1          with tsfiha
   replace  kit11           with tkit1 
   replace  kit21           with tkit2
   replace  snack1          with tsnack
   replace  pastas1         with tpastas
   replace  dishes1         with tdishes 
   replace  promotion1      with tpromotion
   replace  savory1         with tsavory
   replace  salads1         with tsalads
   
   replace  desserte        with  (desserts-dessert1)/desserts
   replace  pizzase         with  (pizzas-pizzas1)/pizzas
   replace  beveragee       with  (beverage-beverage1)/beverage
&&   replace  cbmakere        with  (cbmaker-cbmaker1)/cbmaker
&&   replace  combose         with  (combos-combos1)/combos
   replace  sfihae          with  (sfiha-sfiha1)/sfiha
&&   replace  kit1e           with  (kit1-kit11)/kit1 
&&   replace  kit2e           with  (kit2-kit21)/kit2
   replace  snacke          with  (snack-snack1)/snack
   replace  pastase         with  (pastas-pastas1)/pastas
   replace  dishese         with  (dishes-dishes1)/dishes 
&&   replace  promotione      with  (promotion-promotion1)/promotion
   replace  savorye         with  (savory-savory1)/savory
   replace  saladse         with  (salads-salads1)/salads
  endif
 select 1
 skip
enddo
