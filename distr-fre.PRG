select 1
USE "c:\curso-ml\assignment-1\total-geral2.dbf" EXCLUSIVE
&&select 2
&&use
select 1
go top
maior=0
menor=10000000
do while .not. eof()
 store desserts to tdessert
 if tdessert>maior
  maior=tdessert
 endif
 if tdessert<menor
  menor=tdessert
 endif
 skip
enddo
? maior,menor
wait
rangex = maior-menor
? rangex
passo = rangex/20
? passo
replace all desserte with 0
for faixa = menor to maior step passo
   ? faixa,(faixa+passo)
   go top
   count for desserts>faixa .and. desserts<(faixa+passo) to y
    ? faixa,(faixa+passo),y,y/396
    if (y/396)<0.01
     replace desserte with 1 for desserts>faixa .and. desserts<(faixa+passo)
    endif
endfor