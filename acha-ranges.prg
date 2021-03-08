select 1
USE "c:\curso-ml\assignment-1\total-sp-01" EXCLUSIVE
select 2
USE "c:\curso-ml\assignment-1\ranges.dbf" EXCLUSIVE
variavel="DESSERTS"
select 1
go top
maior=0
menor=10000000
dimension inferior(40)
dimension superior(40)
dimension nome(40)
do while .not. eof()
  if &variavel >maior
   maior = &variavel
  endif
  if &variavel <menor
   menor=&variavel
  endif
  skip
enddo
? maior,menor

************************************************************
* Monta arquivo de ranges
************************************************************
select 2
dele all 
pack
passo = (maior-menor)/40
inferiorx = menor
superiorx = menor+passo
for i = 1 to 40
   if i<10
   store "Range-0"+str(i,1,0) to rangexxx
  else
   store "Range-"+str(i,2,0) to rangexxx
  endif
  nome(i)=rangexxx
  inferior(i) = inferiorx
  superior(i) = superiorx
  ? nome(i),inferior(i),"-",superior(i)
  append blank
  replace faixa with nome(i)
  replace valorinf with inferior(i)
  replace valorsup with superior(i)
  inferiorx=superiorx
  superiorx=superiorx+passo
endfor
******************************************************
* Atribui o range de valores no arquivo no trainning data
**********************************************************
select 1
go top
do while .not. eof()
 store &variavel to tpld
 select 2
 go top
 do while .not. eof()
  if tpld>=valorinf .and. tpld<valorsup
    store faixa to tfaixa
  endif
  skip
 enddo
 select 1
 replace range with tfaixa
 skip
enddo
******************************************************************************
* Acha o limite inferior e superior de cada uma das seis variaveis em cada range
*********************************************************************************




dimension maiorx(40,8)
dimension menorx(40,8)
dimension quantx(40)
dimension valor(40)

for i= 1 to 8
 for i1 = 1 to 40
   quantx(i1) = 0
   valor(i1) = 0
   maiorx(i1,i) = 0
   menorx(i1,i) = 100000
 endfor
endfor
Select 1
go bottom
store recno() to trec
? trec
wait
dimension var(trec,9)
go top
y=0
do while .not. eof()
 y=y+1
 store precipitac to var(y,1)
 store tempmax      to var(y,2)
 store tempmin      to var(y,3)
 store insolacao    to var(y,4)
 store tempmed      to var(y,5)
 store umidade      to var(y,6)
 store diasem       to var(y,7)
 store mes          to var(y,8)
 store &variavel    to var(y,9)
 skip
enddo
go top
z=0
do while .not. eof()
&&? "Maior e menor por range "
z=z+1
&&wait

for zxx1=1 to 40

if zxx1<10
 store "Range "+str(zxx1,1,0) to rangexxx

else
 store "Range "+str(zxx1,2,0) to rangexxx

endif

&&? var(z,5),rangexxx
if val(substr(range,7,2))=zxx1
&&  ? tamanhox, tamanhox1,zxx1,var(z,5),"*",rangexxx,substr(var(z,5),7,(tamanhox-6)),substr(rangexxx,7,(tamanhox1-6))
  && wait
  quantx(zxx1)=quantx(zxx1)+1
  valor(zxx1)=valor(zxx1)+&variavel


 if var(z,1)>=maiorx(zxx1,1)
  maiorx(zxx1,1)=var(z,1)
 endif
 if var(z,1)<menorx(zxx1,1)
  menorx(zxx1,1)=var(z,1)
 endif
 &&wait
 if var(z,2)>=maiorx(zxx1,2)
  maiorx(zxx1,2)=var(z,2)
 endif
 if var(z,2)<menorx(zxx1,2)
  menorx(zxx1,2)=var(z,2)
 endif
 
 if var(z,3)>=maiorx(zxx1,3)
  maiorx(zxx1,3)=var(z,3)
 endif
 if var(z,3)<menorx(zxx1,3)
  menorx(zxx1,3)=var(z,3)
 endif
 
 if var(z,4)>=maiorx(zxx1,4)
  maiorx(zxx1,4)=var(z,4)
 endif
 if var(z,4)<menorx(zxx1,4)
  menorx(zxx1,4)=var(z,4)
 endif
 
  if var(z,5)>=maiorx(zxx1,5)
  maiorx(zxx1,5)=var(z,5)
 endif
 if var(z,5)<menorx(zxx1,5)
  menorx(zxx1,5)=var(z,5)
 endif
 
 if var(z,6)>=maiorx(zxx1,6)
  maiorx(zxx1,6)=var(z,6)
 endif
 if var(z,6)<menorx(zxx1,6)
  menorx(zxx1,6)=var(z,6)
 endif
 if var(z,7)>=maiorx(zxx1,7)
  maiorx(zxx1,7)=var(z,7)
 endif 
  if var(z,7)<menorx(zxx1,7)
  menorx(zxx1,7)=var(z,7)
 endif
 if var(z,8)>=maiorx(zxx1,8)
  maiorx(zxx1,8)=var(z,8)
 endif 
  if var(z,8)<menorx(zxx1,8)
  menorx(zxx1,8)=var(z,8)
 endif
 && ? maior(1,1)
 && wait
endif
endfor



skip

enddo
*************************************************************************************
* grava no arquivo os valores inferiores e superiores de cada um dos seis parametros
**************************************************************************************
select 2
go top
do while .not. eof()
 store substr(faixa,7,2) to tnumero
 replace quant with quantx(val(tnumero))
 ? tnumero
 if menorx(val(tnumero),1)<1000 
  replace inferior1 with menorx(val(tnumero),1)
  replace superior1 with maiorx(val(tnumero),1)
 endif
  if menorx(val(tnumero),2)<1000 
  replace inferior2 with menorx(val(tnumero),2)
  replace superior2 with maiorx(val(tnumero),2)
 endif
  if menorx(val(tnumero),3)<1000 
  replace inferior3 with menorx(val(tnumero),3)
  replace superior3 with maiorx(val(tnumero),3)
 endif
  if menorx(val(tnumero),4)<1000 
  replace inferior4 with menorx(val(tnumero),4)
  replace superior4 with maiorx(val(tnumero),4)
 endif
  if menorx(val(tnumero),5)<1000 
  replace inferior5 with menorx(val(tnumero),5)
  replace superior5 with maiorx(val(tnumero),5)
 endif
 if menorx(val(tnumero),6)<1000 
  replace inferior6 with menorx(val(tnumero),6)
  replace superior6 with maiorx(val(tnumero),6)
 endif 
  if menorx(val(tnumero),7)<1000 
  replace inferior7 with menorx(val(tnumero),7)
  replace superior7 with maiorx(val(tnumero),7)
 endif
  if menorx(val(tnumero),8)<1000 
  replace inferior8 with menorx(val(tnumero),8)
  replace superior8 with maiorx(val(tnumero),8)
 endif
  skip
enddo
*****************************************************************************
* Faz a previsao
*******************************************************************************

select 3
USE "c:\curso-ml\assignment-1\total-sp-2019.dbf" EXCLUSIVE && arquivo cujo preço sera previsto
go bottom
store recno() to trec1
dimension varx(trec1,6)
go top
y = 0
do while .not. eof()
  y=y+1
  store precipitac to varx(y,1)
  store tempmax      to varx(y,2)
  store tempmin       to varx(y,3)
  store insolacao    to varx(y,4)
  store tempmed      to varx(y,5)
  store umidade      to varx(y,6)
  store diasem       to var(y,7)
  store mes          to var(y,8)

 ? var(y,1), var(y,2),var(y,3),var(y,4),var(y,5),var(y,6),var(y,7),var(y,8),
 wait
 select 2
 go top
 y1 = 0
 maisquant=0
 rangecerto=0
 do while .not. eof()
    y1=y1+1
    temp1 = 0
    temp2 = 0
    temp3 = 0
    temp4 = 0
    temp5 = 0
    temp6 = 0
    temp7 = 0
    temp8 = 0    
   if var(y,1)<=superior1 .and. var(y,1)>inferior1
     temp1 = 1
   endif
   if var(y,2)<=superior2 .and. var(y,2)>inferior2
     temp2 = 1
   endif
   if var(y,3)<=superior3 .and. var(y,3)>inferior3
     temp3 = 1
   endif
   if var(y,4)<=superior4 .and. var(y,4)>inferior4
     temp4 = 1
   endif
   if var(y,5)<=superior5 .and. var(y,5)>inferior5
     temp5 = 1
   endif
   if var(y,6)<=superior6 .and. var(y,6)>inferior6
     temp6 = 1
   endif
   if var(y,7)<=superior7 .and. var(y,6)>inferior7
     temp7 = 1
   endif   
   if var(y,8)<=superior8 .and. var(y,8)>inferior8
     temp8 = 1
   endif   
   if (temp1+temp2+temp3+temp4+temp5+temp6+temp7+temp8)=8 &&
     ? y,y1,temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8
   && ? "Esta no range"
   && ? maisquant,quant
   && wait
    if maisquant<quant
     maisquant=quant
     rangecerto=y1
    endif
    ? y,y1,temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8

    &&wait
   endif
   skip
  enddo 
 
  select 3
   replace classe with str(rangecerto,4,0)
 skip
enddo
select 3
go top
do while .not. eof()
 store val(classe) to trange
 select 2
 
 locate for val(substr(faixa,7,2))=trange
 ? val(substr(faixa,7,2)), trange
 wait
 if .not. eof()
  store faixa to ttrange
  store valorsup to ttvalorsup
  store valorinf to ttvalorinf
  select 3
  replace range with ttrange
  replace inferior with ttvalorinf
  replace superior with ttvalorsup
  replace media  with inferior+(superior-inferior)/2

  replace erro with (1-(&variavel-media)/&variavel)
 endif
 select 3
 skip
enddo
 
