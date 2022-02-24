cocurrent'dplyr'

NB. ++++++++++++++++++++++++++++++++++++++
NB. DPLYR for J
NB. ++++++++++++++++++++++++++++++++++++++
NB. Planned syntax:
NB. [+] ('a';'b')  select y
NB. [+] ('a = 1';'b > 5') filter y
NB. [+] (<'c =. a + b') mutate y
NB. [+] (<'a') group y
NB. [+] ('d =. +/ b'; 'e = +/ b') summarize
NB. [+] ungroup y
NB. [+] distinct y
NB. [+] head y
NB. [+] tail y
NB. [ ] ('a';'b') arrange y
NB. [ ] rename y
NB. [ ] verify y
NB. [ ] ('int';104 102 120;'byte';'ECx') key y
NB. [ ] schema y

NB. HIGH-LEVEL WISHLIST
NB. Convert dates so that > and < work for dates.
NB. Get mutate and summarize to work with characters.
NB. Add trailing spaces to searchstring so that string match works all the time.
NB. (<#]) NB. Select less than.
NB. Types? How are types stored in jd?

verify=: 3 : 0
(0 {:: y)
NB. TODO check no duplicate columns.
NB. TODO check no column named 'group'.
)

NB. DPLYR FOR JD FORMAT.

select=: 4 : 0
colsi=. (,x) (e.#i.~) (0 {:: y)
if. 1 < $$ x					NB. Retrieve columns.
do. (colsi {"1 y)
else. ,. colsi {"1 y				NB. Handle single columns, which otherwise have a different rank. (!)
end.
)

derank=: monad : 'if. 1&= }. $ y do. ,y  else. y end.'

scalarize=: 3 : 0
if. (1 < $ y) do. y else. {.y end. 			NB. This is necessary for indexing values returned from 'cut' which only returns boxed vectors.
)
NB. May be the same as derank. ?

isin=: 4 : 0
xbox=. <"1 x
>./ y =/ xbox
)

filter=: 4 : 0
NB. TODO need to add string filtering.
cotemp=. conew 'temp'				NB. Namespace for temporary nouns.
jdidx=. ,. (<'jdindex'),<(,. i.@:# (1 0) {:: y)		NB. Add jdindex.
cols=. 0 {:: (y,.jdidx)				NB. Get all column names.
valuesboxed=. ' ' cut"1 > x			NB. Parse with ' '
columnind=. (, (scalarize&.> valuesboxed)) (e.#i.~) (0 {:: (y,.jdidx))	NB. Get column indices, only indices which are present.
columns=. , columnind { 0 {:: (y,.jdidx)		NB. Select columns.
selecteddata=. |: columnind { 1{:: (y,.jdidx)
assignable=. (,&('_',(>cotemp),'_')) &.> columns		NB. Create variables in temp namespace.
					NB. Assign values to variables in temp namespace.
if. (1 < $ selecteddata)				NB. Handle single columns, which otherwise don't get unboxed in assignment. (!)
do. (' ' joinstring assignable)=. selecteddata
else. (' ' joinstring assignable)=. > selecteddata NB. Remove ,.
end.
evalstring=. (' ' joinstring valuesboxed) rplc , columns ,"0 assignable  NB. Evaluate.
rowind=. I. , > ". evalstring 			NB. Evaluate function to calculate indices.
coerase cotemp				NB. Erase namespace.
|: (0{::y) ,"0 [ (rowind)&{&.> 1 {:: y		NB. Return in original format with names, without jdindex attached.
)

mutate=: 4 : 0
NB. Run with (<'a =. b + c') mutate jdreads''
NB. TODO (<'new =. 1&+  AMOUNT') mutate (<'SEC_ID > 48058') filter NB. TODO This doesn't work. What is going on with this??
NB. Should be integrated with group, such that functions which apply within groups do so automatically.
NB. Works only on numeric columns.
cotemp=. conew 'temp' NB. Namespace for temporary nouns.
parsedstring=. ;: > x                                       NB. Parse x into 'words.'
newcolumn=. {. parsedstring                                 NB. New column name.
valuesboxed=. 2 }. ' ' cut > x		 	NB. Parse function into boxes by spaces ' '
columnind=. (, (scalarize&.> valuesboxed)) (e.#i.~) (0 {:: y)	NB. Get column indices, only indices which are present.
columns=. , columnind { 0 {:: y			NB. Retrieve columns in order.
selecteddata=. |: columnind { 1{:: y
assignable=. (,&('_',(>cotemp),'_')) &.> columns 	NB. Create variables in temp namespace.
(' ' joinstring assignable)=. selecteddata		NB. Assign values to variables in temp namespace.
evalstring=. (, columns ,"0 assignable) rplc~ (' ' joinstring valuesboxed)  NB. Change eval string to namespace variables.
result=. ". evalstring				NB. Evaluate function.
coerase cotemp				NB. Erase namespace.
y,"1(,. ((' ';'') rplc~ >newcolumn); result)		NB. Return in original format with original names.
)

ungroup=: 3 : 0
NB. TODO need to make this fuzzy across columns.
dropcolind=. I. [ 6&= [ ; (+/)&.> ('_group'&e.)&.> (0 {:: y)
columnind=. dropcolind (]#~(-.@e.~)) i. # (0 {:: y) NB. Columns not including drop columns.
columnind {"1 y
)

group=: 4 : 0
NB. (<'A') or ('B';'A')
NB. Perhaps replace string join with ,&> (See Appose.)
columnind=. (,x) (e.#i.~) (0 {:: ungroup y)
groupidx=. ,. (<;x,<'_group'), <(,. (i.~~.) ((,"1)/> [ columnind{ [ ":&.> 1{:: ungroup y) )		NB. Create group index.
groupval=. ,. (<;x,<'_groupvalue'), <((,"1)/> [ columnind{ [ ":&.> 1{:: ungroup y)			NB. Create group value strings.
(ungroup y),"1 groupidx,"1 groupval
)

indexgroup=: 3 : 0
NB. Creates an index within groups.
NB. Should be automatic with group-by?
groupcolind=. I. [ 6&= [ ; (+/)&.> ('_group'&e.)&.> (0 {:: y)
groupcolval=. > ({. groupcolind) { (1 {:: y)
selfclass=. (</. i.@#) groupcolval NB. Self-classify by nub.
groupcount=. ; (i.@#)&.> selfclass
y,"1,.('index_group'); ,. (groupcount /: (; selfclass))
)

summarise=: summarize=: 4 : 0
NB. Only works on numeric columns.
NB. Need to enter functions as though they are unboxing columns.
NB. ('d =. +/&.> b' summarize
NB. Parsed with spaces. 'sum =.' works but 'sum=.' does not work.
NB. TODO In output, replace groupings with values of underlying.
NB. TODO Break if no group column.
NB. TODO parsing eval is kind of sketchy.
NB. TODO Convert groupval back to numeric if originally numeric.
NB. TODO Perhaps switch boxing to Self-Classify: (</. i.@#)
cotemp=. conew 'temp'				NB. Namespace for temporary nouns.
groupcolind=. I. ; +/&.> (<'group')&=&.> '_'&cut&.>(0{::y) NB. select group column.
groups=. , > groupcolind{1{::y
groupnames=. ~. groups
cols=. 0 {:: y				NB. Get all column names.
valuesboxed=. ' ' cut"1 ; x			NB. Parse with ' '
columnind=. (scalarize&.> valuesboxed) (e.#i.~) cols 	NB. Get all column indices, only indices which are present.
columns=. , columnind { 0 {:: y			NB. Select columns.
selecteddata=. |: columnind { 1{:: y
groupsselected=. ((, >selecteddata)&(#~))&.> (<"1 (groupnames =/ groups)) NB. Subset data into boxes by group.
assignable=. ,> (,&('_',(>cotemp),'_')) &.> columns		NB. Assign this box set to be evaluatable.
(assignable)=. groupsselected		             NB. Need parathesises around multiple assignment, unclear why. (!)
evalstring=. (, columns ,"1 <"1 assignable) rplc~ (' ' joinstring [ 2}. valuesboxed) NB. Change eval string to namespace variables.
groupcolname=. < ('_group';'') rplc~ > (groupcolind{0{::y)
groupidxcol=. ,.(groupcolname),(< ,. groupnames)
groupvaluecolind=. I. ; +/&.> (<'groupvalue')&=&.> '_'&cut&.>(0{::y) NB. select group column.
groupvaluecolname=. < ('_groupvalue';'') rplc~ > (groupvaluecolind{0{::y)
groupvals=. ; groupvaluecolind{1{::y
groupval=. ({."1 [ I."1 groupnames =/ groups) { ; groupvaluecolind{1{::y
groupvalcol=. ,.(groupcolname),(< ,. groupval)
evalcol=. ,.({.valuesboxed),(< ,. > ". evalstring)
coerase cotemp				NB. Erase namespace.
NB. groupidxcol 				NB. This contains the grouping, which is dropped here.
groupvalcol,"1 evalcol
)

deselect=: 4 : 0
dropcolind=. x i.~ (0 {:: y)
columnind=. dropcolind (]#~(-.@e.~)) i. # (0 {:: y) NB. Columns not including drop columns.
columnind {"1 y
)

arrange=: 4 : 0
NB. Sort
sortcolind=. (,x) (e.#i.~) (0 {:: y)
sortcolval=. > ({. sortcolind) { (1 {:: y)
order=. (sortcolval /: ((-@:#&.> sortcolval),.tolower&.> sortcolval)) i.~ sortcolval NB. This works because of left string padding.
(0{::y),:(order&{&.> 1{::y)
)

arrangedesc=: 4 : 0
NB. Sort
sortcolind=. (,x) (e.#i.~) (0 {:: y)
sortcolval=. > ({. sortcolind) { (1 {:: y)
order=. (sortcolval \: ((-@:#&.> sortcolval),.tolower&.> sortcolval)) i.~ sortcolval NB. This works because of left string padding.
(0{::y),:(order&{&.> 1{::y)
)

distinct=: 3 : 0
rowi=. (i:(~.)) (,"1)/@:>@:(":&.>) 1{::y
(0{::y),:(rowi&{&.> 1{::y)
)

index=: 3 : 0
jdidx=. ,. (<'jdindex'),<(,.@:i.@:# (1 0){::y)		NB. Add jdindex.
(y,.jdidx)					NB. Get column indices, only indices which are present.
)

revindex=: 3 : 0
revjdidx=. ,. (<'revjdindex'),<(,.@:|.@i.@:# (1 0){::y)	NB. Add jdindex.
(y,.revjdidx)				NB. Get column indices, only indices which are present.
)

head=: 3 : 0
NB. Assumes jdindex is not present.
(<'jdindex < 10') filter y
:
(<'jdindex < ',(": x)) filter y
)

behead=: 3 : 0
NB. Assumes jdindex is not present.
(<'jdindex > 9') filter y
:
(<'jdindex > ',(": <: x)) filter y
)

tail=: 3 : 0
}:"1 (<'revjdindex < 10') filter revindex y
:
}:"1 (<'revjdindex <',(": x)) filter revindex y
)

dim=: 3 : 0
(# > (1 0){::y),(}.$ y)
)

rs2r=: 3 : 0
|: derank&.> y
)

rs2p=: 3 : 0
,|: derank&.> y
)

NB. To do.

split=: 4 : 0
NB. splits a column into two columns.
)

get=: 4 : 0
)

in=: 4 : 0
)

rename=: 4 : 0
NB. (<'a =. b')
)

lag=: 3 : 0
)

lead=: 3 : 0
)

schema=: 3 : 0
NB. jd'info schema'
NB. ┌─────┬──────┬────┬─────┐
NB. │table│column│type│shape│
NB. ├─────┼──────┼────┼─────┤
NB. │a    │akey  │int │ _   │
NB. │a    │adata │int │ _   │
NB. │a    │aref  │int │ _   │
NB. │b    │bref  │int │ _   │
NB. │b    │bb12  │byte│12   │
NB. └─────┴──────┴────┴─────┘
)

NB. ++++++++++++++++++++++++++++++++++++++
NB. CSV read in jd format.
NB. ++++++++++++++++++++++++++++++++++++++
NB. TODO Maybe add some options (header or type).
load 'tables/csv' NB. Must have this addon installed.
jdreadcsv=: monad : '((,.@(0&{::)),"1(,.@((<@:>"1)@|:@}.)))@[@makenum@readcsv y'	NB. Read csv like jd.
jdreadscsv=: monad : '|:@(((0&{::)),.(,.&.>@(<@:>"1)@|:@}.))@[@makenum@readcsv y'	NB. Reads csv like jd.
jdwritecsv=: dyad : '(({.,(|:@:>@:((<"1)&.>)@:{:)) x) writecsv y'	NB. Write 'reads' to csv.

jdreadtsv=: monad : '((,.@(0&{::)),"1(,.@((<@:>"1)@|:@}.)))@[@makenum@('''', readdsv) y'	NB. Read tsv like jd.
jdreadstsv=: (|:@(((0&{::)),.(,.&.>@(<@:>"1)@|:@}.))@[@makenum@('',readdsv)) 	NB. Reads tsv like jd.


NB. ++++++++++++++++++++++++++++++++++++++
NB. JD to json format
NB. ++++++++++++++++++++++++++++++++++++++
r2json=: 3 : 0 NB. convert 'read' query to json string
NB. Credit: Use D3 in J!, Written by Justin Tirrell, edited  by Jordan Tirrell
colcount=. ":#{."1 y
cols=. }.,; {."1 y
dat=. (#~LF&~:) '[',']',~}:;('},',~'{',}:)&.>;@(<@":"0@i.@#([,':"','",',~])&.>])&.><"1|:":&.>>(boxopen"_1)&.>{:"1 y
colcount,cols,dat
)

rs2json=: 3 : 0 NB. convert 'reads' query to json string
colcount=. ": $ 0{::y
cols=. ;(,&' ')&.>0{::y
dat=. (#~LF&~:) '[',']',~}:;('},',~'{',}:)&.>;@(<@":"0@i.@#([,':"','",',~])&.>])&.><"1|:":&.>>(boxopen"_1)&.> 1{:: y
colcount,cols,dat
)

r2jsonl=: 3 : 0 NB. convert 'read' query to json line format.
)

rs2jsonl=: 3 : 0 NB. convert 'reads' query to json line format.
cols=. ('"','"',~])&.> 0{::y
('[{';'{';'}]';'}';'},{';('}',LF,'{')) rplc~ [ (#~LF&~:) '[',']',~}:;('},',~'{',}:)&.>;@((cols)([,':"','",',~])&.>])&.><"1|:dltb&.>":&.>>(boxopen"_1)&.> 1{:: y
)

NB. ++++++++++++++++++++++++++++++++++++++
NB. String date conversion
NB. ++++++++++++++++++++++++++++++++++++++

edatetoint=. 3 : 0
".@(('-';'')&(rplc~)) y
)

inttoedate=. 3 : 0"_
'Y0 Y1 Y2 Y3 m0 m1 d0 d1'=. ": y
(Y0,Y1,Y2,Y3,'-',m0,m1,'-',d0,d1)
)

NB. ++++++++++++++++++++++++++++++++++++++
NB. Box operations
NB. ++++++++++++++++++++++++++++++++++++++

fill=: 3 : 0
NB. For boxes, not for jd
(]&0^:(0&=@#))&.> y
:
(]&x^:(0&=@#))&.> y
)


0 : 0
NB. ++++++++++++++++++++++++++++++++++++++
NB. Comments
NB. ++++++++++++++++++++++++++++++++++++++

Explain this:

Rank:
$ 'a'
$ 1
$ ": 1


NB. ####################################
NB. Examples of the functions above.
NB. ####################################

example1 =. jdreadscsv '/Volumes/scapa/data/pilot/portfolio/052/purchases.csv'
example2 =. (,.(<'A'),(< ,. 10 + i.20)) ,. (,.(<'B'),(< ,. 100 + i.20)),. (,.(<'C'),(< ,. 20 2$ 'abcde'))
example3 =. (<'C') group example2
example4 =. (,.(<'rand'),(< ,. ?50$5)) ,. (,.(<'ind'),(< ,. 100 + i.50)),. (,.(<'alpha'),(< ,. 50 3$ 'abcde'))

example2
example3

(<'Bdiv =. %/&.> A') summarize example3
(<'Bmean =. (+/)&.> B') summarize example3
mean =: (+/%#)
(<'Bmean =. mean&.> B') summarize example3
(<'Bmean =. (+/%#)&.> A') summarize example3
(<'Bmean =. (+/)&.> B') summarize ('A';'B') group example2
(<'Bcopy =. B') mutate example2
(<'B < 105') filter example2
(<'Asum =. +/&.> A') summarize (<'B < 105') filter example3
ungroup ('B';'A') group example2
(<'C') group example2
ungroup (<'C') group example2
ungroup (<'A') group example2
ungroup (<'a =. C_group') mutate example3

head (<'new =. quantity % SEC_ID') mutate head example1
(<'jdindex < 100') filter example1
(<'jdindex = 2') filter example1
(<'jdindex = 20') filter example1
(<'new =. ,. i.21292') mutate example1

(<'indsum =. (+/)&.> ind') summarize (<'alpha') group example4
(<'indsum =. (+/)&.> ind') summarize (<'rand') group example4
(<'indsum =. (+/)&.> ind') summarize (<'rand') group (,.(<'rand'),(< ,. ?50$5)) ,. (,.(<'ind'),(< ,. 100 + i.50)),. (,.(<'alpha'),(< ,. 50 3$ 'abcde'))

ungroup (<'alpha') group example4

(<'Bsum =. (+/)&.> B') summarize ('A';'B') group example2
(<'C') group example2
ungroup ('A';'B') group example2
ungroup (<'C') group example2

jdloadtojson jdreadcsv '/Volumes/scapa/data/pilot/portfolio/052/purchases.csv'
jdtojson [ jdreadscsv '/Volumes/scapa/data/pilot/portfolio/052/purchases.csv'
jdtojson (<'jdindex < 10') filter example1
jdtojson example1

0{::example1
(<'jdindex < 10') filter (<'Amountmean =. (+/%#)&.> AMOUNT') summarize (<'ENDDATE') group example1
(<'jdindex < 10') filter (<'Amountmean =. (+/%#)&.> SEC_ID') summarize (<'ENDDATE') group example1
NB. Which is that ENDDATE should show.

distinct ('rand';'alpha') select example4
distinct ('rand';'ind') select example4
distinct example4

(<'hmm =. A') summarize example3

',. i.21292' eval

NB. DPLYR testing
temp0 =. jd'reads from dailyclean where jdindex < 50'
temp1 =. ('ENDDATE';'SEC_ID';'INDEXED') select temp0
temp2 =. (<'ENDDATE') select temp1
temp3 =. (<'SEC_ID') select temp1

temp4 =. (('ENDDATE = "1999-02-16"') * ('SEC_ID = 7')) filter temp1 NB. Doesn't work anymore because string.
temp5 =. (<'ENDDATE = 1999-02-02') filter (<'SEC_ID = 7') filter temp1 NB. Doesn't work anymore because string.
temp6 =. (<'SEC_ID = 7') filter (<'INDEXED = 1') filter temp1
temp7 =. (<'SEC_ID = 7') filter temp1

example11 =. temp11 =. jd'reads from ibtrades where jdindex < 10'

(<'121.5 < midpointopen') filter temp11
(<'midpointopen > 121.5') filter temp11
(<'242 > midpointopen + midpointclose') filter temp11

example11
(<'sum > 0.98') filter (<'sum') select (<'sum =. midpointclose % midpointhigh') mutate ('midpointclose';'midpointhigh') select example11

(<'10 = +/"1 ''1999-02-02'' ="1 ENDDATE') filter temp1 NB. Doesn't work anymore because string.
   10 = +/"1  '1999-02-02'  ="1 [ 1 0 {:: temp1


NB. JDREADCSV testing
NB. Read csv.
jdreadcsv '/Volumes/scapa/data/pilot/portfolio/052/purchases.csv' NB. jdreads, except numbers not converted.a=. jd'reads jpm2ib.SEC_ID, ibtrades.conid from jpm2ib,jpm2ib.ibtrades where jpm2ib.SEC_ID in (',(','joinstring ":&.>  purchase_sec_ids),')'
NB. Reads csv.
jdreadscsv '/Volumes/scapa/data/pilot/portfolio/052/purchases.csv' NB. jdreads, except numbers not converted.a=. jd'reads jpm2ib.SEC_ID, ibtrades.conid from jpm2ib,jpm2ib.ibtrades where jpm2ib.SEC_ID in (',(','joinstring ":&.>  purchase_sec_ids),')'





NB. ####################################
NB. Scratch.
NB. ####################################


ar =:  5!:1    NB. atomic
ab =:  5!:0
eval =: 1 : 0
 NB. ". replaces this.
 ". 'w =. ' , u
 (ar_base_ < 'w') ab_base_)


NB. Intersection of boxed numeric indices.
(i.5); (2 + i.10); (3 + i.10); 4
~. ((({.@$)=(+/@e.@;))#(;)) [ (i.5); (2 + i.10)
~. ((({.@$)=(+/@e.@;))#(;)) [ (i.5); (2 + i.10); (3 + i.10); 4



(<'ENDDATE') colindex temp1
(<'SEC_ID') colindex temp1
(<'N') colindex temp1

(<'SEC_ID') i.~ (0 {:: temp1)


NB. Union of boxed numeric lists
NB. This works by checking that the count of number is equal to the number of times the number appears.

NB. intersect_boxes [ (i.5); (2 + i.10)

a=. ('SEC_ID > 6' indexf temp1)
b=. ('ENDDATE = "1999-02-12"' indexf temp1)
intersect_boxes a,b
intersect_boxes [ ('ENDDATE = "1999-02-12"' indexf temp1),('SEC_ID > 6' indexf temp1)

(<'SEC_ID = 7') indexf"0 2 temp1
(<'SEC_ID = 7') indexf temp1

'SEC_ID = 7' indexf temp1
'SEC_ID > 7' indexf temp1
'SEC_ID > 6' indexf temp1
('SEC_ID > 6' indexf temp1)
'ENDDATE = "1999-02-12"' indexf temp1
'ENDDATE = "1999-02-16"' indexf temp1


NB. ('ENDDATE = "1999-02-16"'; 'b > 5') indexf"0 3 temp1

union=: ~.@,
intersect=: e. # [
diff=: -.
(i.5) union (1 + i.5)


NB. Type, for the purpose of matching types.
(3!:0) "2014-05-01"




NB. Original filter.
colindex =: 4 : 0
 colsi=. [  x i.~ (0 {:: y)
 colsi)


parsebox =. 3 : 0
 NB. Return column; function; value
 NB. parsebox (<'a = 1')
 NB. parsebox"0 ('ENDDATE = "1999-02-16"'; 'b > 5')
 'column func value' =.  ' ' cut"1 > y
 -. [ 2&= [ (3!:0) value
 try. value =. ". value  NB. Convert num.
 catch.
 end.
 column;func;value)

indexf =: 4 : 0
 NB. Parse boxes into strings
 'column function value' =. parsebox"0 x
 NB. Select columns
 values =. > 1{:: [ (< column) select y
 NB. Generate function
 genfilter =: 1 : (':'; '  x u y')
 filterfn =: dyad : ('y', function ,'x')
 bar =. filterfn genfilter

 NB. Run function on column.
 NB. , value bar values

 NB. Numeric
 if. -. [ 2&= [ (3!:0) value
 do.
  NB. If not character:
  NB. Numeric
  selectind =. I. [, value bar values
 else.
  NB. TODO strings
  value =. ('"';'') stringreplace value
  len =. {. $ value
  selectind =. , I. [ len&= [ +/"1 values bar"1 value
  NB. tempa =.[ > 1{::temp2 NB. Select columns using parsed column index
  NB. selectind =. I. [ 10 = [ +/"1 [ tempa ="1 '1999-02-16' NB. Index columns using parsed function.
 end.
 < selectind)

intersectboxes =. 3 : 0
 NB. If there is only one box, no need to intersect.
 if. 1&=$$ y
 do. intersection =. ~. ((({.@$)=(+/@e.@;))#(;)) y
 else. intersection =. > y
 end.
 intersection)

filter2 =: 4 : 0
 NB. Index of boxes.
 cols =. 0 {:: y
 NB. Select rows.
 rowind =. intersectboxes [ x indexf"0 2 y
 NB. Rejoin.
 |: cols ,"0 [ (rowind)&{&.> 1 {:: y)





NB. monadic adverb.
fn =: 1 : 0
:
  x u y)
fn
bar =: = fn
bar
3 bar 5


NB. dyadic adverb.
fn =: 2 : 0
 z =. v y
 y u z)
fn
bar =: + fn *:
bar
bar 5

fn =: 2 : 0
:
u
x v y)
fn
bar =: [ fn =
bar
3 bar 3



NB. How to generate verb from parsed string?
5!:5 < '='


equal =. dyad : 'x = y'
1 equal 2

f =. 2 : 0
 NB. fn =. dyad : 'x ',y,' y'
 NB. fn
 'x ',y,' y')
f'='


fn
f =. 3 : ('x ','=',' y')
1 f 1
f'='
1 fn 1

2 fn 0

e =: 3 : '(>&0 y1) # y1'
y1 =. 1 0 2 3
e y


 NB. echo coname''
 NB. echo nl ''
 NB. erase 'g'
 NB. echo tradeslow
 NB. echo tradeshigh
 NB. echo assignable
 NB. conames''
 NB. cocurrent

conames''
coerase <'105'
'string' names_z_''
'join' names_z_''
NB. Examples of multiple assignment.
'hello there' =. (i.5);(i.10) NB. Gets unboxed.
'hello' =. <(i.5) NB. Does not get unboxed.

)
