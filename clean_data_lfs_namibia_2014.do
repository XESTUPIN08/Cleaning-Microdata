clear matrix
clear
cap log close
version 13
scalar drop _all
label drop _all


********** NAMIBIA: Overview of DW in 2014 ****************************************************************************
***********************************************************************************************************************

			use "C:\Users\estupinan\Desktop\Stata\Example Namibia\NAM_LFS_2014_FULL.dta", clear
			des, short
 			

			** Conversion of weight: Good rep given 2.5mill population
			
			
			gen wwpp=round(indw)
			
			gen hhid=FormNo
			gen indid=Person
			rename RegionCode region
			rename urbanr urban
			replace urban=0 if urban==2
					label define cd_uri 1 "Urban"
					label define cd_uri 0 "Rural", add
					label values urban cd_uri
					
			
			drop LocationCode SampleHouseHoldNumber QuestionaireNumber TotalNumberOfQuestionaires FieldStatusCode BaseFormNoLink FormNo Line Person
	        
			gen 		hhrel=1 if B03Rel==1
			replace 	hhrel=2 if B03Rel==2
			replace     hhrel=3 if (B03Rel==3|B03Rel==4)
			replace     hhrel=4 if (B03Rel==5|B03Rel==6|B03Rel==7)
			replace		hhrel=5 if hhrel==.
			
								 label define cd_hn 1 "HEAD"
								 label define cd_hn 2 "SPOUSE", add
								 label define cd_hn 3 "SON/DAUGHT", add
								 label define cd_hn 4 "OTHER REL", add
								 label define cd_hn 5 "NON-REL", add

								 label values hhrel cd_hn
			
			gen dumy=1
			egen hhsize=sum(dumy),  by(hhid)
			
			** Dummy to identify DW
			 
			gen 	dworker=1 if (B03Relation==8|B03Relation==9) & H03==1
			* ... but it could be that there are non-relatives who are not domestic workers
					replace dworker=0 if dworker==1 & (Indu>=2 & Indu<=19)|(Indu==21)
			* ... And now, let DWORKER if the combination E05E & Indu allow (even if INDU marks others)		
					replace dworker=1 if dworker==. & E05E==5
					replace dworker=1 if dworker==. & (E05E==6 & Indu==20)
					replace dworker=0 if dworker==.
			 
								label define  cd_dww 1 "Domestic worker"
								label define  cd_dww 0 "Not DW", add
								label values dworker cd_dww
								
			** GENDER dummy,
			
			gen male=1 if B04Sex==2
			    replace male=0 if male==.
				
			gen age=B05Age
			
			** HEAD
			** The variable HEAD=1 or 0 is a binary that - whenever needed - allows to shrink the data from individual level to household level
			** It is also needed to create variables at household level (i.e., vars that are identical for individual that share the same household)
			** To know how many households we have in the data, use 'duplicates report hhid' where HHID identifies households in the data
			 
			   duplicates report hhid
			
			** In the case of Namibia, we see that the the sum of (observations - surplus) given 'duplicates report hhid' equals 10378
			** Therefore, at most we have 10378 households: we have to assign the value HEAD=1 to at most 1 household member.
			** First, we eliminate all households that have only one member (dupli==0) with age below 16 or age above 70 (see selection criteria)
			
									duplicates tag hhid, gen(dupli)
									drop if (age<16|age>70) & dupli==0
									drop dupli
									
									* Note: we drop if age=99 and dupli==0 although we know that '99' is a code, not the age 
									* But given the lack of information for these individuals (single households) we have no choice but to drop them.
			 
			** The cleaing of single atipical households means that the number of households drop from 10378 to 10137							
			** So now we start creating the variable HEAD.
			** Step 1, assign HEAD=1 using the person classified as head of household (original househlld relation variable)
									
									gen head=1 if hhrel==1
									replace head=0 if head==.
			
			** Step 2, summarizes head at household level: if the sum is zero, no head assigned to household and you must assign one
			
									egen shead1=sum(head), by(hhid)
									* If shead1=0, the households does not have an assigned head: we have to assign one
									* If shead1>=2, the household has more than one head assigned: we must change so that only one has the assigment.
									
									* Some households with shead1=0 are unique members. All are given value head=1
									
									duplicates tag hhid if shead1==0, gen(dupli)
									replace head=1 if head==0 & dupli==0
									
						* Now HEAD has changed: so redo shead1 to see the situation
						
									drop dupli shead1
									egen shead1=sum(head), by(hhid)
									
						* To assign a head where no head exists: use INDI (unique value of individuals in hsouehold) 
						* Let the head be the one with lowest INDI value, as long as the person is age 16 to 70 (so they are not eliminated at the stage of sample selection)
						tab head
									egen hh1=min(indi*(shead1==0)), by(hhid)
									
									replace head=1 if head==0 & indi==hh1 & shead1==0 
						
						* ... HEAD has changed: so redo shead1 to see the situation
						tab head
						tab shead1
						drop shead1 hh1
									egen shead1=sum(head), by(hhid)
									tab shead1
									
						* To convert head=0 in households where more than one indiivuals was assigned head=1, use lowest value of indi*age
						* To select, given the possibility of same age, use hh2=indi*age (unique value for each individual at household level)
						* We use age*age because sometimes age*indi = age*indi for two or more in same house (e.g., ind=4 and age=24 // ind=2 and age=48)
						
									egen hh1=max(indi*age*age*(age>15 & age<71)*(shead1>=2 & shead1!=.)*(head==1)), by(hhid)
									
									egen hh1b=max(indi*age*age*(age>15 & age<71)*(shead1>=2 & shead1!=.)*(head==1)) if (age>=15 & age<=70), by(hhid)
									egen hh1c=max(hh1b), by(hhid)
									
									gen hh2=indi*age*age
									
									gen hhead=head
									replace head=0 if head==1 & (shead1>=2 & shead1!=.) & hh2!=hh1c 
												
						* ... HEAD has changed: so redo shead1 to see the situation
						
						tab head
						tab shead1
						drop shead1
									
									egen shead2=sum(head), by(hhid)
									tab shead2
						
						* Only one household left where assigned heads are too old
						* manually replace head=0 if age in this household is older than 70
						
									replace head=0 if head==1 & hhid==5644
									replace head=1 if head==0 & hhid==5644 & age==55 & indi==3
									
						egen shead3=sum(head), by(hhid)
						tab shead3 head
						drop shead* hh1* hh2 hhead
						
			** Ok: Now all households have a unique HEAD to identify them as  households units.
			** A problem, is that of having assinged HEAD=1 for members age <16 and age >70
			** When we apply the selection criteria where we eliminate young and old individuals, we would eliminate the HOUSEHOLD indicator if this was placed on these individuals
			** Therefore, we re-assign HEAD=1 to other households members.
						
						gen 		rehead1=1 if (head==1 & (age<16|age>70))
						replace		rehead1=0 if rehead1==.
									
									* This identifies households where an assigned head is too young or too old
									egen 		rehead=sum(rehead1), by(hhid)
									
									* Pick another member at each of the households with rehead=1
									egen 		rehead2=max(indi*age*age*(rehead==1)*(age>16 & age<71)), by(hhid)
									
									* Comparison assigned to all household members
									egen		rehead3=max(rehead2), by(hhid)
									replace		rehead3=. if rehead3==0
									
									gen			hh1=indi*age*age
						
						tab head
									replace head=-9 if rehead1==1
						tab head			
									replace head=1  if head==0 & rehead==1 & hh1==rehead3 & rehead3!=.
						tab head			
									egen hh5=sum(head==-9), by(hhid)
									egen hh6=sum(head==1), by(hhid)
									
									** If house has a -9 and not a head, we drop the house
									egen hh7=mean((hh5==1)*(hh6==0)), by(hhid)
									drop if hh7==1
									drop hh1 hh5 hh6 hh7 rehead*
									
						* Now, all -9 can be converted to 0 (other household members asigned 1 in line 187)
						
						replace head=0 if head==-9
						
						* Final check to eliminate possible duplicated heads 
						
									duplicates tag hhid if head==1, gen(dupli)
									egen hh0=sum(dupli==1), by(hhid)
									
									gen hh1=indi*age*age*(age>15 & age<71) if (hh0>=1 & hh0!=.)
									
									egen hh2=max(hh1) if hh1!=., by(hhid)
									replace head=0 if head==1 & hh2!=. & hh1!=hh2
									
						duplicates tag hhid if head==1, gen(dupli2)
						egen hh02=sum(dupli2==1), by(hhid)
						
			drop hh0 hh1 hh2 hh02 dupli*
			
			* A non-relative cannot be assigned as head (e.g, if DW, her income should not count)
			
						replace head=0 if head==1 & hhrel==5
						egen nhead=sum(head), by(hhid)
						tab nhead
						
						* Need select new head for households in 73 households (311 indivudals)
						
						gen hh1=indi*age*age*(age>15 & age<71)*(hhrel<=4) if (nhead==0 & hhrel<=4)
						egen hh2=max(hh1), by(hhid)
						gen hh3=indi*age*age if nhead==0
						replace hh3=0 if hh3!=. & hhrel==5
						
						replace head=1 if head==0 & hh2==hh3 & hh2!=. & hhrel<=4
			
			** Anyother household without heads is a combination of very young, very old and non-relative
			** We eliminate these households
			
			drop nhead hh1 hh2 hh3
			
			egen s1=sum(head), by(hhid)
			drop if s1==0
			drop s1
			
			duplicates report hhid 
			tab head
			** Clear two remaining inconsistencies (too young and non-relative)
			
							drop if hhid==4962
							drop if hhid==9994
							
			* End: HEAD now constructed, with all household whose age composition does not enter the selection criteria (age>15 & age<71) eliminated
			* Therefore, we have eliminated all who do not have any attachment to the labour market.
			* All individuals are attached to a household via the indicator HEAD=1
			* In total we have 9700 households, from the original number of 10378 (dropped 678 households: about 2000 individual, very young/ very old)
			* And if HEAD=1 this is a relative of the household
			
			
			** living in DW
			
			gen lvin_dw=1 if E05Employ==5 & (B03Rela==8|B03Rela==9) & H03==1
			replace lvin_dw=0 if lvin_dw==.
		
			** Make sure that living domestic is not head
			
			tab head lvin_dw
			
			** Just in case you had a DW as head, go through the following proceedure
			
							egen dd1=sum(head*lvin_dw), by(hhid)
							egen dd2=max(indid*dd1*(1-lvin_dw)), by(hhid)
							replace head=0 if head==1 & lvin_dw==1
							replace head=1 if head!=1 & lvin_dw==0 & dd1>=1 & dd1!=.
							drop dd1 dd2 
			
				
			* Drop EU and other developed economies/ non-african countries
			gen out=1 if B06>=9
			
			replace out=0 if out==.
			egen hout=sum(out), by(hhid)
			
			
			** We drop 39 households (134 individuals) that are not Namibian or SSAfrica
			drop if hout>=1
			drop out hout
			
			* Now, create migrant dummy where migrants are from other African countries
			gen migrant=1 if B06!=1
			replace migrant=0 if migrant==.
			
								 label define cd_mg 0 "local"
								 label define cd_mg 1 "migrant", add
								 
								 label values migrant cd_mg
			
			
			
			** Partner status
			gen in_rel=1 if (B08>=2 & B08<=3)
			replace in_rel=0 if in_rel==.
			
			
								 label define cd_ir 0 "No partner"
								 label define cd_ir 1 "Partner", add
								 
								 label values in_rel cd_ir
			
			
			
			** Employment Status
			 
			gen emp_er=0 if E05Employ==.
							replace emp_er=1 if (E05Employ==5|E05Em==6) 
							replace emp_er=2 if (E05Em==1|E05Em==3) 
							replace emp_er=3 if (E05Em==2|E05Em==4)
							replace emp_er=4 if (E05Em>=7 & E05Em!=.)
							replace emp_er=5 if ((D10WhyNotWork>=6 & D10WhyNotWork<=8)|D10WhyNotWork==11|D10WhyNotWork==99)
							replace emp_er=0 if emp_er==5 & age<=10
							
			** adjustments
			rename E05Em w_type
			replace emp_er=1 if D01P==1 & emp_er==0|emp_er==5
			replace emp_er=1 if workstatusbroad==1 & emp_er==0
			replace emp_er=2 if emp_er==1 & (w_type<=4)
			replace emp_er=4 if emp_er==1 & (w_type>=7 & w_type!=.)
			replace emp_er=3 if emp_er==2 & (w_type==2)
			replace emp_er=3 if emp_er==2 & (w_type==4)
			replace emp_er=1 if (emp_er==0|emp_er==5) & E13!=.
			
			** Adjusting OLF and UNEMPLOYED
			
			replace emp_er=5 if emp_er==0 & (G01R==1|G03L==1)
			
			
								 label define cd_tr 0 "OLF"
								 label define cd_tr 1 "wage EE", add
								 label define cd_tr 2 "Emplyer", add
								 label define cd_tr 3 "OAW", add
								 label define cd_tr 4 "Unpaid", add
								 label define cd_tr 5 "Unemp", add

								 label values emp_er cd_tr
			
			
	** At the beginning, a few individuals with age=99, which seems to be a code (the jump after 94 is suspicious)
	** So we try to impute information as long as these are in the labour market (left as example: in current data, all 99 already gone)
	** Furthermore, we cannot allow households with heads and spouses of ages below 14
								
								* About 16 households are dropped.
								egen clean1=sum(age<=14 & (hhrel==1|hhrel==2)), by(hhid)
								drop if clean1!=0
		
		
								* Replace with empty to eliminate it from below estimates.
								replace age=. if age==99 
								
								* Heads and spouses of households cannot be below the age of 15 (say)
								
								
								* Identify households where the problem occures
								egen idh99_a=max(hhid*(age==.)), by(hhid)
							    replace idh99_a=. if idh99_a==0
								
								egen ag_re1=max(age*(hhrel==1)), by(hhid)
								egen ag_re2=max(age*(hhrel==2)), by(hhid)
								egen ag_re3=mean(age*(hhrel==3)), by(hhid)
								egen ag_re4=mean(age*(hhrel==4)), by(hhid)
								egen ag_re5=mean(age*(hhrel==1|hhrel==2)), by(hhid)
								
											replace age=ag_re1 if age==. & hhrel==2 & ag_re1>=14 & ag_re1!=.
											replace age=ag_re2 if age==. & hhrel==1 & ag_re2>=14 & ag_re2!=.
											
											* Otherwise, for head and sposue, allow 18+ age if their children
											replace age=ag_re3+18 if age==. & hhrel==2 
											replace age=ag_re3+18 if age==. & hhrel==1
											
											* For other members, let it be
											replace age=ag_re3 if age==. & hhrel==3 
											replace age=ag_re4 if age==. & hhrel==4
											replace age=ag_re5 if age==. & hhrel==5 & (emp>=1 & emp<=4)
											replace age=ag_re3 if age==. & hhrel==5 & (emp==0|emp==5)
											
								
								*If age is missing apply average in the household, irrespective of HHREL, without weights, just a value!
								
								egen ag_hh=mean(age), by(hhid)
								replace age=ag_hh if age==.
								
								drop ag_re* ag_hh
			
			replace age=round(age)
			
			
			** Consistency between AGE and LMS
			tab age emp 
							** Before any cleaning, the lowest age of participants is 10.
							** So we make sure we preserve this in the data
							
							gen clean2=1 if (age<=7 & (emp>=1 & emp<=4))
							
							** All are not heads (by default)
							** Do they declare incomes? Yes, most do. Therefore, we do not eliminate them.
									** They are mostly 'relatives' or 'non-relatives'. So in part, their incomes will not count towards HH income
									** Eliminated at the point of sample selection criteria.
									su E13* if clean2==1
							
								
			** size workforce at work 
			rename E08Num fsize
			
			** Informal
			
			gen 			formal=1 if informal==1
			replace 		formal=0 if informal==2
			replace			formal=0 if (emp_er>=1 & emp_er<=4) & formal==.
			
								 label define cd_frr 1 "1:FORMAL"
								 label define cd_frr 0 "0:INFORMAL", add
								 
								 label values formal cd_frr
								 drop informal
			
			
			** TYPE OF CONTRACT: Assume that 'unspecified duration' implies 'without a formal contract'
			
			
			gen 	ctract=1 if E12C==2
			replace ctract=2 if E12C==1 
			replace ctract=3 if E12C==3 
			
			replace ctract=. if (emp_er==0|emp_er==5)
			replace ctract=3 if ctract==. & (emp_er>=1 & emp_er<=4)
			* Some who claim 'unspecified' are formal, therefore have a contract
			* So we re-classify due to the ambeguity of 'unspecified duration'
			replace ctract=1 if ctract==3 & formal==1
			
								 label define cd_toc 1 "1:PERMANENT"
								 label define cd_toc 2 "2:TEMPORARY", add
								 label define cd_toc 3 "3:W/OUT CRTCT", add
								 
								 label values ctract cd_toc
								
			
			** Actual hours worked per week
			
			mvencode F01* F02* F03* F04* F05* F06* F07* if (emp_er>=1 & emp_er<=4), mv(0) override
			
			forvalues i=1(1)7{
			
			
					replace F0`i'A=. if F0`i'A!=. & (emp_er==0|emp_er==5)
					replace F0`i'B=. if F0`i'B!=. & (emp_er==0|emp_er==5)
					
					replace F0`i'A=9 if F0`i'A>=25 & F0`i'A!=.
					replace F0`i'B=9 if F0`i'B>=25 & F0`i'B!=.
					
								gen d`i'a=1 if (F0`i'A>=1 & F0`i'A<=24) 
								gen d`i'b=1 if (F0`i'B>=1 & F0`i'B<=24)
					
					}
					**
			
			
			gen hrs_wa=F01B+F02B+F03B+F04B+F05B+F06B+F07B
			gen hrs_wu=F01A+F02A+F03A+F04A+F05A+F06A+F07A
			
			
			replace hrs_wa=105 if hrs_wa>=105 & emp_er>=1 & emp_er<=4
			replace hrs_wu=105 if hrs_wu>=105 & emp_er>=1 & emp_er<=4
			replace hrs_wa=. if hrs_wa!=. & (emp_er==0|emp_er==5)
			replace hrs_wu=. if hrs_wu!=. & (emp_er==0|emp_er==5)
			
			mvencode d1* d2* d3* d4* d5* d6* d7* if (emp_er>=1 & emp_er<=4),mv(0) override
			
			
			gen day_wu=d1a+d2a+d3a+d4a+d5a+d6a+d7a if (emp_er>=1 & emp_er<=4)
			gen day_wa=d1b+d2b+d3b+d4b+d5b+d6b+d7b if (emp_er>=1 & emp_er<=4)
			
							
							** Missing hours and day information: usual as actual and actual as usual if missing
							
							replace hrs_wa=hrs_wu if (hrs_wa==.|hrs_wa==0) & hrs_wu!=. & hrs_wu!=0 & (emp_er>=1 & emp_er<=4)
							replace hrs_wu=hrs_wa if (hrs_wu==.|hrs_wu==0) & hrs_wa!=. & hrs_wa!=0 &  (emp_er>=1 & emp_er<=4)
							
							
							replace day_wa=day_wu if (day_wa==.|day_wa==0) & day_wu!=0 & day_wu!=. & (emp_er>=1 & emp_er<=4)
							replace day_wu=day_wa if (day_wu==.|day_wu==0) & day_wa!=0 & day_wa!=. & (emp_er>=1 & emp_er<=4)
			
			** If outside of the market...
							** about 169 are with 'usual and actual hours worked or days worked are zerO"
							** MOSTLY are unemployed if above 15 and olf if 15 or less
							
							replace emp_er=0 if age<=15 & day_wu==0 & emp_er!=0
							replace emp_er=5 if age>=16 & day_wu==0 & emp_er!=5
			
							* Declares paid for work, so cannot be unemployed/olf
							replace emp_er=1 if (emp_er==0|emp_er==5) & E13!=.
			
			
			count if emp_er>=1 & emp_er<=4 & (hrs_wa==0)
			count if emp_er>=1 & emp_er<=4 & (hrs_wa==.)
			
			count if emp_er>=1 & emp_er<=4 & (hrs_wu==0)
			count if emp_er>=1 & emp_er<=4 & (hrs_wu==.)
			
			count if emp_er>=1 & emp_er<=4 & (day_wa==0)
			count if emp_er>=1 & emp_er<=4 & (day_wa==.)
			
			count if emp_er>=1 & emp_er<=4 & (day_wu==0)
			count if emp_er>=1 & emp_er<=4 & (day_wu==.)
			
			su hrs_* day_* if (emp_er>=1 & emp_er<=4) & hrs_wa!=0 [fw=wwpp]
			
			
			** Otherwise.....
			
							** There are about 13 individuals, shared aprox equal between employment types, with zero hrs, days worked (all, usual and actual)
							** All show incomes coherent with working some hours and days. 
							
							** By occupation, industry, region, gender, age, income and type of contract
							
							** AGE GROUPS by their distribution in the population
							gen agec=1 if age<=20 
							replace agec=2 if age>=21 & age<=35
							replace agec=3 if age>=36 & age<=55
							replace agec=4 if age>=56 & age<=70
							replace agec=5 if age>=71
							
							** income groups, by their distribution in the sample
							gen incc=1 if E13I<=500
							replace incc=2 if E13I>500 & E13I<=900
							replace incc=3 if E13I>900 & E13I<=2000
							replace incc=4 if E13I>2000 & E13I<=6800
							replace incc=5 if E13I>6800 & E13I<=15000
							replace incc=6 if E13I>15000 & E13I<=50000
							replace incc=7 if E13I>50000 & E13I!=.
							
							
							
							
							forvalues a=1(1)5{
							forvalues i=1(1)7{
							forvalues o=1(1)11{
							forvalues d=1(1)22{
							
							su hrs_wa if (hrs_wa!=. & hrs_wa>=1)  & agec==`a' & incc==`i' &  Occu==`o' & Ind==`d' [fw=wwpp]
							replace hrs_wa=round(r(mean)) if hrs_wa==0 & (emp_er>=1 & emp_er<=4) & agec==`a' & incc==`i' &  Occu==`o' & Ind==`d' 
							replace hrs_wu=round(r(mean)) if hrs_wu==0 & (emp_er>=1 & emp_er<=4) & agec==`a' & incc==`i' &  Occu==`o' & Ind==`d' 
							su day_wa if (day_wa!=. & day_wa>=1) & agec==`a' & incc==`i' &   Occu==`o' & Ind==`d' [fw=wwpp]
							replace day_wa=round(r(mean)) if day_wa==0 & (emp_er>=1 & emp_er<=4) & agec==`a' & incc==`i' &   Occu==`o' & Ind==`d' 
							replace day_wu=round(r(mean)) if day_wu==0 & (emp_er>=1 & emp_er<=4) & agec==`a' & incc==`i' &   Occu==`o' & Ind==`d' 
							}
							}
							}
							}
							**
							
							** only one individual missing remain unknown (indu, ocu, edu provided)
							su hrs_wa if Indu==14 & Occu==8 & Edu==3 [fw=wwpp]
							replace hrs_wa=r(mean) if hrs_wa==. & emp==1
							replace hrs_wu=r(mean) if hrs_wu==. & emp==1
							
							replace day_wa=5 if day_wa==. & (emp>=1 & emp<=3)
							replace day_wu=5 if day_wu==. & (emp>=1 & emp<=3)
							
							
			** Schooling & Education
			rename Edu edu_cat
			rename Indu indu_cat
			rename Occu ocu_cat
			
			
			** Benefits, dummy fro EMP=1,2,3,4
			
			gen b_food=1 if E09A==1
			gen b_acom=1 if E09B==2
			gen b_cloth=1 if E09C==3
			gen b_trans=1 if E09D==4
			gen b_pensi=1 if E09E==5
			gen b_medic=1 if E09F==6
			gen b_socse=1 if E09G==7
			gen b_other=1 if E09H==8
			gen b_non=1 if E09I==9
			
			mvencode b_* if (emp_er>=1 & emp_er<=4),mv(0) override
			
			drop E09*
			
			** Allowed leave, dummy fro EMP=1,2,3,4
			
			gen lv_sick=1 if E11A==1
			gen lv_mater=1 if E11B==2
			gen lv_vaca=1 if E11C==3
			gen lv_compen=1 if E11D==4
			gen lv_study=1 if E11E==5
			gen lv_other=1 if E11F==6
			gen lv_non=1 if E11G==9
			
			mvencode lv_* if (emp_er>=1 & emp_er<=4),mv(0) override
			
			drop E11*
			
			** Income
			
			gen li_mg=E13Income
			
			drop E13Income
			
			** Inspecting income:
								
								* Not paid, zero income
								replace li_m=0 if emp_er==4
								
								* who is missing?
								gen inc_mis=1 if (emp_er>=1 & emp_er<=4) & li==.
								replace inc_mis=0 if inc_mis==. & (emp_er>=1 & emp_er<=4)
								
								* We could only impute the income of wage employees with basic information
								
								tab indu_cat, gen(indi_)
								tab edu_cat, gen(edui_)
								tab w_type, gen(tp_)
								tab ocu_cat, gen(ocup_)
								gen age2=age*age
								tab region, gen(regi_)
								tab fsize, gen(siz_)
								gen lnhrs=ln(hrs_wa)
								
								gen ln_y=ln(li_mg)
								
										reg ln_y age age2 edui_* tp_* ocup_* male regi_* urban lnhrs if emp_er==1 [fw=wwpp]
										predict py_lny if emp_er==1
										
										su li_mg if emp_er==1
										replace li_mg=exp(py_lny) if emp_er==1 & inc_mis==1
										su li_mg if emp_er==1
										drop edui_* ocup_* siz_* py_lny 
										
										** for anyone else, direct imputation by indu and region
										
										forvalues r=1(1)14{
										forvalues i=1(1)22{
										forvalues m=1(1)9{
													su li_mg if region==`r' & indu_cat==`i' & edu==`m' & emp_er==1 [fw=wwpp]
													replace li_mg=r(mean) if region==`r' & indu_cat==`i' & emp_er==1 &  edu==`m' &  li_mg==.
													}
													}
													}
										**
										
										* Employers, some missing
										reg ln_y indi_* regi_* formal urban male age age2 migrant lnhrs if emp_er==2 [fw=wwpp]
										predict py_lny if emp_er==2
										
										su li_mg if emp_er==2 [fw=wwpp]
										replace li_mg=exp(py_lny) if emp_er==2 & inc_mis==1
										su li_mg if emp_er==2 [fw=wwpp]
										
										
										
										* OAW, most are missing so use direct impurtaiton for all
										* This should not be done, but serves as example for programming - to get GPG using different groups of employees.
										
										gen 		wph=li_mg/(hrs_wa*4.333)
										replace 	wph=. if (emp==0|emp==5)
										replace		wph=. if wph==0 & (emp>=1 & emp<=3)
										replace		wph=0 if wph!=0 & emp==4
										
										
										
										forvalues m=0(1)1{
										forvalues i=1(1)22{
										forvalues e=1(1)9{	
													su wph if wph!=. & indu_c==`i' & male==`m' & emp_er==1 & edu==`e'  [fw=wwpp]
													replace wph=r(mean) if wph==. & indu_c==`i' & male==`m' & emp_er==1  & edu==`e'
													su wph if wph!=. & indu_c==`i' & male==`m' & emp_er==3 & edu==`e'  [fw=wwpp]
													replace wph=r(mean) if wph==. & indu_c==`i' & male==`m' & emp_er==3  & edu==`e'
													su wph if wph!=. & indu_c==`i' & male==`m' & emp_er==2  & edu==`e' [fw=wwpp]
													replace wph=r(mean) if wph==. & indu_c==`i' & male==`m' & emp_er==2  & edu==`e'
													}
													}
													}
													
										**
										 
										** take only 'region' and gender 
																		
										forvalues m=0(1)1{
										forvalues r=1(1)14{
										
													su wph if wph!=. & male==`m' & region==`r' & emp_er==1  [fw=wwpp]
													replace wph=r(mean) if wph==. & region==`r' & male==`m' & emp_er==1
													
													su wph if wph!=. & male==`m' & region==`r' & emp_er==2  [fw=wwpp]
													replace wph=r(mean) if wph==. & region==`r' & male==`m' & emp_er==2
													
													su wph if wph!=. & male==`m' & region==`r' & emp_er==3  [fw=wwpp]
													replace wph=r(mean) if wph==. & region==`r' & male==`m' & emp_er==3
													}
													}
													
										**
										 
								** No imput missing for OAW, but only valid for the example.
								
								replace li_mg=wph*hrs_wa*4.333 if li_mg==. & (emp>=1 & emp<=3)
								replace li_mg=0 if emp==4
								
								** Now just a few missing incomes, we go for education, occupation, gender and region as way to impute few left missing
								
					** Next, in the case of wage employees, we have to 'guess' how much to include in the form of income as result of FOOD, ACCOMODATION and TRANSPORT
					** FOOD = 5%; ACCOM=15% and TRANSPORT=5%
					
										* We can think that at most, inkind is 20% of the actual received salary
										* So, at most, if person receives all three types it will upgrade salary by 20%, with accomodation taking the largest part.
										
										gen inkind=((b_food*0.03)+(b_trans*0.02)+(b_acom*0.15))*li_mg
										replace inkind=0 if inkind==.
										
										gen li_m=li_mg+inkind
										drop li_mg inkind
										
			
			** PER CAPITA HOUSEHOLD INCOME: HOW TO GENERATE IT
			
						** TOTAL INCOME
						** AVERAGE HOUSEHOLD INCOME (INCLUDES ALL)
						** AVERAGE labour income of household's participants (IF PERSON IS ATTACHED TO THE LABOUR MARKET)
						
						
						** FIRST: WE EXCLUDE ALL 'NON-RELATIVES' IN THE HOUSEHOLDS (E.G., DOMESTIC WORKERS AND THEIR CHILDREN) ASSUMING THAT THEIR INCOMES ARE NOT RESOURCES TO THE HOUSEHOLD
						
						egen nadult=sum((age>15 & age!=.)*(hhrel<=4)), by(hhid)
						egen nkids=sum((age<15)*(hhrel<=4)), by(hhid)
						
						** SECOND: EQUIVALENT SCALES, discounting non-relatives where it is recoreded as she or he does not share income in the same way as others
					
									gen ea=(nadult+((nkids)*0.3))^(0.9)
									gen sinc=li_m
									replace sinc=0 if li_m==.
									
									* And for total income we allow only the sum of anyone that is a direct relative to the head
									* At this point, anyone that contributes has to add up to the total (including kids)
									* Later we exclude kids from the analysis because of the age restriction (16,70) and because they are unlikely subjects of wage policies
									
									egen hh_inc=sum(sinc*(hhrel<=4)), by(hhid)
					
											** Some households declare zero income, but have info on H02
											** Of course, here we assume they declare similar information as above.
											
											replace hh_inc=1000 if hh_inc==0 & H02==1
											replace hh_inc=2000 if hh_inc==0 & H02==2
											replace hh_inc=3000 if hh_inc==0 & H02==3
											replace hh_inc=4000 if hh_inc==0 & H02==4
											replace hh_inc=5000 if hh_inc==0 & H02==5
											replace hh_inc=6000 if hh_inc==0 & H02==6
											replace hh_inc=7000 if hh_inc==0 & H02==7
											replace hh_inc=8000 if hh_inc==0 & H02==8
											replace hh_inc=9000 if hh_inc==0 & H02==9
											replace hh_inc=10000 if hh_inc==0 & H02==10
											replace hh_inc=15000 if hh_inc==0 & H02==11
											
									
					** FINALLY: PER CAPITA HOUSEHOLD INCOME
									
									gen pc_hinc=hh_inc/ea
					
					** OTHER MEASURES:
									
									** AVERAGE EARNINGS AMONG INDIVIDUALS IN THE HOUSEHOLD INDIVIDUALS (INCLUDE UNPAID FAMILY WORKERS)
									
									egen nwrks=sum((emp_er>=1 & emp_er<=4)*(hhrel<=4)), by(hhid)
									
									gen pw_li=hh_inc/nwrks
									
									
									** AVERAGE EARNINGS AMONG INDIVIDUALS IN THE HOUSEHOLD (INCLUDE UNPAID FAMILY WORKERS) EXCLUDING OWN INCOME 
									
									gen exlin=hh_inc-sinc
									
									gen pw_lix=exlin/(nwrks-1) if (emp_er>=1 & emp_er<=4)
			
			** Finally, number of workers in household who are related to the household head
			
			egen num_ws=sum((emp>=1 & emp<=4)*(hhrel<=4)), by(hhid)
			
					
			*** done: drop all other vars.
			
			drop B03Relation B04 B05 B06 B07AOldAge B07BWarVet B07CDisability B07DChildMaint B07EFoster B07FSpecialMaint B07GWorkComp B07HOtherGrant D01PayProfit D02Business D03UnpaidWork D04HHFarm	
			drop B07INoGrant B07JDontKnow B08
			drop C01Schooling C03HighestGrade D05WaterFuel D06Goods4Sale D08Fish D09Work2Return D10WhyNotWork D11PaidWhileAbsent E02Occup E06Registered E07EntityType
			drop D07Construction E04Indus E12Contract F01AMonUsual F01BMonActual F02ATueUsual F02BTueActual F03AWedUsual F03BWedActual F04AThuUsual F04BThuActual F05AFriUsual F05BFriActual F06ASatUsual F06BSatActual F07ASunUsual F07BSunActual F08ATotalUsual F08BTotalActual F10WhereMore F09LikeMore F11HowManyHours F12LookAdditional G01Ready2Work G03Look4Work G04ARegisterMOL G04BRegisterOther G04CDirectApply G04DCheckWorkSites G04EMediaAdvert G04FFriendsAssists G04GStartBusiness G04HOther G05YNotLook G06HowLongNoWork G07Worked12Months G09LastOccup G11LastIndus G11LastIndus G12LastRegion G13YLeftJob G13YLeftJob G14HowSupport workstatusbroad workstatstrict agegroup
			drop d1a d1b d2a d2b d3b d3a d4a d4b d5a d5b d6a d6b d7a d7b
			drop indw  E10PA G02L
			drop indi_* tp_* age2 regi_* lnhrs ln_y py_lny wph
			drop H* w_type lvin_dw 
			drop sinc agec incc nwrks clean1 clean2 exlin
			
			** Finally, clean var domestic worker
			
			replace dworker=0 if (emp==0|emp==2|emp==3|emp==5)
			
			
***********************************************************************************************************************
*****************************************************************************************************************

**END DO FILE

			save C:\Users\estupinan\Desktop\Stata\Example Namibia\NLFS_2014_MASTERDATA_Oct2022, replace
			
