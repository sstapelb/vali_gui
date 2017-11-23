; scores mainly taken from http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/

pro skills_n_scores, reference, fcd, skills = skills, no_data_value = no_data_value	, $	; input
		hit = hit, false = false, miss = miss, core = core			, $ 	; input
		hss = hss, fbi = fbi, ets = ets, pec = pec, pod = pod, far = far	, $	; output
		ths = ths, tss = tss, pcf = pcf, mir = mir, puc = puc, all = all	, $	; output
		string_all = string_all 							; write all in one string in defined order

	fillv = keyword_set(no_data_value) ? no_data_value : -999; kind of obsolete but anyway!

	if keyword_set(hit) and keyword_set(false) and keyword_set(miss) and keyword_set(core) then begin
		aa = hit	; hit
		bb = false	; false alarm
		cc = miss	; missed
		dd = core	; correct rejection
	endif else if keyword_set(skills) then begin
		aa = long64(skills[0]); hit
		bb = long64(skills[1]); false alarm
		cc = long64(skills[2]); missed
		dd = long64(skills[3]); correct rejection
	endif else begin
		if n_elements(reference) ne n_elements(fcd) then begin
			print,'Observed and forcasted data must have same dimension!'
			return
		endif
		dum = where(reference ne fillv and fcd ne fillv and reference eq 1 and fcd eq 1,aa,/l64)	; hit
		dum = where(reference ne fillv and fcd ne fillv and reference eq 0 and fcd eq 1,bb,/l64)	; false alarm
		dum = where(reference ne fillv and fcd ne fillv and reference eq 1 and fcd eq 0,cc,/l64)	; missed
		dum = where(reference ne fillv and fcd ne fillv and reference eq 0 and fcd eq 0,dd,/l64)	; correct rejection
		skills = [aa,bb,cc,dd]
	endelse

	if arg_present(hss)+arg_present(fbi)+arg_present(ets)+arg_present(pec)+arg_present(pod)+ $
	   arg_present(far)+arg_present(ths)+arg_present(tss)+arg_present(pcf)+arg_present(mir)+ $
	   arg_present(puc)+arg_present(all)+arg_present(string_all) eq 0 then print_it = 1 else print_it = 0

	if arg_present(string_all) then begin
		idx   = [arg_present(pec) or keyword_set(pec),arg_present(pod) or keyword_set(pod), $
			 arg_present(far) or keyword_set(far),arg_present(mir) or keyword_set(mir), $
			 arg_present(ths) or keyword_set(ths),arg_present(fbi) or keyword_set(fbi), $
			 arg_present(hss) or keyword_set(hss),arg_present(ets) or keyword_set(ets), $
			 arg_present(tss) or keyword_set(tss),arg_present(pcf) or keyword_set(pcf), $
			 arg_present(puc)]
	endif

	; Genauigkeitsmaße

	; percent correct 
	pec = ( 100. * (aa + dd) ) / (double( aa + bb + cc + dd ) > 1d0)
	; percent uncorrect
	puc = ( 100. * (bb + cc) ) / (double( aa + bb + cc + dd ) > 1d0)
	; probability of detection ; sensitivity : wahrscheinlichkeit das wenn Wolken verhergesagt wurden auch wolken auftraten
	; Problematisches Mass wenn zum Bsp nur Wolken und niemals Wolkenfrei vorhergesagt wird ist POD auch immer 1
	pod = aa / (double( aa + cc ) > 1d0)
	; prob. of cloud free detection ; specificity wahrscheinlichkeit das wenn wolkenfrei verhergesagt wurde auch keine wolken auftraten
	pcf = dd / (double( dd + bb) > 1d0)
	; false alarm ratio!!; verhältnis von ereignissen wo Ja (FALSE ALARM) vorhergesagt wurde aber keiner stattgefunden hat.
	far2 = bb / (double( aa + bb ) > 1d0)
	; false alarm rate!! (Probability of false detection); (false alarms) / (number of observations of the non-event)
	far = bb / (double( bb + dd ) > 1d0)
	; missing ratio ; missing / summe aller vorhergesagten wolkenfrei Fälle
	mir = cc / (double( cc + dd ) > 1d0)
	; frequency bias
	fbi = ( aa + bb ) / (double( aa + cc ) > 1d0)
	; threat score ; Gütemaß
	ths  = aa / (double( aa + bb + cc ) > 1d0)

	; !!skill scores : Vorhersagen werden mit Referenzprognosen (Zufall, Persistenz, klimatologisches Mittel) verglichen

	; heidke skill score (HSS)
	; von -1 bis 1, 1 perfekte vorhersage, 0 Vorhersage gleich der Referenzvorhersage (Zufall), -1 schlechter als Referenzvorhersage 
	rr   = ( ( aa + bb ) * ( aa + cc ) + ( cc + dd ) * ( bb + dd ) ) / (double( aa + bb + cc + dd) > 1d0) ; referenzvorhersage : Zufall
	hss  = ( aa + dd - rr) / (double( aa + bb + cc + dd -rr ) > 1d0)
	hss2 = ( 2 * ( aa*dd - bb*cc ) ) / (double( ( (aa + cc) * (cc + dd) ) + ( ( aa + bb ) * ( bb + dd ) ) ) > 1d0)
	; true skill statistic / Hansen-Kuiper = Hitrate (POD) - False Alarm Rate (FAR) 
	; wie HSS nur frei von systematischen Abweichungen (unbiased)
	; von -1 bis 1, 1 perfekte vorhersage, 0 Vorhersage gleich der Referenzvorhersage (Zufall), -1 schlechter als Referenzvorhersage 
	tss = ( ( aa * dd ) - ( cc * bb ) ) / ( double( ( aa + cc ) * ( bb + dd ) ) > 1d0)
	; equitable threat score
	ch_aa = ( ( aa + bb ) * ( aa + cc ) ) / ( double( aa + bb + cc + dd ) > 1d0) ; Wert für Stichprobenzufall von aa
	ets = ( aa - ch_aa ) / (double( aa + bb + cc - ch_aa) > 1d0)

	all = {	percent_correct:pec,prob_of_detection:pod,false_alarm_ratio:far2,false_alarm_rate:far,missing_ratio:mir,$
		threat_score:ths,frequency_bias:fbi,heidke_skill_score:hss,equitable_threat_score:ets,true_skill_statistic:tss,$
		prob_of_cf_detection:pcf,percent_uncorrect:puc }

	if arg_present(string_all) then begin
		array = [pec,pod,far,mir,ths,fbi,hss,ets,tss,pcf,puc]
		if total(idx) eq 0 then idx = intarr(10)+1
		string_all = strjoin(string(array[where(idx eq 1)],f='(f9.4)'),' ,')
	endif

	if print_it then begin
		print, 'hit               : ', aa
		print, 'false alarm       : ', bb
		print, 'missed            : ', cc
		print, 'correct rejection : ', dd
		shelp,all
	endif
end