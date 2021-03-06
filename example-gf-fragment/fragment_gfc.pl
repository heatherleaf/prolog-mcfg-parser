%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Simple GFC grammar in Prolog-readable format
%% Autogenerated from the Grammatical Framework

%% The following predicate is defined:
%% 	 rule(Fun, Cat, c(Cat,...), LinTerm)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Language module: gfc_FragmentSwedish

gfc_FragmentSwedish : rule(d_many, 'D', c, 
	rec([(s = tbl([(('Neu' ^ []) = tok(m�nga)), (('Utr' ^ []) = tok(m�nga))])), (n = ('Pl' ^ []))])).
gfc_FragmentSwedish : rule(d_one, 'D', c, 
	rec([(s = tbl([(('Neu' ^ []) = tok(ett)), (('Utr' ^ []) = tok(en))])), (n = ('Sg' ^ []))])).
gfc_FragmentSwedish : rule(n_fish, 'N', c, 
	rec([(s = tbl([(('Sg' ^ []) = tok(fisk)), (('Pl' ^ []) = tok(fiskar))])), (g = ('Utr' ^ []))])).
gfc_FragmentSwedish : rule(n_lion, 'N', c, 
	rec([(s = tbl([(('Sg' ^ []) = tok(lejon)), (('Pl' ^ []) = tok(lejon))])), (g = ('Neu' ^ []))])).
gfc_FragmentSwedish : rule(np1, 'NP', c('N'), 
	rec([(s = (arg('N', 1, [s]) / ('Pl' ^ [])))])).
gfc_FragmentSwedish : rule(np2, 'NP', c('D', 'N'), 
	rec([(s = ((arg('D', 1, [s]) / arg('N', 2, [g])) + (arg('N', 2, [s]) / arg('D', 1, [n]))))])).
gfc_FragmentSwedish : rule(s, 'S', c('NP', 'VP'), 
	rec([(s = tbl([(('Decl' ^ []) = (arg('NP', 1, [s]) + (arg('VP', 2, [s1]) + arg('VP', 2, [s2])))), (('Que' ^ []) = (arg('VP', 2, [s1]) + (arg('NP', 1, [s]) + arg('VP', 2, [s2]))))]))])).
gfc_FragmentSwedish : rule(v_eat, 'V', c, 
	rec([(s = tok(�ter))])).
gfc_FragmentSwedish : rule(v_hunt, 'V', c, 
	rec([(s = tok(jagar))])).
gfc_FragmentSwedish : rule(vp, 'VP', c('V', 'NP'), 
	rec([(s1 = arg('V', 1, [s])), (s2 = arg('NP', 2, [s]))])).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Language module: gfc_FragmentEnglish

gfc_FragmentEnglish : rule(d_many, 'D', c, 
	rec([(s = tok(many)), (n = ('Pl' ^ []))])).
gfc_FragmentEnglish : rule(d_one, 'D', c, 
	rec([(s = tok(a)), (n = ('Sg' ^ []))])).
gfc_FragmentEnglish : rule(n_fish, 'N', c, 
	rec([(s = tbl([(('Sg' ^ []) = tok(fish)), (('Pl' ^ []) = tok(fish))]))])).
gfc_FragmentEnglish : rule(n_lion, 'N', c, 
	rec([(s = tbl([(('Sg' ^ []) = tok(lion)), (('Pl' ^ []) = tok(lions))]))])).
gfc_FragmentEnglish : rule(np1, 'NP', c('N'), 
	rec([(s = (arg('N', 1, [s]) / ('Pl' ^ []))), (n = ('Pl' ^ []))])).
gfc_FragmentEnglish : rule(np2, 'NP', c('D', 'N'), 
	rec([(s = (arg('D', 1, [s]) + (arg('N', 2, [s]) / arg('D', 1, [n])))), (n = arg('D', 1, [n]))])).
gfc_FragmentEnglish : rule(s, 'S', c('NP', 'VP'), 
	rec([(s = (arg('NP', 1, [s]) + (arg('VP', 2, [s]) / arg('NP', 1, [n]))))])).
gfc_FragmentEnglish : rule(v_eat, 'V', c, 
	rec([(s = tbl([(('Sg' ^ []) = tok(eats)), (('Pl' ^ []) = tok(eat))]))])).
gfc_FragmentEnglish : rule(v_hunt, 'V', c, 
	rec([(s = tbl([(('Sg' ^ []) = tok(hunts)), (('Pl' ^ []) = tok(hunt))]))])).
gfc_FragmentEnglish : rule(vp, 'VP', c('V', 'NP'), 
	rec([(s = tbl([(('Sg' ^ []) = ((arg('V', 1, [s]) / ('Sg' ^ [])) + arg('NP', 2, [s]))), (('Pl' ^ []) = ((arg('V', 1, [s]) / ('Pl' ^ [])) + arg('NP', 2, [s])))]))])).

