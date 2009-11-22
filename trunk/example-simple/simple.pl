
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% simple English grammar

%% s -> np(Num) vp(Num)
english: rule(s, s, c(np_sg, vp_sg),
	      [p1 = [arg(np_sg,1,p1), arg(vp_sg,2,p1)]]).
english: rule(s, s, c(np_pl, vp_pl),
	      [p1 = [arg(np_pl,1,p1), arg(vp_pl,2,p1)]]).

%% np(Num) -> d(Num) n(Num)
english: rule(np2, np_sg, c(d_sg, n_sg),
	      [p1 = [arg(d_sg,1,p1), arg(n_sg,2,p1)]]).
english: rule(np2, np_pl, c(d_pl, n_pl),
	      [p1 = [arg(d_pl,1,p1), arg(n_pl,2,p1)]]).

%% np(pl) -> n(pl)
english: rule(np1, np_pl, c(n_pl),
	      [p1 = [arg(n_pl,1,p1)]]).

%% vp(Num) -> v(Num) np(_)
english: rule(vp, vp_sg, c(v_sg, np_sg),
	      [p1 = [arg(v_sg,1,p1), arg(np_sg,2,p1)]]).
english: rule(vp, vp_sg, c(v_sg, np_pl),
	      [p1 = [arg(v_sg,1,p1), arg(np_pl,2,p1)]]).
english: rule(vp, vp_pl, c(v_pl, np_sg),
	      [p1 = [arg(v_pl,1,p1), arg(np_sg,2,p1)]]).
english: rule(vp, vp_pl, c(v_pl, np_pl),
	      [p1 = [arg(v_pl,1,p1), arg(np_pl,2,p1)]]).

%% d(sg) -> "a"
english: rule(d_one, d_sg, c, [p1 = [tok(a)]]).

%% d(pl) -> "many"
english: rule(d_many, d_pl, c, [p1 = [tok(many)]]).

%% n(sg) -> "lion"
%% n(pl) -> "lions"
english: rule(n_lion, n_sg, c, [p1 = [tok(lion)]]).
english: rule(n_lion, n_pl, c, [p1 = [tok(lions)]]).

%% n(_) -> "fish"
english: rule(n_fish, n_sg, c, [p1 = [tok(fish)]]).
english: rule(n_fish, n_pl, c, [p1 = [tok(fish)]]).

%% v(sg) -> "eats"
%% v(pl) -> "eat"
english: rule(v_eat, v_sg, c, [p1 = [tok(eats)]]).
english: rule(v_eat, v_pl, c, [p1 = [tok(eat)]]).

%% v(sg) -> "hunts"
%% v(pl) -> "hunt"
english: rule(v_hunt, v_sg, c, [p1 = [tok(hunts)]]).
english: rule(v_hunt, v_pl, c, [p1 = [tok(hunt)]]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% simple Swedish grammar

%% s(decl) -> np vp1 vp2
%% s(que)  -> vp1 np vp2
swedish: rule(s, s_decl, c(np, vp),
	      [p1 = [arg(np,1,p1), arg(vp,2,p1), arg(vp,2,p2)]]).
swedish: rule(s, s_que, c(np, vp),
	      [p1 = [arg(vp,2,p1), arg(np,1,p1), arg(vp,2,p2)]]).

%% np -> d(GenNum) n(GenNum)
swedish: rule(np2, np, c(d_utr_sg, n_utr_sg),
	      [p1 = [arg(d_utr_sg,1,p1), arg(n_utr_sg,2,p1)]]).
swedish: rule(np2, np, c(d_neu_sg, n_neu_sg),
	      [p1 = [arg(d_neu_sg,1,p1), arg(n_neu_sg,2,p1)]]).
swedish: rule(np2, np, c(d_pl, n_pl),
	      [p1 = [arg(d_pl,1,p1), arg(n_pl,2,p1)]]).

%% np -> n(_,pl)
swedish: rule(np1, np, c(n_pl),
	      [p1 = [arg(n_pl,1,p1)]]).

%% vp -> v np
swedish: rule(vp, vp, c(v, np),
	      [p1 = [arg(v,1,p1)],
	       p2 = [arg(np,2,p1)]]).

%% d(utr_sg) -> "en"
%% d(neu_sg) -> "ett"
swedish: rule(d_one, d_utr_sg, c, [p1 = [tok(en)]]).
swedish: rule(d_one, d_utr_pl, c, [p1 = [tok(ett)]]).

%% d(pl) -> "många"
swedish: rule(d_many, d_pl, c, [p1 = [tok(många)]]).

%% n(neu_sg) -> "lejon"
%% n(pl) -> "lejon"
swedish: rule(n_lion, n_neu_sg, c, [p1 = [tok(lejon)]]).
swedish: rule(n_lion, n_pl, c, [p1 = [tok(lejon)]]).

%% n(utr_sg) -> "fisk"
%% n(pl) -> "fiskar"
swedish: rule(n_fish, n_utr_sg, c, [p1 = [tok(fisk)]]).
swedish: rule(n_fish, n_pl, c, [p1 = [tok(fiskar)]]).

%% v -> "äter"
swedish: rule(v_eat, v, c, [p1 = [tok(äter)]]).

%% v -> "jagar"
swedish: rule(v_hunt, v, c, [p1 = [tok(jagar)]]).
