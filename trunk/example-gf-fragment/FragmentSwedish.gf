
concrete FragmentSwedish of FragmentAbstract = open FragmentResource in {

lincat 

S  = { s : Order => Str };
VP = { s1 : Str; s2 : Str };
N  = { s : Num => Str; g : Gen };
D  = { s : Gen => Str; n : Num };

lin

s   x y = { s = table { Que  => y.s1 ++ x.s ++ y.s2; 
       	       	       	 Decl => x.s ++ y.s1 ++ y.s2 } };

np1 x   = { s = x.s!Pl };
np2 x y = { s = x.s!y.g ++ y.s!x.n };

vp  x y = { s1 = x.s; s2 = y.s };

d_one   = { s = table { Utr => "en"; Neu => "ett" }; n = Sg };
d_many  = { s = \\_ => "många"; n = Pl };

n_lion  = { s = \\_ => "lejon"; g = Neu };
n_fish  = { s = table { Sg => "fisk"; Pl => "fiskar" }; g = Utr };

v_eat   = { s = "äter" };
v_hunt  = { s = "jagar" };

}

