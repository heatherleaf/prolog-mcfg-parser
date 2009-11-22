
concrete FragmentEnglish of FragmentAbstract = open FragmentResource in {

lincat 

N  = { s : Num => Str };
V  = { s : Num => Str };
VP = { s : Num => Str };

D  = { s : Str; n : Num };
NP = { s : Str; n : Num };

lin

s   x y = { s = x.s ++ y.s!x.n };

np1 x   = { s = x.s!Pl; n = Pl };
np2 x y = { s = x.s ++ y.s!x.n; n = x.n };

vp  x y = { s = \\z => x.s!z ++ y.s };

d_one   = { s = "a"; n = Sg };
d_many  = { s = "many"; n = Pl };

n_lion  = { s = table { Sg => "lion"; Pl => "lions" } };
n_fish  = { s = \\_ => "fish" };

v_eat   = { s = table { Sg => "eats" ; Pl => "eat" } };
v_hunt  = { s = table { Sg => "hunts" ; Pl => "hunt" } };

}

