
abstract FragmentAbstract = {

cat S; NP; VP; D; N; V;

fun

s      : NP -> VP -> S;

np1    : N     	  -> NP;
np2    : D  -> N  -> NP;

vp     : V  -> NP -> VP;

d_one  : D;
d_many : D;

n_lion : N;
n_fish : N;

v_eat  : V;
v_hunt : V;

}

