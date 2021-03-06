# Copyright (C) Peter Ljungl�f

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

$IDENT = qr/[a-zA-Z0-9_]+/;
$TOKEN = qr/(\\\"|[^\"])+/;
$MAP   = qr/\[[0-9\s\,\;\[\]]+\]/;

sub convertmap {
  $map =~ s/\,/-/g;
  $map =~ s/\;/,/g;
  $map =~ s/\] \s* \[/], [/gx;
  $lbl = 0;
  $map =~ s/\[/ "s" . $lbl++ . "=[" /gex;
  $map =~ s/([0-9]+) - ([0-9]+)/ "arg('" . $_[$1] . "'," . ($1+1) . ",s" . $2 . ")" /gex;
}

# header
print "%% Prolog MCFG grammar, autogenerated from Caml source\n\n";
print ":- op(600, xfy, [==>, ::=]).\n\n";

while (<>) {
  next unless /\S/;

  print "\n%% $_";

  if (($c) = /^ \s* ($IDENT) \s* --> \s* \"\"/x) {
    # empty rule
    $rhs = "c";
    $map = "s0=[]";

  } elsif (($c, $tok) = /^ \s* ($IDENT) \s* --> \s* \"($TOKEN)\"/x) {
    # terminal rule
    $rhs = "c";
    $map = "s0=[tok('$tok')]";

  } elsif (($c, $c1, $map) = /^ \s* ($IDENT) \s* --> \s* ($IDENT) \s* ($MAP)/x) {
    # unary rule
    $rhs = "c('$c1')";
    &convertmap($c1);

  } elsif (($c, $c1, $c2, $map) = /^ \s* ($IDENT) \s* --> \s* ($IDENT) \s+ ($IDENT) \s* ($MAP)/x) {
    # binary rule
    $rhs = "c('$c1', '$c2')";
    &convertmap($c1, $c2);

  } else {
    # parse error
    print "%% PARSE ERROR: $_";
    print STDERR "PARSE ERROR on line $NR: $_";
    next;
  }
  $lhs = "'$c'";
  $profile = $lhs;

  print "rule($profile, $lhs, $rhs, [$map]).\n";
}


