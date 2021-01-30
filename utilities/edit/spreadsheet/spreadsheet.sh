#!/bin/sh
# This is a shell archive (produced by GNU sharutils 4.2).
# To extract the files from this archive, save it to some FILE, remove
# everything before the `!/bin/sh' line above, then type `sh FILE'.
#
# Made on 1997-02-28 15:49 PST by <deleeuw@galton>.
# Source directory was `/u/galton/m3/ahtdocs/develop/lisp/xlisp/xlisp-stat/code/utilities/edit/spreadsheet'.
#
# Existing files will *not* be overwritten unless `-c' is specified.
#
# This shar contains:
# length mode       name
# ------ ---------- ------------------------------------------
#   1668 -rw-rw-r-- README
#   2712 -rw-rw-r-- iris.lsp
#   2687 -rw-rw-r-- make-proto.lsp
#   6864 -rw-rw-r-- spreadsheet.lsp
#
save_IFS="${IFS}"
IFS="${IFS}:"
gettext_dir=FAILED
locale_dir=FAILED
first_param="$1"
for dir in $PATH
do
  if test "$gettext_dir" = FAILED && test -f $dir/gettext \
     && ($dir/gettext --version >/dev/null 2>&1)
  then
    set `$dir/gettext --version 2>&1`
    if test "$3" = GNU
    then
      gettext_dir=$dir
    fi
  fi
  if test "$locale_dir" = FAILED && test -f $dir/shar \
     && ($dir/shar --print-text-domain-dir >/dev/null 2>&1)
  then
    locale_dir=`$dir/shar --print-text-domain-dir`
  fi
done
IFS="$save_IFS"
if test "$locale_dir" = FAILED || test "$gettext_dir" = FAILED
then
  echo=echo
else
  TEXTDOMAINDIR=$locale_dir
  export TEXTDOMAINDIR
  TEXTDOMAIN=sharutils
  export TEXTDOMAIN
  echo="$gettext_dir/gettext -s"
fi
touch -am 1231235999 $$.touch >/dev/null 2>&1
if test ! -f 1231235999 && test -f $$.touch; then
  shar_touch=touch
else
  shar_touch=:
  echo
  $echo 'WARNING: not restoring timestamps.  Consider getting and'
  $echo "installing GNU \`touch', distributed in GNU File Utilities..."
  echo
fi
rm -f 1231235999 $$.touch
#
if mkdir _sh02550; then
  $echo 'x -' 'creating lock directory'
else
  $echo 'failed to create lock directory'
  exit 1
fi
# ============= README ==============
if test -f 'README' && test "$first_param" != -c; then
  $echo 'x -' SKIPPING 'README' '(file already exists)'
else
  $echo 'x -' extracting 'README' '(text)'
  sed 's/^X//' << 'SHAR_EOF' > 'README' &&
This file contains a small spreadsheet editor in Xlisp-Stat, with
an example (the iris data, obviously).
X
This is part of our introductory statistics project, an earlier
version was integrated with the ucladata project.
X
One nasty problem I had was that the list-item-proto is fairly
limited. I wanted a spreadsheet with variable labels in the
first row and case labels in the first column. But scrolling
horizontally should not scroll case labels out of sight, and scrolling
vertically should not scroll variable labels out of sight. 
This editor is the current solution.
X
To start it, simply load "iris.lsp". This will load "spreadsheet.lsp"
and it will create an instance of the multi-variable-proto filled
with iris data and labels. This instance is called "iris". Then say
(send iris :edit), and take it from there.
X
There are two variations. We can say
(send iris :edit '(0 1))
which will only load the first two variables into the editor. Or
(send iris :edit '(0 1) t)
which create the editor as a modal dialog (default is modeless).
X
You edit a cell by double clicking, you can scroll through the 
variables by the scroll bar at the bottom, you can transform a
variable by double clicking its name. This all feels fairly
natural to me. You can revert a variable by the revert button
(this only reverts the currently active -- i.e. visible --
variable).
X
I have made sure that this looks good on both X11 and Mac, I
dont know about MSW. Also, it can be byte-compiled on both
types of machines. 
X
Weights and labels cannot be reverted. It needs a save method.
It is easy to extend it with a menu that has save, revert,
plot, and univariate statistics. 
X
--- Jan
SHAR_EOF
  $shar_touch -am 0217114395 'README' &&
  chmod 0664 'README' ||
  $echo 'restore of' 'README' 'failed'
  if ( md5sum --help 2>&1 | grep 'sage: md5sum \[' ) >/dev/null 2>&1 \
  && ( md5sum --version 2>&1 | grep -v 'textutils 1.12' ) >/dev/null; then
    md5sum -c << SHAR_EOF >/dev/null 2>&1 \
    || $echo 'README:' 'MD5 check failed'
156bcae370849a12c01b0be2b495a5b1  README
SHAR_EOF
  else
    shar_count="`LC_ALL= LC_CTYPE= LANG= wc -c < 'README'`"
    test 1668 -eq "$shar_count" ||
    $echo 'README:' 'original size' '1668,' 'current size' "$shar_count!"
  fi
fi
# ============= iris.lsp ==============
if test -f 'iris.lsp' && test "$first_param" != -c; then
  $echo 'x -' SKIPPING 'iris.lsp' '(file already exists)'
else
  $echo 'x -' extracting 'iris.lsp' '(binary)'
  sed 's/^X//' << 'SHAR_EOF' | uudecode &&
begin 600 iris.lsp
M*&QO860@(G-P<F5A9'-H965T(BD*"BA$148@25))4R`H455/5$4@*"@U+C$@
M-"XY(#0N-R`T+C8@-2`U+C0@-"XV(#4@-"XT(#0N.2`U+C0@-"XX(#0N."`T
M+C,@-2XX(#4N-R`U+C0@-2XQ(#4N-R`U+C$@-2XT(#4N,2`T+C8@-2XQ(#0N
M."`U(#4@-2XR(#4N,B`T+C<@-"XX(#4N-"`U+C(@-2XU(#0N.2`U(#4N-2`T
M+CD@-"XT(#4N,2`U(#0N-2`T+C0@-2`U+C$@-"XX(#4N,2`T+C8@-2XS(#4@
M-R`V+C0@-BXY(#4N-2`V+C4@-2XW(#8N,R`T+CD@-BXV(#4N,B`U(#4N.2`V
M(#8N,2`U+C8@-BXW(#4N-B`U+C@@-BXR(#4N-B`U+CD@-BXQ(#8N,R`V+C$@
M-BXT(#8N-B`V+C@@-BXW(#8@-2XW(#4N-2`U+C4@-2XX(#8@-2XT(#8@-BXW
M(#8N,R`U+C8@-2XU(#4N-2`V+C$@-2XX(#4@-2XV(#4N-R`U+C<@-BXR(#4N
M,2`U+C<@-BXS(#4N."`W+C$@-BXS(#8N-2`W+C8@-"XY(#<N,R`V+C<@-RXR
M(#8N-2`V+C0@-BXX(#4N-R`U+C@@-BXT(#8N-2`W+C<@-RXW(#8@-BXY(#4N
M-B`W+C<@-BXS(#8N-R`W+C(@-BXR(#8N,2`V+C0@-RXR(#<N-"`W+CD@-BXT
M(#8N,R`V+C$@-RXW(#8N,R`V+C0@-B`V+CD@-BXW(#8N.2`U+C@@-BXX(#8N
M-R`V+C<@-BXS(#8N-2`V+C(@-2XY*2`H,RXU(#,@,RXR(#,N,2`S+C8@,RXY
M(#,N-"`S+C0@,BXY(#,N,2`S+C<@,RXT(#,@,R`T(#0N-"`S+CD@,RXU(#,N
M."`S+C@@,RXT(#,N-R`S+C8@,RXS(#,N-"`S(#,N-"`S+C4@,RXT(#,N,B`S
M+C$@,RXT(#0N,2`T+C(@,RXQ(#,N,B`S+C4@,RXV(#,@,RXT(#,N-2`R+C,@
M,RXR(#,N-2`S+C@@,R`S+C@@,RXR(#,N-R`S+C,@,RXR(#,N,B`S+C$@,BXS
M(#(N."`R+C@@,RXS(#(N-"`R+CD@,BXW(#(@,R`R+C(@,BXY(#(N.2`S+C$@
M,R`R+C<@,BXR(#(N-2`S+C(@,BXX(#(N-2`R+C@@,BXY(#,@,BXX(#,@,BXY
M(#(N-B`R+C0@,BXT(#(N-R`R+C<@,R`S+C0@,RXQ(#(N,R`S(#(N-2`R+C8@
M,R`R+C8@,BXS(#(N-R`S(#(N.2`R+CD@,BXU(#(N."`S+C,@,BXW(#,@,BXY
M(#,@,R`R+C4@,BXY(#(N-2`S+C8@,RXR(#(N-R`S(#(N-2`R+C@@,RXR(#,@
M,RXX(#(N-B`R+C(@,RXR(#(N."`R+C@@,BXW(#,N,R`S+C(@,BXX(#,@,BXX
M(#,@,BXX(#,N."`R+C@@,BXX(#(N-B`S(#,N-"`S+C$@,R`S+C$@,RXQ(#,N
M,2`R+C<@,RXR(#,N,R`S(#(N-2`S(#,N-"`S*2`H,2XT(#$N-"`Q+C,@,2XU
M(#$N-"`Q+C<@,2XT(#$N-2`Q+C0@,2XU(#$N-2`Q+C8@,2XT(#$N,2`Q+C(@
M,2XU(#$N,R`Q+C0@,2XW(#$N-2`Q+C<@,2XU(#$@,2XW(#$N.2`Q+C8@,2XV
M(#$N-2`Q+C0@,2XV(#$N-B`Q+C4@,2XU(#$N-"`Q+C4@,2XR(#$N,R`Q+C0@
M,2XS(#$N-2`Q+C,@,2XS(#$N,R`Q+C8@,2XY(#$N-"`Q+C8@,2XT(#$N-2`Q
M+C0@-"XW(#0N-2`T+CD@-"`T+C8@-"XU(#0N-R`S+C,@-"XV(#,N.2`S+C4@
M-"XR(#0@-"XW(#,N-B`T+C0@-"XU(#0N,2`T+C4@,RXY(#0N."`T(#0N.2`T
M+C<@-"XS(#0N-"`T+C@@-2`T+C4@,RXU(#,N."`S+C<@,RXY(#4N,2`T+C4@
M-"XU(#0N-R`T+C0@-"XQ(#0@-"XT(#0N-B`T(#,N,R`T+C(@-"XR(#0N,B`T
M+C,@,R`T+C$@-B`U+C$@-2XY(#4N-B`U+C@@-BXV(#0N-2`V+C,@-2XX(#8N
M,2`U+C$@-2XS(#4N-2`U(#4N,2`U+C,@-2XU(#8N-R`V+CD@-2`U+C<@-"XY
M(#8N-R`T+CD@-2XW(#8@-"XX(#0N.2`U+C8@-2XX(#8N,2`V+C0@-2XV(#4N
M,2`U+C8@-BXQ(#4N-B`U+C4@-"XX(#4N-"`U+C8@-2XQ(#4N,2`U+CD@-2XW
M(#4N,B`U(#4N,B`U+C0@-2XQ*2`H,"XR(#`N,B`P+C(@,"XR(#`N,B`P+C0@
M,"XS(#`N,B`P+C(@,"XQ(#`N,B`P+C(@,"XQ(#`N,2`P+C(@,"XT(#`N-"`P
M+C,@,"XS(#`N,R`P+C(@,"XT(#`N,B`P+C4@,"XR(#`N,B`P+C0@,"XR(#`N
M,B`P+C(@,"XR(#`N-"`P+C$@,"XR(#`N,B`P+C(@,"XR(#`N,2`P+C(@,"XR
M(#`N,R`P+C,@,"XR(#`N-B`P+C0@,"XS(#`N,B`P+C(@,"XR(#`N,B`Q+C0@
M,2XU(#$N-2`Q+C,@,2XU(#$N,R`Q+C8@,2`Q+C,@,2XT(#$@,2XU(#$@,2XT
M(#$N,R`Q+C0@,2XU(#$@,2XU(#$N,2`Q+C@@,2XS(#$N-2`Q+C(@,2XS(#$N
M-"`Q+C0@,2XW(#$N-2`Q(#$N,2`Q(#$N,B`Q+C8@,2XU(#$N-B`Q+C4@,2XS
M(#$N,R`Q+C,@,2XR(#$N-"`Q+C(@,2`Q+C,@,2XR(#$N,R`Q+C,@,2XQ(#$N
M,R`R+C4@,2XY(#(N,2`Q+C@@,BXR(#(N,2`Q+C<@,2XX(#$N."`R+C4@,B`Q
M+CD@,BXQ(#(@,BXT(#(N,R`Q+C@@,BXR(#(N,R`Q+C4@,BXS(#(@,B`Q+C@@
M,BXQ(#$N."`Q+C@@,2XX(#(N,2`Q+C8@,2XY(#(@,BXR(#$N-2`Q+C0@,BXS
M(#(N-"`Q+C@@,2XX(#(N,2`R+C0@,BXS(#$N.2`R+C,@,BXU(#(N,R`Q+CD@
M,B`R+C,@,2XX*2DI*0HH9&5F('-E<&%L+6QE;F=T:"`H96QT(&ER:7,@,"DI
M"BAD968@<V5P86PM=VED=&@@("AE;'0@:7)I<R`Q*2D**&1E9B!P971A;"UL
M96YG=&@@("AE;'0@:7)I<R`R*2D**&1E9B!P971A;"UW:61T:"`H96QT(&ER
M:7,@,RDI"BAD968@:7)I<RUV87)S("AL:7-T(")397!A;"!,96YG=&@B(")3
M97!A;"!7:61T:"(@"B`@("`@("`@("`@("`@("`@("`@(")0971A;"!,96YG
M=&@B(")0971A;"!7:61T:"(I*0HH9&5F(&ER:7,M9&%T82`H;&ES="!S97!A
M;"UL96YG=&@@<V5P86PM=VED=&@@<&5T86PM;&5N9W1H('!E=&%L+7=I9'1H
M*2D**&1E9B!I<FES("AS96YD(&UU;'1I+79A<FEA8FQE+7!R;W1O(#IN97<@
M:7)I<RUD871A*2D**'-E;F0@:7)I<R`Z<V5T+79A<FEA8FQE+6QA8F5L<R!I
,<FES+79A<G,I"@H*
`
end
SHAR_EOF
  $shar_touch -am 0217115195 'iris.lsp' &&
  chmod 0664 'iris.lsp' ||
  $echo 'restore of' 'iris.lsp' 'failed'
  if ( md5sum --help 2>&1 | grep 'sage: md5sum \[' ) >/dev/null 2>&1 \
  && ( md5sum --version 2>&1 | grep -v 'textutils 1.12' ) >/dev/null; then
    md5sum -c << SHAR_EOF >/dev/null 2>&1 \
    || $echo 'iris.lsp:' 'MD5 check failed'
322d4b5833b0120a3912c8cfc9bc8c79  iris.lsp
SHAR_EOF
  else
    shar_count="`LC_ALL= LC_CTYPE= LANG= wc -c < 'iris.lsp'`"
    test 2712 -eq "$shar_count" ||
    $echo 'iris.lsp:' 'original size' '2712,' 'current size' "$shar_count!"
  fi
fi
# ============= make-proto.lsp ==============
if test -f 'make-proto.lsp' && test "$first_param" != -c; then
  $echo 'x -' SKIPPING 'make-proto.lsp' '(file already exists)'
else
  $echo 'x -' extracting 'make-proto.lsp' '(binary)'
  sed 's/^X//' << 'SHAR_EOF' | uudecode &&
begin 600 make-proto.lsp
M.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[
M.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL*.SL*.SL@0V]U<&QE(&]F(&UA
M8W)O<R!T;R!C;VYS=')U8W0@=&AE(#II<VYE=R!M971H;V0@86YD(&%L;"!A
M<W-E<W-O<@H[.R!M971H;V1S(&9O<B!T:&4@<VQO=',@;V8@82!P<F]T;W1Y
M<&4N($%F=&5R(&1E9FEN:6YG('1H92!P<F]T;W1Y<&4*.SL@=&AE('-L;W1S
M(&%R92!E:71H97(@87)G<VQO=',@;W(@:V5Y<VQO=',N($%R9W-L;W1S(&%R
M92!M86YD871O<GD*.SL@87)G=6UE;G1S(&9O<B!T:&4@.FES;F5W(&UE=&AO
M9"P@:V5Y<VQO=',@87)E(&ME>7=O<F0@87)G=6UE;G1S+@H[.R!4:&4@;6%C
M<F]S(&-R96%T92!T:&4@.FES;F5W(&UE=&AO9"P@86YD(&$@.G-E="UF;V\@
M87-S97-S;W(*.SL@;65T:&]D(&9O<B!E86-H('-L;W0@*'=H:6-H(')E='5R
M;G,@=&AE(&-O;G1E;G1S(&EF(&-A;&QE9"!W:71H;W5T"CL[(&%R9V5M96YT
M+"!A;F0@<V5T<R!T:&4@8V]N=&5N="!I9B!C86QL960@=VET:"!A;B!A<F=U
M;65N="X*.SL*.SL@2F%N(&1E($QE975W+"`P,RTQ,"TY-0H[.PH[.R!M;V1I
M9FEE9"!T;R!C;VYF;W)M('=I=&@@;6%K92UP<F]T;RYL<W`@,#,M,3,M.34*
M.SL*.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[
M.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL[.SL["@HH9&5F;6%C<F\@;6%K
M92UO;F4M87-S97-S;W(@*&ME>2!S;&]T('!R;W1O='EP92D*8"AD969M971H
M("QP<F]T;W1Y<&4@+&ME>2`H)F]P=&EO;F%L("AC;VYT96YT(&YI;"!S970I
M*0H@("`H:68@<V5T("AS971F("AS;&]T+79A;'5E("<L<VQO="D@8V]N=&5N
M="DI"B`@("AS;&]T+79A;'5E("<L<VQO="DI*0H**&1E9FUA8W)O(&UA:V4M
M<')O=&\@*'!R;W1O='EP92!S;&]T<R!P87)E;G0I"F`H9&5F<')O=&\@+'!R
M;W1O='EP92`L<VQO=',@*"D@+'!A<F5N="D**0H**&1E9FUA8W)O(&UA:V4M
M:7,M;F5W+6UE=&AO9"`H<')O=&]T>7!E(&%R9W-L;W1S(&ME>7-L;W1S*0I@
M*&1E9FUE=&@@+'!R;W1O='EP92`Z:7-N97<@"B@L0"AM87!C87(@(R<H;&%M
M8F1A("AX*0H@("`@("`@("AI;G1E<FX@*&-O;F-A=&5N871E("=S=')I;F<@
M(E1(12TB"B`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@
M("`@("`@*'-Y;6)O;"UN86UE('@I*2DI(&%R9W-L;W1S*2`*)FME>2`*+$`H
M;6%P8V%R(",G*&QA;6)D82`H>"D*("`@("`@("`H:6YT97)N("AC;VYC871E
M;F%T92`G<W1R:6YG(")42$4M(@H@("`@("`@("`@("`@("`@("`@("`@("`@
M("`@("`@("`@("`@("`@("`@("AS>6UB;VPM;F%M92!X*2DI*2!K97ES;&]T
M<RDI"BQ`*&UA<&-A<B`C)RAL86UB9&$@*'@@>2D@8"AS96YD('-E;&8@+'@@
M+'DI*2`**&UA<&-A<B`C)RAL86UB9&$@*'@I"B`@("`@("`@*&EN=&5R;B`H
M8V]N8V%T96YA=&4@)W-T<FEN9R`B4T54+2(*("`@("`@("`@("`@("`@("`@
M("`@("`@("`@("`@("`@("`@("`@("`@("`H<WEM8F]L+6YA;64@>"DI("=K
M97EW;W)D*2D@87)G<VQO=',I"BAM87!C87(@(R<H;&%M8F1A("AY*0H@("`@
M("`@("AI;G1E<FX@*&-O;F-A=&5N871E("=S=')I;F<@(E1(12TB"B`@("`@
M("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@*'-Y;6)O
M;"UN86UE('DI*2DI(&%R9W-L;W1S*2D*+$`H;6%P8V%R(",G*&QA;6)D82`H
M>"!Y('HI(&`H<V5N9"!S96QF("QX("AI9B`L>2`L>2`H<V5N9"!S96QF("QZ
M*2DI*2`**&UA<&-A<B`C)RAL86UB9&$@*'@I"B`@("`@("`@*&EN=&5R;B`H
M8V]N8V%T96YA=&4@)W-T<FEN9R`B4T54+2(*("`@("`@("`@("`@("`@("`@
M("`@("`@("`@("`@("`@("`@("`@("`@("`H<WEM8F]L+6YA;64@>"DI("=K
M97EW;W)D*2D@:V5Y<VQO=',I"BAM87!C87(@(R<H;&%M8F1A("AY*0H@("`@
M("`@("AI;G1E<FX@*&-O;F-A=&5N871E("=S=')I;F<@(E1(12TB"B`@("`@
M("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@*'-Y;6)O
M;"UN86UE('DI*2DI(&ME>7-L;W1S*0HH;6%P8V%R(",G*&QA;6)D82`H>BD*
M("`@("`@("`H:6YT97)N("AC;VYC871E;F%T92`G<W1R:6YG(")-04M%+2(*
M("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`H
M<WEM8F]L+6YA;64@>BDI("=K97EW;W)D*2D@:V5Y<VQO=',I*2D**0H**&1E
M9FUA8W)O(&UA:V4M87-S97-S;W)S("AP<F]T;W1Y<&4@87)G<VQO=',@:V5Y
M<VQO=',I"B`@8"AP<F]G;B`*("`@("Q`*&UA<&-A<B`C)RAL86UB9&$@*'@@
M>2D@8"AM86ME+6]N92UA<W-E<W-O<B`L>"`L>2`L<')O=&]T>7!E*2D*("`@
M("`@("`@("`@("`H;6%P8V%R(",G*&QA;6)D82`H>"D*("`@("`@("`@("`@
M("`@("`@("`@("`@("`H:6YT97)N("AC;VYC871E;F%T92`G<W1R:6YG(")3
M150M(@H@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@("`@
M("`@("`@("`@("`@("AS>6UB;VPM;F%M92!X*2D@)VME>7=O<F0I*2`H87!P
M96YD(&%R9W-L;W1S(&ME>7-L;W1S*2D*("`@("`@("`@("`@("`H;6%P8V%R
M(",G*&QA;6)D82`H>"D*("`@("`@("`@("`@("`@("`@("`@("`@("`H:6YT
M97)N("AS>6UB;VPM;F%M92!X*2DI("AA<'!E;F0@87)G<VQO=',@:V5Y<VQO
@=',I*2DI"BD*"BAP<F]V:61E(")M86ME+7!R;W1O(BEG
`
end
SHAR_EOF
  $shar_touch -am 0313122995 'make-proto.lsp' &&
  chmod 0664 'make-proto.lsp' ||
  $echo 'restore of' 'make-proto.lsp' 'failed'
  if ( md5sum --help 2>&1 | grep 'sage: md5sum \[' ) >/dev/null 2>&1 \
  && ( md5sum --version 2>&1 | grep -v 'textutils 1.12' ) >/dev/null; then
    md5sum -c << SHAR_EOF >/dev/null 2>&1 \
    || $echo 'make-proto.lsp:' 'MD5 check failed'
48b7079b143db4452e5e0c21bd0c00ec  make-proto.lsp
SHAR_EOF
  else
    shar_count="`LC_ALL= LC_CTYPE= LANG= wc -c < 'make-proto.lsp'`"
    test 2687 -eq "$shar_count" ||
    $echo 'make-proto.lsp:' 'original size' '2687,' 'current size' "$shar_count!"
  fi
fi
# ============= spreadsheet.lsp ==============
if test -f 'spreadsheet.lsp' && test "$first_param" != -c; then
  $echo 'x -' SKIPPING 'spreadsheet.lsp' '(file already exists)'
else
  $echo 'x -' extracting 'spreadsheet.lsp' '(text)'
  sed 's/^X//' << 'SHAR_EOF' > 'spreadsheet.lsp' &&
(require "make-proto.lsp")
X
(defproto multi-variable-proto 
X  '(data
X    title 
X    weights
X    case-labels 
X    variable-labels 
X    backup)  () *object*
X  "Prototype for a generic Multivariable") 
X
(make-is-new-method multi-variable-proto (data) 
X               (title weights case-labels variable-labels backup))
X
X
(make-assessors multi-variable-proto (data) 
X               (title weights case-labels variable-labels backup))                                                 
X     
X
X
(defmeth multi-variable-proto :make-title ()
"Args: NONE
Sets the default title."
(send self :set-title "My Own Multivariable")
)
X
(defmeth multi-variable-proto :make-case-labels ()
X  (mapcar #'(lambda (x) (format nil "~a" x))
X          (iseq 0 (- (send self :nobs) 1)))
)
X
(defmeth multi-variable-proto :make-variable-labels ()
X  (mapcar #'(lambda (x) (format nil "~a" x))
X          (iseq 0 (- (send self :nvar) 1)))
)
X
X
(defmeth multi-variable-proto :make-weights ()
"Args: NONE
Sets the default weights (a vector of ones)."
X  (repeat 1 (send self :nobs))
)
X
(defmeth multi-variable-proto :make-backup ()
"Args: NONE
Sets the default backup."
X  (copy-tree (send self :set-data))
)
X
(defmeth multi-variable-proto :nobs ()
X  (length (first (send self :set-data)))
)
X
X
(defmeth multi-variable-proto :nvar ()
X   (length (send self :set-data))
)
X
(defmeth multi-variable-proto :edit 
X  (&optional (ivar (iseq (send self :nvar))) (is-modal nil))
(let* (
X       (n (send self :nobs))
X       (m (length ivar))
X       (kv 0)
X       (dt (select (send self :set-data) ivar))
X       (vt (select (send self :set-variable-labels) ivar))
X       (cl (send self :set-case-labels))
X       (wl (send self :set-weights))
X       (pm (transpose (make-array (list 3 n) :initial-contents
X                                  (list cl 
X                                        (to-string-list wl)
X                                        (to-string-list (first dt))))))
X       (ll (send list-item-proto :new pm :columns 3))
X       (dd (send ll :slot-value 'size))
X       (l2 (make-array (list 1 3) :initial-contents
X                       (list (list "Lables") (list "Weights") 
X                             (list (first vt)))))
X       (la (send list-item-proto :new l2 :columns 3))
X       (ok (if is-modal
X               (send modal-button-proto :new "Enough !" 
X                   :location (list 10 (+ (second dd) 50)))
X               (send button-item-proto :new "Enough !" 
X                   :location (list 10 (+ (second dd) 50)))))
X       (rv (send button-item-proto :new "Revert !" 
X                   :location (list 150 (+ (second dd) 50))))
X       (sb (send sequence-scroll-item-proto :new (iseq m) 
X                 :size (list 150 16)
X                 :location (list 310 (+ (second dd) 50))))
X       (the-dialog (if is-modal
X                       (send modal-dialog-proto :new 
X                             (list
X                              (list la)
X                              (list ll) 
X                              (list ok rv sb))
X                             :default-button ok 
X                             :size (list (+ (first dd) 20)
X                                         (+ (second dd) 80)))
X                       (send dialog-proto :new 
X                             (list
X                              (list la)
X                              (list ll) 
X                              (list ok rv sb))
X                             :default-button ok 
X                             :size (list (+ (first dd) 20)
X                                         (+ (second dd) 80))
X                             :title (send self :set-title))))
X       )
X  (send ll :slot-value 'action
X        #'(lambda (x)
X            (if x (send self :click-edit (elt ivar kv) 
X                        (send ll :selection)
X                        (send ll :replace-cell)))))
X  (send la :slot-value 'action
X        #'(lambda (x) (if x
X                          (if (= (second (send la :selection)) 2)
X                              (let (
X                                    (lv (elt ivar kv))
X                                    (fv (intern (string-upcase
X                                                 (get-string-dialog 
X                                                  "Enter a transformation:"))))
X                                    )
X                                (send self :transform fv lv)
X                                (send ll :update (to-string-list
X                                                  (elt (send self :set-data) lv)))
X                                )))))
X  (send  rv :slot-value 'action 
X         #'(lambda ()
X             (let (
X                   (lv (elt ivar kv))
X                   ) 
X               (send self :revert lv)
X               (send ll :update (to-string-list
X                                 (elt (send self :set-data) lv)))
X               )))
X  (send sb :slot-value 'action
X        #'(lambda (x)
X            (setf kv x)
X            (send ll :update (to-string-list (elt dt x)))
X            (send la :set-text '(0 2) (elt vt x))))
X  (if is-modal nil
X      (send ok :slot-value 'action
X            #'(lambda () (send the-dialog :close))))
X  (if is-modal (send the-dialog :modal-dialog))
X  )
)
X
X
X
(defmeth multi-variable-proto :click-edit (ivar ind new)
(let (
X     (k1 (first ind))
X     (k2 (second ind))
X     (aa (elt (send self :set-data) ivar))
X     )
(cond ((= k2 0) (setf (elt (send self :set-case-labels) k1) new))
X      ((= k2 1) (setf (elt (send self :set-weights) k1) new))
X      (t (setf (elt aa k1) new) 
))))
X
(defmeth list-item-proto :update (var)
(dotimes (i (length var))
X         (send self :set-text (list i 2) (elt var i)))
)
X
(defmeth list-item-proto :replace-cell ()
(let* (
X       (kk (send self :selection))
X       (k2 (second kk))
X       (aa  (if (= k2 0) 
X                (get-string-dialog "Enter a label:")
X                (first (get-value-dialog "Enter an expression:"))))
X       )
(send self :set-text kk 
X      (if (stringp aa) aa (num-to-string aa)))
aa))
X
(defmeth multi-variable-proto :revert (var)
(let (
X     (ls (copy-list (elt (send self :set-backup) var)))
X     )
(setf (elt (send self :set-data) var) ls)
))
X
(defmeth multi-variable-proto :transform (func var)
(let (
X     (ls (funcall func (elt (send self :set-data) var)))
X     )
(setf (elt (send self :set-data) var) ls)
))
X
(defun to-string-list (ls)
"Args: LIST
Converts LIST to list of strings."
(mapcar #'(lambda (x) (format nil "~a" x)) ls)
)
X
X
X
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Example of the use of the Spread Sheet program
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
X
#|  <--- these are block comments
X
(def dat (list (normal-rand 10) (iseq 10) (normal-rand 10) (normal-rand 10)))
X
(def myspread (send multi-variable-proto :new dat))
X
(send myspread :edit)
X
|#  
X
X
X
X
SHAR_EOF
  $shar_touch -am 0905135696 'spreadsheet.lsp' &&
  chmod 0664 'spreadsheet.lsp' ||
  $echo 'restore of' 'spreadsheet.lsp' 'failed'
  if ( md5sum --help 2>&1 | grep 'sage: md5sum \[' ) >/dev/null 2>&1 \
  && ( md5sum --version 2>&1 | grep -v 'textutils 1.12' ) >/dev/null; then
    md5sum -c << SHAR_EOF >/dev/null 2>&1 \
    || $echo 'spreadsheet.lsp:' 'MD5 check failed'
df363f3416a408e7c7a9b250dd590780  spreadsheet.lsp
SHAR_EOF
  else
    shar_count="`LC_ALL= LC_CTYPE= LANG= wc -c < 'spreadsheet.lsp'`"
    test 6864 -eq "$shar_count" ||
    $echo 'spreadsheet.lsp:' 'original size' '6864,' 'current size' "$shar_count!"
  fi
fi
rm -fr _sh02550
exit 0
