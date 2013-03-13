all: rodent.sys
#all: rodent.sys rodent.exe

rodent.sys: rodent.asm rodent.def
   masm -Mx -t -I. -Ig:\ddk\inc rodent.asm;
   link /nod /noi /e /a:16 /map rodent,rodent.sys,,g:\ddk\lib\os2286.lib,rodent.def
   mapsym rodent

#
# Note: The following needs to be redone for version 3.0 of the compiler.
#

#rodent.exe: rodent.c rodentx.def
#   set include=.;e:\ibmcpp\include;e:\ibmcpp\ibmclass;e:\toolkt30\h
#   set lib=e:\toolkt30\lib;e:\ibmcpp\lib
#   icc /C /Fd /Gims /Ms /O /Re /Sas /Sp1 /Tc"rodent.c"
#   link386 /nol /bat /e /m:full /noi /align:4 /base:0x010000 /st:0x4000 rodent,rodent,rodentx,,rodentx;
