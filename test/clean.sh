#!/bin/sh
# clean.sh - clean up files after test runs
#
# Usage:  clean.sh
#
# History:  95/12/08 kmflynn
#
# remove check output and miscellaneous other output files
  for Fil in check.out ANNIE.LOG ERROR.FIL XPAD.DAT ; do
    if [ -f $Fil ] ; then rm $Fil ; fi
  done
#
# remove wdm
  if [ -f cane.wdm ]  ; then rm cane.wdm  ; fi
#
# remove output from each test
  for Fil in canepr.exp caneob.exp test2.out test3.out fort.70 ; do
    if [ -f $Fil ] ; then rm $Fil ; fi
  done

# end of shell
