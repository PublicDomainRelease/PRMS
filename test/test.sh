#!/bin/sh
# test.sh -- run prms test data sets
#
# Usage: test.sh [start [stop]] [| tee test.log]
#
#        where: start = starting test number
#                stop = ending test number (may be same as start)
#
# History: 95/12/05 kmflynn
#
# pathname
  WrdA=/usr/opt/wrdapp

#*******************************************************************
#***** You should not need to modify anything below this line. *****
#*******************************************************************

  Prgm=$WrdA/prms2.1
  Data=$Prgm/data
  Anne=$WrdA/annie2.2
  Chck=$WrdA/prms2.1/test

  exec 2>&1                                     # stderr shows up in log file
  Start=${1-1}                                  # by default, start at 1
  Stop=${2-3}                                   # by default, stop at 3
                                                # 1 - annie, import data
                                                # 2 - prms, daily simulation
                                                # 3 - prms, storm simulation

# begin test runs
  echo
  echo _________________________________________________________________
  echo " Begin processing PRMS test runs $Start to $Stop"
  echo "  prms program from: $Prgm"
  echo "     test data from: $Data"
  echo " annie program from: $Anne"
  echo `date`
#
  Test=$Start
  if [ $Test -eq 1 ] ; then
#   build the wdm file
    echo _________________________________________________________________
    echo Test run number 1
#   remove any old files
    for File in ANNIE.LOG ERROR.FIL cane.wdm canepr.exp caneob.exp ; do
       if [ -f $File ] ; then rm $File ; fi
    done
#   link in data files
    ln -s $Data/canepr.exp canepr.exp
    ln -s $Data/caneob.exp caneob.exp

    $Anne/bin/annie <<-EOT
	@$Data/test1.log
	EOT
#   remove unneeded files
    rm canepr.exp caneob.exp ANNIE.LOG ERROR.FIL

    if [ $Test -ne $Stop ] ; then Test=2 ; fi

  fi

  while [ $Test -gt 1 -a $Test -le $Stop -a $Test -le 3 ] ; do
#   run prms tests
    echo
    echo _________________________________________________________________
    echo "Test run number $Test"
    echo
#   remove old output files
    for Sufx in out hru pgd pgu pgp ; do
      if [ -f test$Test.$Sufx ] ; then rm test$Test.$Sufx ; fi
    done
#   add links to input files
    for Sufx in g1 g2 g3 g4 ; do
      if [ -f $Data/test$Test.$Sufx ] 
        then ln -s $Data/test$Test.$Sufx test$Test.$Sufx
      fi
    done

    $Prgm/bin/prms <<-EOT
	$Data/test$Test.mtr
	EOT

#   remove linked files
    for Sufx in g1 g2 g3 g4 ; do
      if [ -f test$Test.$Sufx ] ; then rm test$Test.$Sufx ; fi
    done

    Test=`expr $Test + 1`
  done
#
  echo
  echo
  echo _________________________________________________________________
  echo "Completed PRMS test runs $Start to $Stop"
  echo
# check output against original in data directory
  $Chck/check.sh $Data

# end of shell
