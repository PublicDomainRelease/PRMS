#
# Makefile --
#
# Top-level makefile for the PRMS
#
#-------------------------------------------------------------------
# $Id: Makefile 3270 2011-05-24 20:06:13Z rsregan $
#-------------------------------------------------------------------

include ./makelist

#
# Standard Targets for Users
#

all: standard

prms_dev: prmsdevelop

standard:
# Create lib directory, if necessary
	@if [ ! -d $(MMFDIR) ]   ; then        \
	  mkdir $(MMFDIR) ;                   \
	  echo  Created directory $(MMFDIR) ; \
	fi
# Create bin directory, if necessary
	@if [ ! -d $(BINDIR) ]   ; then        \
	  mkdir $(BINDIR) ;                   \
	  echo  Created directory $(BINDIR) ; \
	fi
	cd $(MMFDIR); $(MAKE);
	cd $(PRMSDIR); $(MAKE);

prmsdevelop:
# Create lib directory, if necessary
	@if [ ! -d $(MMFDIR) ]   ; then        \
	  mkdir $(MMFDIR) ;                   \
	  echo  Created directory $(MMFDIR) ; \
	fi
# Create bin directory, if necessary
	@if [ ! -d $(BINDIR) ]   ; then        \
	  mkdir $(BINDIR) ;                   \
	  echo  Created directory $(BINDIR) ; \
	fi
	cd $(MMFDIR); $(MAKE);
	cd $(PRMSDIR); $(MAKE);
	cd ./branch; $(MAKE);

clean:
	cd $(MMFDIR); $(MAKE) clean;
	cd $(PRMSDIR); $(MAKE) clean;
	cd $(PRMSDEVDIR); $(MAKE) clean;
	$(RM) $(BINDIR)/prmsIV $(BINDIR)/prmsIV_dev *~

