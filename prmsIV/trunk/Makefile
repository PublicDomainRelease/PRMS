# prms libraray makefile
# $Id: Makefile 5213 2013-01-14 18:18:13Z markstro $

include ../makelist
PRMSLIB2 = $(LIBDIR)/libprmsiv2.a
PRMSLIB3 = $(LIBDIR)/libprmsiv3.a
TARGET 	= $(BINDIR)/prmsIV
LIBS	= $(PRMSBASE) $(PRMSLIB2) $(PRMSLIB3) $(MMFLIB)

####################################################
# Rules for targets
####################################################
all: $(TARGET)

$(TARGET): call_modules.o $(LIBS)
	$(RM) $(TARGET)
	$(CC) $(LDFLAGS) -o $(TARGET) call_modules.o $(LIBS) $(FLIBS)

$(MMFLIB):
	$(CD) $(MMFDIR);make

#
# Define all object files which make up the library
#

LIBOBJS = \
	basin.o \
	cascade.o \
        soltab.o \
        obs.o \
        potet_jh.o \
        potet_pt.o \
        potet_hs.o \
        potet_pan.o \
        potet_hamon.o \
        ddsolrad.o \
        ccsolrad.o \
        ide_dist.o \
        xyz_dist.o \
        climate_hru.o \
        precip_1sta.o \
        precip_laps.o \
        precip_dist2.o \
        temp_1sta.o \
        temp_laps.o \
        temp_dist2.o \
        transp_frost.o \
        transp_tindex.o \
        frost_date.o \
        srunoff_carea.o \
        srunoff_smidx.o \
        soilzone.o \
        gwflow.o \
        strmflow.o \
        strmflow_lake.o \
        subbasin.o \
        map_results.o \
        muskingum.o \
        write_climate_hru.o \
        prms_summary.o \
        musroute.o \
        basin_sum.o \
        utils_prms.o

LIBOBJS1 = \
        climateflow.o \
        potet_jh_hru_mo.o

LIBOBJS2 = \
        soltab_prms.o \
        temp_2sta_prms.o \
        ddsolrad_prms.o \
        ccsolrad_prms.o \
        potet_hamon_prms.o \
        intcp.o \
        snowcomp.o \
        smbal_prms.o \
        ssflow_prms.o \
        hru_sum_prms.o

all: lib

install: lib

lib: $(LIBS)

$(PRMSLIB2): call_modules.o $(LIBOBJS1)
	$(RM) $(PRMSLIB2)
	$(AR) $(PRMSLIB2) $(LIBOBJS1)
	$(RANLIB) $(PRMSLIB2)

$(PRMSBASE): $(LIBOBJS)
	$(RM) $(PRMSBASE)
	$(AR) $(PRMSBASE) $(LIBOBJS)
	$(RANLIB) $(PRMSBASE)

$(PRMSLIB3): $(LIBOBJS2)
	$(RM) $(PRMSLIB3)
	$(AR) $(PRMSLIB3) $(LIBOBJS2)
	$(RANLIB) $(PRMSLIB3)

clean:
	$(RM) $(PRMSLIB2) $(PRMSLIB3) $(PRMSBASE)
	$(RM) *.o *.mod *~

basin_sum.o: basin_sum.f90
	gfortran -c $(FFLAGS) basin_sum.f90

subbasin.o: subbasin.f90
	gfortran -c $(FFLAGS) subbasin.f90

ddsolrad.o: ddsolrad.f90
	gfortran -c $(FFLAGS) ddsolrad.f90

ccsolrad.o: ccsolrad.f90
	gfortran -c $(FFLAGS) ccsolrad.f90

snowcomp.o: snowcomp.f90
	gfortran -c $(FFLAGS) snowcomp.f90

gwflow.o: gwflow.f90
	gfortran -c $(FFLAGS) gwflow.f90

utils_prms.o: utils_prms.f90
	gfortran -c $(FFLAGS) utils_prms.f90

hru_sum_prms.o: hru_sum_prms.f90
	gfortran -c $(FFLAGS) hru_sum_prms.f90

prms_summary.o: prms_summary.f90
	gfortran -c $(FFLAGS) prms_summary.f90

muskingum.o: muskingum.f90
	gfortran -c $(FFLAGS) muskingum.f90

intcp.o: intcp.f90
	gfortran -c $(FFLAGS) intcp.f90

musroute.o: musroute.f90
	gfortran -c $(FFLAGS) musroute.f90

map_results.o: map_results.f90
	gfortran -c $(FFLAGS) map_results.f90

soltab.o: soltab.f90
	gfortran -c $(FFLAGS) soltab.f90

frost_date.o: frost_date.f90
	gfortran -c $(FFLAGS) frost_date.f90

precip_1sta.o: precip_1sta.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) precip_1sta.f90

transp_tindex.o: transp_tindex.f90
	gfortran -c $(FFLAGS) transp_tindex.f90

transp_frost.o: transp_frost.f90
	gfortran -c $(FFLAGS) transp_frost.f90

temp_1sta.o: temp_1sta.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) temp_1sta.f90

temp_laps.o: temp_laps.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) temp_laps.f90

precip_laps.o: precip_laps.f90 prms_climatevars.mod
	gfortran -c $(FFLAGS) precip_laps.f90

strmflow.o: strmflow.f90 prms_module.mod prms_basin.mod prms_obs.mod prms_flowvars.mod prms_gwflow.mod
	gfortran -c $(FFLAGS) strmflow.f90

potet_jh.o: potet_jh.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_jh.f90

potet_pt.o: potet_pt.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_pt.f90

potet_hs.o: potet_hs.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_hs.f90

potet_jh_hru_mo.o: potet_jh_hru_mo.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_jh_hru_mo.f90

potet_pan.o: potet_pan.f90  prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_pan.f90

potet_hamon.o: potet_hamon.f90  prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_obs.mod
	gfortran -c $(FFLAGS) potet_hamon.f90

write_climate_hru.o: write_climate_hru.f90  prms_module.mod prms_basin.mod prms_climatevars.mod
	gfortran -c $(FFLAGS) write_climate_hru.f90

climate_hru.o: climate_hru.f90  prms_module.mod prms_basin.mod prms_climatevars.mod
	gfortran -c $(FFLAGS) climate_hru.f90

climateflow.o: climateflow.f  prms_module.mod prms_basin.mod cascade.o
	gfortran -c $(FFLAGS) climateflow.f

cascade.o: cascade.f90 prms_module.mod prms_basin.mod
	gfortran -c $(FFLAGS) cascade.f90

basin.o: basin.f90 prms_module.mod
	gfortran -c $(FFLAGS) basin.f90

obs.o: obs.f90 prms_module.mod prms_climatevars.mod
	gfortran -c $(FFLAGS) obs.f90

srunoff_carea.o: srunoff_carea.f prms_intcp.mod prms_snow.mod
	gfortran -c $(FFLAGS) srunoff_carea.f

srunoff_smidx.o: srunoff_smidx.f prms_intcp.mod prms_snow.mod
	gfortran -c $(FFLAGS) srunoff_smidx.f

prms_climatevars.mod: climateflow.o
prms_flowvars.mod: climateflow.o
prms_module.mod: call_modules.o
prms_gwflow.mod: gwflow.o
prms_obs.mod: obs.o
prms_basin.mod: basin.o
prms_soltab.mod: soltab.o
prms_intcp.mod: intcp.o
prms_snow.mod: snowcomp.o
