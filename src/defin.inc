C  Tue Aug 15 12:00:22 EDT 1995
C___________________________________________________________________ DALL.INC
C
C     + + + DEFINITIONS + + +
C     DARU   - Drainage area of each HRU
C     DAT    - Total drainage area of basin
C     DIRL   - Not currently used
C     HORAD  - Potential shortwave radiation on a horizontal surface
C     INTSW  - Switch indicating whether intercepted precip available
C     ITSW   - Switch indicating whether vegetation dormant or transpiring
C     JLDY   - Calendar year julian date
C     JSOL   - Solar year julian date
C     JWDY   - Water year julian date
C     KDS    - Rain gage associated with each HRU
C     MDY    - Current day counter
C     MO     - Current month counter
C     MXDY   - Number of days in year
C     MYR    - Current year counter
C     NDPK   - Number of times snowpack will be adjusted during year
C     NDS    - Number of precip data sets
C     NRU    - Number of HRU's
C     ORAD   - Computed shortwave radiation on horizontal surface
C     ORO    - Daily value of observed mean discharge
C     PET    - Daily potential evapotranspiration
C     PP     - Daily area weighted precip for basin
C     PPT    - Measured precipitation
C     PPTI   - Adjusted precip for impervious area of each HRU
C     PTN    - Net precip on each HRU
C     SCODE  - Two digit state code
C     SWRD   - Computed shortwave radiation on each HRU
C     TAVC   - Daily average temperature in degrees C
C     TAVF   - Daily average temperature in degrees F
C     TMNF   - Daily minimum temperature in degrees F
C     TMXF   - Daily maximum temperature in degrees F
C     XINLOS - Interception loss for each HRU
C___________________________________________________________________ DBS.INC
C
C     + + + DEFINITIONS + + +
C     ARG    - Watershed area associated with specific GW reservoir
C     ARS    - Watershed area associated with specific subsurface reservoir
C     BAS    - Total daily groundwater flow
C     BASQ   - Daily flow from each groundwater reservoir
C     EXSS   - Daily total addition to each subsurface reservoir
C     GSNK   - Groundwater sink coefficient
C     GW     - Storage in each groundwater reservoir
C     GWS    - Total daily inflow to groundwater reservoirs
C     GWSNK  - Total seepage to sink for each groundwater reservoir
C     KGW    - Groundwater index for each HRU
C     KNT    - Number of HRU's with snow cover
C     KRES   - Subsurface index for each HRU
C     KRSP   - Groundwater index for each subsurface reservoir
C     NGW    - Number of groundwater reservoirs
C     NRES   - Number of subsurface reservoirs
C     RAS    - Total daily subsurface flow
C     RASQ   - Daily subsurface flow from each reservoir
C     RCB    - Routing coefficient for each groundwater reservoir
C     RCF    - Linear routing coefficient for each subsurface reservoir
C     RCP    - Non-linear routing coefficient for each subsurface reservoir
C     RES    - Storage in each subsurface reservoir
C     RESMX  - Coefficient for routing water from subsurface to groundwater
C     REXP   - Coefficient for routing water from subsurface to groundwater
C     RSEP   - Seepage from subsurface to groundwater
C     SEP    - Seepage rate from soil excess to groundwater
C     SGW    - Total daily flow from all groundwater reservoirs
C     SRS    - Total daily flow from all subsurface reservoirs
C     SSGW   - Total subsurface to groundwater flowfor year
C     SX     - Portion of snowmelt that become surface runoff
C     UGS    - Seepage to groundwater reservoirs from each HRU
C     USS    - Seepage to subsurface reservoirs from each HRU
C
C___________________________________________________________________ DDATES.INC
C
C     + + + DEFINITIONS + + +
C     BDY    - Begin day of run
C     BMO    - Begin month of run
C     BYR    - Begin year of run
C     EDY    - End day of run
C     EMO    - End month of run
C     EYR    - End year of run
C     IDB    - Begin day of month
C     IDE    - End day of month
C     JDS    - Number of precipitation data sets
C     MB     - Begin month of water year
C     ME     - End month of water year
C
C___________________________________________________________________ DDATOP.INC
C
C     + + + DEFINITIONS + + +
C     BDYIN  - Begin day of initialization period
C     BDYOP  - Begin day of optimization or sensitivity analysis period
C     BMOIN  - Begin month of initialization period
C     BMOOP  - Begin month of optimization or sensitivity analysis period
C     BYRIN  - Begin year of initialization period
C     BYROP  - Begin year of optimization or sensitivity analysis period
C     EDYIN  - End day of init
C     EDYOP  - End day of opt or sens
C     EMOIN  - End month of init
C     EMOOP  - End month of opt or sens
C     EYRIN  - End year of init
C     EYROP  - End year of opt or sens
C     GWE1   -
C     GWE2   -
C     IGOPT  -
C
C___________________________________________________________________ DDFHSG.INC
C
C     + + + DEFINITIONS + + +
C     DFH    - Begin and end times for each hydrograph segment in day
C     IS     - Storm event index
C     NS     - Number of hydrograph segments in storm period
C     RFL    - Read flag for continuation of storm from previous day
C     SFL    - Store flag to continue storm to next day
C     WY     - Water year of storm event
C     WYD    - Water year day of storm event
C
C___________________________________________________________________ DDPR.INC
C
C     + + + DEFINITIONS + + +
C     AJMX   - Monthly factor to adjust snow in rain/snow mix
C     BST    - Base temperature to determine rain or snow
C     COVDNS - Summer cover density for major vegetation type
C     COVDNW - Winter cover density for major vegetation type
C     DRCOR  - Daily correction factor for rain events
C     DSCOR  - Daily correction factor for snow events
C     IDPK   -
C     INSW   - Interception form switch
C     IPPT   - Initialization switch for precipitaiton variables
C     MPC1   - Override switch for DRCOR and DSCOR
C     MPCN   - Month that override of DR/DSCOR ends
C     MPCS   - Month that override of DR/DSCOR starts
C     PAT    - Temperature above which preciptiation is all rain
C     PCONR  - Override value for DRCOR when MPC1=1
C     PCONS  - Override value for DSCOR when MPC1=1
C     PPAR   - Area associated with each precipitation gage
C     RNSTS  - Maximum rain interception storage in summer
C     RNSTW  - Maximum rain interception storage in winter
C     SNST   - Maximum snow interception storage
C     XIN    - Interception for each HRU
C
C___________________________________________________________________ DDSM.INC
C
C     + + + DEFINITIONS + + +
C     BMSA   - Moisture storage in inches in recharge zone of soil profile
C     SMSA   - Accumulated infiltration in inches for each HRU
C
C___________________________________________________________________ DDTMN1.INC
C
C     + + + DEFINITIONS + + +
C     DTMN   - Output switch, storm mode
C     IPRT   - Storm hydrograph print switch
C     JPG    - Switch for storm data to plotgen file, 0=none, 1=output
C     JPL    - Switch to plot storm data, 0=none, 1=plots
C     JPR    - Switch to print storm data, 0=none, 1=print
C
C___________________________________________________________________ DDUMV.INC
C
C     + + + DEFINITIONS + + +
C     DUM1    - User specified input variable
C     DUM2    - User specified input variable
C___________________________________________________________________ DDVDT.INC
C
C     + + + DEFINITIONS + + +
C     DS     - Daily climate data for one year, for up to 7 data types
C     DVP    - Daily observed precip in inches for 1 year, up to 5 gages
C     DVSP   - Daily snow pillow data for 1 year, up to 5 sites
C     DVTN   - Daily minimum temperature for 1 year, up to 5 sites
C     DVTX   - Daily maximum temperature for 1 year, up to 5 sites
C     IPSR   - Daily precipitation form adjust switch, 0=none, 1=snow, 2=rain
C     IPSW   - Precipition form data switch, 0=none, 1=read adjustment data
C
C___________________________________________________________________ DET.INC
C
C     + + + DEFINITIONS + + +
C     CTS    - Monthly air temperature coefficient for ET computations
C     CTW    - Snowpack sublimation computation coefficient
C     CTX    - HRU air temperature coefficient for ET computations
C     DYL    - Possible hours of sunshine in units of 12 hours
C     EPAN   - Daily pan evaporation
C     EVC    - Monthly pan coefficients
C     IPET   - Potential ET computation method switch
C     IT     - Switch to test for start of transpiration period
C     ITND   - Month transpiration ends
C     ITST   - Month transpiration starts
C     MTSE   - Month thunderstorms end
C     MTSS   - Month thunderstorms start
C     SSH    - Daylight hours for each rad plane
C     TST    - Switch to determine beginning of transpiration
C___________________________________________________________________ DGEO1.INC
C
C     + + + DEFINITIONS + + +
C     ARSA   -
C     ARSM   -
C     ITSOL  -
C     PCR    -
C     PCS    -
C     RTB    -
C     RTC    -
C     TSOLX  -
C
C___________________________________________________________________ DIMPRV.INC
C
C     + + + DEFINITIONS + + +
C     DARIP  - Impervious drainage area in acres, each HRU
C     DARP   - Pervious drainage area in acres, each HRU
C     DATIP  - Total impervious drainage area in acres
C     DATP   - Total pervious drainage area
C     EVIMP  - Eavaporation loss from impervious area in inches, each HRU
C     RETIP  - Max retention storage on impervious area in inches, each HRU
C     RSTOR  - Retention storage on impervious area in inches, each HRU
C
C___________________________________________________________________ DINITC.INC
C
C     + + + DEFINITIONS + + +
C     AA     - Initial area value for each x-sect for each OF and ch
C     AAI    - AA for impervious area
C     QMXA   - Maximum routed discharge for storm event, for each OF and ch
C     QQ     - Initial discharge value for each x-sect for each OF and ch
C     QQI    - QQ for impervious area
C     TT     - Initial sediment values for each x-sect for each OF and ch
C___________________________________________________________________ DJM.INC
C
C     + + + DEFINITIONS + + +
C     DEBUG  -
C     IPROB  -
C     PROB   -
C
C___________________________________________________________________ DOPSN2.INC
C
C     + + + DEFINITIONS + + +
C     ISNOSW - Initialization switch, subroutine SNOBAL
C     ISO    - Snow pack melt switch, each HRU, 1=accum. stage, 2=melt stage
C     LSO    - Counter, consecutive days of isothermal snow pack
C     MSO    - Energy balance switch, each HRU, 1=winter, 2=look for spring
C
C___________________________________________________________________ DOPSNO.INC
C
C     + + + DEFINITIONS + + +
C     DPT    - Depth of snowpack in inches, each HRU
C     ISNOYR - Initialization switch, subroutine SNOBAL
C     LST    - Snowpack albedo adjustment switch
C              0 = none
C              1 = adjust for snowfall
C     PSS    - Accum sum of net precip, beginning on day 1 of snowpack
C              formation
C     SALB   - Albedo on day prior to last snow, each HRU
C     SLST   - Number of days since last snowfall, each HRU
C     SNSV   - Depth of new snow in inches, each HRU
C
C___________________________________________________________________ DOPT.INC
C
C     + + + DEFINITIONS + + +
C     IDOUT  - Switch to store predicted daily values
C              0 = not stored
C              >0 = stored
C     INIT   - Initialization switch
C     IOBF   - Objective function selection switch, 1=absolute, 2=squared
C     IOBS   - Observed discharge switch, 0=not available, 1=available
C     IOBSW  - Objective function compuation switch
C              0 = not included
C              1 = included
C     IOPT   - Optimization mode switch, 0=none, 1,2=daily, 3,4,5=storm
C     IOSW   - Input/output switch
C     IPOP1  - Print summary switch, 0=table, 1=annual, 2=monthly, 3=daily
C     IPOP2  - Hru print switch, 0=none, 1=annual, 2=monthly, 3=daily
C     IPUN1  - Julian day to start printing HRU values
C     IPUN2  - Julian day to stop printing HRU values
C     IPUN3  - Julian day to start printing daily summary
C     IPUN4  - Julian day to stop printing daily summary
C     ISEN   - Sensitivity analysis switch, 0=none, 1,2=daily, 3,4,5=storm
C     ISIM   - Simulation mode switch, 0=daily, 1,2,3=storm
C     ISSOL  - Initialization switch for Subroutine SOLRAD
C     ISTMP  - Initialization switch for Subroutine TEMP
C     IUOUT  - Switch to store predicted storm values
C              0 = not stored
C              >0 = stored
C     IWSW   - Input/output switch, optimization and sensitivity
C     LOP    - Index number of parameter to be optimized
C     MFN    - End month for objective function compuation
C     MFS    - Begin month for objective function computation
C     NDOP   - Optimization switch, 0=continue, 1=end
C     NPRNT  - Print switch during optimization, 0=no print, 1=print
C     NYRI   - Number of years in initialization period
C     OBF    - Computed objective functions
C     TESTNO - Switch to include storm in opt & sens
C              0 = not included
C              1 = included
C
C___________________________________________________________________ DOPT2.INC
C
C     + + + DEFINITIONS + + +
C     GU     -
C     GUS    -
C     HU     -
C     HUS    -
C
C___________________________________________________________________ DPCRCH.INC
C
C     + + + DEFINITIONS + + +
C     ALPIA  - Coefficient in kinematic routing equation, imperv area,
C              OF planes
C     ALPI1A - Inverse of ALPIA
C     ALPRA  - Coefficient in kinematic routing equation, OF planes and
C              channels
C     ALPR1A - Inverse of ALPRA
C     CMIA   - Exponent in kinematic routing equation, imperv area,
C              OF planes
C     CMI1A  - Inverse of CMIA
C     CMPA   - Exponent in kinematic routing equation, OF planes and
C              channels
C     CMP1A  - Inverse of CMPA
C     DTDXA  - DT/DX for each OF and channel routing segment in sec/ft
C     DTDXMA - (DT/DX)*CM for each OF and channel segment
C     DTMA   - Routing time intermal in minutes, each OF and ch
C     DTOSA  - Print of plot interval in minutes, each OF and ch
C     DTSA   - Routing time in seconds, each OF and ch
C     DXA    - Length/NDX, each OF and ch
C     DXDTA  - DX/DT in ft/sec, each OF and ch
C     D50A   -
C     ENA    - Sediment parameter, each OF and ch
C     HCA    - Sediment parameter, each OF and ch
C     KFA    - Sedimant parameter, each OF and ch
C     KRA    - Sediment parameter, each OF and ch
C     LBA    - Index of OF segment input to channel segment
C     MMA    - Sediment parameter, each OF and ch
C     NDELSA - Number of routing time increments, each OF and ch
C     NXSA   - Number of cross-sections for routing, each OF and ch
C     ODELSA - Number of print or plot time increments, each OF and ch
C     OFIPA  - Portion of OF plane that is impervious, decimal percent
C     PCRIDA - ID characters for each, OF, ch, res, or junction
C     PROUTA - Output outflow from OF or ch, 0=no, 1=print, 2=plot, 3=pltgen
C     PRTINA - Output inflow to OF or ch, 0=no, 1=print, 2=plot, 3=pltgen
C     RBA    - Index of OF plane to input to channel segment
C     RITEA  - Write switch for each ch, res or junction
C     THRESA - Minimum depth of flow to continue routing in feet
C     TYPEA  - Type of OF plane or ch segment
C     UPA    - Upstream inflow ch ID foe each channel segment
C     UPSWA  - Upstream inflow switch for each channel, 0=no, 1=yes
C
C___________________________________________________________________ DPKADJ.INC
C
C     + + + DEFINITIONS + + +
C     IPKAD  -
C     PKFAC  -
C
C___________________________________________________________________ DPLOT.INC
C
C     + + + DEFINITIONS + + +
C     IDPLB  - Begin day for daily plots
C     IDPLE  - End day for daily plots
C     IMPLB  - Begin month for daily plots
C     IMPLE  - End month for daily plots
C     IPLOT  - Daily values plot switch, 0=none, 1=plots
C     IPLTYP - Daily plot type switch, 0=linear, 1=semi-log
C     PLMN   - Minimum value for daily plot axis
C     PLMX   - Maximum value for daily plot axis
C     PRECX  -
C     PRORO  - Observed daily mean discharge values for water year
C     PRPRO  - Predicted daily mean discharge values for water year
C
C___________________________________________________________________ DPROBN.INC
C
C     + + + DEFINITIONS + + +
C     BDYFC  -
C     BDYPR  -
C     BMOFC  -
C     BMOPR  -
C     BYRFC  -
C     BYRPR  -
C     EDYFC  -
C     EDYPR  -
C     EMOFC  -
C     EMOPR  -
C     EYRFC  -
C     EYRPR  -
C
C___________________________________________________________________ DQSOUT.INC
C
C     + + + DEFINITIONS + + +
C     QSDY   - Day of output record for plot or print
C     QSID   - OF or ch ID for output record for print or plot
C     QSIO   - I/O switch for output record, 1=inflow, 2=outflow
C     QSMO   - Month of output record
C     QSQS   - Data type switch
C              1=print Q, 2=print sed, 3=plot Q, 4=plot sed
C     QSRPD  - Values per day for output record
C     QSWY   - Water year for output record
C
C___________________________________________________________________ DRD.INC
C
C     + + + DEFINITIONS + + +
C     COSSL  -
C     IRD    - Radiation plane index
C     MRDC   - Method switch for solar radiation computations
C     NRD    - Number of radiation planes in basin
C     PARS   - Precipitation correction factor for summer solar rad
C     PARW   - Precipitation correction factor for winter solar rad
C     RAD    - Potential shortwave radiation
C     RDB    - Coefficient in sky cover/solar rad computation
C     RDC    - Y-intercept in solar rad computation equations
C     RDM    - Slope in solar rad computation equations
C     RDMX   - Maximum fraction of potential solar radiation
C     RDP    - Coefficient in sky cover/solar rad equations
C
C___________________________________________________________________ DSCS.INC
C
C     + + + DEFINITIONS + + +
C     ILPS   -
C     KDC    -
C     KSP    -
C     KTS    -
C     NPLW   -
C     NTS    -
C     PLWE   -
C
C___________________________________________________________________ DSED.INC
C
C     + + + DEFINITIONS + + +
C     BSED   -
C     PQ     - Total routed flow in inches, each storm
C     PSED   - Total routed sediment load in tons, each storm
C     SEDSG  -
C     TSUW   - Sediment load from each OF plane in tons/day, each storm
C
C___________________________________________________________________ DSENT.INC
C
C     + + + DEFINITIONS + + +
C     IWSW2  - Write switch for optimization and sensitivity analysis
C     NDSN   - Sensitivity analysis switch; 0=continue, 1=end
C     NMOBF  - Number of months in daily objective function
C     NOBF   - Number of days or events included in optimization
C     NV     - Number of parameters to be optimized
C     XOPT   - Final values of parameters obtained from optimization
C     XX     - Optimization or sensitivity analysis parameter array
C
C___________________________________________________________________ DSENT1.INC
C
C     + + + DEFINITIONS + + +
C     ASENS  - Matrix of sensitivity values
C     CX     - Initial parameter values for optimization and sensitivity
C     G      - Lower parameter constraint for optimization
C     H      - Upper parameter constraint for optimization
C     ILOPL  - Type of adjustment increment switch, 0=equal, 1= proportional
C     IP     - Autoregressive order for correlated daily errors
C     IPRIOR - Prior information switch, 0=none, 1=available
C     IQ     - Moving average for correlated daily errors
C     IR     - IP + IQ
C     NDVR   - Indices of distributed parameters to be adjusted
C     NTRY   - Number of optimization cycles
C     NVAR   - Number of values of a distributed parameter to be adjusted
C     OBFOPT - Final objective function value
C     OBSRUN - Observed runoff array
C     P      - Initial adjustment factor for each parameter
C     PINC   - Fraction by which parameters are adjusted
C     SMVOPT - Predicted runoff produced from final parameter values
C     VBIN   - Prior information covariance matrix
C     XXAVE  - Average value of distributed parameters
C     XXDEV  - Deviation of distributed parameters from average
C     ZINFC  - Information matrix
C     ZINV   - Inverse of information matrix
C
C___________________________________________________________________ DSENT2.INC
C
C     + + + DEFINITIONS + + +
C     DIAG   - Square root of diagonal elements of information matrix
C     IOBSWK - Objective function computation switch, 1=included, 0=not
C     IPTEMP -
C     OFSPR  -
C     PHI    - Array of autoregressive and moving average parameters
C
C___________________________________________________________________ DSENT3.INC
C
C     + + + DEFINITIONS + + +
C     IFTS   -
C     IMAT   -
C     ISTAT  - Statistical summary switch, 0=no, 1=yes
C     ITRANS - Log transformation switch, 0=no, 1=transform
C     NCNT   -
C     NREC   - Index for direct access file
C     TSM    - Sum of daily max temp to determine start of ET
C
C___________________________________________________________________ DSIZE.INC
C
C     + + + DEFINITIONS  + + +
C     BWY    - Begin water year for simulation
C     EWY    - End water year for simulation
C     IDS    - Rain gage data set index
C     IDUS   - Date use switch for types 1-10, 0=not used, 1=used
C     IDY    - Current day of month counter
C     IMO    - Current month counter
C     IWY    - Current water year counter
C     LMO    - Previous month counter
C     LYR    - Previous year counter
C     NDTY   - Number of data types used in simulation
C     NDY    - Number of days in each month
C     NMO    -
C     NYR    - Number of water years to be simulated
C     PARMCA - 5-digit parameter code for WATSTORE data identification
C     STATCA - 5-digit statistic code for WATSTORE data identification
C
C___________________________________________________________________ DSM.INC
C
C     + + + DEFINITIONS + + +
C     AET    - Actual daily ET on each HRU
C     ENFIL  - Precipitation available for infiltration on each HRU
C     EXCS   - Daily soil water in excess of SMAX for each HRU
C     ICOV   - Vegetation cover type for each HRU
C     ISOIL  - Soil type for each HRU
C     RECHR  - Storage in soil zone where losses occur as evap. and trans.
C     REMX   - Maximum RECHR for HRU
C     SAS    - Total surface runoff from all HRU's
C     SC1    - Coefficient in contributing area/surface runoff equations
C     SCN    - Used to determine contributing area for surface runoff
C     SCT    - Difference between max and min contributing area for sro
C     SCX    - Used to determine contributing area for surface runoff
C     SLP    - Slope of HRU
C     SMAV   - Daily available water in soil profile for HRU
C     SMAX   - Max available water holding capacity of soil profile
C     SRO    - Surface runoff from each HRU
C     SRX    - Max daily snowmelt infiltration at field capacity
C
C___________________________________________________________________ DSNCV.INC
C
C     + + + DEFINITIONS + + +
C     AIMX   -
C     ASC    -
C     ASCSV  -
C     IASW   -
C     KSDC   -
C     NDC    -
C     PKSV   -
C     PST    -
C     SCA    -
C     SCRV   -
C
C___________________________________________________________________ DSNO.INC
C
C     + + + DEFINITIONS + + +
C     ALB    - Computed albedo, each HRU
C     ANSW   -
C     DEN    - Density of snowpack, each HRU
C     DENI   - Initial density of new-fallen snow, decimal percent
C     DENMX  - Average maximum density of snow pack, decimal percent
C     EAIR   - Emissivity of air on days with no precipitation
C     FREWT  - Free water content of snow, each HRU
C     FWCAP  - Free water holding capacity of snow pack, decimal percent
C              of PWEQV
C     ISNO   - Snow switch, 0=no snow pack, 1=snow pack
C     ISP1   - Julian day to start looking for spring snowmelt stage
C     ISP2   - Julian day to force snowpack to spring snowmelt stage
C     PACT   - Snow-pack temperature in degrees C, each HRU
C     PICE   - Inches of snowpack existing as ice, each HRU
C     PKDEF  - Calories required to bring snow pack to isothermal, each HRU
C     PWEQV  - Snow-pack water equivalent in inches, each HRU
C     SETCON - Snow-pack settlement time constant
C     SMLT   - Daily snowmelt in inches, each HRU
C     SNEV   - Evaporation form snow pack in inches, each HRU
C     TCAL   - Daily net energy balance, each HRU with snow cover
C     TRNCF  - Transmission coefficient for sw radiation thru canopy,
C              each HRU
C
C___________________________________________________________________ DSNOP.INC
C
C     + + + DEFINITIONS + + +
C     CECN   - Convection-condensation energy coef by month, cal/degree>0
C     NSW    -
C     PKAD   - Adjusted snow-pack water equiv computed from snow course data
C     PRMX   - Percent rain in mixed rain/snow event
C     PSN    - Water equiv of new snow in inches, each HRU
C     RMXA   - Reset albedo if PRMX<RMXA, snow accumulation stage
C     RMXM   - Reset albedo if PRMX<RMXA, snow melt stage
C
C___________________________________________________________________ DSTACH.INC
C
C     + + + DEFINITIONS + + +
C     STAIDC - Climate data station id numbers, up to 8 stations
C     STAIDP - Precip data station ids, daily and storm, up to 5 gages
C     STAIDS - Snow pillow station ids, up to 5 sites
C     STAIDT - Temperature station ids, up to 5 sites
C     TITL   - Title for printed output
C
C___________________________________________________________________ DSTOR.INC
C
C     + + + DEFINITIONS + + +
C     C5     - Y-intercept for 5-min interval puls routing
C     C15    - Y-intercept for 15-min interval puls routing
C     C24    - Y-intercept for daily interval puls routing
C     DIN1   - SW reservoir inflow in cfs, previous time step
C     IDWN   - Switch on if reservoir flows into another reservoir
C     IRTYP  - Routing type for surface reservoir, 8=puls, 9=linear
C     IRUP1  - Index of upstream SW reservoir 1, each SW reservoir
C     IRUP2  - Index of upstream SW reservoir 2, each SW reservoir
C     IRUP3  - Index of upstream SW reservoir 3, each SW reservoir
C     KSTOR  - Index of SW reservoir receiving runoff, each HRU
C     NSOS   - Number of storage-outflow values in table for puls routing
C     NSTOR  - Number of SW storage reservoirs
C     O2     - Outflow-storage table values for puls routing
C     QRO    - Initial day mean outflow in cfs for each SW storage reservoir
C     RCS    - Linear routing coefficient for each SW storage reservoir
C     STO    - Initial storage in cfs-days for each SW storage reservoir
C     S2     - Outflow-storage table values for puls routing
C     S5     - Slope for 5-min interval puls routing
C     S15    - Slope for 15-min interval puls routing
C     S24    - Slope for daily interval puls routing
C     WV5    - Calculated values of 2S/DT+O to determine 5-min outflow, puls
C     WV15   - Calculated values of 2S/DT+O to determine 15-min outflow,
C              puls
C     WVD    - Calculated values of 2S/DT+O to determine daily outflow, puls
C
C___________________________________________________________________ DSWTCH.INC
C
C     + + + DEFINITIONS + + +
C     ISBAS  - Initialization switch, subroutine BASFLW
C     ISSR1  - Surface runoff method switch, 0=linear, 1=non-linear
C     ISSRO  - Initialziation switch, subroutine SRFRO
C     ISUN   - Storm subsurface and gw routing switch, 0=not done, 1=done
C     ISX1   - Initialization switch, subroutine UNITD
C
C___________________________________________________________________ DUNSS.INC
C
C     + + + DEFINITIONS + + +
C     CDA    - Cumulative drainage area in acres for each channel segment.
C     PROLST -
C     UINF   - 15-minute subsurface flow in inches for eash subsurface res.
C     UNBAS  - 15-minute gw flow in inches for each gw res.
C___________________________________________________________________ DUNTRF.INC
C
C     + + + DEFINITIONS + + +
C     HA     - Routed outflow depth for OF or ch for 1-60 minutes
C     INQ    - Accumulated upstream input to channel  segment
C     INS    - Weighted average upstream sediment input to channel
C     QA     - Routed outflow in cfs/ft for OF or ch for 1-60 minutes
C     QLAT   - Accumulated lateral inflow to channel for 1-60 minutes
C     TA     - Routed sediment outflow in kg/cu.ft/s for OF and ch, 1-60 min
C     TLAT   - Lateral sediment inflow to channel for 1-60 minutes
C     UPE    - Precipitation excess in inches for 1-60 minute intervals
C
C___________________________________________________________________ DUPR.INC
C
C     + + + DEFINITIONS + + +
C     IMPERV - Impervious area, decimal percent, each HRU
C     PERV   - Pervious area, decimal percent, each HRU
C     UPCOR  - Precipitation correction factor for storms, each HRU
C
C___________________________________________________________________ DUVRT.INC
C
C     + + + DEFINITIONS + + +
C     UVWY   - Storm day switch for each day of water year
C
C___________________________________________________________________ DVOL.INC
C
C     + + + DEFINITIONS + + +
C     BPK    - Computed peak flow in cfs, each storm
C     BRO    - Computed runoff volume in inches, each storm
C     ICHG   - Computation mode switch, 0=storm, 1=daily
C     ISAVE  - Print/plot record counter, storm mode
C     KRU    -
C     NCRSEG - Number of channel routing segments
C     NOFSEG - Number of overland flow planes
C     NST    - Total number of hydrograph segments, all storms
C     OBPK   - Observed peak flow in cfs, each storm
C     OBRO   - Observed runoff volume in inches, each storm
C     SIMOPT -
C     X      - Infiltration parameters, each HRU
C___________________________________________________________________ DWDSN.INC
C
C     + + + DEFINITIONS + + +
C     DSNC   - WDM data set numbers for evaporation, solar rad and discharge
C     DSNP   - WDM data set numbers for precipitation
C     DSNS   - WDM data set numbers for snow
C     DSNT   - WDM data set numbers for temperature
C     FMT    - Format of data set (WDM or TSS)
C
C___________________________________________________________________ DWX.INC
C
C     + + + DEFINITIONS + + +
C     CSEL   - Climate station elevation
C     ELV    - Elevation of HRU
C     TLN    - Minimum temperature lapse rate
C     TLPS   -
C     TLX    - Maximum temperature lapse rate
C     TMN    - Daily minimum temperature at climate station
C     TMX    - Daily maximum temperature at climate station
C     TNAJ   - Daily minimum temperature adjusted to HRU
C     TXAJ   - Daily maximum temperature adjusted to HRU
C     SA     - Slope and aspect designation for solar radiation planes
C
