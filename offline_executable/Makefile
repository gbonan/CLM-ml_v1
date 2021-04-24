# ###############################################
# Makefile
#
# To run: type 'make' or 'make -f Makefile'
#
# To remove obj files and mod files: type 'make clean'
# ###############################################
 
# LIB_NETCDF = /usr/local/netcdf-c-4.6.1-f-4.4.4-pgi-pgcc-pgf-18.1/lib
# MOD_NETCDF = /usr/local/netcdf-c-4.6.1-f-4.4.4-pgi-pgcc-pgf-18.1/include
 
LIB_NETCDF = $(NETCDF_PATH)/lib
MOD_NETCDF = $(NETCDF_PATH)/include
 
# Source code directories
 
shr = ../cime_src_share_util
main = ../clm_src_main
util = ../clm_src_utils
bgp = ../clm_src_biogeophys
can = ../multilayer_canopy
drv = ../offline_driver
cpl = ../clm_src_cpl
 
# Directory for object files

O = obj

# Executable is "prgm.exe"

prgm = prgm.exe

# Fortran 95 compiler

cmplr = pgf90 -C -Minform=inform -Ktrap=fp -L$(LIB_NETCDF) -I$(MOD_NETCDF) -lnetcdff -lblas -llapack -lm -L/usr/lib64

# Specify module files (that all other files use). Order matters, because some files
# are used by other files in mods list

mods = ${O}/shr_kind_mod.o ${O}/clm_varcon.o ${O}/clm_varctl.o ${O}/MLclm_varctl.o ${O}/clm_varpar.o \
	${O}/MLclm_varcon.o ${O}/MLclm_varpar.o ${O}/spmdMod.o \
	${O}/ColumnType.o ${O}/PatchType.o ${O}/GridcellType.o \
	${O}/CanopyStateType.o ${O}/EnergyFluxType.o ${O}/FrictionVelocityMod.o ${O}/SoilStateType.o \
	${O}/SolarAbsorbedType.o ${O}/SurfaceAlbedoType.o ${O}/TemperatureType.o ${O}/WaterFluxType.o \
	${O}/WaterStateType.o ${O}/atm2lndType.o \
	${O}/decompMod.o ${O}/pftconMod.o ${O}/abortutils.o ${O}/ncdio_pio.o \
	${O}/shr_file_mod.o ${O}/fileutils.o ${O}/clm_time_manager.o ${O}/MLWaterVaporMod.o \
	${O}/SoilTexMod.o ${O}/TowerDataMod.o ${O}/clm_varorb.o ${O}/shr_orb_mod.o \
	${O}/histFileMod.o ${O}/restUtilMod.o ${O}/MLCanopyFluxesType.o ${O}/filterMod.o \
	${O}/MLMathToolsMod.o

# Specify files needed for clm_instMod

src0 = ${O}/SurfaceAlbedoMod.o ${O}/SoilStateInitTimeConstMod.o ${O}/initVerticalMod.o

# Specify other files. Order matters, because some files are used by other files in src1 list

src1 = ${O}/clm_instMod.o ${O}/SoilWaterMovementMod.o ${O}/controlMod.o \
	${O}/SoilTemperatureMod.o ${O}/MLLeafHeatCapacityMod.o \
	${O}/MLLeafBoundaryLayerMod.o ${O}/MLLeafPhotosynthesisMod.o ${O}/MLLeafFluxesMod.o \
	${O}/MLLongwaveRadiationMod.o ${O}/MLPlantHydraulicsMod.o \
	${O}/MLSoilFluxesMod.o ${O}/MLSolarRadiationMod.o ${O}/SurfaceResistanceMod.o \
	${O}/TowerMetMod.o ${O}/clmDataMod.o ${O}/MLCanopyWaterMod.o ${O}/MLCanopyNitrogenProfileMod.o \
	${O}/MLCanopyTurbulenceMod.o ${O}/MLinitVerticalMod.o

# Specify all other files. Order matters, because some files are used by other files in src2 list

src2 = ${O}/initSubgridMod.o ${O}/initGridCellsMod.o ${O}/MLCanopyFluxesMod.o \
	${O}/clm_driver.o ${O}/lnd_comp_mct.o \
	${O}/clm_initializeMod.o ${O}/CLMml_driver.o ${O}/CLMml.o

# Object files

objs = $(mods) $(src0) $(src1) $(src2)

# Compile code into an executable file defined by "prgm"

$(prgm): $(objs)
	$(cmplr) -o $(prgm) $(objs)

# mods files first, because they are used by other files

${O}/shr_kind_mod.o: ${shr}/shr_kind_mod.F90
	$(cmplr) -c ${shr}/shr_kind_mod.F90
	mv shr_kind_mod.o ${O}

${O}/shr_file_mod.o: ${shr}/shr_file_mod.F90 ${O}/shr_kind_mod.o ${O}/clm_varctl.o ${O}/abortutils.o 
	$(cmplr) -c ${shr}/shr_file_mod.F90
	mv shr_file_mod.o ${O}

${O}/shr_orb_mod.o: ${shr}/shr_orb_mod.F90 ${O}/shr_kind_mod.o ${O}/clm_varcon.o
	$(cmplr) -c ${shr}/shr_orb_mod.F90
	mv shr_orb_mod.o ${O}

${O}/clm_varcon.o: ${main}/clm_varcon.F90 ${O}/shr_kind_mod.o
	$(cmplr) -c ${main}/clm_varcon.F90
	mv clm_varcon.o ${O}

${O}/clm_varctl.o: ${main}/clm_varctl.F90 ${O}/shr_kind_mod.o
	$(cmplr) -c ${main}/clm_varctl.F90
	mv clm_varctl.o ${O}

${O}/clm_varpar.o: ${main}/clm_varpar.F90 ${O}/shr_kind_mod.o ${O}/MLclm_varctl.o
	$(cmplr) -c ${main}/clm_varpar.F90
	mv clm_varpar.o ${O}

${O}/decompMod.o: ${main}/decompMod.F90 ${O}/shr_kind_mod.o
	$(cmplr) -c ${main}/decompMod.F90
	mv decompMod.o ${O}

${O}/pftconMod.o: ${main}/pftconMod.F90 ${O}/shr_kind_mod.o ${O}/clm_varpar.o
	$(cmplr) -c ${main}/pftconMod.F90
	mv pftconMod.o ${O}

${O}/abortutils.o: ${main}/abortutils.F90 ${O}/shr_kind_mod.o ${O}/clm_varctl.o
	$(cmplr) -c ${main}/abortutils.F90
	mv abortutils.o ${O}

${O}/clm_time_manager.o: ${util}/clm_time_manager.F90 ${O}/shr_kind_mod.o ${O}/clm_varctl.o ${O}/abortutils.o
	$(cmplr) -c ${util}/clm_time_manager.F90
	mv clm_time_manager.o ${O}

${O}/clm_varorb.o: ${util}/clm_varorb.F90 ${O}/shr_kind_mod.o
	$(cmplr) -c ${util}/clm_varorb.F90
	mv clm_varorb.o ${O}

${O}/fileutils.o: ${util}/fileutils.F90 ${O}/shr_kind_mod.o ${O}/clm_varctl.o ${O}/abortutils.o ${O}/shr_file_mod.o
	$(cmplr) -c ${util}/fileutils.F90
	mv fileutils.o ${O}

${O}/spmdMod.o: ${util}/spmdMod.F90 ${O}/shr_kind_mod.o
	$(cmplr) -c ${util}/spmdMod.F90
	mv spmdMod.o ${O}

${O}/MLclm_varcon.o: ${can}/MLclm_varcon.F90 ${O}/shr_kind_mod.o ${O}/clm_varcon.o
	$(cmplr) -c ${can}/MLclm_varcon.F90
	mv MLclm_varcon.o ${O}

${O}/MLclm_varctl.o: ${can}/MLclm_varctl.F90 ${O}/shr_kind_mod.o
	$(cmplr) -c ${can}/MLclm_varctl.F90
	mv MLclm_varctl.o ${O}

${O}/MLclm_varpar.o: ${can}/MLclm_varpar.F90 ${O}/shr_kind_mod.o
	$(cmplr) -c ${can}/MLclm_varpar.F90
	mv MLclm_varpar.o ${O}

${O}/MLMathToolsMod.o: ${can}/MLMathToolsMod.F90 ${O}/shr_kind_mod.o ${O}/MLclm_varctl.o ${O}/abortutils.o
	$(cmplr) -c ${can}/MLMathToolsMod.F90
	mv MLMathToolsMod.o ${O}

${O}/MLWaterVaporMod.o: ${can}/MLWaterVaporMod.F90 ${O}/shr_kind_mod.o ${O}/clm_varcon.o ${O}/MLclm_varcon.o
	$(cmplr) -c ${can}/MLWaterVaporMod.F90
	mv MLWaterVaporMod.o ${O}

${O}/SoilTexMod.o: ${drv}/SoilTexMod.F90 ${O}/shr_kind_mod.o
	$(cmplr) -c ${drv}/SoilTexMod.F90
	mv SoilTexMod.o ${O}

${O}/TowerDataMod.o: ${drv}/TowerDataMod.F90 ${O}/shr_kind_mod.o
	$(cmplr) -c ${drv}/TowerDataMod.F90
	mv TowerDataMod.o ${O}

${O}/atm2lndType.o: ${main}/atm2lndType.F90 ${O}/shr_kind_mod.o ${O}/clm_varpar.o ${O}/decompMod.o
	$(cmplr) -c ${main}/atm2lndType.F90
	mv atm2lndType.o ${O}

${O}/ColumnType.o: ${main}/ColumnType.F90 ${O}/shr_kind_mod.o ${O}/clm_varpar.o ${O}/clm_varcon.o
	$(cmplr) -c ${main}/ColumnType.F90
	mv ColumnType.o ${O}

${O}/PatchType.o: ${main}/PatchType.F90 ${O}/shr_kind_mod.o ${O}/clm_varcon.o
	$(cmplr) -c ${main}/PatchType.F90
	mv PatchType.o ${O}

${O}/GridcellType.o: ${main}/GridcellType.F90 ${O}/shr_kind_mod.o ${O}/clm_varcon.o
	$(cmplr) -c ${main}/GridcellType.F90
	mv GridcellType.o ${O}

${O}/ncdio_pio.o: ${main}/ncdio_pio.F90 ${O}/shr_kind_mod.o ${O}/abortutils.o
	$(cmplr) -c ${main}/ncdio_pio.F90
	mv ncdio_pio.o ${O}

${O}/histFileMod.o: ${main}/histFileMod.F90 ${O}/shr_kind_mod.o ${O}/decompMod.o
	$(cmplr) -c ${main}/histFileMod.F90
	mv histFileMod.o ${O}

${O}/restUtilMod.o: ${util}/restUtilMod.F90 ${O}/shr_kind_mod.o ${O}/ncdio_pio.o
	$(cmplr) -c ${util}/restUtilMod.F90
	mv restUtilMod.o ${O}

${O}/CanopyStateType.o: ${bgp}/CanopyStateType.F90 ${O}/shr_kind_mod.o ${O}/clm_varcon.o ${O}/decompMod.o
	$(cmplr) -c ${bgp}/CanopyStateType.F90
	mv CanopyStateType.o ${O}

${O}/EnergyFluxType.o: ${bgp}/EnergyFluxType.F90 ${O}/shr_kind_mod.o ${O}/clm_varcon.o ${O}/decompMod.o
	$(cmplr) -c ${bgp}/EnergyFluxType.F90
	mv EnergyFluxType.o ${O}

${O}/FrictionVelocityMod.o: ${bgp}/FrictionVelocityMod.F90 ${O}/shr_kind_mod.o ${O}/clm_varcon.o ${O}/decompMod.o
	$(cmplr) -c ${bgp}/FrictionVelocityMod.F90
	mv FrictionVelocityMod.o ${O}

${O}/SoilStateType.o: ${bgp}/SoilStateType.F90 ${O}/shr_kind_mod.o ${O}/clm_varcon.o ${O}/clm_varpar.o ${O}/decompMod.o
	$(cmplr) -c ${bgp}/SoilStateType.F90
	mv SoilStateType.o ${O}

${O}/SolarAbsorbedType.o: ${bgp}/SolarAbsorbedType.F90 ${O}/shr_kind_mod.o ${O}/clm_varcon.o ${O}/decompMod.o
	$(cmplr) -c ${bgp}/SolarAbsorbedType.F90
	mv SolarAbsorbedType.o ${O}

${O}/SurfaceAlbedoType.o: ${bgp}/SurfaceAlbedoType.F90 ${O}/shr_kind_mod.o ${O}/clm_varcon.o ${O}/clm_varpar.o ${O}/decompMod.o
	$(cmplr) -c ${bgp}/SurfaceAlbedoType.F90
	mv SurfaceAlbedoType.o ${O}

${O}/TemperatureType.o: ${bgp}/TemperatureType.F90 ${O}/shr_kind_mod.o ${O}/clm_varcon.o ${O}/clm_varpar.o ${O}/decompMod.o
	$(cmplr) -c ${bgp}/TemperatureType.F90
	mv TemperatureType.o ${O}

${O}/WaterFluxType.o: ${bgp}/WaterFluxType.F90 ${O}/shr_kind_mod.o ${O}/clm_varcon.o ${O}/decompMod.o
	$(cmplr) -c ${bgp}/WaterFluxType.F90
	mv WaterFluxType.o ${O}

${O}/WaterStateType.o: ${bgp}/WaterStateType.F90 ${O}/shr_kind_mod.o ${O}/clm_varcon.o ${O}/clm_varpar.o ${O}/decompMod.o
	$(cmplr) -c ${bgp}/WaterStateType.F90
	mv WaterStateType.o ${O}

${O}/MLCanopyFluxesType.o: ${can}/MLCanopyFluxesType.F90 ${O}/shr_kind_mod.o ${O}/clm_varcon.o ${O}/clm_varpar.o \
${O}/decompMod.o ${O}/histFileMod.o ${O}/restUtilMod.o ${O}/ncdio_pio.o ${O}/shr_file_mod.o ${O}/MLclm_varpar.o
	$(cmplr) -c ${can}/MLCanopyFluxesType.F90
	mv MLCanopyFluxesType.o ${O}

${O}/filterMod.o: ${main}/filterMod.F90 ${O}/shr_kind_mod.o ${O}/clm_varcon.o
	$(cmplr) -c ${main}/filterMod.F90
	mv filterMod.o ${O}

# src0 files, which use mod files

${O}/SurfaceAlbedoMod.o: ${bgp}/SurfaceAlbedoMod.F90 $(mods)
	$(cmplr) -c ${bgp}/SurfaceAlbedoMod.F90
	mv SurfaceAlbedoMod.o ${O}

${O}/SoilStateInitTimeConstMod.o: ${bgp}/SoilStateInitTimeConstMod.F90 $(mods)
	$(cmplr) -c ${bgp}/SoilStateInitTimeConstMod.F90
	mv SoilStateInitTimeConstMod.o ${O}

${O}/initVerticalMod.o: ${main}/initVerticalMod.F90 $(mods)
	$(cmplr) -c ${main}/initVerticalMod.F90
	mv initVerticalMod.o ${O}

# src1 files, which use mod files and src0 files

${O}/clm_instMod.o: ${main}/clm_instMod.F90 $(mods) $(src0)
	$(cmplr) -c ${main}/clm_instMod.F90
	mv clm_instMod.o ${O}

${O}/controlMod.o: ${drv}/controlMod.F90 $(mods)
	$(cmplr) -c ${drv}/controlMod.F90
	mv controlMod.o ${O}

${O}/SoilWaterMovementMod.o: ${bgp}/SoilWaterMovementMod.F90 $(mods)
	$(cmplr) -c ${bgp}/SoilWaterMovementMod.F90
	mv SoilWaterMovementMod.o ${O}

${O}/SoilTemperatureMod.o: ${bgp}/SoilTemperatureMod.F90 $(mods)
	$(cmplr) -c ${bgp}/SoilTemperatureMod.F90
	mv SoilTemperatureMod.o ${O}

${O}/TowerMetMod.o: ${drv}/TowerMetMod.F90 $(mods)
	$(cmplr) -c ${drv}/TowerMetMod.F90
	mv TowerMetMod.o ${O}

${O}/clmDataMod.o: ${drv}/clmDataMod.F90 $(mods)
	$(cmplr) -c ${drv}/clmDataMod.F90
	mv clmDataMod.o ${O}

${O}/SurfaceResistanceMod.o: ${bgp}/SurfaceResistanceMod.F90 $(mods)
	$(cmplr) -c ${bgp}/SurfaceResistanceMod.F90
	mv SurfaceResistanceMod.o ${O}

${O}/MLSolarRadiationMod.o: ${can}/MLSolarRadiationMod.F90 $(mods)
	$(cmplr) -c ${can}/MLSolarRadiationMod.F90
	mv MLSolarRadiationMod.o ${O}

${O}/MLCanopyTurbulenceMod.o: ${can}/MLCanopyTurbulenceMod.F90 $(mods)
	$(cmplr) -c ${can}/MLCanopyTurbulenceMod.F90
	mv MLCanopyTurbulenceMod.o ${O}

${O}/MLLeafBoundaryLayerMod.o: ${can}/MLLeafBoundaryLayerMod.F90 $(mods)
	$(cmplr) -c ${can}/MLLeafBoundaryLayerMod.F90
	mv MLLeafBoundaryLayerMod.o ${O}

${O}/MLLeafHeatCapacityMod.o: ${can}/MLLeafHeatCapacityMod.F90 $(mods)
	$(cmplr) -c ${can}/MLLeafHeatCapacityMod.F90
	mv MLLeafHeatCapacityMod.o ${O}

${O}/MLLeafFluxesMod.o: ${can}/MLLeafFluxesMod.F90 $(mods)
	$(cmplr) -c ${can}/MLLeafFluxesMod.F90
	mv MLLeafFluxesMod.o ${O}

${O}/MLLeafPhotosynthesisMod.o: ${can}/MLLeafPhotosynthesisMod.F90 $(mods)
	$(cmplr) -c ${can}/MLLeafPhotosynthesisMod.F90
	mv MLLeafPhotosynthesisMod.o ${O}

${O}/MLLongwaveRadiationMod.o: ${can}/MLLongwaveRadiationMod.F90 $(mods)
	$(cmplr) -c ${can}/MLLongwaveRadiationMod.F90
	mv MLLongwaveRadiationMod.o ${O}

${O}/MLPlantHydraulicsMod.o: ${can}/MLPlantHydraulicsMod.F90 $(mods)
	$(cmplr) -c ${can}/MLPlantHydraulicsMod.F90
	mv MLPlantHydraulicsMod.o ${O}

${O}/MLSoilFluxesMod.o: ${can}/MLSoilFluxesMod.F90 $(mods)
	$(cmplr) -c ${can}/MLSoilFluxesMod.F90
	mv MLSoilFluxesMod.o ${O}

${O}/MLCanopyWaterMod.o: ${can}/MLCanopyWaterMod.F90 $(mods)
	$(cmplr) -c ${can}/MLCanopyWaterMod.F90
	mv MLCanopyWaterMod.o ${O}

${O}/MLCanopyNitrogenProfileMod.o: ${can}/MLCanopyNitrogenProfileMod.F90 $(mods)
	$(cmplr) -c ${can}/MLCanopyNitrogenProfileMod.F90
	mv MLCanopyNitrogenProfileMod.o ${O}

${O}/MLinitVerticalMod.o: ${can}/MLinitVerticalMod.F90 $(mods)
	$(cmplr) -c ${can}/MLinitVerticalMod.F90
	mv MLinitVerticalMod.o ${O}

# src2 files, which use mod files and src1 files

${O}/initSubgridMod.o: ${main}/initSubgridMod.F90 $(mods)
	$(cmplr) -c ${main}/initSubgridMod.F90
	mv initSubgridMod.o ${O}

${O}/initGridCellsMod.o: ${main}/initGridCellsMod.F90 $(mods) ${O}/initSubgridMod.o
	$(cmplr) -c ${main}/initGridCellsMod.F90
	mv initGridCellsMod.o ${O}

${O}/MLCanopyFluxesMod.o: ${can}/MLCanopyFluxesMod.F90 $(mods) $(src1)
	$(cmplr) -c ${can}/MLCanopyFluxesMod.F90
	mv MLCanopyFluxesMod.o ${O}

${O}/clm_driver.o: ${main}/clm_driver.F90 $(mods) $(src0) $(src1) ${O}/MLCanopyFluxesMod.o
	$(cmplr) -c ${main}/clm_driver.F90
	mv clm_driver.o ${O}

${O}/clm_initializeMod.o: ${main}/clm_initializeMod.F90 $(mods) ${O}/initGridCellsMod.o ${O}/MLCanopyTurbulenceMod.o
	$(cmplr) -c ${main}/clm_initializeMod.F90
	mv clm_initializeMod.o ${O}

${O}/lnd_comp_mct.o: ${cpl}/lnd_comp_mct.F90 $(mods) ${O}/clm_initializeMod.o ${O}/clm_driver.o
	$(cmplr) -c ${cpl}/lnd_comp_mct.F90
	mv lnd_comp_mct.o ${O}

${O}/CLMml_driver.o: ${drv}/CLMml_driver.F90 $(mods) $(src1) ${O}/lnd_comp_mct.o
	$(cmplr) -c ${drv}/CLMml_driver.F90
	mv CLMml_driver.o ${O}

${O}/CLMml.o: ${drv}/CLMml.F90 $(mods) ${O}/CLMml_driver.o
	$(cmplr) -c ${drv}/CLMml.F90
	mv CLMml.o ${O}

# clean up object and mod files

clean:
	rm ${O}/*.o
	rm *.mod
