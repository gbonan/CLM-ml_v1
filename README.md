# CLM-ml_v1

This file describes some details for the multilayer canopy model
(CLM-ml.v1) and its use in offline (uncoupled) simulations.

This is the code described in:

Bonan, G.B., E.G. Patton, J.J. Finnigan, D.D. Baldocchi, and I.N. Harman. 2021. Moving beyond
the incorrect but useful paradigm: reevaluating big-leaf and multilayer plant canopies to model
biosphere-atmosphere fluxes – a review. Agricultural and Forest Meteorology, in press.

1. Directory structure

   a. The main multilayer canopy model code is in the directory:

      o multilayer_canopy

      Model variables are defined in MLCanopyFluxesType.F90
      Model physical constants are defined in MLclm_varcon.F90
      Model parameters (array dimensions) are defined in MLclm_varpar.F90
      Model run control variables are defined in MLclm_varctl.F90

      This code needs to be moved to the CLM5 mods directory when coupled to CLM5

   b. There are two directories for the uncoupled simulations:

      o offline_driver     = driver code for the offline case. The main driver is CLMml.F90
      o offline_executable = directory containing the makefile, executable, and namelists

      The offline code (uncoupled to CLM5) has a namelist file read
      in offline_driver/controlMod.F90. This namelist file sets the
      tower site, the year, and the month of the simulation. The
      code controlMod.F90 also sets the directories for the tower
      forcing files, the clm history files, and the output files.

   c. The following directories are dummy CLM5 stub code only used in the offline case:

      o cime_src_share_util = dummy CLM5 stub code from: cime/src/share/util
      o clm_src_biogeophys  = dummy CLM5 stub code from: components/clm/src/biogeophys
      o clm_src_cpl         = dummy CLM5 stub code from: components/clm/src/utils
      o clm_src_main        = dummy CLM5 stub code from: components/clm/src/main
      o clm_src_utils       = dummy CLM5 stub code from: components/clm/src/utils

      When coupled to CLM5, the CLM5 code is used instead. Some files need to be modified
      to couple with the multilayer canopy.

   d. The code to create the RSL psihat look-up file is in the directory:

      o rsl_lookup_tables

2. Run the model for a particular tower site.

   Input files are provided to run the model (offline) for the US-UMB tower site for July 2006.
   From offline_executable, use the command:

   ./prgm.exe < nl.US-UMB.2006
   
   Input files are read from the input_files directory
   Model output is written to the output_files_directory
