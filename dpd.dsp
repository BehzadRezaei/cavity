# Microsoft Developer Studio Project File - Name="dpd" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=dpd - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "dpd.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "dpd.mak" CFG="dpd - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "dpd - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "dpd - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "dpd - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x429 /d "NDEBUG"
# ADD RSC /l 0x429 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "dpd - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /browser /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x429 /d "_DEBUG"
# ADD RSC /l 0x429 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "dpd - Win32 Release"
# Name "dpd - Win32 Debug"
# Begin Source File

SOURCE=.\config_module.f90
NODEP_F90_CONFI=\
	".\Debug\kinds_f90.mod"\
	".\Debug\setup_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\density_module.f90
NODEP_F90_DENSI=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\param_module.mod"\
	".\Debug\setup_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\dpd.f90
NODEP_F90_DPD_F=\
	".\Debug\config_module.mod"\
	".\Debug\density_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\param_module.mod"\
	".\Debug\setup_module.mod"\
	".\Debug\statistics_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\dpd_forces.f90
NODEP_F90_DPD_FO=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\param_module.mod"\
	".\Debug\random_module.mod"\
	".\Debug\setup_module.mod"\
	".\Debug\statistics_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\error.f90
NODEP_F90_ERROR=\
	".\Debug\kinds_f90.mod"\
	".\Debug\setup_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\export_data.f90
NODEP_F90_EXPOR=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\setup_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\external_field_module.f90
NODEP_F90_EXTER=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\setup_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\input_routines.f90
NODEP_F90_INPUT=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\param_module.mod"\
	".\Debug\random_module.mod"\
	".\Debug\setup_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\integrator_routines.f90
NODEP_F90_INTEG=\
	".\Debug\config_module.mod"\
	".\Debug\external_field_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\param_module.mod"\
	".\Debug\setup_module.mod"\
	".\Debug\statistics_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\kinds_f90.f90
# End Source File
# Begin Source File

SOURCE=.\kinetic_routines.f90
NODEP_F90_KINET=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\link_cell_pairs.f90
NODEP_F90_LINK_=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\setup_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\numeric_routines.f90
NODEP_F90_NUMER=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\param_module.mod"\
	".\Debug\setup_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\output_write.f90
NODEP_F90_OUTPU=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\param_module.mod"\
	".\Debug\setup_module.mod"\
	".\Debug\statistics_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\param_module.f90
DEP_F90_PARAM=\
	".\Debug\external_field_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\setup_module.mod"\
	".\Debug\statistics_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\random_module.f90
NODEP_F90_RANDO=\
	".\Debug\kinds_f90.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\rdf.f90
NODEP_F90_RDF_F=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\param_module.mod"\
	".\Debug\setup_module.mod"\
	".\Debug\statistics_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\set_bounds.f90
NODEP_F90_SET_B=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\param_module.mod"\
	".\Debug\setup_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\set_halo_particles.f90
NODEP_F90_SET_H=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\setup_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\setup_module.f90
NODEP_F90_SETUP=\
	".\Debug\kinds_f90.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\statistics.f90
NODEP_F90_STATI=\
	".\Debug\config_module.mod"\
	".\Debug\density_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\param_module.mod"\
	".\Debug\setup_module.mod"\
	".\Debug\statistics_module.mod"\
	".\Debug\viscosity_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\statistics_module.f90
NODEP_F90_STATIS=\
	".\Debug\kinds_f90.mod"\
	".\Debug\setup_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\system_init.f90
NODEP_F90_SYSTE=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\param_module.mod"\
	".\Debug\setup_module.mod"\
	".\Debug\statistics_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\system_restart.f90
NODEP_F90_SYSTEM=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\param_module.mod"\
	".\Debug\setup_module.mod"\
	".\Debug\statistics_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\temperature_routines.f90
NODEP_F90_TEMPE=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\param_module.mod"\
	".\Debug\setup_module.mod"\
	".\Debug\statistics_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\trajectory_write.f90
NODEP_F90_TRAJE=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\setup_module.mod"\
	".\Debug\statistics_module.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\viscosity_module.f90
NODEP_F90_VISCO=\
	".\Debug\config_module.mod"\
	".\Debug\kinds_f90.mod"\
	".\Debug\param_module.mod"\
	".\Debug\setup_module.mod"\
	
# End Source File
# End Target
# End Project
