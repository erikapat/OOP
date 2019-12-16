# Microsoft Developer Studio Project File - Name="proy" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=proy - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "proy.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "proy.mak" CFG="proy - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "proy - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "proy - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "proy - Win32 Release"

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
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x200a /d "NDEBUG"
# ADD RSC /l 0x200a /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "proy - Win32 Debug"

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
# ADD BASE F90 /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x200a /d "_DEBUG"
# ADD RSC /l 0x200a /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "proy - Win32 Release"
# Name "proy - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\conversion.f90
DEP_F90_CONVE=\
	".\Debug\operaciones.mod"\
	

!IF  "$(CFG)" == "proy - Win32 Release"

!ELSEIF  "$(CFG)" == "proy - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"conversion"

# End Source File
# Begin Source File

SOURCE=.\operaciones.f90

!IF  "$(CFG)" == "proy - Win32 Release"

!ELSEIF  "$(CFG)" == "proy - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"operaciones"

# End Source File
# Begin Source File

SOURCE=.\P_principal.f90
DEP_F90_P_PRI=\
	".\Debug\conversion.mod"\
	".\Debug\operaciones.mod"\
	".\Debug\sistemas_lineales.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\sistemas_lineales.f90
DEP_F90_SISTE=\
	".\Debug\operaciones.mod"\
	

!IF  "$(CFG)" == "proy - Win32 Release"

!ELSEIF  "$(CFG)" == "proy - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"sistemas_lineales"

# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\b.txt
# End Source File
# Begin Source File

SOURCE=.\dat.txt
# End Source File
# Begin Source File

SOURCE=.\data..txt
# End Source File
# Begin Source File

SOURCE=.\data.txt
# End Source File
# Begin Source File

SOURCE=.\data2.txt
# End Source File
# Begin Source File

SOURCE=.\dataa.txt
# End Source File
# Begin Source File

SOURCE=.\gauss.txt
# End Source File
# Begin Source File

SOURCE=.\m1.txt
# End Source File
# Begin Source File

SOURCE=.\m2.txt
# End Source File
# Begin Source File

SOURCE=.\n.txt
# End Source File
# End Group
# End Target
# End Project
