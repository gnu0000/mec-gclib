# Microsoft Developer Studio Project File - Name="gclib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=gclib - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "gclib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "gclib.mak" CFG="gclib - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "gclib - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "gclib - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "gclib - Win32 Release"

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
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
# Begin Special Build Tool
OutDir=.\Release
ProjDir=.
SOURCE="$(InputPath)"
PostBuild_Cmds=if  not  defined  ITIINC  echo  You  need  to  set  the  ITIINC  environment  variable!  	if  not  defined  ITILIB  echo  You  need  to  set  the  ITILIB  environment  variable!  	if  defined  ITIINC  copy  $(ProjDir)\GCLib.h  %ITIINC%\  	if  defined  ITILIB  copy  $(OutDir)\*.lib  %ITILIB%\ 
# End Special Build Tool

!ELSEIF  "$(CFG)" == "gclib - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 1
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /D "_AS_LIB" /FR /YX"stdafx.h" /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"Debug\gclib_debug.lib"
# Begin Special Build Tool
OutDir=.\Debug
ProjDir=.
SOURCE="$(InputPath)"
PostBuild_Cmds=if  not  defined  ITIINC  echo  You  need  to  set  the  ITIINC  environment  variable!  	if  not  defined  ITILIB  echo  You  need  to  set  the  ITILIB  environment  variable!  	if  defined  ITIINC  copy  $(ProjDir)\GCLib.h  %ITIINC%\  	if  defined  ITILIB  copy  $(OutDir)\*.lib  %ITILIB%\ 
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "gclib - Win32 Release"
# Name "gclib - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\CHECK.cpp
# End Source File
# Begin Source File

SOURCE=.\CODMAINT.cpp
# End Source File
# Begin Source File

SOURCE=.\Compile.cpp
# End Source File
# Begin Source File

SOURCE=.\DEFINE.cpp
# End Source File
# Begin Source File

SOURCE=.\dis.cpp
# End Source File
# Begin Source File

SOURCE=.\ERROR.cpp
# End Source File
# Begin Source File

SOURCE=.\Extern.cpp
# End Source File
# Begin Source File

SOURCE=.\Gen.cpp
# End Source File
# Begin Source File

SOURCE=.\GENEXPR.cpp
# End Source File
# Begin Source File

SOURCE=.\Genfn.cpp
# End Source File
# Begin Source File

SOURCE=.\GENGLOB.cpp
# End Source File
# Begin Source File

SOURCE=.\Genop.cpp
# End Source File
# Begin Source File

SOURCE=.\INTERNAL.cpp
# End Source File
# Begin Source File

SOURCE=.\KEEP.cpp
# End Source File
# Begin Source File

SOURCE=.\Label.cpp
# End Source File
# Begin Source File

SOURCE=.\LOADGX.cpp
# End Source File
# Begin Source File

SOURCE=.\MEM.cpp
# End Source File
# Begin Source File

SOURCE=.\NODE.cpp
# End Source File
# Begin Source File

SOURCE=.\OPT.cpp
# End Source File
# Begin Source File

SOURCE=.\PARSE.cpp
# End Source File
# Begin Source File

SOURCE=.\StdAfx.cpp
# End Source File
# Begin Source File

SOURCE=.\SYMBOL.cpp
# End Source File
# Begin Source File

SOURCE=.\Tokenize.cpp
# End Source File
# Begin Source File

SOURCE=.\TYPE.cpp
# End Source File
# Begin Source File

SOURCE=.\Vm.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\Binfile.h
# End Source File
# Begin Source File

SOURCE=.\Check.h
# End Source File
# Begin Source File

SOURCE=.\Codmaint.h
# End Source File
# Begin Source File

SOURCE=.\Define.h
# End Source File
# Begin Source File

SOURCE=.\dis.h
# End Source File
# Begin Source File

SOURCE=.\Error.h
# End Source File
# Begin Source File

SOURCE=.\Extern.h
# End Source File
# Begin Source File

SOURCE=.\Gc.h
# End Source File
# Begin Source File

SOURCE=.\Gclib.h
# End Source File
# Begin Source File

SOURCE=.\Gen.h
# End Source File
# Begin Source File

SOURCE=.\Genexpr.h
# End Source File
# Begin Source File

SOURCE=.\Genfn.h
# End Source File
# Begin Source File

SOURCE=.\Genglob.h
# End Source File
# Begin Source File

SOURCE=.\Genop.h
# End Source File
# Begin Source File

SOURCE=.\Internal.h
# End Source File
# Begin Source File

SOURCE=.\Label.h
# End Source File
# Begin Source File

SOURCE=.\Mem.h
# End Source File
# Begin Source File

SOURCE=.\Node.h
# End Source File
# Begin Source File

SOURCE=.\Opcodes.h
# End Source File
# Begin Source File

SOURCE=.\Opt.h
# End Source File
# Begin Source File

SOURCE=.\Parse.h
# End Source File
# Begin Source File

SOURCE=.\StdAfx.h
# End Source File
# Begin Source File

SOURCE=.\Symbol.h
# End Source File
# Begin Source File

SOURCE=.\Tokenize.h
# End Source File
# Begin Source File

SOURCE=.\Type.h
# End Source File
# Begin Source File

SOURCE=.\Vm.h
# End Source File
# End Group
# End Target
# End Project
