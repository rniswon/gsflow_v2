

                      GSFLOW - Version: 1.2.2
          Coupled Groundwater and Surface-water FLOW model


NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

GSFLOW version 1.2.2 is packaged for personal computers using one of the 
Microsoft Windows operating systems. An executable file compiled for 64-bit
operating systems is provided. The executable was compiled on a personal
computer with the Intel(R) Xeon(R) CPU E3-1545M v5 Processor, running the
Microsoft Windows 8.1 Enterprise, 64-bit operating system, using the 
Microsoft Visual Studio 2015 Version 14.0.25425.01 Update 3, development environment 
and the Intel® Parallel Studio XE 2017 Composer Edition for Fortran Windows* 
Integration for Microsoft Visual Studio* 2015, Version 17.0.0047.14 
and Intel® Parallel Studio XE 2017 Update 2 Composer Edition for C++ Windows* 
Integration for Microsoft Visual Studio* 2015, Version 17.0.72.14 compilers.

The source code and Linux Makefiles are provided to aid users in compilation
on other computers. However, no support is provided for compilation.

IMPORTANT: Users should review the file 'GSFLOW_Release_Notes.pdf' for a 
description of, and references for, this software. Changes that have been 
introduced into GSFLOW with each official release also are described in this 
file; these changes may substantially affect users.

Instructions for installation, execution, and testing of this version of
GSFLOW are provided below.



                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING THE SOFTWARE
                         D. TESTING
                         E. COMPILING


A. DISTRIBUTION FILE

The following distribution file is for use on personal computers:

         gsflowv1_2_2.zip

The distribution file contains:

          Executable and source code for GSFLOW.
          GSFLOW documentation.
          Related documentation for PRMS, MODFLOW, and MODFLOW-NWT.
          Three GSFLOW example problems.
          An Excel spreadsheet for analysis of GSFLOW water-budget results.

The distribution file is for use on personal computers running Windows 
operating systems.  Unzipping the distribution file creates numerous 
individual files contained in several directories. The following 
directory structure will be created in the installation directory:

   |
   |--GSFLOW_1.2.2
   |    |--bin           ; Compiled GSFLOW executable for personal computers
   |    |--data          ; Three example GSFLOW application models described
                            in USGS reports TM6-D1 and TM6-D3.
   |    |--doc           ; Documentation reports for GSFLOW and related
                            software.
   |    |--src           
   |        |--gsflow           ; Source code for GSFLOW Modules
   |        |--mmf              ; Source code for MMF software
   |        |--modflow          ; Source code for MODFLOW-2005 and MODFLOW-NWT 
                                  Packages
   |        |--prms             ; Source code for PRMS Modules
   |    |--water-budget utility ; Utility program for analysis of GSFLOW output


It is recommended that no user files be kept in the GSFLOW_1.2.2 directory
structure.  If you do plan to put your own files in the GSFLOW_1.2.2
directory structure, do so only by creating additional subdirectories of
the GSFLOW_1.2.2\data subdirectory.

Included with the release are several documents that use the Portable Document 
Format (PDF) file structure. The PDF files are readable and printable on various 
computer platforms using Acrobat Reader from Adobe. The Acrobat Reader is freely 
available from the following World Wide Web site: http://www.adobe.com/


B. INSTALLING

To make the executable version of GSFLOW accessible from any directory, the 
directory containing the executable (GSFLOW_1.2.2\bin) should be included in the 
PATH environment variable. Also, if a prior release of GSFLOW is installed on your 
system, the directory containing the executable for the prior release should be 
removed from the PATH environment variable.
  
As an alternative, the executable file in the GSFLOW_1.2.2\bin directory 
can be copied into a directory already included in the PATH environment 
variable. The sample problems provided with the release (described below)
have sample batch files that provide an alternative, additional approach for
accessing the executable files.


C. EXECUTING THE SOFTWARE

A 64-bit (gsflow.exe) executable is provided in the GSFLOW_1.2.2\bin directory. 
After the GSFLOW_1.2.2\bin directory is included in your PATH, GSFLOW is 
initiated in a Windows Command-Prompt window using the command:

      gsflow.exe [Fname]

The optional Fname argument is the name of the GSFLOW Control File.  If 
no argument is used, then GSFLOW will look for a Control File named 
"control" in the user’s current directory.

The arrays in GSFLOW are dynamically allocated, so models are not limited
by the size of input data. However, it is best to have at least 4 MB of 
random-access memory (RAM) for model execution and more RAM for large models.
If there is less available RAM than the model requires, which depends
on the size of the application, the program will use virtual memory; however,
this can slow execution significantly. If there is insufficient memory to 
run the model, then GSFLOW will not initiate the beginning of the simulation; 
however, the Windows Command-Prompt window may continue to indicate that 
GSFLOW is executing. For this circumstance, the program must be terminated 
manually using the Windows Task Manager application.

Some of the files written by GSFLOW are unformatted files. The structure
of these files depends on the compiler and options in the code. For Windows
based computers, GSFLOW is compiled with the unformatted file type specified
as "BINARY". Any program that reads the unformatted files produced by GSFLOW 
must be compiled with a compiler that produces programs that use the same 
structure for unformatted files.  For example, Zonebudget and Modpath use 
unformatted budget files produced by the MODFLOW component of GSFLOW. Another 
example are head files that are generated by one GSFLOW simulation and used 
in a following simulation as initial heads. Both simulations must be run 
using an executable version of GSFLOW that uses the same unformatted file 
structure.


D. TESTING

Three sample problems with GSFLOW data sets are provided in the 'data' sub-
directory to verify that GSFLOW is correctly installed and running on the 
user's system. The sample problems also may be looked at as examples of how 
to use the program. See the 'Readme.txt' file in that subdirectory for a 
description of the three sample problems.


E. COMPILING

The executable file provided in GSFLOW_1.2.2\bin was created using the Intel
Visual Fortran and C++ compilers.  Although executable versions of the program
are provided, the source code also is provided in the GSFLOW_1.2.2\src 
directory so that GSFLOW can be recompiled if necessary.  However, the USGS
cannot provide assistance to those compiling GSFLOW. In general, the 
requirements are a Fortran compiler, a compatible C compiler, and the knowledge
of using the compilers. Makefiles are included in the GSFLOW_1.2.2\src 
directories as an example for compiling GSFLOW.
