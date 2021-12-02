

                      GSFLOW - Version: 2.2.1
          Coupled Groundwater and Surface-water FLOW model


Any use of trade, product, or firm names is for descriptive purposes only and
does not imply endorsement by the U.S. Government. This software has been 
approved for release by the U.S. Geological Survey (USGS). Although the 
software has been subjected to rigorous review, the USGS reserves the right to
update the software as needed pursuant to further analysis and review. No 
warranty, expressed or implied, is made by the USGS or the U.S. Government as 
to the functionality of the software and related material nor shall the fact 
of release constitute any such warranty. Furthermore, the software is released 
on condition that neither the USGS nor the U.S. Government shall be held liable 
for any damages resulting from its authorized or unauthorized use.

GSFLOW is packaged for personal computers using one of the Linux or Microsoft 
Windows operating systems. An executable file compiled for 64-bit operating 
systems is provided.  The Linux executable was compiled on a personal computer 
using gfortran and gcc compilers GCC-8.3.0 version. The Windows executable was 
compiled using the Microsoft Visual Studio 2019 Version 16.3.7, development 
environment and the Intel� Parallel Studio XE 2019 Update 5 Composer Edition 
for Fortran Windows* Integration for Microsoft Visual Studio* 2019 and
Microsoft Visual C++ 2019 compilers. Both executables were compiled on a 
personal computer with the Intel(R) Xeon(R) E-2186M CPU, running the Microsoft 
Windows 10 Enterprise, Version 1809, 64-bit operating system.

The source code and Linux Makefiles are provided to aid users in compilation
on other computers. However, no support is provided for compilation.

IMPORTANT: Users should review the file 'GSFLOW_Release_Notes_2.2.1.pdf' for a 
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

The following distribution files are for use on personal computers:

         gsflow_2.2.1.zip for Windows-based personal computers
         gsflow_2.2.1_linux.zip for Linux-based personal computers

The distribution file contains:

          Executable and source code for GSFLOW.
          GSFLOW documentation.
          Related documentation for PRMS and MODFLOW.
          SFLOW example problems.
          An Excel spreadsheet for analysis of GSFLOW water-budget results.

Unzipping the distribution file creates numerous individual files contained in 
several subdirectories. The following directory structure will be created in the 
installation directory:

   |
   |--gsflow_2.2.1
   |    |--bin           ; Compiled GSFLOW executable for personal computers
   |    |--data          ; Example GSFLOW application models
   |    |--doc           ; Documentation reports for GSFLOW and related software
			|--Related reports	; Additional reports and information files
   |    |--src
   |        |--gsflow           ; Source code for GSFLOW Modules
   |        |--mmf              ; Source code for MMF software
   |        |--modflow          ; Source code for MODFLOW Packages
   |        |--prms             ; Source code for PRMS Modules
   |        |--merge			; Source code for program to merge restart files
   |    |--water-budget utility ; Utility program for analysis of GSFLOW output


It is recommended that no user files be kept in the gsflow_2.2.1 directory
structure.  If you do plan to put your own files in the gsflow_2.2.1
directory structure, do so only by creating additional subdirectories of
the "data" subdirectory.

Included with the release are several documents that use the Portable Document 
Format (PDF) file structure. The PDF files are readable and printable on various 
computer platforms using Acrobat Reader from Adobe. The Acrobat Reader is freely 
available from the following World Wide Web site: http://www.adobe.com/


B. INSTALLING

To make the executable version of GSFLOW accessible from any directory, the 
directory containing the executable (gsflow_2.2.1\bin on Linux-based computers  
or gsflow_2.2.1\bin on Windows-based computers) should be included in the PATH
environment variable. Also, if a prior release of GSFLOW is installed on your 
system, the directory containing the executable for the prior release should be 
removed from the PATH environment variable.

As an alternative, the executable file in the "bin" subdirectory can be copied 
into a directory already included in the PATH environment variable. Note, the 
example problems provided with the release (described below) have example Linux 
shell script or Windows batch files that require the executable be in the "bin" 
subdirectory.


C. EXECUTING THE SOFTWARE

A 64-bit Windows or 32-bit Linux executable is provided in the "bin" 
subdirectory. After the "bin" subdirectory is included in your PATH, 
GSFLOW is initiated in a Command-Prompt window using the command:

      gsflow [Fname]

The optional Fname argument is the name of the GSFLOW Control File.  If no 
argument is used, then GSFLOW will look for a Control File named "control" 
in the user's current directory.

The arrays in GSFLOW are dynamically allocated, so models are not limited
by the size of input data. However, it is best to have at least 4 MB of 
random-access memory (RAM) for model execution and more RAM for large models.
If there is less available RAM than the model requires, which depends
on the size of the application, the program will use virtual memory; however,
this can slow execution significantly. If there is insufficient memory to 
run the model, then GSFLOW will not initiate the beginning of the simulation; 
however, if on a Windows-based computer, the Command-Prompt window may continue 
to indicate that GSFLOW is executing. For this circumstance, the program must be 
terminated manually using the Windows Task Manager application.

D. INITIAL CONDITIONS FILES

Some of the files written by GSFLOW are binary files. The format of these files
is based on compiling the source with the binary file type specified as
"UNFORMATTED". Any program that reads binary files produced by GSFLOW must be 
compiled with a compiler that produces programs that use the same binary file 
structure. For example, the Zonebudget and Modpath software use binary budget 
files produced by the MODFLOW component of GSFLOW. Another example are head files 
that are generated by one GSFLOW simulation and used in a following simulation as 
initial heads. Both simulations must be run using an executable version of GSFLOW 
that uses the same binary file structure. It is possible that binary files written
by previous versions of GSFLOW are not compatible with the current version, thus
it is recommended those files are made compatible with the current version. 


E. TESTING

Sample problems with GSFLOW data sets are provided in the "data" subdirectory
to verify that GSFLOW is correctly installed and running on the user's system.
The sample problems also may be looked at as examples of how to use the program. 
See the 'Readme.txt' file in that subdirectory for a description of the 
sample problems.


F. COMPILING

The executable file provided in distribution was created using compilers as 
described above. Although executable versions of the program are provided, the 
source code also is provided in the "src" subdirectory so that GSFLOW can be 
recompiled if necessary.  However, the USGS cannot provide assistance to those 
compiling GSFLOW. In general, the requirements are a Fortran compiler, a 
compatible C compiler, and the knowledge of using the compilers. Makefiles are 
included in the "src" subdirectories as an example for compiling GSFLOW.

