set PATH=%PATH%;C:\Program Files (x86)\mingw-w64\i686-8.1.0-posix-dwarf-rt_v6-rev0\mingw32\bin
gfortran --version
gcc --version
:: C:\Users\rniswon\AppData\Local\Continuum\miniconda3\envs\py39\python.exe make_gfortran.py -fc gfortran -cc gcc -sd -mc -e ..\GSFLOW\src gsflow.exe
make_gfortran.py -fc gfortran -cc gcc -sd -mc ..\GSFLOW\src gsflow.exe
pause