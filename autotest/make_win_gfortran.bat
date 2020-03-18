set PATH=%PATH%;C:\MinGW\bin
gfortran --version
gcc --version
:: C:\Users\rniswon\AppData\Local\Continuum\miniconda3\envs\gsflow\python.exe make_gfortran.py -fc gfortran -cc gcc -sd -mc -e ..\GSFLOW\src gsflow.exe
make_gfortran.py -fc gfortran -cc gcc -sd -mc ..\GSFLOW\src gsflow.exe
pause