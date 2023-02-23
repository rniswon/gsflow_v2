set PATH=%PATH%;C:\Program Files (x86)\mingw64\bin
gfortran --version
gcc --version
C:\Users\rniswon\Anaconda3\envs\py39\python.exe make_gfortran.py -fc gfortran -cc gcc -sd -mc -e ..\GSFLOW\src gsflow.exe
make_gfortran.py -fc gfortran -cc gcc -sd -mc ..\GSFLOW\src gsflow.exe
pause