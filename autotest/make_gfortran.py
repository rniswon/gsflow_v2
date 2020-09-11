#! /usr/bin/env python
try:
    import pymake
except:
    msg =  'Error. Pymake package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install https://github.com/modflowpy/pymake/zipball/master'
    print(msg)
    raise Exception()
import os
import shutil
import platform


def copytree(src, dst, symlinks=False, ignore=None):
    for item in os.listdir(src):
        s = os.path.join(src, item)
        d = os.path.join(dst, item)
        if os.path.isdir(s):
            shutil.copytree(s, d, symlinks, ignore)
        else:
            shutil.copy2(s, d)


def cleanup(srcdir, tempdir):
    """
    Method to copy source code and cleanup the gsflow code base
    for gfortran compilation.
    """
    if os.path.isdir(tempdir):
        # shutil.rmtree(tempdir)
        return tempdir
    os.makedirs(tempdir)
    copytree(srcdir, "./stemp")
    os.remove(os.path.join('stemp', 'merge', "CSV_merge.f90"))

    try:
        if platform.system().lower() == "windows":
            os.remove(os.path.join('stemp', 'prms', 'utils_prms_linux.f90'))
        else:
            os.remove(os.path.join('stemp', 'prms', 'utils_prms.f90'))
    except:
        pass
    
    return tempdir


if __name__ == "__main__":
    debug = False
    if debug:
        srcdir = os.path.join("..", "GSFLOW", "src")
        target = "gsflow.exe"
        fc = "gfortran"
        cc = "gcc"
        sd = True
        double = False
        makefile = False
        debug = False
        expedite = False
        makeclean = True
        dryrun = False
        arch = "intel64"
    else:
        args = pymake.parser()
        srcdir = args.srcdir
        target = args.target
        fc = args.fc
        cc = args.cc
        sd = args.subdirs
        double = False
        makefile = False
        debug = args.debug
        expedite = args.expedite
        makeclean = args.makeclean
        dryrun = args.dryrun
        arch = args.arch

    srcdir = cleanup(srcdir, "./stemp")

    optlevel = "-O -Bstatic"
    fflags = optlevel  + " -fno-second-underscore" + " -ffree-line-length-512"
    if platform.system().lower() == 'windows':
        cflags = optlevel + " -DWINDOWS -Wall"
    else:
        cflags = optlevel + " -DLINUX -Wall"
    syslibs = ["-lgfortran", "-lgcc", "-lm"]

    exclude_files = []

    try:
        pymake.main(srcdir, target, fc, cc, makeclean,
                    expedite, dryrun, double, debug,
                    sd, fflags, cflags, syslibs=syslibs,
                    arch=arch, makefile=makefile,
                    excludefiles=exclude_files)
    except AttributeError:
        pymake.main(srcdir, target, fc, cc, makeclean,
                    expedite, dryrun, double, debug,
                    sd, fflags, cflags, syslibs=syslibs,
                    arch=arch, makefile=makefile,
                    excludefiles=exclude_files)

    shutil.rmtree(srcdir)
