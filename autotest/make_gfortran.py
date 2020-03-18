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
    copytree(srcdir, "./temp")
    os.remove(os.path.join('temp', 'merge', "CSV_merge.f90"))
    if platform.system().lower() == "windows":
        os.remove(os.path.join('temp','prms', 'utils_prms_linux.f90'))
    else:
        os.remove(os.path.join('temp', 'prms', 'utils_prms.f90'))
    return tempdir


if __name__ == "__main__":

    args = pymake.pymake.parser()

    srcdir = args.srcdir
    srcdir = cleanup(srcdir, "./temp")

    optlevel = "-O -Bstatic"
    fflags = optlevel  + " -fno-second-underscore"
    if platform.system().lower() == 'windows':
        cflags = optlevel + " -D WINDOWS -Wall"
    else:
        cflags = optlevel + " -D LINUX -Wall"
    syslibs = ["-lgfortran", "-lgcc", "-lm"]
    args.double = False
    args.makefile = False

    exclude_files = []
    # exclude_files = [os.path.join('temp', 'merge', "CSV_merge.f90")]
    # if platform.system().lower() == "windows":
    #     exclude_files.append(os.path.join('temp','prms',
    #                                       'utils_prms_linux.f90'))
    # else:
    #     exclude_files.append(os.path.join('temp', 'prms', 'utils_prms.f90'))

    pymake.pymake.main(srcdir, args.target, args.fc, args.cc, args.makeclean,
                       args.expedite, args.dryrun, args.double, args.debug,
                       args.subdirs, fflags, cflags, syslibs=syslibs,
                       arch=args.arch, makefile=args.makefile,
                       excludefiles=exclude_files)

    shutil.rmtree(srcdir)
