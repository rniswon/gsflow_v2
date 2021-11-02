import pymake

 

pmobj = pymake.Pymake(verbose=True)

pmobj.target = "gsflow"

pmobj.appdir = "../bin"

pmobj.srcdir = "./"

pmobj.include_subdirs = True

pmobj.inplace = True

pmobj.makeclean = True

pmobj.build()