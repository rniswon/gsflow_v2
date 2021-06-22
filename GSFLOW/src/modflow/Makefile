# mfnwt libraray makefile

include ../makelist

MFNWTLIB = $(LIBDIR)/libmfnwt.a

LIBOBJS = \
        utl7.o \
        mach_mod.o \
        NWT1_module.o \
        gwf2bas7_NWT.o \
        pcg7_NWT.o \
        sip7_NWT.o \
        de47_NWT.o \
        gwf2bcf7.o \
        gwf2lpf7.o \
        gwf2huf7.o \
        gwf2drn7_NWT.o \
        gwf2drt7.o \
        gwf2riv7_NWT.o \
        gwf2res7.o \
        gwf2sub7_NWT.o \
        gwf2str7.o \
        gwf2ets7.o \
        gwf2evt7.o \
        gwf2rch7.o \
        obs2bas7.o \
        gwf2ibs7.o \
        hufutl7.o \
        gwf2gag7.o \
        gwf2upw1.o \
        gwfuzfmodule_NWT.o \
        gwfsfrmodule_NWT.o \
        gwflakmodule_NWT.o \
        gwf2lak7_NWT.o \
        gwf2sfr7_NWT.o \
        gwf2ag1_NWT.o \
        gwf2wel7_NWT.o \
        gwf2uzf1_NWT.o \
        gwf2ghb7_NWT.o \
        gwf2fhb7.o \
        gwf2mnw17_NWT.o \
        gwf2mnw27_NWT.o \
        gwf2mnw2i7.o \
        NWT1_ilupc_mod.o \
        NWT1_xmdlib.o \
        NWT1_xmd.o \
        NWT1_gmres.o \
        NWT1_solver.o \
        gwf2chd7.o \
        gwf2hfb7_NWT.o \
        gwf2swi27.o \
        gwf2swt7.o \
        gwf2swr7.o \
        gwf2swr7util.o \
        lmt8_NWT.o \
        parutl7.o \
        gwf2hydmod7.o \
        obs2chd7.o \
        obs2ghb7.o \
        obs2riv7.o \
        obs2str7.o \
        obs2drn7.o \
        Irestart.o

all: lib
	echo made $(MFNWTLIB)

install: lib

lib: $(MFNWTLIB)

$(MFNWTLIB): $(LIBOBJS)
	$(RM) $(MFNWTLIB)
	$(AR) $(MFNWTLIB) $(LIBOBJS)
	$(RANLIB) $(MFNWTLIB)

clean:
	$(RM) *.o *.mod *~ $(MFNWTLIB)

# on Windows machines comment out next 3 lines
#openspec.inc:
#	$(RM) openspec.inc
#	$(CP) openspec_linux.inc openspec.inc

modules.o: modules.f90 mach_mod.o
	$(FC) $(FFLAGS) -c modules.f90

mach_mod.o: mach_mod.f90
	$(FC) $(FFLAGS) -c mach_mod.f90

NWT1_gmres.o: NWT1_gmres.f90
	$(FC) $(FFLAGS) -c NWT1_gmres.f90

NWT1_ilupc_mod.o: NWT1_ilupc_mod.f90
	$(FC) $(FFLAGS) -c NWT1_ilupc_mod.f90

NWT1_module.o: NWT1_module.f mach_mod.o
	$(FC) $(FFLAGS) -c NWT1_module.f

NWT1_xmd.o: NWT1_xmd.f NWT1_xmdlib.o gwf2bas7_NWT.o NWT1_module.o
	$(FC) $(FFLAGS) -c NWT1_xmd.f

NWT1_xmdlib.o: NWT1_xmdlib.f
	$(FC) $(FFLAGS) -c NWT1_xmdlib.f

NWT1_solver.o: NWT1_solver.f gwf2bas7_NWT.o NWT1_module.o gwf2upw1.o NWT1_xmd.o NWT1_gmres.o NWT1_ilupc_mod.o mach_mod.o
	$(FC) $(FFLAGS) -c NWT1_solver.f

utl7.o: utl7.f openspec.inc
	$(FC) $(FFLAGS) -c utl7.f

gwf2bas7_NWT.o: gwf2bas7_NWT.f openspec.inc
	$(FC) $(FFLAGS) -c gwf2bas7_NWT.f

gwf2bcf7.o: gwf2bcf7.f gwf2bas7_NWT.o
	$(FC) $(FFLAGS) -c gwf2bcf7.f

hufutl7.o: hufutl7.f gwf2bas7_NWT.o gwf2huf7.o
	$(FC) $(FFLAGS) -c hufutl7.f

gwf2upw7.o: gwf2upw1.f gwf2bas7_NWT.o NWT1_module.o
	$(FC) $(FFLAGS) -c gwf2upw1.f

gwf2wel7.o: gwf2wel7.f gwf2bas7_NWT.o NWT1_module.o gwf2upw1.o
	$(FC) $(FFLAGS) -c gwf2wel7.f
	
gwf2ag1_NWT.o: gwf2ag1_NWT.f gwf2bas7_NWT.o NWT1_module.o gwf2upw1.o prms_module.mod prms_basin.mod prms_flowvars.mod prms_climatevars.mod gsfmodflow.mod prms_soilzone.mod mach_mod.o
	$(FC) $(FFLAGS) -c gwf2ag1_NWT.f
	
gwf2sfr7_NWT.o: gwf2sfr7_NWT.f gwf2bas7_NWT.o gwfsfrmodule_NWT.o gwf2lpf7.o gwf2bcf7.o gwf2huf7.o gwf2upw1.o gwfuzfmodule_NWT.o NWT1_module.o gwflakmodule_NWT.o
	$(FC) $(FFLAGS) -c gwf2sfr7_NWT.f

gwf2uzf1_NWT.o: gwf2uzf1_NWT.f gwf2bas7_NWT.o gwfsfrmodule_NWT.o gwf2lpf7.o gwf2bcf7.o gwf2huf7.o gwf2upw1.o gwfuzfmodule_NWT.o NWT1_module.o gwflakmodule_NWT.o
	$(FC) $(FFLAGS) -c gwf2uzf1_NWT.f

gwf2mnw17_NWT.o: gwf2mnw17_NWT.f gwf2bas7_NWT.o sip7_NWT.o de47_NWT.o pcg7_NWT.o NWT1_module.o gwf2bcf7.o gwf2lpf7.o gwf2huf7.o gwf2upw1.o
	$(FC) $(FFLAGS) -c gwf2mnw17_NWT.f

gwf2mnw27_NWT.o: gwf2mnw27_NWT.f gwf2bas7_NWT.o sip7_NWT.o de47_NWT.o pcg7_NWT.o NWT1_module.o gwf2bcf7.o gwf2lpf7.o gwf2huf7.o gwf2upw1.o
	$(FC) $(FFLAGS) -c gwf2mnw27_NWT.f

gwf2mnw2i7.o: gwf2mnw2i7.f gwf2bas7_NWT.o gwf2mnw27_NWT.o
	$(FC) $(FFLAGS) -c gwf2mnw2i7.f

gwf2chd7.o: gwf2chd7.f gwf2bas7_NWT.o
	$(FC) $(FFLAGS) -c gwf2chd7.f

gwf2fbh7.o: gwf2fhb7.f gwf2bas7_NWT.o
	$(FC) $(FFLAGS) -c gwf2fhb7.f

gwf2gag7.o: gwf2gag7.f gwf2bas7_NWT.o gwflakmodule_NWT.o gwf2sfr7_NWT.o
	$(FC) $(FFLAGS) -c gwf2gag7.f

gwf2ghb7_NWT.o: gwf2ghb7_NWT.f gwf2bas7_NWT.o
	$(FC) $(FFLAGS) -c gwf2ghb7_NWT.f

gwf2lpf7.o: gwf2lpf7.f gwf2bas7_NWT.o
	$(FC) $(FFLAGS) -c gwf2lpf7.f

gwf2hfb7_NWT.o: gwf2hfb7_NWT.f openspec.inc gwf2bas7_NWT.o
	$(FC) $(FFLAGS) -c gwf2hfb7_NWT.f

pcg7_NWT.o: pcg7_NWT.f gwf2bas7_NWT.o
	$(FC) $(FFLAGS) -c pcg7_NWT.f

sip7_NWT.o: sip7_NWT.f gwf2bas7_NWT.o
	$(FC) $(FFLAGS) -c sip7_NWT.f

Irestart.o: Irestart.f gwfuzfmodule_NWT.o gwfsfrmodule_NWT.o gsfmodflow.mod
	$(FC) $(FFLAGS) -c Irestart.f

prms_module.mod:
	$(CD) ../gsflow;make prms_module.mod
	$(CP) ../gsflow/prms_module.mod .
	
gsfmodflow.mod:
	$(CD) ../gsflow;make gsfmodflow.mod
	$(CP) ../gsflow/gsfmodflow.mod .
	
prms_basin.mod:
	$(CD) ../prms;make prms_basin.mod
	$(CP) ../prms/prms_basin.mod .

prms_flowvars.mod:
	$(CD) ../prms;make prms_flowvars.mod
	$(CP) ../prms/prms_flowvars.mod .

prms_climatevars.mod:
	$(CD) ../prms;make prms_climatevars.mod
	$(CP) ../prms/prms_climatevars.mod .

prms_soilzone.mod:
	$(CD) ../prms;make prms_soilzone.mod
	$(CP) ../prms/prms_soilzone.mod .

de47_NWT.o: de47_NWT.f gwf2bas7_NWT.o
	$(FC) $(FFLAGS) -c de47_NWT.f

parutl7.o: parutl7.f gwf2bas7_NWT.o
	$(FC) $(FFLAGS) -c parutl7.f

obs2bas7.o: obs2bas7.f gwf2bas7_NWT.o
	$(FC) $(FFLAGS) -c obs2bas7.f

obs2chd7.o: obs2chd7.f gwf2bas7_NWT.o
	$(FC) $(FFLAGS) -c obs2chd7.f

obs2drn7.o: obs2drn7.f gwf2bas7_NWT.o
	$(FC) $(FFLAGS) -c obs2drn7.f

gwf2lak7_NWT.o: gwf2lak7_NWT.f gwflakmodule_NWT.o gwf2bas7_NWT.o gwfsfrmodule_NWT.o gwfuzfmodule_NWT.o
	$(FC) $(FFLAGS) -c gwf2lak7_NWT.f

obs2ghb7.o: obs2ghb7.f gwf2bas7_NWT.o gwf2ghb7_NWT.o
	$(FC) $(FFLAGS) -c obs2ghb7.f

gwfsfrmodule_NWT.o: gwfsfrmodule_NWT.f
	$(FC) $(FFLAGS) -c gwfsfrmodule_NWT.f

gwfuzfmodule_NWT.o: gwfuzfmodule_NWT.f
	$(FC) $(FFLAGS) -c gwfuzfmodule_NWT.f

gwflakmodule_NWT.o: gwflakmodule_NWT.f
	$(FC) $(FFLAGS) -c gwflakmodule_NWT.f

global.mod: gwf2bas7_NWT.o
	$(FC) $(FFLAGS) -c gwf2bas7_NWT.f

pcgmodule.mod: pcg7_NWT.o
	$(FC) $(FFLAGS) -c pcg7_NWT.f

lmt8_NWT.o: lmt8_NWT.f gwf2bas7_NWT.o gwfsfrmodule_NWT.o gwfuzfmodule_NWT.o gwflakmodule_NWT.o gwf2bcf7.o
	$(FC) $(FFLAGS) -c lmt8_NWT.f

gwf2swi27.o: gwf2swi27.f gwf2bas7_NWT.o gwf2bcf7.o gwf2upw1.o gwf2lpf7.o
	$(FC) $(FFLAGS) -c gwf2swi27.f

gwf2swt7.o: gwf2swt7.f gwf2bas7_NWT.o
	$(FC) $(FFLAGS) -c gwf2swt7.f

gwf2swr7.o: gwf2swr7.f gwf2bas7_NWT.o
	$(FC) $(FFLAGS) -c gwf2swr7.f

gwf2swr7util.o: gwf2swr7util.f
	$(FC) $(FFLAGS) -c gwf2swr7util.f

#  end
