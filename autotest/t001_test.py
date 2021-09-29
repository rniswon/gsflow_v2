import os
import platform
import shutil
import flopy as fp
import gsflow


print(os.getcwd())

gsflow_exe_name = "gsflow"
if platform.system().lower() == "windows":
    gsflow_exe_name = "gsflow.exe"

gsflow_exe = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                          gsflow_exe_name)

isgsflow = fp.which(gsflow_exe)

data_dir = os.path.join("..", "GSFLOW", "data")
out_dir = os.path.join(".", "temp")

# relative model path with regard to data directory
# add new models here to the test scenarios!
model_name = [os.path.join("Ag_EP1a", "gsflow_modflow_high.control"),
              os.path.join("Ag_EP1a", "gsflow_modflow_low.control"),
              os.path.join("Ag_EP1b", "gsflow_modflow_high.control"),
              os.path.join("Ag_EP1b", "gsflow_modflow_low.control"),
              os.path.join("Ag_EP2a", "windows", "gsflow_gsflowHighKc.control"),
              os.path.join("Ag_EP2a", "windows", "gsflow_gsflowLowKc.control"),
              os.path.join("Ag_EP2b", "windows", "gsflow_gsflow_HighTrig.control"),
              os.path.join("Ag_EP2b", "windows", "gsflow_gsflow_LowTrig.control"),
              os.path.join("sagehen", "windows", "gsflow.control"),
              os.path.join("tahoe_restart", "windows", "Tahoe.control"),
              os.path.join("Ex_prob1a", "gsflow_modflow_1a.control"),
              os.path.join("Ex_prob1b", "gsflow_modflow_1b.control"),
              os.path.join("Ex_prob2", "gsflow_modflow_2.control"),
              os.path.join("Ex_prob3", "gsflow_modflow_3.control"),
              os.path.join("SFR_LAK_floodplain", "gsflow_modflow_fp.control"),
              os.path.join("Sfr2weltab", "gsflow_modflow_wt.control"),
              os.path.join("UZF_cap_ET", "gsflow_modflow_cap.control"),
              os.path.join("UZF_testproblem2", "gsflow_modflow_uzfp2.control")]


models = [os.path.join(data_dir, model) for model in model_name]

has_external = {"gsflow_modflow_high.control":
                    (os.path.join("input", "seg1_high.tab"),
                     os.path.join("input", "seg1.tab"),
                     os.path.join("input", "seg9.tab"),
                     os.path.join("input", "Agwater1.uzf"),
                     os.path.join("input", "Agwater1.ag")),
                "gsflow_modflow_low.control":
                    (os.path.join("input", "seg1_low.tab"),
                     os.path.join("input", "seg1.tab"),
                     os.path.join("input", "seg9.tab"),
                     os.path.join("input", "Agwater1.uzf"),
                     os.path.join("input", "Agwater1.ag")),
                "gsflow_gsflowHighKc.control":
                    (os.path.join("..", "input", "modflow", "seg18_tab.txt"),
                     os.path.join("..", "input", "modflow", "seg19_tab.txt"),
                     os.path.join("..", "input", "modflow", "well_tab.txt")),
                "gsflow_gsflowLowKc.control":
                    (os.path.join("..", "input", "modflow", "seg18_tab.txt"),
                     os.path.join("..", "input", "modflow", "seg19_tab.txt"),
                     os.path.join("..", "input", "modflow", "well_tab.txt")),
                "sagehen_gsflow_HighTrig.control":
                    (os.path.join("..", "input", "modflow", "seg18_tab.txt"),
                     os.path.join("..", "input", "modflow", "seg19_tab.txt"),
                     os.path.join("..", "input", "modflow", "well_tab.txt")),
                "sagehen_gsflow_LowTrig.control":
                    (os.path.join("..", "input", "modflow", "seg18_tab.txt"),
                     os.path.join("..", "input", "modflow", "seg19_tab.txt"),
                     os.path.join("..", "input", "modflow", "well_tab.txt")),
                "gsflow.control":
                    (os.path.join("..", "input", "modflow", "sagehen.uzf"),
                     os.path.join("..", "input", "modflow", "sagehen.sfr"),
                     os.path.join("..", "input", "modflow", "sagehen.dis")),
                "Tahoe.control":
                    (os.path.join("..", "input", "modflow", "Cascade_bath.txt"),
                     os.path.join("..", "input", "modflow", "Echo_bath.txt"),
                     os.path.join("..", "input", "modflow", "FallenLeaf_bath.txt"),
                     os.path.join("..", "input", "modflow", "Gilmore_bath.txt"),
                     os.path.join("..", "input", "modflow", "Marlette_bath.txt"),
                     os.path.join("..", "input", "modflow", "Spooner_bath.txt"),
                     os.path.join("..", "input", "modflow", "Tahoe_bath.txt"),
                     os.path.join("..", "input", "modflow", "Tahoe_outflow.txt")),
                "gsflow_modflow_fp.control":
                    (os.path.join("..", "input", "SFR_LAK_floodplain.uzf"),
                     os.path.join("..", "input", "SFR_LAK_floodplain.gag"),
                     os.path.join("..", "input", "SFR_LAK_floodplain_bath.txt"),
                     os.path.join("..", "input", "SFR_LAK_floodplain.tab")),
                "gsflow_modflow_wt.control":
                    (os.path.join("..", "input", "weltab1.txt"),
                     os.path.join("..", "input", "weltab2.txt"),
                     os.path.join("..", "input", "Sfr2weltab.gag")),
                "gsflow_modflow_cap.control":
                    (os.path.join("..", "input", "UZF_cap_ET.uzf"),
                     os.path.join("..", "input", "UZF_cap_ET.gag"),
                     os.path.join("..", "input", "seg1.tab"),
                     os.path.join("..", "input", "seg9.tab")),
                "gsflow_modflow_uzfp2.control":
                    (os.path.join("..", "input", "UZFtest2.uzf"),
                     os.path.join("..", "input", "UZFtest2.gag")),
               }


def external_files(model, ows, f):
    # external file patch for flopy deficiency
    iws, _ = os.path.split(model)
    _, foo = os.path.split(f)
    shutil.copyfile(os.path.join(iws, f), os.path.join(ows, foo))


def do_model(model):
    model_ws, name = os.path.split(model)
    if name in ("placeholder",):
        copyfile = False
    else:
        # need to trick flopy....
        # model_ws, _ = os.path.split(model_ws)
        # if name in ("SWRSample05-nwt.nam",):
        #     model_ws, _ = os.path.split(model_ws)
        # shutil.copyfile(model, os.path.join(model_ws, name))
        # copyfile = True
        pass

    if platform.system().lower() != "windows":
        # fix paths for linux!
        mf_nam = None
        with open(os.path.join(model_ws, name)) as foo:
            s = ""
            for line in foo:
                if ".nam" in line.lower():
                    mf_nam = line.strip().split()[0]
                    mf_nam = mf_nam.replace("\\", "/")

                s += line
            s = s.replace("\\", "/")
        with open(os.path.join(model_ws, name), "w") as foo:
            foo.write(s)

        if mf_nam is not None:
            s = ""
            with open(os.path.join(model_ws, mf_nam)) as foo:
                for line in foo:
                    s += line
                s = s.replace("\\", "/")

            with open(os.path.join(model_ws, mf_nam), "w") as foo:
                foo.write(s)

    gsf = gsflow.GsflowModel.load_from_file(model, gsflow_exe=gsflow_exe,
                                            forgive=False)
    external_fnames = gsf.mf.external_fnames
    gsf.mf.external_fnames = [os.path.split(p)[-1] for p in external_fnames]
    gsf.write_input(workspace=out_dir)

    # remove the temporary name file
    # if copyfile:
    #     os.remove(os.path.join(model_ws, name))

    # ml.change_model_ws(out_dir)

    if name in has_external:
        ext_f = has_external[name]
        for f in ext_f:
            try:
                external_files(model, out_dir, f)
            except FileNotFoundError:
                pass

    gsf = gsflow.GsflowModel.load_from_file(os.path.join(out_dir, name),
                                            gsflow_exe=gsflow_exe,
                                            forgive=False)

    try:
        success, _ = gsf.run_model(forgive=True)
    except:
        success = False
    if not success:
        raise AssertionError(isgsflow)


    # ml.write_input()

    # fix the name files that we can"t load a package with in flopy
    """
    if name in ("Sfr2Weltab.nam", "UZF_cap_ET.nam", "Prob1.nam",
                "Agwater1_high.nam", "Agwater1_low.nam"):
        with open(os.path.join(out_dir, name)) as foo:
            tmp = [line for line in foo]
        with open(os.path.join(out_dir, name), "w") as foo:
            foo.writelines(tmp)

            if name == "Sfr2Weltab.nam":
                foo.write("WEL   91   Sfr2weltab.wel")
            elif name == "UZF_cap_ET.nam":
                foo.write("UZF   19  UZF_cap_ET.uzf")
            elif name == "UZFtest2.nam":
                foo.write("UZF   19  UZFtest2.uzf")
            elif name in ("Agwater1_high.nam", "Agwater1_low.nam"):
                foo.write("UZF  19  Agwater1.uzf\n")
                foo.write("AG   57  Agwater1.ag\n")
            else:
                pass

    ml = fp.modflow.Modflow.load(name,
                                 exe_name=nwt_exe,
                                 model_ws=out_dir,
                                 check=False,
                                 forgive=True)
    # try:
    success, _ = ml.run_model()
    # except:
    #     success = False
    assert success, ismfnwt
    """

def test_pwd():
    wd = os.getcwd()
    _, cur = os.path.split(wd)
    assert cur == "autotest", os.getcwd()


def test_gsflow_exists():
    flist = os.listdir(".")
    if gsflow_exe_name not in flist:
        assert False, flist


def test_run_model():
    for ix, model in enumerate(models[0:6]):
        yield do_model, model
    return


def test_run_long_model():
    for ix, model in enumerate(models[6:]):
        if platform.system().lower() == "windows":
            yield do_model, model
        else:
            continue


if __name__ == "__main__":
    test_pwd()
    test_gsflow_exists()
    # test_run_model()
    for model in models[0:2]:
        do_model(model)
