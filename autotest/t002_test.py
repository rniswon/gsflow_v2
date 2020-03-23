# test to compare head file output of models
import os
import numpy as np
import flopy as fp
import pandas as pd
import platform


t_dir = os.path.join(".", "temp")
valid_dir = os.path.join(".", "output")

formatted = ["Agwater1_high.hed",
             "Agwater1_low.hed",
             "EP1b_Agwater1_high.hed",
             "EP1b_Agwater1_low.hed",
             "head_sagehen_HighKc.out",
             "head_sagehen_LowKc.out",
             "head_sagehen_HighTrig.out",
             "head_sagehen_LowTrig.out",
             "head_sagehen.out"]


def validate(sim_array, valid_array):
    validate = (sim_array - valid_array) / valid_array

    failure = np.where(np.abs(validate) > 0.01)
    if failure[0].size > 0:
        check = abs(validate) * sim_array
        check[np.isinf(check)] = 0.
        check[np.isnan(check)] = 0
        if np.max(check) < 0.5:
            return False
        else:
            return True
    else:
        return False


def validate_dataframe(sim_df, valid_df):
    for col in list(valid_df):
        if col.lower() == 'date':
            continue
        valid_array = valid_df[col].values
        sim_array = sim_df[col].values
        failed = validate(sim_array, valid_array)

        if failed:
            return True

    return False


def do_compare_formatted_head_file(output):
    sim = fp.utils.FormattedHeadFile(os.path.join(t_dir, output))
    sim_head = sim.get_alldata()
    valid = fp.utils.FormattedHeadFile(os.path.join(valid_dir, output))
    valid_head = valid.get_alldata()

    failed = validate(sim_head, valid_head)

    if failed:
        raise AssertionError("Simulated head out of defined tolerance")


def do_compare_gsflow_out_files(output):
    sim_df = pd.read_csv(os.path.join(t_dir, output))
    valid_df = pd.read_csv(os.path.join(valid_dir, output))

    failed = validate_dataframe(sim_df, valid_df)

    if failed:
        raise AssertionError("Simulated head out of defined tolerance")


def do_check_outputs(output):
    pth = os.path.join(t_dir, output)

    if not os.path.exists(pth):
        err = "{} was not properly written".format(pth)
        raise AssertionError(err)


def test_outputs_exist():
    for output in formatted:
        if platform.system().lower() == "windows":
            yield do_check_outputs, output
        else:
            if output in ("head_sagehen_HighTrig.out",
                          "head_sagehen_LowTrig.out",
                          "head_sagehen.out",
                          "tahoe.hed"):
                pass
            else:
                yield  do_check_outputs, output
    return


def test_formatted_outputs():
    for output in formatted:
        if platform.system().lower() == "windows":
            yield do_compare_formatted_head_file, output
        else:
            if output in ("head_sagehen_HighTrig.out",
                          "head_sagehen_LowTrig.out",
                          "head_sagehen.out",
                          "tahoe.hed"):
                pass
            else:
                yield do_compare_formatted_head_file, output
    return


if __name__ == "__main__":
    for output in formatted:
        do_check_outputs(output)
    for output in formatted:
        do_compare_formatted_head_file(output)
