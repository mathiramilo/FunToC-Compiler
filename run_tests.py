import os
import shutil
import sys

def remove_temp_files():
    if os.path.exists("localtests"):
        shutil.rmtree("localtests")
    os.makedirs("localtests", exist_ok=True)

def copy_files():
    tests_files = os.listdir("tests")
    for file in tests_files:
        if file.endswith(".fun"):
            shutil.copy(os.path.join("tests", file), "localtests")

def run_tests(tests_idx: list[int], err: bool = False):
    for test_idx in tests_idx:
        if err:
            os.system(f"runhaskell Compiler.hs localtests/ejemplo{test_idx}err \
                > localtests/ejemplo{test_idx}err.err")
        else:
            os.system(f"runhaskell Compiler.hs localtests/ejemplo{test_idx}")
            os.system(f"runhaskell Compiler.hs -o localtests/ejemplo{test_idx}")

def compare_each_line(tests_idx: list[int], err: bool = False, opt: bool = False):
    file1_lines = []
    file2_lines = []

    for test_idx in tests_idx:
        if err:
            file1_dir = f"localtests/ejemplo{test_idx}err.err"
            file2_dir = f"tests/ejemplo{test_idx}err.err"
        elif opt:
            file1_dir = f"localtests/ejemplo{test_idx}.c"
            file2_dir = f"tests/ejemplo{test_idx}.c"
        else:
            file1_dir = f"localtests/ejemplo{test_idx}_opt.c"
            file2_dir = f"tests/ejemplo{test_idx}_opt.c"

        if not os.path.exists(file1_dir) or not os.path.exists(file2_dir):
            print(f"Test {file1_dir} - {file2_dir} not found ❌", end="\n\n")
            continue

        with open(file1_dir, "r") as file1:
            file1_lines = file1.readlines()
        with open(file2_dir, "r") as file2:
            file2_lines = file2.readlines()

        is_correct = True
        for i in range(len(file1_lines)):
            different_sizes = len(file2_lines) <= i or len(file1_lines) <= i
            if different_sizes or file1_lines[i] != file2_lines[i]:
                if is_correct:
                    print("-" * 115)
                    print(f"Error in test {file1_dir} - {file2_dir} ❌")

                if not different_sizes:
                    print(f'Line {i + 1}:')
                    print(f"Expected: {file2_dir} - {file2_lines[i]}", end="")
                    print(f"Obtained: {file1_dir} - {file1_lines[i]}")
                else:
                    print(f"Files have different sizes")
                    break

                is_correct = False 

        if not is_correct:
            print("-" * 115, end="\n\n")
        else:
            print(f"Test {file1_dir} - {file2_dir} passed ✅", end="\n\n")

if __name__ == "__main__":
    only_compare = bool(sys.argv[1]) if len(sys.argv) > 1 else False
    tests_with_errors = range(1, 5)
    tests_without_errors = range(1, 11)

    if not only_compare:
        remove_temp_files()
        copy_files()
        run_tests(tests_with_errors, err=True)
        run_tests(tests_without_errors, err=False)
    
    compare_each_line(tests_without_errors)
    compare_each_line(tests_without_errors, opt=True)
    compare_each_line(tests_with_errors, err=True)
    