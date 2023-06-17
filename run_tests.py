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

def compare_each_line(tests_idx: list[int], err: bool = False):
    file1_lines = []
    file2_lines = []

    for test_idx in tests_idx:
        if err:
            with open(f"localtests/ejemplo{test_idx}err.err", "r") as file1:
                file1_lines = file1.readlines()
            with open(f"tests/ejemplo{test_idx}err.err", "r") as file2:
                file2_lines = file2.readlines()
        else:
            with open(f"localtests/ejemplo{test_idx}err.err", "r") as file1:
                file1_lines = file1.readlines()
            with open(f"tests/ejemplo{test_idx}err.err", "r") as file2:
                file2_lines = file2.readlines()

        for i in range(len(file1_lines)):
            if file1_lines[i] != file2_lines[i]:
                print(f"Error in test error: {err} - id: {test_idx} in line {i+1}")
                print(f"Expected: {file1_lines[i]}")
                print(f"Obtained: {file2_lines[i]}")
                if os.path.exists(f"localtests/ejemplo{test_idx}err.err"):
                    print(f"localtests/ejemplo{test_idx}err.err: {file1_lines[i]}")
                if os.path.exists(f"tests/ejemplo{test_idx}err.err"):
                    print(f"tests/ejemplo{test_idx}err.err: {file2_lines[i]}")

if __name__ == "__main__":
    only_compare = bool(sys.argv[1]) if len(sys.argv) > 1 else False
    tests_with_errors = range(1, 5)
    tests_without_errors = range(1, 10)


    if not only_compare:
        remove_temp_files()
        copy_files()
        run_tests(tests_with_errors, err=True)
        run_tests(tests_without_errors, err=False)
    
    compare_each_line(tests_with_errors, err=True)
    compare_each_line(tests_without_errors, err=False)
    