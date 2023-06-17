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
            file1_dir = f"localtests/ejemplo{test_idx}err.err"
            file2_dir = f"tests/ejemplo{test_idx}err.err"
        else:
            file1_dir = f"localtests/ejemplo{test_idx}.out"
            file2_dir = f"tests/ejemplo{test_idx}.out"

        if not os.path.exists(file1_dir) or not os.path.exists(file2_dir):
            continue

        with open(file1_dir, "r") as file1:
            file1_lines = file1.readlines()
        with open(file2_dir, "r") as file2:
            file2_lines = file2.readlines()

        for i in range(len(file1_lines)):
            if file1_lines[i] != file2_lines[i]:
                print("-" * 115)
                print(f"Error in test {file1_dir} - {file2_dir} ❌")
                print(f"Expected: {file1_lines[i]}", end="")
                print(f"Obtained: {file2_lines[i]}", end="")
                if os.path.exists(f"localtests/ejemplo{test_idx}err.err"):
                    print(f"localtests/ejemplo{test_idx}err.err: {file1_lines[i]}", end="")
                if os.path.exists(f"tests/ejemplo{test_idx}err.err"):
                    print(f"tests/ejemplo{test_idx}err.err: {file2_lines[i]}", end="")
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
    
    compare_each_line(tests_with_errors, err=True)
    compare_each_line(tests_without_errors, err=False)
    