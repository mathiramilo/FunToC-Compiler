import os
import shutil

def run_tests(tests: list[int], err: bool = False):
    if not os.path.exists("localtests"):
        os.makedirs("localtests", exist_ok=True)
    
    local_tests_files = os.listdir("localtests")
    for file in local_tests_files:
        os.remove(os.path.join("localtests", file))

    tests_files = os.listdir("tests")
    for file in tests_files:
        shutil.copy(os.path.join("tests", file), "localtests")

    for test_idx in tests:
        if err:
            os.system(f"runhaskell Compiler.hs localtests/ejemplo{test_idx}err \
                > localtests/ejemplo{test_idx}err.err")
        else:
            os.system(f"runhaskell Compiler.hs localtests/ejemplo{test_idx}")
            os.system(f"runhaskell Compiler.hs -o localtests/ejemplo{test_idx}")

if __name__ == "__main__":
    tests_with_errors = range(1, 5)
    tests_without_errors = range(1, 10)
    run_tests(tests_with_errors, err=True)
    run_tests(tests_without_errors, err=False)
    