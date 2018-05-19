#!/usr/bin/env python3.6
import difflib
import glob
from pathlib import Path
import re
import subprocess
import sys
from typing import Tuple, Dict, List

BLACKLIST = {
    "echo_module",
    "listm",
    "use_module",
    "dontopen",
    "typed_listm",
    "qsort",
    "aspattern",
    "permutations",
    "importedcons",
    "wlabel",  # relies on unimplemented behavior
}


def print_diff(s1: str, s2: str) -> None:
    lines1 = [l + "\n" for l in s1.splitlines()]
    lines2 = [l + "\n" for l in s2.splitlines()]
    print("".join(difflib.context_diff(lines1, lines2)))


def parse_expected_file(path: Path) -> Tuple[str, str]:
    output: Dict[str, List[str]] = {}
    current_label: str = ""
    with path.open() as f:
        for line in f:
            match = re.match(r"^%%(.*)%%\n$", line)
            if match:
                current_label = match.group(1)
                output[current_label] = []
            else:
                assert current_label
                output[current_label].append(line)
    return "".join(output.get("stdout", [])), "".join(output.get("stderr", []))


def run_all_tests() -> None:
    results = []
    for test_file in map(Path, sorted(glob.glob("test/*.lam"))):
        if test_file.stem in BLACKLIST:
            continue
        results.append(run_test(test_file))
    return all(results)


def run_test(test_file: Path) -> bool:
    test_name = test_file.stem
    print(f"--- running {test_name} ---")
    expected_file = test_file.parent / f"{test_name}.expected"
    if not expected_file.exists():
        print(f"no .expected file for {test_name}")
        return False
    real = subprocess.run(
        ["./lambda", str(test_file)],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        encoding="ascii",
    )
    expected = parse_expected_file(expected_file)
    if expected[0] != real.stdout:
        print(f"stdout does not match for {test_name}")
        print_diff(expected[0], real.stdout)
        return False
    if expected[1] != real.stderr:
        print(f"stderr does not match for {test_name}")
        print_diff(expected[1], real.stderr)
        return False
    return True


if __name__ == "__main__":
    if len(sys.argv) > 1:
        result = run_test(Path(sys.argv[1]))
    else:
        result = run_all_tests()
    sys.exit(0 if result else 1)
