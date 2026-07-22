#!/usr/bin/env python3
"""Format all Haskell sources with fourmolu (config: src/fourmolu.yaml).

EXCLUDED: src/MtgPure/Model/Object/ToObjectN/Instances/
The .hs files in that directory are generated and are extremely slow to
compile. Reformatting them would rewrite their contents and bump their
modification timestamps, which busts the compile cache and forces a very slow
rebuild for no gain. So we deliberately skip that directory and leave those
files' timestamps untouched. (Only that directory is excluded; the cheap
generated aggregator ToObjectN/Instances.hs beside it is still formatted.)

Pass --everything to override the exclusion and format those files too.
"""

import argparse
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent
SRC = REPO_ROOT / "src"
EXCLUDED_DIR = SRC / "MtgPure" / "Model" / "Object" / "ToObjectN" / "Instances"


def make_parser() -> argparse.ArgumentParser:
    assert __doc__ is not None
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "--everything",
        action="store_true",
        help=(
            "also format the generated files in "
            f"{EXCLUDED_DIR.relative_to(REPO_ROOT)} "
            "(they are slow to compile; formatting them busts the compile cache)"
        ),
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    args = make_parser().parse_args(argv)

    files = [
        p
        for p in SRC.rglob("*.hs")
        if args.everything or EXCLUDED_DIR not in p.parents
    ]
    files.sort()

    if not files:
        print("No .hs files found.", file=sys.stderr)
        return 1

    excluding = "nothing" if args.everything else EXCLUDED_DIR.relative_to(REPO_ROOT)
    print(f"Formatting {len(files)} file(s) with fourmolu (excluding {excluding})...")
    result = subprocess.run(
        ["fourmolu", "-i", *[str(p) for p in files]],
        cwd=REPO_ROOT,
    )
    return result.returncode


if __name__ == "__main__":
    raise SystemExit(main())
