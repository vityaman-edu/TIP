#!/usr/bin/env python3
"""Golden testing framework for TIP language examples.

Usage:
  python3 test/golden.py           # check output matches golden files
  python3 test/golden.py --update  # regenerate golden files
"""

import difflib
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

PROJECT_ROOT = Path(__file__).parent.parent
EXAMPLES_DIR = PROJECT_ROOT / "examples"

# Analysis flags placed before the source file argument (e.g. ["-types", "-cfg"])
FLAGS: list[str] = ["-types"]

_ANSI = re.compile(r"\x1b(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")


def _tip_files() -> list[Path]:
    return sorted(EXAMPLES_DIR.glob("*.tip"))


def _run_sbt(commands: list[str]) -> str:
    result = subprocess.run(
        ["sbt"] + commands,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        cwd=str(PROJECT_ROOT),
    )
    return _ANSI.sub("", result.stdout)


def _split_by_file(output: str) -> dict[str, list[str]]:
    """Split sbt output into per-file sections keyed by file stem."""
    sections: dict[str, list[str]] = {}
    current: str | None = None
    buf: list[str] = []

    for line in output.splitlines():
        # Handles optional flags and trailing output-dir in the running line.
        m = re.match(r"\[info\] running tip\.Tip .*\./examples/(.+?)\.tip", line)
        if m:
            if current is not None:
                sections[current] = buf
            current = m.group(1)
            buf = []
        elif current is not None:
            buf.append(line)

    if current is not None:
        sections[current] = buf

    return sections


def _has_total_time(lines: list[str]) -> bool:
    return any("Total time:" in line for line in lines)


def _canonize(lines: list[str]) -> list[str]:
    """Filter to relevant lines and remove run-specific data."""
    out: list[str] = []
    for line in lines:
        if line.startswith("[info]"):
            # Skip the absolute-path "written to" message from Output.output.
            if re.match(r"\[info\] Results of .+ analysis of .+ written to ", line):
                continue
            out.append(line)
        elif line.startswith("[error]"):
            content = line[len("[error]"):].strip()
            if re.match(r"at ", content) or re.match(r"\.\.\.", content):
                continue
            if "stack trace is suppressed" in content:
                continue
            if content.startswith("(Compile / runMain)"):
                continue
            line = re.sub(
                r"Total time: \d+ s, completed .+$",
                "Total time: <duration>",
                line,
            )
            out.append(line)
        elif line.startswith("[success]"):
            line = re.sub(
                r"Total time: \d+ s, completed .+$",
                "Total time: <duration>",
                line,
            )
            out.append(line)
    return out


def _exit_code(lines: list[str]) -> int:
    return 0 if any(line.startswith("[success]") for line in lines) else 1


def _log_text(lines: list[str]) -> str:
    return "\n".join(_canonize(lines)) + f"\n[exit] {_exit_code(lines)}\n"


def _diff(golden: Path, actual: str) -> str | None:
    expected = golden.read_text()
    if actual == expected:
        return None
    return "".join(
        difflib.unified_diff(
            expected.splitlines(keepends=True),
            actual.splitlines(keepends=True),
            fromfile=f"golden/{golden.name}",
            tofile=f"actual/{golden.name}",
        )
    )


def _assert_or_update(
    f: Path, section: list[str], update: bool, out_dir: Path
) -> str | None:
    """Update golden files or return the first error found."""
    # .log
    log_path = f.with_suffix(".log")
    actual_log = _log_text(section)

    if update:
        log_path.write_text(actual_log)
        print(f"  updated {log_path.name}")
    else:
        if not log_path.exists():
            return f"MISSING {f.name}: no golden file {log_path.name}"
        if d := _diff(log_path, actual_log):
            return f"DIFF {log_path.name}:\n{d}"

    # output files (e.g. __types.ttip, __cfg.dot)
    for golden in sorted(EXAMPLES_DIR.glob(f"{f.name}__*")):
        generated = out_dir / golden.name
        if update:
            # Tool already wrote to out_dir == EXAMPLES_DIR; just confirm.
            print(f"  updated {golden.name}")
        else:
            if not generated.exists():
                return f"MISSING {golden.name}: not generated in this run"
            if d := _diff(golden, generated.read_text()):
                return f"DIFF {golden.name}:\n{d}"

    return None


def _sbt_cmd(f: Path, out_dir: Path) -> str:
    # Tip.scala requires: [flags…] source outdir
    parts = ["runMain tip.Tip"] + FLAGS
    parts.append(f"./{f.relative_to(PROJECT_ROOT)}")
    parts.append(str(out_dir))
    return " ".join(parts)


def _parse_filter() -> re.Pattern | None:
    args = sys.argv[1:]
    for i, arg in enumerate(args):
        if arg == "--filter" and i + 1 < len(args):
            return re.compile(args[i + 1])
    return None


def main() -> int:
    update = "--update" in sys.argv
    filter_re = _parse_filter()
    files = _tip_files()
    if filter_re:
        files = [f for f in files if filter_re.search(f.stem)]

    tmp: str | None = None
    if update:
        out_dir = EXAMPLES_DIR
    else:
        tmp = tempfile.mkdtemp()
        out_dir = Path(tmp)

    try:
        return _run(update, files, out_dir)
    finally:
        if tmp:
            shutil.rmtree(tmp, ignore_errors=True)


def _run(update: bool, files: list[Path], out_dir: Path) -> int:
    pending_start = 0
    batch = 0

    print("Running sbt...")

    while pending_start < len(files):
        pending = files[pending_start:]
        batch += 1
        commands = ["compile"] + [_sbt_cmd(f, out_dir) for f in pending]

        print(f"  batch {batch}: {len(pending)} file(s)...", end=" ", flush=True)
        output = _run_sbt(commands)
        sections = _split_by_file(output)

        if not sections:
            print("no output (compile failed?)")
            return 1

        ran_until = -1
        called_sys_exit = False

        for i, f in enumerate(pending):
            if f.stem not in sections:
                break
            section = sections[f.stem]
            ran_until = i
            if not _has_total_time(section):
                called_sys_exit = True
                break

        if ran_until == -1:
            print("no files ran")
            return 1

        if called_sys_exit:
            print(f"ran {ran_until + 1} (sys.exit in {pending[ran_until].name})")
        else:
            print(f"ran {ran_until + 1}")

        for f in pending[: ran_until + 1]:
            if err := _assert_or_update(f, sections[f.stem], update, out_dir):
                print(err)
                if not update:
                    print("Run `python3 test/golden.py --update` to regenerate.")
                    return 1

        if not called_sys_exit:
            break

        pending_start += ran_until + 1

    if update:
        print(f"Updated {len(files)} golden files.")
        return 0

    print(f"All {len(files)} golden tests passed.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
