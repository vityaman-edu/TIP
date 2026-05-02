#!/usr/bin/env python3
"""Golden testing framework for TIP language examples.

Usage:
  python3 test/golden.py           # check output matches golden .log files
  python3 test/golden.py --update  # regenerate golden .log files
"""

import difflib
import re
import subprocess
import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).parent.parent
EXAMPLES_DIR = PROJECT_ROOT / "examples"

# Analysis flags appended after the source file argument (e.g. ["-types", "-cfg"])
FLAGS: list[str] = []

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
        m = re.match(r"\[info\] running tip\.Tip \./examples/(.+?)\.tip", line)
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


def _assert_or_update(f: Path, section: list[str], update: bool) -> str | None:
    """Update the golden file or return an error message if it differs."""
    log_path = f.with_suffix(".log")
    actual = _log_text(section)

    if update:
        log_path.write_text(actual)
        print(f"  updated {log_path.name}")
        return None

    if not log_path.exists():
        return f"MISSING {f.name}: no golden file {log_path.name}"

    expected = log_path.read_text()
    if actual == expected:
        return None

    diff = "".join(
        difflib.unified_diff(
            expected.splitlines(keepends=True),
            actual.splitlines(keepends=True),
            fromfile=f"golden/{log_path.name}",
            tofile=f"actual/{log_path.name}",
        )
    )
    return f"DIFF {f.name}:\n{diff}"


def _canonize(lines: list[str]) -> list[str]:
    """Filter to relevant lines and remove run-specific data."""
    out: list[str] = []
    for line in lines:
        if line.startswith("[info]"):
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


def main() -> int:
    update = "--update" in sys.argv
    files = _tip_files()
    pending_start = 0
    batch = 0

    print("Running sbt...")

    while pending_start < len(files):
        pending = files[pending_start:]
        batch += 1
        suffix = " ".join(FLAGS)
        commands = ["compile"] + [
            f"runMain tip.Tip ./{f.relative_to(PROJECT_ROOT)}" + (f" {suffix}" if suffix else "")
            for f in pending
        ]

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
            if err := _assert_or_update(f, sections[f.stem], update):
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
