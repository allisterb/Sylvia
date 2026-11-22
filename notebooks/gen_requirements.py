"""Generate a pinned, hashed requirements file from an installed venv WITHOUT running it.

Versions: as installed, except overrides (name=version). Every pinned version must be at least
MIN_AGE days old. Hashes: every WHEEL for that version that can install on CPython 3.13 /
win_amd64 (sdists left out on purpose: installing one runs its build backend).

usage: python -I gen_reqs.py <site-packages> <out.txt> <min_age_days> [name=version ...]
"""
import email.parser, json, os, re, sys, time, urllib.request
from datetime import datetime, timezone

site, out, min_age = sys.argv[1], sys.argv[2], int(sys.argv[3])
overrides = {re.sub(r"[-_.]+", "-", k).lower(): v for k, v in (a.split("=", 1) for a in sys.argv[4:])}
norm = lambda s: re.sub(r"[-_.]+", "-", s).lower()
SKIP = {"pip"}                      # comes from ensurepip with Python itself, not from PyPI
now = datetime.now(timezone.utc)

def compatible(fn):
    if not fn.endswith(".whl"): return False
    parts = fn[:-4].split("-")
    py, abi, plat = parts[-3], parts[-2], parts[-1]
    if not any(p in ("any", "win_amd64") for p in plat.split(".")): return False
    for t in py.split("."):
        if t in ("py3", "py313", "cp313") and abi in ("none", "cp313", "abi3"): return True
        if t in ("py2.py3",) or t == "py3": return True
        m = re.fullmatch(r"cp3(\d+)", t)
        if m and "abi3" in abi.split(".") and int(m.group(1)) <= 13: return True
    return False

def pypi(name, ver):
    req = urllib.request.Request(f"https://pypi.org/pypi/{name}/{ver}/json", headers={"User-Agent": "venv-audit/1.0"})
    return json.load(urllib.request.urlopen(req, timeout=30))

pkgs = []
for d in sorted(os.listdir(site)):
    if not d.endswith(".dist-info"): continue
    m = email.parser.Parser().parsestr(open(os.path.join(site, d, "METADATA"), encoding="utf-8", errors="replace").read(), headersonly=True)
    if norm(m["Name"]) in SKIP: continue
    pkgs.append((m["Name"], overrides.get(norm(m["Name"]), m["Version"]), m["Version"]))

lines, problems = [], []
for name, ver, installed in sorted(pkgs, key=lambda p: norm(p[0])):
    j = pypi(name, ver)
    files = [f for f in j["urls"] if not f.get("yanked")]
    first = min(datetime.fromisoformat(f["upload_time_iso_8601"].replace("Z", "+00:00")) for f in files)
    age = (now - first).days
    if age < min_age: problems.append(f"{name} {ver} is only {age} days old")
    wheels = sorted((f["digests"]["sha256"] for f in files if compatible(f["filename"])))
    if not wheels: problems.append(f"{name} {ver}: no compatible wheel (sdist only)"); continue
    # A comment may not follow a line-continuation backslash, so the note gets its own line.
    note = f"# {norm(name)}: {installed} was installed; held back to the newest release >= {min_age} days old\n" if ver != installed else ""
    lines.append(f"{note}{norm(name)}=={ver} \\\n" + " \\\n".join(f"    --hash=sha256:{h}" for h in wheels))
    time.sleep(0.05)

hdr = (f"# Pinned, hash-locked requirements for the Sylvia notebooks venv (python/).\n"
       f"# Generated {now:%Y-%m-%d} from the installed set; every version below was released at least\n"
       f"# {min_age} days before that date. Wheels only (CPython 3.13, win_amd64): no sdist hashes, so\n"
       f"# nothing is built from source. Install into a fresh venv with:\n"
       f"#   python -m pip install --require-hashes --only-binary :all: --no-deps -r <this file>\n"
       f"# (pip itself is excluded: it comes from Python's bundled ensurepip.)\n\n")
open(out, "w", encoding="utf-8", newline="\n").write(hdr + "\n".join(lines) + "\n")
print(f"{len(lines)} packages written to {out}")
for p in problems: print("PROBLEM:", p)
