<#
.SYNOPSIS
    Register the Sylvia F# Jupyter kernel ("sylvia-fsharp") with Jupyter.

.DESCRIPTION
    Writes a kernel spec whose argv runs this project's built DLL, then installs it with
    jupyter_client. By default it goes into the given Python environment's own prefix (the repo's
    python\ venv, share\jupyter\kernels), so only that environment's Jupyter sees it; use -User to
    install for every Jupyter on this account (%APPDATA%\jupyter\kernels).

    Build the project first (dotnet build). The spec points at the build output in place, so a
    rebuild is picked up by the next kernel start. A running kernel locks its DLLs: restart or
    shut down notebook kernels before rebuilding.

    Remove by deleting the installed sylvia-fsharp directory (the script prints where it went).

.EXAMPLE
    .\install-kernel.ps1
.EXAMPLE
    .\install-kernel.ps1 -User -Configuration Release
#>
param(
    [string] $Python = (Join-Path $PSScriptRoot '..\..\..\..\python\Scripts\python.exe'),
    [string] $Configuration = 'Debug',
    [string] $Name = 'sylvia-fsharp',
    [string] $DisplayName = 'Sylvia (F#)',
    [switch] $User
)
$ErrorActionPreference = 'Stop'

$dll = Join-Path $PSScriptRoot "bin\$Configuration\net10.0\Sylvia.Interactive.Jupyter.dll"
if (-not (Test-Path -LiteralPath $dll)) { throw "Kernel not built: $dll. Run 'dotnet build' in $PSScriptRoot first." }
$dll = (Resolve-Path -LiteralPath $dll).Path
if (-not (Test-Path -LiteralPath $Python)) { throw "Python not found: $Python. Pass -Python <path to python.exe with jupyter installed>." }

$spec = [ordered]@{
    argv           = @('dotnet', $dll, '{connection_file}')
    display_name   = $DisplayName
    language       = 'F#'
    # Shell handles interrupt_request on the control channel; Windows has no SIGINT for .NET.
    interrupt_mode = 'message'
}

$staging = Join-Path ([IO.Path]::GetTempPath()) "sylvia-kernelspec-$([Guid]::NewGuid().ToString('N'))\$Name"
New-Item -ItemType Directory -Force -Path $staging | Out-Null
try {
    # UTF-8 without a BOM: Jupyter's JSON reader rejects a BOM.
    [IO.File]::WriteAllText((Join-Path $staging 'kernel.json'), ($spec | ConvertTo-Json), (New-Object Text.UTF8Encoding($false)))
    # jupyter_client's API rather than `python -m jupyter kernelspec`: the `jupyter` dispatcher
    # runs Scripts\jupyter-kernelspec.exe, whose launcher hard-codes the venv's ORIGINAL path, so
    # in a moved or copied venv it installs into the wrong environment. `-I` keeps PYTHON* out.
    $install = @'
import sys
from jupyter_client.kernelspec import KernelSpecManager
src, name, user = sys.argv[1], sys.argv[2], sys.argv[3] == "user"
ksm = KernelSpecManager()
dest = ksm.install_kernel_spec(src, kernel_name=name, user=user, prefix=None if user else sys.prefix)
print("installed", name, "in", dest)
for n, d in sorted(ksm.find_kernel_specs().items()): print(f"  {n:<18} {d}")
'@
    # As a file, not `-c`: Windows PowerShell strips double quotes from native-command arguments.
    $installPy = Join-Path (Split-Path $staging) 'install.py'
    [IO.File]::WriteAllText($installPy, $install, (New-Object Text.UTF8Encoding($false)))
    & $Python -I $installPy $staging $Name $(if ($User) { 'user' } else { 'prefix' })
    if ($LASTEXITCODE -ne 0) { throw "kernel spec install failed ($LASTEXITCODE)." }
}
finally {
    Remove-Item -LiteralPath (Split-Path $staging) -Recurse -Force
}
