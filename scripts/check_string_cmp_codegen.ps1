$ErrorActionPreference = "Stop"

$repoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
Set-Location $repoRoot

$fixtures = @(
    "test/std_errors_pkg_test.gs",
    "test/std_flag_pkg_test.gs",
    "test/std_testing_pkg_test.gs"
)

$offenders = @()

foreach ($fixture in $fixtures) {
    if (!(Test-Path $fixture)) {
        Write-Error "Missing fixture: $fixture"
    }

    Write-Host "Building $fixture ..."
    cargo run -q --bin gs -- build $fixture
    if ($LASTEXITCODE -ne 0) {
        Write-Error "Failed to build fixture: $fixture"
    }

    $fixtureAbs = (Resolve-Path $fixture).Path
    $llPath = [System.IO.Path]::ChangeExtension($fixtureAbs, ".ll")
    if (!(Test-Path $llPath)) {
        Write-Error "Expected LLVM IR output missing: $llPath"
    }

    # String values are aggregate (%string) and must not be compared by raw icmp.
    $hits = Select-String -Path $llPath -Pattern "\bicmp\b[^\r\n]*%string"
    if ($hits) {
        $offenders += [PSCustomObject]@{
            Fixture = $fixture
            LlPath   = $llPath
            Hits     = $hits
        }
    }
}

if ($offenders.Count -gt 0) {
    Write-Host "Detected invalid string icmp in generated LLVM:"
    foreach ($offender in $offenders) {
        Write-Host "  Fixture: $($offender.Fixture)"
        Write-Host "  IR:      $($offender.LlPath)"
        foreach ($m in $offender.Hits | Select-Object -First 5) {
            Write-Host ("    L{0}: {1}" -f $m.LineNumber, $m.Line.Trim())
        }
    }
    Write-Error "String compare codegen guard failed."
}

Write-Host "String compare codegen guard passed."
