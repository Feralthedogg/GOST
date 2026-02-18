$ErrorActionPreference = "Stop"

$repoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
Set-Location $repoRoot

$baselinePath = Join-Path $PSScriptRoot "codegen_reject_strings.baseline"
if (!(Test-Path $baselinePath)) {
    Write-Error "Missing baseline file: $baselinePath"
}

$regex = '"([^"\\]|\\.)*(expects|not supported)([^"\\]|\\.)*"'
$current = Get-ChildItem -Path "src/codegen" -Filter "*.rs" -File |
    ForEach-Object {
        Select-String -Path $_.FullName -Pattern $regex -AllMatches |
            ForEach-Object { $_.Matches } |
            ForEach-Object { $_.Value }
    } |
    Sort-Object -Unique

$baseline = Get-Content $baselinePath |
    Where-Object { $_ -and -not $_.StartsWith("#") } |
    Sort-Object -Unique

$fallbackMatches = Select-String -Path "src/compile.rs" -Pattern "file_contains_asm_labels_or_goto" -SimpleMatch
if ($fallbackMatches) {
    Write-Error "Found stale AST fallback helper in src/compile.rs: file_contains_asm_labels_or_goto"
}

$legacyRejectHelpers = Select-String -Path "src/codegen/*.rs" -Pattern "reject_if_invariant" -SimpleMatch
if ($legacyRejectHelpers) {
    Write-Error "Found legacy reject_if_invariant helper/call in src/codegen; use invariant-only checks."
}

$newEntries = Compare-Object -ReferenceObject $baseline -DifferenceObject $current |
    Where-Object { $_.SideIndicator -eq "=>" } |
    Select-Object -ExpandProperty InputObject

if ($newEntries.Count -gt 0) {
    Write-Host "Detected new user-facing reject strings in src/codegen:"
    foreach ($entry in $newEntries) {
        Write-Host "  $entry"
    }
    Write-Error "Guardrail failed. Move rejects to sema or update baseline deliberately."
}

Write-Host "Codegen reject-string guardrail passed."
