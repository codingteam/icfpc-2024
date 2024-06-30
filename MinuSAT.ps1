# This is a script to remove a solution from SAT (by adding a clause with all bits inverted).
param (
    $inputPath = "data/efficiency8.sat.out"
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$lines = Get-Content $inputPath
$myLine = $lines[1]
$result = $myLine.Split(' ') | ForEach-Object { [int]$x = $_; -$x } | Join-String -Separator ' '
$result
