[CmdletBinding()]
param(
    [string]$DataDirectory = (Join-Path $PSScriptRoot '..\EloAnalysis'),
    [string]$FidePlayersUrl = 'https://ratings.fide.com/download/players_list_xml.zip',
    [string]$UrRatingsUrl = $env:UR_RATINGS_URL
)

$ErrorActionPreference = 'Stop'

$resolvedDataDirectory = Resolve-Path -LiteralPath $DataDirectory -ErrorAction SilentlyContinue
if ($null -eq $resolvedDataDirectory) {
    New-Item -ItemType Directory -Path $DataDirectory | Out-Null
    $resolvedDataDirectory = Resolve-Path -LiteralPath $DataDirectory
}

$playersZip = Join-Path $env:TEMP 'fide_players_list_xml.zip'
$extractDirectory = Join-Path $env:TEMP 'fide_players_list_xml'
$playersXml = Join-Path $resolvedDataDirectory 'players.xml'
$ratingsCsv = Join-Path $resolvedDataDirectory 'ratings.csv'

Write-Host "Downloading FIDE player list..."
Invoke-WebRequest -Uri $FidePlayersUrl -OutFile $playersZip

if (Test-Path -LiteralPath $extractDirectory) {
    Remove-Item -LiteralPath $extractDirectory -Recurse -Force
}

Expand-Archive -LiteralPath $playersZip -DestinationPath $extractDirectory -Force
$downloadedXml = Get-ChildItem -LiteralPath $extractDirectory -Filter '*.xml' -Recurse |
    Sort-Object Length -Descending |
    Select-Object -First 1

if ($null -eq $downloadedXml) {
    throw "FIDE archive did not contain an XML file."
}

Copy-Item -LiteralPath $downloadedXml.FullName -Destination $playersXml -Force
Write-Host "Wrote $playersXml"

if ([string]::IsNullOrWhiteSpace($UrRatingsUrl)) {
    Write-Warning "No Universal Rating CSV URL was provided. Re-run with -UrRatingsUrl or set UR_RATINGS_URL to download ratings.csv."
    exit 0
}

Write-Host "Downloading Universal Rating CSV..."
Invoke-WebRequest -Uri $UrRatingsUrl -OutFile $ratingsCsv
Write-Host "Wrote $ratingsCsv"
