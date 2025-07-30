module Program

open FSharp.Data
open XPlot.Plotly
open System.IO
open System.Text.Json

type FideXml = XmlProvider<"players_sample.xml">
type UrRatingCsv = CsvProvider<"ratings_sample.csv">

type AnalysisConfig =
    { MinRating: int
      MinPlayersInCountry: int
      ElitePercentage: float
      CachedPlayersJsonPath: string
      PlayersXmlPath: string
      RatingsCsvPath: string }

type CountryStats =
    { Country: string
      PlayerCount: int
      OverallAvgGap: float
      OverallMedianGap: float
      ElitePlayerCount: int
      EliteAvgGap: float
      EliteMedianGap: float
      EliteStdDev: float }

type RatingTierStats =
    { TierName: string
      MinRating: int
      MaxRating: int option
      PlayerCount: int
      AvgGap: float
      MedianGap: float
      StdDev: float
      AvgFideRating: float
      AvgUrRating: float }

type CountryTierStats =
    { Country: string
      TierName: string
      MinRating: int
      MaxRating: int option
      PlayerCount: int
      AvgGap: float
      MedianGap: float
      StdDev: float
      AvgFideRating: float
      AvgUrRating: float
      DeflationPercent: float }

type CountryInflation =
    { Country: string
      PlayerCount: int
      AvgRatingGap: float }

type StrategicOpponent =
    { FideId: int
      Name: string
      Country: string
      FideRating: int
      UniversalRating: int
      RatingGap: int
      Title: string }

type StrategicOpponentsResult =
    { ReferencePlayerId: int
      TargetMinRating: int
      TargetMaxRating: int
      TopFavorableCountries: CountryInflation list
      TopOpponents: StrategicOpponent list }

type Player =
    { FideId: int
      Name: string
      Country: string
      Sex: string
      Title: string
      WTitle: string
      OTitle: string
      FoaTitle: string
      FideRating: int
      FideGames: int
      FideK: int
      FideRapidRating: int
      FideRapidGames: int
      FideRapidK: int
      FideBlitzRating: int
      FideBlitzGames: int
      FideBlitzK: int
      Birthday: int
      Flag: string
      WorldRank: int
      UniversalRating: int
      RapidGap: int
      BlitzGap: int
      CountryRank: int
      GameCount: int }

let loadFidePlayersAsync (config: AnalysisConfig) : Async<Map<int, Player>> =
    async {
        let fideXml = FideXml.Load(config.PlayersXmlPath)

        let map =
            fideXml.Players
            |> Seq.choose (fun p ->
                try
                    Some(
                        p.Fideid,
                        { FideId = p.Fideid
                          Name = p.Name
                          Country = p.Country
                          Sex = p.Sex
                          Title = string p.Title
                          WTitle = string p.WTitle
                          OTitle = string p.OTitle
                          FoaTitle = string p.FoaTitle
                          FideRating = p.Rating
                          FideGames = p.Games
                          FideK = p.K
                          FideRapidRating = p.RapidRating
                          FideRapidGames = p.RapidGames
                          FideRapidK = p.RapidK
                          FideBlitzRating = p.BlitzRating
                          FideBlitzGames = p.BlitzGames
                          FideBlitzK = p.BlitzK
                          Birthday = p.Birthday
                          Flag = p.Flag |> Option.defaultValue ""
                          WorldRank = 0
                          UniversalRating = p.Rating
                          RapidGap = 0
                          BlitzGap = 0
                          CountryRank = 0
                          GameCount = 0 }
                    )
                with _ ->
                    None)
            |> Map.ofSeq

        return map
    }

type UrRatingData =
    { WorldRank: int
      UniversalRating: int
      RapidGap: int
      BlitzGap: int
      CountryRank: int
      GameCount: int }

let loadUrRatingsAsync (config: AnalysisConfig) : Async<Map<int, UrRatingData>> =
    async {
        let urCsv = UrRatingCsv.Load(config.RatingsCsvPath)

        let ur =
            urCsv.Rows
            |> Seq.map (fun r ->
                (r.FIDE_PlayerCode,
                 { WorldRank = r.WorldRank
                   UniversalRating = r.URating
                   RapidGap = r.RGap
                   BlitzGap = r.BGap
                   CountryRank = r.CountryRank
                   GameCount = r.GameCount }))
            |> Map.ofSeq

        return ur
    }

let mergePlayerDataAsync (config: AnalysisConfig) : Async<Player[]> =
    async {

        let! fideChild = Async.StartChild(loadFidePlayersAsync config)
        let! urChild = Async.StartChild(loadUrRatingsAsync config)

        let! fideMap = fideChild
        let! urMap = urChild

        let merged =
            fideMap
            |> Map.map (fun fideId player ->
                match urMap.TryFind fideId with
                | Some urData ->
                    { player with
                        WorldRank = urData.WorldRank
                        UniversalRating = urData.UniversalRating
                        RapidGap = urData.RapidGap
                        BlitzGap = urData.BlitzGap
                        CountryRank = urData.CountryRank
                        GameCount = urData.GameCount }
                | None -> player)
            |> Map.values
            |> Seq.toArray

        return merged
    }

let saveToJson (config: AnalysisConfig) (players: Player seq) =
    let playersArray = players |> Seq.toArray
    let options = JsonSerializerOptions()
    options.WriteIndented <- false
    let jsonString = JsonSerializer.Serialize(playersArray, options)
    File.WriteAllText(config.CachedPlayersJsonPath, jsonString)
    printfn $"Saved %d{playersArray.Length} players to JSON"

let loadFromJson (config: AnalysisConfig) : Player[] =
    let jsonString = File.ReadAllText(config.CachedPlayersJsonPath)
    let players = JsonSerializer.Deserialize<Player[]>(jsonString)
    printfn $"Loaded %d{players.Length} players from JSON"
    players

let jsonExists (config: AnalysisConfig) =
    File.Exists(config.CachedPlayersJsonPath)


let displayTopPlayers (players: Player seq) =
    players
    |> Seq.sortByDescending (fun p -> p.FideRating)
    |> Seq.take 10
    |> Seq.iter (fun p ->
        printfn $"FIDE ID: {p.FideId}, Name: {p.Name}, Rating: {p.FideRating}, Country: {p.Country}, Title: {p.Title}")

let displayTopUniversalRatings (players: Player seq) =
    players
    |> Seq.sortByDescending (fun p -> p.UniversalRating)
    |> Seq.take 10
    |> Seq.iter (fun p ->
        printfn
            $"FIDE ID: {p.FideId}, Name: {p.Name}, Universal Rating: {p.UniversalRating}, Country: {p.Country}, Title: {p.Title}")

let calculateMedian (values: int list) : float =
    if values.IsEmpty then
        0.0
    else
        let sorted = List.sort values
        let length = sorted.Length

        if length % 2 = 0 then
            float (sorted[length / 2 - 1] + sorted[length / 2]) / 2.0
        else
            float sorted[length / 2]

let analyzeAndDisplayCountryRatings (config: AnalysisConfig) (players: Player seq) : CountryStats list =
    printfn "\n\n--- 🌍 Advanced Country Rating Analysis (UR vs FIDE) ---\n"

    let validPlayers =
        players
        |> Seq.filter (fun p ->
            p.Country <> ""
            && p.UniversalRating > config.MinRating
            && p.FideRating > config.MinRating)
        |> Seq.map (fun p ->
            let gap = p.UniversalRating - p.FideRating
            (p.Country, gap, p.UniversalRating))
        |> List.ofSeq

    printfn $"Processing {validPlayers.Length} players with valid ratings > {config.MinRating}..."

    if validPlayers.Length = 0 then
        printfn "No players met the initial criteria. Analysis cannot continue."
        []
    else
        let countryData =
            validPlayers
            |> List.groupBy (fun (country, _, _) -> country)
            |> List.filter (fun (_, playersInGroup) -> playersInGroup.Length >= config.MinPlayersInCountry)
            |> List.map (fun (country, countryPlayers) ->
                let playersByUR = countryPlayers |> List.sortByDescending (fun (_, _, ur) -> ur)
                let allGaps = playersByUR |> List.map (fun (_, gap, _) -> gap)

                let playerCount = allGaps.Length
                let overallAvgGap = List.averageBy float allGaps
                let overallMedianGap = calculateMedian allGaps

                let eliteCount = max 1 (int (config.ElitePercentage * float playerCount))

                let eliteGaps =
                    playersByUR |> List.take eliteCount |> List.map (fun (_, gap, _) -> gap)

                let eliteAvgGap = List.averageBy float eliteGaps
                let eliteMedianGap = calculateMedian eliteGaps
                let variance = eliteGaps |> List.averageBy (fun g -> (float g - eliteAvgGap) ** 2.0)
                let eliteStdDev = sqrt variance

                { Country = country
                  PlayerCount = playerCount
                  OverallAvgGap = overallAvgGap
                  OverallMedianGap = overallMedianGap
                  ElitePlayerCount = eliteCount
                  EliteAvgGap = eliteAvgGap
                  EliteMedianGap = eliteMedianGap
                  EliteStdDev = eliteStdDev })
            |> List.sortByDescending (fun stats -> stats.EliteAvgGap)

        printfn
            $"Analysis complete for {countryData.Length} countries with at least {config.MinPlayersInCountry} players."

        printfn
            "\n%-12s %-10s | %-10s %-10s | %-10s %-10s %-10s %-10s"
            "Country"
            "Plyr Count"
            "OverallAvg"
            "OverallMed"
            "EliteAvg"
            "EliteMed"
            "EliteStdDev"
            "EliteCount"

        printfn "%s" (String.replicate 100 "-")

        countryData
        |> List.iter (fun stats ->
            printfn
                $"%-12s{stats.Country} %-10d{stats.PlayerCount} | %-+10.1f{stats.OverallAvgGap} %-+10.1f{stats.OverallMedianGap} | %-+10.1f{stats.EliteAvgGap} %-+10.1f{stats.EliteMedianGap} %-10.1f{stats.EliteStdDev} %-10d{stats.ElitePlayerCount}")

        if not countryData.IsEmpty then
            let mostUnderratedElite = countryData |> List.maxBy (fun s -> s.EliteAvgGap)
            let mostOverratedElite = countryData |> List.minBy (fun s -> s.EliteAvgGap)
            let mostInconsistent = countryData |> List.maxBy (fun s -> s.EliteStdDev)

            let biggestElitePremium =
                countryData |> List.maxBy (fun s -> s.EliteAvgGap - s.OverallAvgGap)

            printfn "\n--- Key Insights ---"

            printfn
                "• Positive Gap = Universal Rating is higher (suggests FIDE rating may be 'deflated' or 'underrated')"

            printfn
                $"• Most Underrated Elite: %s{mostUnderratedElite.Country} (Avg Gap: %+.1f{mostUnderratedElite.EliteAvgGap})"

            printfn
                $"• Most Overrated Elite:  %s{mostOverratedElite.Country} (Avg Gap: %+.1f{mostOverratedElite.EliteAvgGap})"

            printfn
                $"• Most Inconsistent Elite: %s{mostInconsistent.Country} (Std Dev: %.1f{mostInconsistent.EliteStdDev})"

            printfn
                $"• Biggest Elite Premium:   %s{biggestElitePremium.Country} (Elite gap is %+.1f{biggestElitePremium.EliteAvgGap - biggestElitePremium.OverallAvgGap} points higher than their national average)"

        countryData

let analyzeRatingTiers (config: AnalysisConfig) (players: Player seq) : RatingTierStats list =
    printfn "\n\n--- 📊 Rating Inflation/Deflation by Tier Analysis (UR vs FIDE) ---\n"

    let validPlayers =
        players
        |> Seq.filter (fun p -> p.UniversalRating > config.MinRating && p.FideRating > config.MinRating)
        |> Seq.toList

    printfn $"Processing {validPlayers.Length} players with valid ratings > {config.MinRating}..."

    if validPlayers.Length = 0 then
        printfn "No players met the criteria. Analysis cannot continue."
        []
    else
        let ratingTiers =
            [ ("2600+", 2600, None)
              ("2500-2599", 2500, Some 2599)
              ("2400-2499", 2400, Some 2499)
              ("2300-2399", 2300, Some 2399)
              ("2200-2299", 2200, Some 2299)
              ("2100-2199", 2100, Some 2199)
              ("2000-2099", 2000, Some 2099)
              ("1900-1999", 1900, Some 1999)
              ("1800-1899", 1800, Some 1899)
              ("1700-1799", 1700, Some 1799)
              ("1600-1699", 1600, Some 1699)
              ("1500-1599", 1500, Some 1599)
              ("1400-1499", 1400, Some 1499)
              ("1300-1399", 1300, Some 1399)
              ("1200-1299", 1200, Some 1299) ]

        let tierStats =
            ratingTiers
            |> List.choose (fun (tierName, minRating, maxRatingOpt) ->
                let playersInTier =
                    validPlayers
                    |> List.filter (fun p ->
                        p.FideRating >= minRating
                        && (match maxRatingOpt with
                            | Some maxRating -> p.FideRating <= maxRating
                            | None -> true))

                if playersInTier.Length >= 10 then // Minimum players for meaningful analysis
                    let gaps = playersInTier |> List.map (fun p -> p.UniversalRating - p.FideRating)
                    let fideRatings = playersInTier |> List.map (fun p -> p.FideRating)
                    let urRatings = playersInTier |> List.map (fun p -> p.UniversalRating)

                    let avgGap = List.averageBy float gaps
                    let medianGap = calculateMedian gaps
                    let variance = gaps |> List.averageBy (fun g -> (float g - avgGap) ** 2.0)
                    let stdDev = sqrt variance
                    let avgFideRating = List.averageBy float fideRatings
                    let avgUrRating = List.averageBy float urRatings

                    Some
                        { TierName = tierName
                          MinRating = minRating
                          MaxRating = maxRatingOpt
                          PlayerCount = playersInTier.Length
                          AvgGap = avgGap
                          MedianGap = medianGap
                          StdDev = stdDev
                          AvgFideRating = avgFideRating
                          AvgUrRating = avgUrRating }
                else
                    None)

        printfn $"Analysis complete for {tierStats.Length} rating tiers with sufficient players.\n"

        printfn
            "%-12s %-10s | %-10s %-10s %-10s | %-10s %-10s %-10s"
            "Tier"
            "Players"
            "Avg Gap"
            "Med Gap"
            "Std Dev"
            "Avg FIDE"
            "Avg UR"
            "Deflation%"

        printfn "%s" (String.replicate 95 "-")

        tierStats
        |> List.iter (fun stats ->
            let deflationPercent = (stats.AvgGap / stats.AvgFideRating) * 100.0

            printfn
                $"%-12s{stats.TierName} %-10d{stats.PlayerCount} | %-+10.1f{stats.AvgGap} %-+10.1f{stats.MedianGap} %-10.1f{stats.StdDev} | %-10.1f{stats.AvgFideRating} %-10.1f{stats.AvgUrRating} %-+10.2f{deflationPercent}%%")

        if not tierStats.IsEmpty then
            let mostDeflated = tierStats |> List.maxBy (fun s -> s.AvgGap)
            let leastDeflated = tierStats |> List.minBy (fun s -> s.AvgGap)
            let mostInconsistent = tierStats |> List.maxBy (fun s -> s.StdDev)

            printfn "\n--- Key Insights ---"
            printfn "• Positive Gap = Universal Rating > FIDE Rating (suggests FIDE rating is 'deflated')"
            printfn "• Deflation%% = (Gap / Avg FIDE Rating) × 100"

            printfn
                $"• Most Deflated Tier: %s{mostDeflated.TierName} (Avg Gap: %+.1f{mostDeflated.AvgGap}, %.2f{(mostDeflated.AvgGap / mostDeflated.AvgFideRating) * 100.0}%% deflation)"

            printfn
                $"• Least Deflated Tier: %s{leastDeflated.TierName} (Avg Gap: %+.1f{leastDeflated.AvgGap}, %.2f{(leastDeflated.AvgGap / leastDeflated.AvgFideRating) * 100.0}%% deflation)"

            printfn $"• Most Inconsistent: %s{mostInconsistent.TierName} (Std Dev: %.1f{mostInconsistent.StdDev})"

            let highTierAvg =
                tierStats
                |> List.filter (fun s -> s.MinRating >= 2400)
                |> List.averageBy (fun s -> s.AvgGap)

            let midTierAvg =
                tierStats
                |> List.filter (fun s -> s.MinRating >= 2000 && s.MinRating < 2400)
                |> List.averageBy (fun s -> s.AvgGap)

            let lowTierAvg =
                tierStats
                |> List.filter (fun s -> s.MinRating < 2000)
                |> List.averageBy (fun s -> s.AvgGap)

            printfn $"\n--- Tier Group Analysis ---"
            printfn $"• High Tier (2400+) Avg Gap: %+.1f{highTierAvg}"
            printfn $"• Mid Tier (2000-2399) Avg Gap: %+.1f{midTierAvg}"
            printfn $"• Low Tier (<2000) Avg Gap: %+.1f{lowTierAvg}"

            if highTierAvg > midTierAvg then
                printfn "• Result: FIDE ratings appear MORE deflated for higher-rated players"
            else
                printfn "• Result: FIDE ratings appear LESS deflated for higher-rated players"

        tierStats

let analyzeCountryRatingTiers (config: AnalysisConfig) (players: Player seq) : CountryTierStats list =
    printfn "\n\n--- 🌍📊 Country-Specific Rating Tier Analysis (UR vs FIDE) ---\n"

    let validPlayers =
        players
        |> Seq.filter (fun p ->
            p.Country <> ""
            && p.UniversalRating > config.MinRating
            && p.FideRating > config.MinRating)
        |> Seq.toList

    printfn $"Processing {validPlayers.Length} players with valid ratings > {config.MinRating} and countries..."

    if validPlayers.Length = 0 then
        printfn "No players met the criteria. Analysis cannot continue."
        []
    else
        let ratingTiers =
            [ ("2500+", 2500, None)
              ("2400-2499", 2400, Some 2499)
              ("2300-2399", 2300, Some 2399)
              ("2200-2299", 2200, Some 2299)
              ("2100-2199", 2100, Some 2199)
              ("2000-2099", 2000, Some 2099)
              ("1900-1999", 1900, Some 1999)
              ("1800-1899", 1800, Some 1899)
              ("1700-1799", 1700, Some 1799)
              ("1600-1699", 1600, Some 1699)
              ("1500-1599", 1500, Some 1599)
              ("1400-1499", 1400, Some 1499)
              ("1300-1399", 1300, Some 1399)
              ("1200-1299", 1200, Some 1299) ]

        let countryTierStats =
            validPlayers
            |> List.groupBy (fun p -> p.Country)
            |> List.filter (fun (_, countryPlayers) -> countryPlayers.Length >= 10) // Min players per country (reduced from 50)
            |> List.collect (fun (country, countryPlayers) ->
                ratingTiers
                |> List.choose (fun (tierName, minRating, maxRatingOpt) ->
                    let playersInTier =
                        countryPlayers
                        |> List.filter (fun p ->
                            p.FideRating >= minRating
                            && (match maxRatingOpt with
                                | Some maxRating -> p.FideRating <= maxRating
                                | None -> true))

                    if playersInTier.Length >= 3 then // Minimum players for meaningful tier analysis (reduced from 5)
                        let gaps = playersInTier |> List.map (fun p -> p.UniversalRating - p.FideRating)
                        let fideRatings = playersInTier |> List.map (fun p -> p.FideRating)
                        let urRatings = playersInTier |> List.map (fun p -> p.UniversalRating)

                        let avgGap = List.averageBy float gaps
                        let medianGap = calculateMedian gaps
                        let variance = gaps |> List.averageBy (fun g -> (float g - avgGap) ** 2.0)
                        let stdDev = sqrt variance
                        let avgFideRating = List.averageBy float fideRatings
                        let avgUrRating = List.averageBy float urRatings
                        let deflationPercent = (avgGap / avgFideRating) * 100.0

                        Some
                            { Country = country
                              TierName = tierName
                              MinRating = minRating
                              MaxRating = maxRatingOpt
                              PlayerCount = playersInTier.Length
                              AvgGap = avgGap
                              MedianGap = medianGap
                              StdDev = stdDev
                              AvgFideRating = avgFideRating
                              AvgUrRating = avgUrRating
                              DeflationPercent = deflationPercent }
                    else
                        None))

        printfn $"Analysis complete for {countryTierStats.Length} country-tier combinations.\n"

        let topDeflatedByTier =
            ratingTiers
            |> List.choose (fun (tierName, _, _) ->
                let tierData = countryTierStats |> List.filter (fun s -> s.TierName = tierName)

                if not tierData.IsEmpty then
                    let mostDeflated = tierData |> List.maxBy (fun s -> s.AvgGap)
                    let leastDeflated = tierData |> List.minBy (fun s -> s.AvgGap)
                    Some(tierName, mostDeflated, leastDeflated, tierData.Length)
                else
                    None)

        printfn "--- Most/Least Deflated Countries by Rating Tier ---"
        printfn "%-12s | %-15s %-8s | %-15s %-8s | Countries" "Tier" "Most Deflated" "(Gap)" "Least Deflated" "(Gap)"
        printfn "%s" (String.replicate 85 "-")

        topDeflatedByTier
        |> List.iter (fun (tierName, mostDeflated, leastDeflated, countryCount) ->
            printfn
                $"%-12s{tierName} | %-15s{mostDeflated.Country} (%+6.1f{mostDeflated.AvgGap}) | %-15s{leastDeflated.Country} (%+6.1f{leastDeflated.AvgGap}) | %d{countryCount}")

        let extremeDeflations =
            countryTierStats
            |> List.sortByDescending (fun s -> abs s.DeflationPercent)
            |> List.take (min 20 countryTierStats.Length)

        printfn "\n--- Top 20 Most Extreme Deflation/Inflation Cases ---"

        printfn
            "%-12s %-12s | %-8s %-8s %-8s | %-8s %-8s %-10s"
            "Country"
            "Tier"
            "Players"
            "Avg Gap"
            "Med Gap"
            "Avg FIDE"
            "Avg UR"
            "Deflation%"

        printfn "%s" (String.replicate 95 "-")

        extremeDeflations
        |> List.iter (fun stats ->
            printfn
                $"%-12s{stats.Country} %-12s{stats.TierName} | %-8d{stats.PlayerCount} %-+8.1f{stats.AvgGap} %-+8.1f{stats.MedianGap} | %-8.1f{stats.AvgFideRating} %-8.1f{stats.AvgUrRating} %-+10.2f{stats.DeflationPercent}%%")

        countryTierStats

let plotRatingTiers (tierStats: RatingTierStats list) =
    if tierStats.Length = 0 then
        printfn "No tier data available for plotting."
    else
        let tierNames = tierStats |> List.map (fun s -> s.TierName)
        let avgGaps = tierStats |> List.map (fun s -> s.AvgGap)
        let medianGaps = tierStats |> List.map (fun s -> s.MedianGap)
        let playerCounts = tierStats |> List.map (fun s -> float s.PlayerCount)

        let avgGapTrace =
            Bar(x = tierNames, y = avgGaps, name = "Average Gap", marker = Marker(color = "rgba(55, 128, 191, 0.8)"))

        let medianGapTrace =
            Bar(x = tierNames, y = medianGaps, name = "Median Gap", marker = Marker(color = "rgba(255, 128, 64, 0.8)"))

        let layout =
            Layout(
                title = "Rating Deflation/Inflation by Rating Tier (UR vs FIDE)",
                xaxis = Xaxis(title = "Rating Tier", tickangle = -45),
                yaxis = Yaxis(title = "Rating Gap (UR - FIDE)"),
                barmode = "group",
                showlegend = true
            )

        let chart = [ avgGapTrace; medianGapTrace ] |> Chart.Plot |> Chart.WithLayout layout
        chart.Show()
        printfn "\nTier analysis chart created and opened in browser."

let plotCountryTierAnalysis (countryTierStats: CountryTierStats list) =
    if countryTierStats.Length = 0 then
        printfn "No country-tier data available for plotting."
    else
        let extremeCases =
            countryTierStats
            |> List.sortByDescending (fun s -> abs s.DeflationPercent)
            |> List.take (min 15 countryTierStats.Length)

        let labels = extremeCases |> List.map (fun s -> $"{s.Country}-{s.TierName}")
        let deflationPercents = extremeCases |> List.map (fun s -> s.DeflationPercent)
        let avgGaps = extremeCases |> List.map (fun s -> s.AvgGap)

        let deflationTrace =
            Bar(
                x = labels,
                y = deflationPercents,
                name = "Deflation %",
                marker = Marker(color = "rgba(255, 99, 132, 0.8)")
            )

        let layout =
            Layout(
                title = "Most Extreme Rating Deflation/Inflation by Country-Tier",
                xaxis = Xaxis(title = "Country-Tier", tickangle = -45),
                yaxis = Yaxis(title = "Deflation Percentage (%)"),
                showlegend = true
            )

        let chart = [ deflationTrace ] |> Chart.Plot |> Chart.WithLayout layout
        chart.Show()
        printfn "\nCountry-tier analysis chart created and opened in browser."

let plotCountryRatings (countryData: CountryStats list) =
    if countryData.Length = 0 then
        printfn "No data available for plotting."
    else
        let sortedData =
            countryData |> List.sortByDescending (fun stats -> stats.EliteAvgGap)

        let top10 = sortedData |> List.take (min 10 sortedData.Length)

        let bottom10 =
            sortedData |> List.rev |> List.take (min 10 sortedData.Length) |> List.rev

        let combinedData = top10 @ bottom10

        let countries = combinedData |> List.map (fun stats -> stats.Country)
        let eliteAvgGaps = combinedData |> List.map (fun stats -> stats.EliteAvgGap)
        let eliteMedianGaps = combinedData |> List.map (fun stats -> stats.EliteMedianGap)
        let overallAvgGaps = combinedData |> List.map (fun stats -> stats.OverallAvgGap)

        let eliteAvgTrace =
            Bar(
                x = countries,
                y = eliteAvgGaps,
                name = "Elite Avg Gap",
                marker = Marker(color = "rgba(55, 128, 191, 0.8)")
            )

        let eliteMedianTrace =
            Bar(
                x = countries,
                y = eliteMedianGaps,
                name = "Elite Median Gap",
                marker = Marker(color = "rgba(255, 128, 64, 0.8)")
            )

        let overallAvgTrace =
            Bar(
                x = countries,
                y = overallAvgGaps,
                name = "Overall Avg Gap",
                marker = Marker(color = "rgba(128, 255, 128, 0.6)")
            )

        let layout =
            Layout(
                title = "Country Rating Analysis: Elite vs Overall Gaps (UR vs FIDE)",
                xaxis = Xaxis(title = "Country"),
                yaxis = Yaxis(title = "Rating Gap (UR - FIDE)"),
                barmode = "group",
                showlegend = true
            )

        let chart =
            [ eliteAvgTrace; eliteMedianTrace; overallAvgTrace ]
            |> Chart.Plot
            |> Chart.WithLayout layout

        chart.Show()

        printfn
            "\nChart created and opened in browser. Showing top 10 and bottom 10 countries by elite average rating gap."

let findStrategicOpponents (players: Player seq) (referencePlayerId: int) (ratingWindow: int) (minPlayersPerCountry: int) (topCountriesCount: int) (topOpponentsCount: int) : StrategicOpponentsResult option =
    let playersArray = players |> Array.ofSeq
    
    match playersArray |> Array.tryFind (fun p -> p.FideId = referencePlayerId) with
    | None -> 
        printfn $"Player with FIDE ID {referencePlayerId} not found."
        None
    | Some referencePlayer ->
        printfn $"Analyzing strategic opponents for {referencePlayer.Name} (FIDE: {referencePlayer.FideRating}, UR: {referencePlayer.UniversalRating})"
        
        let targetMinRating = referencePlayer.FideRating
        let targetMaxRating = referencePlayer.FideRating + ratingWindow
        
        printfn $"Target rating window: {targetMinRating} - {targetMaxRating}"
        
        let validPlayers = 
            playersArray
            |> Array.filter (fun p -> 
                p.Country <> "" 
                && p.FideId <> referencePlayerId
                && p.FideRating > 0 
                && p.UniversalRating > 0)
        
        let countryInflation = 
            validPlayers
            |> Array.groupBy (fun p -> p.Country)
            |> Array.filter (fun (_, players) -> players.Length >= minPlayersPerCountry)
            |> Array.map (fun (country, countryPlayers) ->
                let avgGap = countryPlayers |> Array.averageBy (fun p -> float (p.UniversalRating - p.FideRating))
                { Country = country
                  PlayerCount = countryPlayers.Length  
                  AvgRatingGap = avgGap })
            |> Array.filter (fun ci -> ci.AvgRatingGap < 0.0)
            |> Array.sortBy (fun ci -> ci.AvgRatingGap)
        
        let favorableCountriesCount = countryInflation.Length
        let topFavorableCountries = 
            countryInflation
            |> Array.take (min topCountriesCount favorableCountriesCount)
            |> List.ofArray
        
        if topFavorableCountries.IsEmpty then
            printfn "No countries found with negative rating gaps (overrated federations)."
            Some { ReferencePlayerId = referencePlayerId
                   TargetMinRating = targetMinRating
                   TargetMaxRating = targetMaxRating  
                   TopFavorableCountries = []
                   TopOpponents = [] }
        else
            printfn $"Found {topFavorableCountries.Length} favorable countries with overrated players:"
            topFavorableCountries 
            |> List.iter (fun ci -> printfn $"  {ci.Country}: {ci.PlayerCount} players, avg gap: {ci.AvgRatingGap:F1}")
            
            let favorableCountries = topFavorableCountries |> List.map (fun ci -> ci.Country) |> Set.ofList
            
            let potentialOpponents = 
                validPlayers
                |> Array.filter (fun p -> 
                    favorableCountries.Contains(p.Country)
                    && p.FideRating >= targetMinRating 
                    && p.FideRating <= targetMaxRating)
                |> Array.map (fun p -> 
                    { FideId = p.FideId
                      Name = p.Name
                      Country = p.Country
                      FideRating = p.FideRating
                      UniversalRating = p.UniversalRating
                      RatingGap = p.UniversalRating - p.FideRating
                      Title = p.Title })
                |> Array.sortBy (fun op -> op.RatingGap)
                |> Array.take (min topOpponentsCount (Array.length (Array.filter (fun p -> favorableCountries.Contains(p.Country) && p.FideRating >= targetMinRating && p.FideRating <= targetMaxRating) validPlayers)))
                |> List.ofArray
            
            printfn $"Found {potentialOpponents.Length} strategic opponents in target rating range:"
            potentialOpponents 
            |> List.iteri (fun i op -> 
                printfn $"  {i+1}. {op.Name} ({op.Country}) - FIDE: {op.FideRating}, UR: {op.UniversalRating}, Gap: {op.RatingGap}")
            
            Some { ReferencePlayerId = referencePlayerId
                   TargetMinRating = targetMinRating
                   TargetMaxRating = targetMaxRating
                   TopFavorableCountries = topFavorableCountries  
                   TopOpponents = potentialOpponents }

[<EntryPoint>]
let main _ =
    try
        let config =
            { MinRating = 1200
              MinPlayersInCountry = 500
              ElitePercentage = 0.10
              CachedPlayersJsonPath = "players_cache.json"
              PlayersXmlPath = "players.xml"
              RatingsCsvPath = "ratings.csv" }

        let players =
            if jsonExists config then
                printfn "JSON cache exists, loading players from JSON..."
                loadFromJson config
            else
                printfn "JSON cache not found, loading and merging player data from source files..."
                let mergedPlayers = mergePlayerDataAsync config |> Async.RunSynchronously

                printfn "Saving to JSON cache..."
                saveToJson config mergedPlayers
                mergedPlayers

        printfn
            $"Configuration: Min Rating = {config.MinRating}, Min Players per Country = {config.MinPlayersInCountry}, Elite Percentage = {config.ElitePercentage * 100.0}%%"

        // --- Execute and Display Analysis ---
        displayTopUniversalRatings players
        let countryData = analyzeAndDisplayCountryRatings config players
        plotCountryRatings countryData

        // --- Rating Tier Analysis ---
        let tierData = analyzeRatingTiers config players
        plotRatingTiers tierData

        // --- Country-Tier Analysis ---
        let countryTierData = analyzeCountryRatingTiers config players
        // plotCountryTierAnalysis countryTierData

        // --- Strategic Opponent Analysis Example ---
        // Find a player to use as example (first player with FIDE rating > 2000)
        let examplePlayer = 
            players 
            |> Array.tryFind (fun p -> p.FideId = 26065843)
        
        match examplePlayer with
        | Some player ->
            printfn $"\n\n--- 🎯 Strategic Opponent Analysis Example ---"
            let strategicResult = findStrategicOpponents players player.FideId 150 50 5 5
            match strategicResult with
            | Some result ->
                printfn "\n--- Summary ---"
                printfn $"Reference Player: {player.Name} (ID: {result.ReferencePlayerId})"
                printfn $"Target Rating Range: {result.TargetMinRating} - {result.TargetMaxRating}"
                printfn $"Favorable Countries Found: {result.TopFavorableCountries.Length}"
                printfn $"Strategic Opponents Found: {result.TopOpponents.Length}"
            | None -> printfn "Strategic analysis failed."
        | None -> printfn "No suitable example player found for strategic analysis."

        printfn "Data processing completed successfully!"
        0
    with ex ->
        printfn $"Error: %s{ex.Message}"
        1
