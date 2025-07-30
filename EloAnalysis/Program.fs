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

type AgeGroupStats =
    { Age: int
      PlayerCount: int
      AvgFideRating: float
      AvgUniversalRating: float
      AvgRatingGap: float
      MedianFideRating: float
      MedianUniversalRating: float }

type GenerationComparison =
    { YouthCount: int
      VeteranCount: int
      YouthAvgGap: float
      VeteranAvgGap: float
      YouthAvgFide: float
      VeteranAvgFide: float
      YouthAvgUR: float
      VeteranAvgUR: float
      GapDifference: float }

type ProdigyPlayer =
    { FideId: int
      Name: string
      Age: int
      Country: string
      FideRating: int
      UniversalRating: int
      RatingGap: int
      Title: string
      WorldRank: int }

type TimeControlSpecialist =
    { FideId: int
      Name: string
      Country: string
      ClassicalRating: int
      RapidRating: int
      BlitzRating: int
      RapidDiff: int // Rapid - Classical
      BlitzDiff: int // Blitz - Classical
      SpecializationType: string
      Title: string }

type CountryTimeControlStats =
    { Country: string
      PlayerCount: int
      AvgClassical: float
      AvgRapid: float
      AvgBlitz: float
      AvgRapidDiff: float
      AvgBlitzDiff: float
      RapidSpecialists: int
      BlitzSpecialists: int
      ClassicalSpecialists: int }

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
                          Title = p.Title.XElement.Value
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

let calculateAge (birthday: int) : int option =
    if birthday <= 0 then None
    else
        let currentYear = System.DateTime.Now.Year
        let birthYear = birthday
        if birthYear > 1900 && birthYear <= currentYear then
            Some (currentYear - birthYear)
        else None

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
            |> List.filter (fun (_, countryPlayers) -> countryPlayers.Length >= config.MinPlayersInCountry) // Min players per country (reduced from 50)
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

let analyzePeakPerformanceAge (config: AnalysisConfig) (players: Player seq) : AgeGroupStats list =
    printfn "\n\n--- 🎂 Peak Performance Age Analysis ---\n"
    
    let playersWithAge = 
        players
        |> Seq.choose (fun p ->
            match calculateAge p.Birthday with
            | Some age when age >= 10 && age <= 80 && p.FideRating > config.MinRating && p.UniversalRating > config.MinRating ->
                Some (age, p.FideRating, p.UniversalRating, p.UniversalRating - p.FideRating)
            | _ -> None)
        |> List.ofSeq
    
    printfn $"Processing {playersWithAge.Length} players with valid ages and ratings > {config.MinRating}..."
    
    if playersWithAge.IsEmpty then
        printfn "No players with valid age data found."
        []
    else
        let ageGroups = 
            playersWithAge
            |> List.groupBy (fun (age, _, _, _) -> age)
            |> List.filter (fun (_, players) -> players.Length >= 10) // Minimum players for meaningful analysis
            |> List.map (fun (age, agePlayerData) ->
                let fideRatings = agePlayerData |> List.map (fun (_, fide, _, _) -> fide)
                let urRatings = agePlayerData |> List.map (fun (_, _, ur, _) -> ur)
                let gaps = agePlayerData |> List.map (fun (_, _, _, gap) -> gap)
                
                { Age = age
                  PlayerCount = agePlayerData.Length
                  AvgFideRating = List.averageBy float fideRatings
                  AvgUniversalRating = List.averageBy float urRatings
                  AvgRatingGap = List.averageBy float gaps
                  MedianFideRating = calculateMedian fideRatings
                  MedianUniversalRating = calculateMedian urRatings })
            |> List.sortBy (fun stats -> stats.Age)
        
        printfn $"Analysis complete for {ageGroups.Length} age groups with sufficient players.\n"
        
        printfn "%-4s %-8s | %-10s %-10s %-10s | %-10s %-10s" "Age" "Players" "Avg FIDE" "Avg UR" "Avg Gap" "Med FIDE" "Med UR"
        printfn "%s" (String.replicate 80 "-")
        
        ageGroups
        |> List.iter (fun stats ->
            printfn $"%-4d{stats.Age} %-8d{stats.PlayerCount} | %-10.1f{stats.AvgFideRating} %-10.1f{stats.AvgUniversalRating} %-10.1f{stats.AvgRatingGap} | %-10.1f{stats.MedianFideRating} %-10.1f{stats.MedianUniversalRating}")
        
        if not ageGroups.IsEmpty then
            let peakFideAge = ageGroups |> List.maxBy (fun s -> s.AvgFideRating)
            let peakURAge = ageGroups |> List.maxBy (fun s -> s.AvgUniversalRating)
            let mostAccurateAge = ageGroups |> List.minBy (fun s -> abs s.AvgRatingGap)
            
            printfn "\n--- Key Insights ---"
            printfn $"• Peak FIDE Rating Age: {peakFideAge.Age} years (Avg: {peakFideAge.AvgFideRating:F1})"
            printfn $"• Peak Universal Rating Age: {peakURAge.Age} years (Avg: {peakURAge.AvgUniversalRating:F1})"
            printfn $"• Most Accurate Ratings Age: {mostAccurateAge.Age} years (Gap: {mostAccurateAge.AvgRatingGap:F1})"
            
            // Identify typical career phases
            let youth = ageGroups |> List.filter (fun s -> s.Age <= 20)
            let prime = ageGroups |> List.filter (fun s -> s.Age > 20 && s.Age <= 35)
            let veteran = ageGroups |> List.filter (fun s -> s.Age > 35)
            
            if not youth.IsEmpty && not prime.IsEmpty && not veteran.IsEmpty then
                let youthAvg = youth |> List.averageBy (fun s -> s.AvgFideRating)
                let primeAvg = prime |> List.averageBy (fun s -> s.AvgFideRating)
                let veteranAvg = veteran |> List.averageBy (fun s -> s.AvgFideRating)
                
                printfn $"\n--- Career Phase Analysis ---"
                printfn $"• Youth (≤20): Avg FIDE {youthAvg:F1}"
                printfn $"• Prime (21-35): Avg FIDE {primeAvg:F1}"
                printfn $"• Veteran (>35): Avg FIDE {veteranAvg:F1}"
        
        ageGroups

let analyzeYouthVsVeterans (config: AnalysisConfig) (players: Player seq) : GenerationComparison option =
    printfn "\n\n--- 👶 Youth vs Veterans Analysis (Rating Accuracy) ---\n"
    
    let playersWithAge = 
        players
        |> Seq.choose (fun p ->
            match calculateAge p.Birthday with
            | Some age when p.FideRating > config.MinRating && p.UniversalRating > config.MinRating ->
                Some (age, p.FideRating, p.UniversalRating, p.UniversalRating - p.FideRating)
            | _ -> None)
        |> List.ofSeq
    
    if playersWithAge.IsEmpty then
        printfn "No players with valid age and rating data found."
        None
    else
        let youth = playersWithAge |> List.filter (fun (age, _, _, _) -> age < 21)
        let veterans = playersWithAge |> List.filter (fun (age, _, _, _) -> age > 40)
        
        if youth.IsEmpty || veterans.IsEmpty then
            printfn "Insufficient data for youth or veteran groups."
            None
        else
            let youthFide = youth |> List.map (fun (_, fide, _, _) -> fide)
            let youthUR = youth |> List.map (fun (_, _, ur, _) -> ur)
            let youthGaps = youth |> List.map (fun (_, _, _, gap) -> gap)
            
            let veteranFide = veterans |> List.map (fun (_, fide, _, _) -> fide)
            let veteranUR = veterans |> List.map (fun (_, _, ur, _) -> ur)
            let veteranGaps = veterans |> List.map (fun (_, _, _, gap) -> gap)
            
            let comparison = {
                YouthCount = youth.Length
                VeteranCount = veterans.Length
                YouthAvgGap = List.averageBy float youthGaps
                VeteranAvgGap = List.averageBy float veteranGaps
                YouthAvgFide = List.averageBy float youthFide
                VeteranAvgFide = List.averageBy float veteranFide
                YouthAvgUR = List.averageBy float youthUR
                VeteranAvgUR = List.averageBy float veteranUR
                GapDifference = (List.averageBy float youthGaps) - (List.averageBy float veteranGaps)
            }
            
            printfn $"Youth Players (<21): {comparison.YouthCount:N0} players"
            printfn $"Veteran Players (>40): {comparison.VeteranCount:N0} players"
            printfn ""
            printfn "%-12s | %-10s %-10s %-10s" "Group" "Avg FIDE" "Avg UR" "Avg Gap"
            printfn "%s" (String.replicate 50 "-")
            let youthLabel = "Youth (<21)"
            let veteranLabel = "Veterans (>40)"
            printfn $"%-12s{youthLabel} | %-10.1f{comparison.YouthAvgFide} %-10.1f{comparison.YouthAvgUR} %-10.1f{comparison.YouthAvgGap}"
            printfn $"%-12s{veteranLabel} | %-10.1f{comparison.VeteranAvgFide} %-10.1f{comparison.VeteranAvgUR} %-10.1f{comparison.VeteranAvgGap}"
            
            printfn "\n--- Analysis ---"
            if abs comparison.GapDifference < 5.0 then
                printfn $"• Gap Difference: {comparison.GapDifference:F1} - Ratings are similarly accurate across generations"
            elif comparison.GapDifference > 0.0 then
                printfn $"• Gap Difference: {comparison.GapDifference:F1} - Youth have MORE accurate ratings (higher UR relative to FIDE)"
            else
                printfn $"• Gap Difference: {comparison.GapDifference:F1} - Veterans have MORE accurate ratings (higher UR relative to FIDE)"
            
            if comparison.YouthAvgFide > comparison.VeteranAvgFide then
                printfn $"• Youth average FIDE rating ({comparison.YouthAvgFide:F1}) is higher than veterans ({comparison.VeteranAvgFide:F1})"
            else
                printfn $"• Veterans average FIDE rating ({comparison.VeteranAvgFide:F1}) is higher than youth ({comparison.YouthAvgFide:F1})"
            
            Some comparison

let generateProdigyWatchlist (config: AnalysisConfig) (players: Player seq) (maxAge: int) (topCount: int) : ProdigyPlayer list =
    printfn $"\n\n--- 🌟 Prodigy Watchlist (Top {topCount} Players Under {maxAge}) ---\n"
    
    let prodigies = 
        players
        |> Seq.choose (fun p ->
            match calculateAge p.Birthday with
            | Some age when age < maxAge && p.FideRating > config.MinRating && p.UniversalRating > config.MinRating ->
                Some { FideId = p.FideId
                       Name = p.Name
                       Age = age
                       Country = p.Country
                       FideRating = p.FideRating
                       UniversalRating = p.UniversalRating
                       RatingGap = p.UniversalRating - p.FideRating
                       Title = p.Title
                       WorldRank = p.WorldRank }
            | _ -> None)
        |> Seq.sortByDescending (fun p -> p.FideRating)
        |> Seq.take topCount
        |> List.ofSeq
    
    if prodigies.IsEmpty then
        printfn $"No players under {maxAge} found with ratings > {config.MinRating}."
        []
    else
        printfn $"Found {prodigies.Length} top young players:\n"
        printfn "%-4s %-25s %-3s %-4s %-5s %-5s %-5s %-8s %-10s" "Rank" "Name" "Age" "Ctry" "FIDE" "UR" "Gap" "Title" "World Rank"
        printfn "%s" (String.replicate 85 "-")
        
        prodigies
        |> List.iteri (fun i p ->
            let worldRankStr = if p.WorldRank > 0 then $"#{p.WorldRank}" else "Unranked"
            printfn $"%-4d{i+1} %-25s{p.Name} %-3d{p.Age} %-4s{p.Country} %-5d{p.FideRating} %-5d{p.UniversalRating} %-5d{p.RatingGap} %-8s{p.Title} %-10s{worldRankStr}")
        
        let avgAge = prodigies |> List.averageBy (fun p -> float p.Age)
        let avgFide = prodigies |> List.averageBy (fun p -> float p.FideRating)
        let avgGap = prodigies |> List.averageBy (fun p -> float p.RatingGap)
        
        printfn $"\n--- Prodigy Statistics ---"
        printfn $"• Average Age: {avgAge:F1} years"
        printfn $"• Average FIDE Rating: {avgFide:F1}"
        printfn $"• Average Rating Gap: {avgGap:F1}"
        
        let topCountries = 
            prodigies 
            |> List.groupBy (fun p -> p.Country)
            |> List.sortByDescending (fun (_, players) -> players.Length)
            |> List.take 3
        
        let countryList = topCountries |> List.map (fun (country, players) -> sprintf "%s (%d)" country players.Length) |> String.concat ", "
        printfn $"• Top Countries: {countryList}"
        
        prodigies

let identifyTimeControlSpecialists (config: AnalysisConfig) (players: Player seq) (topCount: int) : TimeControlSpecialist list * TimeControlSpecialist list * TimeControlSpecialist list =
    printfn "\n\n--- ⚡ Time Control Specialists Analysis ---\n"
    
    let validPlayers = 
        players
        |> Seq.filter (fun p -> 
            p.FideRating >= config.MinRating 
            && p.FideRapidRating >= config.MinRating 
            && p.FideBlitzRating >= config.MinRating
            && p.FideRating > 0 
            && p.FideRapidRating > 0 
            && p.FideBlitzRating > 0)
        |> Seq.map (fun p ->
            let rapidDiff = p.FideRapidRating - p.FideRating
            let blitzDiff = p.FideBlitzRating - p.FideRating
            
            let specialization = 
                if abs rapidDiff > abs blitzDiff && rapidDiff > 50 then "Rapid Specialist"
                elif abs blitzDiff > abs rapidDiff && blitzDiff > 50 then "Blitz Specialist"
                elif p.FideRating > p.FideRapidRating && p.FideRating > p.FideBlitzRating && (p.FideRating - max p.FideRapidRating p.FideBlitzRating) > 50 then "Classical Specialist"
                else "Balanced"
            
            { FideId = p.FideId
              Name = p.Name
              Country = p.Country
              ClassicalRating = p.FideRating
              RapidRating = p.FideRapidRating
              BlitzRating = p.FideBlitzRating
              RapidDiff = rapidDiff
              BlitzDiff = blitzDiff
              SpecializationType = specialization
              Title = p.Title })
        |> List.ofSeq
    
    printfn $"Processing %d{validPlayers.Length} players with valid ratings across all time controls..."
    
    let rapidSpecialists = 
        validPlayers
        |> List.filter (fun p -> p.SpecializationType = "Rapid Specialist")
        |> List.sortByDescending (fun p -> p.RapidDiff)
        |> List.take (min topCount (validPlayers |> List.filter (fun p -> p.SpecializationType = "Rapid Specialist") |> List.length))
    
    let blitzSpecialists = 
        validPlayers
        |> List.filter (fun p -> p.SpecializationType = "Blitz Specialist")
        |> List.sortByDescending (fun p -> p.BlitzDiff)
        |> List.take (min topCount (validPlayers |> List.filter (fun p -> p.SpecializationType = "Blitz Specialist") |> List.length))
    
    let classicalSpecialists = 
        validPlayers
        |> List.filter (fun p -> p.SpecializationType = "Classical Specialist")
        |> List.sortByDescending (fun p -> p.ClassicalRating - max p.RapidRating p.BlitzRating)
        |> List.take (min topCount (validPlayers |> List.filter (fun p -> p.SpecializationType = "Classical Specialist") |> List.length))
    
    printfn $"\n--- TOP %d{topCount} RAPID SPECIALISTS ---"
    printfn "%-25s %-4s %-6s %-6s %-6s %-5s %-8s" "Name" "Ctry" "Class" "Rapid" "Blitz" "R-Diff" "Title"
    printfn "%s" (String.replicate 70 "-")
    rapidSpecialists
    |> List.iteri (fun i p ->
        printfn $"%-25s{p.Name} %-4s{p.Country} %-6d{p.ClassicalRating} %-6d{p.RapidRating} %-6d{p.BlitzRating} +%-4d{p.RapidDiff} %-8s{p.Title}")
    
    printfn $"\n--- TOP %d{topCount} BLITZ SPECIALISTS ---"
    printfn "%-25s %-4s %-6s %-6s %-6s %-5s %-8s" "Name" "Ctry" "Class" "Rapid" "Blitz" "B-Diff" "Title"
    printfn "%s" (String.replicate 70 "-")
    blitzSpecialists
    |> List.iteri (fun i p ->        printfn $"%-25s{p.Name} %-4s{p.Country} %-6d{p.ClassicalRating} %-6d{p.RapidRating} %-6d{p.BlitzRating} +%-4d{p.BlitzDiff} %-8s{p.Title}")
    
    printfn $"\n--- TOP %d{topCount} CLASSICAL SPECIALISTS ---"
    printfn "%-25s %-4s %-6s %-6s %-6s %-5s %-8s" "Name" "Ctry" "Class" "Rapid" "Blitz" "C-Adv" "Title"
    printfn "%s" (String.replicate 70 "-")
    classicalSpecialists
    |> List.iteri (fun i p ->
        let classicalAdv = p.ClassicalRating - max p.RapidRating p.BlitzRating
        printfn $"%-25s{p.Name} %-4s{p.Country} %-6d{p.ClassicalRating} %-6d{p.RapidRating} %-6d{p.BlitzRating} +%-4d{classicalAdv} %-8s{p.Title}")
    
    let totalSpecialists = rapidSpecialists.Length + blitzSpecialists.Length + classicalSpecialists.Length
    let balanced = validPlayers.Length - (validPlayers |> List.filter (fun p -> p.SpecializationType <> "Balanced") |> List.length)
    
    printfn "\n--- SPECIALIZATION SUMMARY ---"
    printfn $"• Total Players Analyzed: %d{validPlayers.Length}"
    printfn "• Rapid Specialists: %d" (validPlayers |> List.filter (fun p -> p.SpecializationType = "Rapid Specialist") |> List.length)
    printfn "• Blitz Specialists: %d" (validPlayers |> List.filter (fun p -> p.SpecializationType = "Blitz Specialist") |> List.length)
    printfn "• Classical Specialists: %d" (validPlayers |> List.filter (fun p -> p.SpecializationType = "Classical Specialist") |> List.length)
    printfn $"• Balanced Players: %d{balanced}"
    
    (rapidSpecialists, blitzSpecialists, classicalSpecialists)

let analyzeNationalTimeControlStyles (config: AnalysisConfig) (players: Player seq) : CountryTimeControlStats list =
    printfn "\n\n--- 🌍 National Time Control Analysis ---\n"
    
    let validPlayers = 
        players
        |> Seq.filter (fun p -> 
            p.Country <> ""
            && p.FideRating >= config.MinRating 
            && p.FideRapidRating >= config.MinRating 
            && p.FideBlitzRating >= config.MinRating
            && p.FideRating > 0 
            && p.FideRapidRating > 0 
            && p.FideBlitzRating > 0)
        |> List.ofSeq
    
    let countryStats = 
        validPlayers
        |> List.groupBy (fun p -> p.Country)
        |> List.filter (fun (_, players) -> players.Length >= config.MinPlayersInCountry) // Minimum players for meaningful analysis
        |> List.map (fun (country, countryPlayers) ->
            let classicalRatings = countryPlayers |> List.map (fun p -> p.FideRating)
            let rapidRatings = countryPlayers |> List.map (fun p -> p.FideRapidRating)
            let blitzRatings = countryPlayers |> List.map (fun p -> p.FideBlitzRating)
            let rapidDiffs = countryPlayers |> List.map (fun p -> p.FideRapidRating - p.FideRating)
            let blitzDiffs = countryPlayers |> List.map (fun p -> p.FideBlitzRating - p.FideRating)
            
            let rapidSpecCount = countryPlayers |> List.filter (fun p -> (p.FideRapidRating - p.FideRating) > 50) |> List.length
            let blitzSpecCount = countryPlayers |> List.filter (fun p -> (p.FideBlitzRating - p.FideRating) > 50) |> List.length
            let classicalSpecCount = countryPlayers |> List.filter (fun p -> p.FideRating > p.FideRapidRating && p.FideRating > p.FideBlitzRating && (p.FideRating - max p.FideRapidRating p.FideBlitzRating) > 50) |> List.length
            
            { Country = country
              PlayerCount = countryPlayers.Length
              AvgClassical = List.averageBy float classicalRatings
              AvgRapid = List.averageBy float rapidRatings
              AvgBlitz = List.averageBy float blitzRatings
              AvgRapidDiff = List.averageBy float rapidDiffs
              AvgBlitzDiff = List.averageBy float blitzDiffs
              RapidSpecialists = rapidSpecCount
              BlitzSpecialists = blitzSpecCount
              ClassicalSpecialists = classicalSpecCount })
        |> List.sortByDescending (fun s -> s.AvgBlitzDiff + s.AvgRapidDiff) // Countries best at speed chess relative to classical
    
    printfn $"Processing %d{countryStats.Length} countries with at least 50 players across all time controls..."
    printfn ""
    printfn "%-12s %-6s | %-6s %-6s %-6s | %-6s %-6s | %-4s %-4s %-4s" "Country" "Players" "Class" "Rapid" "Blitz" "R-Diff" "B-Diff" "R-Sp" "B-Sp" "C-Sp"
    printfn "%s" (String.replicate 85 "-")
    
    countryStats
    |> List.iter (fun stats ->
        printfn $"%-12s{stats.Country} %-6d{stats.PlayerCount} | %-6.0f{stats.AvgClassical} %-6.0f{stats.AvgRapid} %-6.0f{stats.AvgBlitz} | %+6.1f{stats.AvgRapidDiff} %+6.1f{stats.AvgBlitzDiff} | %-4d{stats.RapidSpecialists} %-4d{stats.BlitzSpecialists} %-4d{stats.ClassicalSpecialists}")
    
    if not countryStats.IsEmpty then
        let bestSpeedCountry = countryStats |> List.maxBy (fun s -> s.AvgBlitzDiff + s.AvgRapidDiff)
        let bestClassicalCountry = countryStats |> List.minBy (fun s -> s.AvgBlitzDiff + s.AvgRapidDiff)
        let mostRapidSpecialists = countryStats |> List.maxBy (fun s -> float s.RapidSpecialists / float s.PlayerCount)
        let mostBlitzSpecialists = countryStats |> List.maxBy (fun s -> float s.BlitzSpecialists / float s.PlayerCount)
        
        printfn "\n--- KEY INSIGHTS ---"
        printfn $"• Best Speed Chess Nation: %s{bestSpeedCountry.Country} (Combined diff: %+.1f{bestSpeedCountry.AvgRapidDiff + bestSpeedCountry.AvgBlitzDiff})"
        printfn $"• Strongest Classical Nation: %s{bestClassicalCountry.Country} (Combined diff: %+.1f{bestClassicalCountry.AvgRapidDiff + bestClassicalCountry.AvgBlitzDiff})"
        printfn "• Most Rapid-Oriented: %s (%.1f%% rapid specialists)" mostRapidSpecialists.Country (100.0 * float mostRapidSpecialists.RapidSpecialists / float mostRapidSpecialists.PlayerCount)
        printfn "• Most Blitz-Oriented: %s (%.1f%% blitz specialists)" mostBlitzSpecialists.Country (100.0 * float mostBlitzSpecialists.BlitzSpecialists / float mostBlitzSpecialists.PlayerCount)
    
    countryStats

let plotTimeControlAnalysis (countryStats: CountryTimeControlStats list) =
    if countryStats.Length = 0 then
        printfn "No time control data available for plotting."
    else
        let top15 = countryStats |> List.take (min 15 countryStats.Length)
        
        let countries = top15 |> List.map (fun s -> s.Country)
        let rapidDiffs = top15 |> List.map (fun s -> s.AvgRapidDiff)
        let blitzDiffs = top15 |> List.map (fun s -> s.AvgBlitzDiff)
        
        let rapidTrace = 
            Bar(x = countries, y = rapidDiffs, name = "Rapid Advantage", marker = Marker(color = "rgba(54, 162, 235, 0.8)"))
        
        let blitzTrace = 
            Bar(x = countries, y = blitzDiffs, name = "Blitz Advantage", marker = Marker(color = "rgba(255, 99, 132, 0.8)"))
        
        let layout = 
            Layout(
                title = "National Time Control Specializations (Top 15 Speed Chess Nations)",
                xaxis = Xaxis(title = "Country", tickangle = -45),
                yaxis = Yaxis(title = "Rating Advantage vs Classical"),
                barmode = "group",
                showlegend = true
            )
        
        let chart = [rapidTrace; blitzTrace] |> Chart.Plot |> Chart.WithLayout layout
        chart.Show()
        printfn "\nTime control analysis chart created and opened in browser."

let findStrategicOpponents (players: Player seq) (referencePlayerId: int) (ratingWindow: int) (minPlayersPerCountry: int) (topCountriesCount: int) (topOpponentsCount: int) : StrategicOpponentsResult option =
    let playersArray = players |> Array.ofSeq
    
    match playersArray |> Array.tryFind (fun p -> p.FideId = referencePlayerId) with
    | None -> 
        printfn $"Player with FIDE ID %d{referencePlayerId} not found."
        None
    | Some referencePlayer ->
        printfn $"Analyzing strategic opponents for %s{referencePlayer.Name} (FIDE: %d{referencePlayer.FideRating}, UR: %d{referencePlayer.UniversalRating})"
        
        let targetMinRating = referencePlayer.FideRating
        let targetMaxRating = referencePlayer.FideRating + ratingWindow
        
        printfn $"Target rating window: %d{targetMinRating} - %d{targetMaxRating}"
        
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
            printfn $"Found %d{topFavorableCountries.Length} favorable countries with overrated players:"
            topFavorableCountries 
            |> List.iter (fun ci -> printfn $"  %s{ci.Country}: %d{ci.PlayerCount} players, avg gap: %.1f{ci.AvgRatingGap}")
            
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
            
            printfn $"Found %d{potentialOpponents.Length} strategic opponents in target rating range:"
            potentialOpponents 
            |> List.iteri (fun i op -> 
                printfn $"  %d{i+1}. %s{op.Name} (%s{op.Country}) - FIDE: %d{op.FideRating}, UR: %d{op.UniversalRating}, Gap: %d{op.RatingGap}")
            
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

        // --- Age Analysis ---
        let ageGroupData = analyzePeakPerformanceAge config players
        let generationComparison = analyzeYouthVsVeterans config players
        // let prodigies = generateProdigyWatchlist config players 18 20

        // --- Time Control Analysis ---
        // let (rapidSpecs, blitzSpecs, classicalSpecs) = identifyTimeControlSpecialists config players 10
        let nationalTimeControlStats = analyzeNationalTimeControlStyles config players
        plotTimeControlAnalysis nationalTimeControlStats

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
                printfn $"Reference Player: %s{player.Name} (ID: %d{result.ReferencePlayerId})"
                printfn $"Target Rating Range: %d{result.TargetMinRating} - %d{result.TargetMaxRating}"
                printfn $"Favorable Countries Found: %d{result.TopFavorableCountries.Length}"
                printfn $"Strategic Opponents Found: %d{result.TopOpponents.Length}"
            | None -> printfn "Strategic analysis failed."
        | None -> printfn "No suitable example player found for strategic analysis."

        printfn "Data processing completed successfully!"
        0
    with ex ->
        printfn $"Error: %s{ex.Message}"
        1
