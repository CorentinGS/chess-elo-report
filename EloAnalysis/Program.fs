module Program

open FSharp.Data
open XPlot.Plotly
open System.IO

type FideXml = XmlProvider<"players_sample.xml">
type UrRatingCsv = CsvProvider<"ratings_sample.csv">

type AnalysisConfig = {
    MinRating: int
    MinPlayersInCountry: int
    ElitePercentage: float
    CachedPlayersCsvPath: string
    PlayersXmlPath: string
    RatingsCsvPath: string
}

type CountryStats = {
    Country: string
    PlayerCount: int
    OverallAvgGap: float
    OverallMedianGap: float
    ElitePlayerCount: int
    EliteAvgGap: float
    EliteMedianGap: float
    EliteStdDev: float
}

type RatingTierStats = {
    TierName: string
    MinRating: int
    MaxRating: int option
    PlayerCount: int
    AvgGap: float
    MedianGap: float
    StdDev: float
    AvgFideRating: float
    AvgUrRating: float
}
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

let playerToCsvLine (player: Player) : string =
    let escapeName = player.Name.Replace("\"", "\"\"")
    let fields = [
        player.FideId.ToString()
        "\"" + escapeName + "\""
        "\"" + player.Country + "\""
        "\"" + player.Sex + "\""
        "\"" + player.Title + "\""
        "\"" + player.WTitle + "\""
        "\"" + player.OTitle + "\""
        "\"" + player.FoaTitle + "\""
        player.FideRating.ToString()
        player.FideGames.ToString()
        player.FideK.ToString()
        player.FideRapidRating.ToString()
        player.FideRapidGames.ToString()
        player.FideRapidK.ToString()
        player.FideBlitzRating.ToString()
        player.FideBlitzGames.ToString()
        player.FideBlitzK.ToString()
        player.Birthday.ToString()
        "\"" + player.Flag + "\""
        player.WorldRank.ToString()
        player.UniversalRating.ToString()
        player.RapidGap.ToString()
        player.BlitzGap.ToString()
        player.CountryRank.ToString()
        player.GameCount.ToString()
    ]
    String.concat "," fields

let csvLineToPlayer (line: string) : Player option =
    try
        let parts = line.Split(',')
        if parts.Length >= 25 then
            Some {
                FideId = int parts[0]
                Name = parts[1].Trim('"').Replace("\"\"", "\"")
                Country = parts[2].Trim('"')
                Sex = parts[3].Trim('"')
                Title = parts[4].Trim('"')
                WTitle = parts[5].Trim('"')
                OTitle = parts[6].Trim('"')
                FoaTitle = parts[7].Trim('"')
                FideRating = int parts[8]
                FideGames = int parts[9]
                FideK = int parts[10]
                FideRapidRating = int parts[11]
                FideRapidGames = int parts[12]
                FideRapidK = int parts[13]
                FideBlitzRating = int parts[14]
                FideBlitzGames = int parts[15]
                FideBlitzK = int parts[16]
                Birthday = int parts[17]
                Flag = parts[18].Trim('"')
                WorldRank = int parts[19]
                UniversalRating = int parts[20]
                RapidGap = int parts[21]
                BlitzGap = int parts[22]
                CountryRank = int parts[23]
                GameCount = int parts[24]
            }
        else
            None
    with
    | _ -> None

let saveToCsv (config: AnalysisConfig) (players: Player seq) =
    let header = "FideId,Name,Country,Sex,Title,WTitle,OTitle,FoaTitle,FideRating,FideGames,FideK,FideRapidRating,FideRapidGames,FideRapidK,FideBlitzRating,FideBlitzGames,FideBlitzK,Birthday,Flag,WorldRank,UniversalRating,RapidGap,BlitzGap,CountryRank,GameCount"
    let lines = players |> Seq.map playerToCsvLine |> Seq.toArray
    let allLines = Array.concat [[|header|]; lines]
    File.WriteAllLines(config.CachedPlayersCsvPath, allLines)
    printfn $"Saved %d{Seq.length players} players to CSV"

let loadFromCsv (config: AnalysisConfig) : Player[] =
    let lines = File.ReadAllLines(config.CachedPlayersCsvPath)
    let players = 
        lines
        |> Array.skip 1 // Skip header
        |> Array.choose csvLineToPlayer
    printfn $"Loaded %d{players.Length} players from CSV"
    players

let csvExists (config: AnalysisConfig) = File.Exists(config.CachedPlayersCsvPath)


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
    if values.IsEmpty then 0.0
    else
        let sorted = List.sort values
        let length = sorted.Length
        if length % 2 = 0 then
            float (sorted[length/2 - 1] + sorted[length/2]) / 2.0
        else
            float sorted[length/2]

let analyzeAndDisplayCountryRatings (config: AnalysisConfig) (players: Player seq) : CountryStats list =
    printfn "\n\n--- 🌍 Advanced Country Rating Analysis (UR vs FIDE) ---\n"

    let validPlayers =
        players
        |> Seq.filter (fun p ->
            p.Country <> "" &&
            p.UniversalRating > config.MinRating &&
            p.FideRating > config.MinRating)
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
                let eliteGaps = playersByUR |> List.take eliteCount |> List.map (fun (_, gap, _) -> gap)
                
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

        printfn $"Analysis complete for {countryData.Length} countries with at least {config.MinPlayersInCountry} players."
        printfn "\n%-12s %-10s | %-10s %-10s | %-10s %-10s %-10s %-10s" 
                "Country" "Plyr Count" "OverallAvg" "OverallMed" "EliteAvg" "EliteMed" "EliteStdDev" "EliteCount"
        printfn "%s" (String.replicate 100 "-")

        countryData
        |> List.iter (fun stats ->
            printfn $"%-12s{stats.Country} %-10d{stats.PlayerCount} | %-+10.1f{stats.OverallAvgGap} %-+10.1f{stats.OverallMedianGap} | %-+10.1f{stats.EliteAvgGap} %-+10.1f{stats.EliteMedianGap} %-10.1f{stats.EliteStdDev} %-10d{stats.ElitePlayerCount}")
        
        if not countryData.IsEmpty then
            let mostUnderratedElite = countryData |> List.maxBy (fun s -> s.EliteAvgGap)
            let mostOverratedElite = countryData |> List.minBy (fun s -> s.EliteAvgGap)
            let mostInconsistent = countryData |> List.maxBy (fun s -> s.EliteStdDev)
            let biggestElitePremium = countryData |> List.maxBy (fun s -> s.EliteAvgGap - s.OverallAvgGap)

            printfn "\n--- Key Insights ---"
            printfn "• Positive Gap = Universal Rating is higher (suggests FIDE rating may be 'deflated' or 'underrated')"
            printfn $"• Most Underrated Elite: %s{mostUnderratedElite.Country} (Avg Gap: %+.1f{mostUnderratedElite.EliteAvgGap})"
            printfn $"• Most Overrated Elite:  %s{mostOverratedElite.Country} (Avg Gap: %+.1f{mostOverratedElite.EliteAvgGap})"
            printfn $"• Most Inconsistent Elite: %s{mostInconsistent.Country} (Std Dev: %.1f{mostInconsistent.EliteStdDev})"
            printfn $"• Biggest Elite Premium:   %s{biggestElitePremium.Country} (Elite gap is %+.1f{biggestElitePremium.EliteAvgGap - biggestElitePremium.OverallAvgGap} points higher than their national average)"

        countryData

let analyzeRatingTiers (config: AnalysisConfig) (players: Player seq) : RatingTierStats list =
    printfn "\n\n--- 📊 Rating Inflation/Deflation by Tier Analysis (UR vs FIDE) ---\n"
    
    let validPlayers =
        players
        |> Seq.filter (fun p ->
            p.UniversalRating > config.MinRating &&
            p.FideRating > config.MinRating)
        |> Seq.toList
    
    printfn $"Processing {validPlayers.Length} players with valid ratings > {config.MinRating}..."
    
    if validPlayers.Length = 0 then
        printfn "No players met the criteria. Analysis cannot continue."
        []
    else
        let ratingTiers = [
            ("2600+", 2600, None)
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
            ("1200-1299", 1200, Some 1299)
        ]
        
        let tierStats =
            ratingTiers
            |> List.choose (fun (tierName, minRating, maxRatingOpt) ->
                let playersInTier =
                    validPlayers
                    |> List.filter (fun p ->
                        p.FideRating >= minRating &&
                        (match maxRatingOpt with
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
                    
                    Some {
                        TierName = tierName
                        MinRating = minRating
                        MaxRating = maxRatingOpt
                        PlayerCount = playersInTier.Length
                        AvgGap = avgGap
                        MedianGap = medianGap
                        StdDev = stdDev
                        AvgFideRating = avgFideRating
                        AvgUrRating = avgUrRating
                    }
                else
                    None)
        
        printfn $"Analysis complete for {tierStats.Length} rating tiers with sufficient players.\n"
        printfn "%-12s %-10s | %-10s %-10s %-10s | %-10s %-10s %-10s" 
                "Tier" "Players" "Avg Gap" "Med Gap" "Std Dev" "Avg FIDE" "Avg UR" "Deflation%"
        printfn "%s" (String.replicate 95 "-")
        
        tierStats
        |> List.iter (fun stats ->
            let deflationPercent = (stats.AvgGap / stats.AvgFideRating) * 100.0
            printfn $"%-12s{stats.TierName} %-10d{stats.PlayerCount} | %-+10.1f{stats.AvgGap} %-+10.1f{stats.MedianGap} %-10.1f{stats.StdDev} | %-10.1f{stats.AvgFideRating} %-10.1f{stats.AvgUrRating} %-+10.2f{deflationPercent}%%")
        
        if not tierStats.IsEmpty then
            let mostDeflated = tierStats |> List.maxBy (fun s -> s.AvgGap)
            let leastDeflated = tierStats |> List.minBy (fun s -> s.AvgGap)
            let mostInconsistent = tierStats |> List.maxBy (fun s -> s.StdDev)
            
            printfn "\n--- Key Insights ---"
            printfn "• Positive Gap = Universal Rating > FIDE Rating (suggests FIDE rating is 'deflated')"
            printfn "• Deflation%% = (Gap / Avg FIDE Rating) × 100"
            printfn $"• Most Deflated Tier: %s{mostDeflated.TierName} (Avg Gap: %+.1f{mostDeflated.AvgGap}, %.2f{(mostDeflated.AvgGap / mostDeflated.AvgFideRating) * 100.0}%% deflation)"
            printfn $"• Least Deflated Tier: %s{leastDeflated.TierName} (Avg Gap: %+.1f{leastDeflated.AvgGap}, %.2f{(leastDeflated.AvgGap / leastDeflated.AvgFideRating) * 100.0}%% deflation)"
            printfn $"• Most Inconsistent: %s{mostInconsistent.TierName} (Std Dev: %.1f{mostInconsistent.StdDev})"
            
            let highTierAvg = tierStats |> List.filter (fun s -> s.MinRating >= 2400) |> List.averageBy (fun s -> s.AvgGap)
            let midTierAvg = tierStats |> List.filter (fun s -> s.MinRating >= 2000 && s.MinRating < 2400) |> List.averageBy (fun s -> s.AvgGap)
            let lowTierAvg = tierStats |> List.filter (fun s -> s.MinRating < 2000) |> List.averageBy (fun s -> s.AvgGap)
            
            printfn $"\n--- Tier Group Analysis ---"
            printfn $"• High Tier (2400+) Avg Gap: %+.1f{highTierAvg}"
            printfn $"• Mid Tier (2000-2399) Avg Gap: %+.1f{midTierAvg}" 
            printfn $"• Low Tier (<2000) Avg Gap: %+.1f{lowTierAvg}"
            
            if highTierAvg > midTierAvg then
                printfn "• Result: FIDE ratings appear MORE deflated for higher-rated players"
            else
                printfn "• Result: FIDE ratings appear LESS deflated for higher-rated players"
        
        tierStats

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

let plotCountryRatings (countryData: CountryStats list) =
    if countryData.Length = 0 then
        printfn "No data available for plotting."
    else
        let sortedData = countryData |> List.sortByDescending (fun stats -> stats.EliteAvgGap)
        let top10 = sortedData |> List.take (min 10 sortedData.Length)

        let bottom10 =
            sortedData |> List.rev |> List.take (min 10 sortedData.Length) |> List.rev

        let combinedData = top10 @ bottom10

        let countries = combinedData |> List.map (fun stats -> stats.Country)
        let eliteAvgGaps = combinedData |> List.map (fun stats -> stats.EliteAvgGap)
        let eliteMedianGaps = combinedData |> List.map (fun stats -> stats.EliteMedianGap)
        let overallAvgGaps = combinedData |> List.map (fun stats -> stats.OverallAvgGap)

        let eliteAvgTrace =
            Bar(x = countries, y = eliteAvgGaps, name = "Elite Avg Gap", marker = Marker(color = "rgba(55, 128, 191, 0.8)"))
        
        let eliteMedianTrace =
            Bar(x = countries, y = eliteMedianGaps, name = "Elite Median Gap", marker = Marker(color = "rgba(255, 128, 64, 0.8)"))
            
        let overallAvgTrace =
            Bar(x = countries, y = overallAvgGaps, name = "Overall Avg Gap", marker = Marker(color = "rgba(128, 255, 128, 0.6)"))

        let layout =
            Layout(
                title = "Country Rating Analysis: Elite vs Overall Gaps (UR vs FIDE)",
                xaxis = Xaxis(title = "Country"),
                yaxis = Yaxis(title = "Rating Gap (UR - FIDE)"),
                barmode = "group",
                showlegend = true
            )

        let chart = [ eliteAvgTrace; eliteMedianTrace; overallAvgTrace ] |> Chart.Plot |> Chart.WithLayout layout

        chart.Show()
        printfn "\nChart created and opened in browser. Showing top 10 and bottom 10 countries by elite average rating gap."

[<EntryPoint>]
let main _ =
    try
        let config = {
            MinRating = 1200
            MinPlayersInCountry = 500
            ElitePercentage = 0.10
            CachedPlayersCsvPath = "players_caches.csv"
            PlayersXmlPath = "players.xml"
            RatingsCsvPath = "ratings.csv"
        }

        let players =
            if csvExists config then
                printfn "CSV cache exists, loading players from CSV..."
                loadFromCsv config
            else
                printfn "CSV cache not found, loading and merging player data from source files..."
                let mergedPlayers = mergePlayerDataAsync config |> Async.RunSynchronously

                printfn "Saving to CSV cache..."
                saveToCsv config mergedPlayers
                mergedPlayers

        printfn $"Configuration: Min Rating = {config.MinRating}, Min Players per Country = {config.MinPlayersInCountry}, Elite Percentage = {config.ElitePercentage * 100.0}%%"
        
        // --- Execute and Display Analysis ---
        displayTopUniversalRatings players
        let countryData = analyzeAndDisplayCountryRatings config players
        plotCountryRatings countryData
        
        // --- Rating Tier Analysis ---
        let tierData = analyzeRatingTiers config players
        plotRatingTiers tierData

        printfn "Data processing completed successfully!"
        0
    with ex ->
        printfn $"Error: %s{ex.Message}"
        1
