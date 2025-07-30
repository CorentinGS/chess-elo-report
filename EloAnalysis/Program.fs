module Program

open FSharp.Data
open LiteDB

type FideXml = XmlProvider<"players_sample.xml">
type UrRatingCsv = CsvProvider<"ratings_sample.csv">

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
      WorldRank: int option
      UniversalRating: int
      RapidGap: int option
      BlitzGap: int option
      CountryRank: int option
      GameCount: int option }

let loadFidePlayersAsync () : Async<Map<int, Player>> =
    async {
        let fideXml = FideXml.Load("players.xml")

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
                          WorldRank = None
                          UniversalRating = p.Rating
                          RapidGap = None
                          BlitzGap = None
                          CountryRank = None
                          GameCount = None }
                    )
                with _ ->
                    None)
            |> Map.ofSeq

        return map
    }

type UrRatingData =
    { WorldRank: int option
      UniversalRating: int option
      RapidGap: int option
      BlitzGap: int option
      CountryRank: int option
      GameCount: int option }

let loadUrRatingsAsync () : Async<Map<int, UrRatingData>> =
    async {
        let urCsv = UrRatingCsv.Load("ratings.csv")

        let ur =
            urCsv.Rows
            |> Seq.map (fun r ->
                (r.FIDE_PlayerCode,
                 { WorldRank = Some r.WorldRank
                   UniversalRating = Some r.URating
                   RapidGap = Some r.RGap
                   BlitzGap = Some r.BGap
                   CountryRank = Some r.CountryRank
                   GameCount = Some r.GameCount }))
            |> Map.ofSeq
        return ur
    }
    
let mergePlayerDataAsync () : Async<Player[]> = async {
    
    let! fideChild = Async.StartChild (loadFidePlayersAsync ())
    let! urChild   = Async.StartChild (loadUrRatingsAsync ())
      
    let! fideMap = fideChild
    let! urMap   = urChild

    let merged =
        fideMap
        |> Map.map (fun fideId player ->
            match urMap.TryFind fideId with
            | Some urData ->
                { player with
                    WorldRank = urData.WorldRank
                    UniversalRating = urData.UniversalRating.Value // Assuming URating is always present
                    RapidGap = urData.RapidGap
                    BlitzGap = urData.BlitzGap
                    CountryRank = urData.CountryRank
                    GameCount = urData.GameCount }
            | None -> player)
        |> Map.values
        |> Seq.toArray
    return merged
}

let saveToLiteDb (players: Player seq) =
    use db = new LiteDatabase("players.db")
    let collection = db.GetCollection<Player>("players")

    collection.DeleteAll() |> ignore
    collection.InsertBulk(players) |> ignore

    printfn $"Saved %d{Seq.length players} players to LiteDB"

let loadFromLiteDb () : Player[] =
    use db = new LiteDatabase("players.db")
    let collection = db.GetCollection<Player>("players")
    let players = collection.FindAll() |> Seq.toArray
    printfn $"Loaded %d{players.Length} players from LiteDB"
    players

let databaseExists () =
    System.IO.File.Exists("players.db")


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
        printfn $"FIDE ID: {p.FideId}, Name: {p.Name}, Universal Rating: {p.UniversalRating}, Country: {p.Country}, Title: {p.Title}")
    
let analyzeCountryRatings (players: Player seq) =
    printfn "\n\n--- 🌍 Country Rating Analysis (UR vs FIDE) ---\n"
    printfn "Analyzing rating gaps between Universal Rating and FIDE Rating..."
    
    let allPlayers = players |> List.ofSeq
    let totalPlayers = allPlayers.Length
    printfn $"Total players in dataset: {totalPlayers}"
    
    // Check data availability first
    let playersWithBothRatings = 
        allPlayers
        |> List.filter (fun p -> p.UniversalRating > 0 && p.FideRating > 0)
    
    printfn $"Players with both UR and FIDE ratings: {playersWithBothRatings.Length}"
    
    if playersWithBothRatings.Length = 0 then
        printfn "No players found with both Universal Rating and FIDE Rating data."
        printfn "This might indicate a data loading or merging issue."
    else
        // Use more lenient filters initially
        let validPlayers = 
            playersWithBothRatings
            |> List.choose (fun p -> 
                if p.Country <> "" && p.UniversalRating > 1000 && p.FideRating > 1000 then
                    let gap = p.UniversalRating - p.FideRating
                    Some (p.Country, gap, p.UniversalRating, p.FideGames)
                else None)
        
        printfn $"Players meeting criteria (both ratings > 1000): {validPlayers.Length}"
        
        let countryGroups = validPlayers |> List.groupBy (fun (country, _, _, _) -> country) |> List.filter (fun (_, players) -> List.length players > 1000)
        printfn $"Countries represented: {countryGroups.Length}"
        
        printfn "\n%-12s %-8s %-10s %-10s %-8s %-10s" "Country" "Players" "Avg Gap" "Median" "Std Dev" "Top 10%"
        printfn "----------------------------------------------------------------"
        
        countryGroups
        |> List.filter (fun (_, players) -> List.length players >= 1000) // At least 10 players per country
        |> List.map (fun (country, countryPlayers) ->
            let gaps = countryPlayers |> List.map (fun (_, gap, _, _) -> gap)
            let playerCount = gaps.Length
            
            let avgGap = List.averageBy float gaps
            
            // Standard deviation
            let variance = gaps |> List.averageBy (fun g -> (float g - avgGap) ** 2.0)
            let stdDev = sqrt variance
            
            // Top 10% average gap (by Universal Rating)
            let top10Count = max 1 (playerCount / 10)
            let top10ByUR = 
                countryPlayers 
                |> List.sortByDescending (fun (_, _, ur, _) -> ur)
                |> List.take top10Count
                |> List.map (fun (_, gap, _, _) -> gap)
                |> List.averageBy float
            
            (country, playerCount, avgGap, stdDev, top10ByUR))
        |> List.sortByDescending (fun (_, _, avg, _, _) -> avg)
        |> List.iter (fun (country, count, avg, stdDev, top10) ->
            printfn $"%-12s{country} %-8d{count} %-10.1f{avg} %-8.1f{stdDev} %-10.1f{top10}")
        
        printfn "\nKey insights:"
        printfn "• Positive gap = Universal Rating higher (potentially FIDE underrated)"
        printfn "• High std dev indicates rating inconsistency within the country"
        printfn "• Top 10%% shows gap trend for elite players in each country"

[<EntryPoint>]
let main _ =
    try
        let players = 
            if databaseExists() then
                printfn "Database exists, loading players from LiteDB..."
                loadFromLiteDb()
            else
                printfn "Database not found, loading and merging player data from source files..."
                let mergedPlayers = mergePlayerDataAsync () |> Async.RunSynchronously
                
                printfn "Saving to LiteDB..."
                saveToLiteDb mergedPlayers
                mergedPlayers
        
        // --- Execute and Display Analysis ---
        // displayTopPlayers players
        displayTopUniversalRatings players
        analyzeCountryRatings players

        printfn "Data processing completed successfully!"
        0
    with ex ->
        printfn $"Error: %s{ex.Message}"
        1
