namespace NeuroShogun

open System.Reflection
open System.Text.RegularExpressions
open BepInEx
open BepInEx.Logging
open HarmonyLib
open NeuroFSharp

type Direction =
    | Left
    | Right

module Dir =
    let ofStr s =
        match s with
        | "left" -> Left
        | "right" -> Right
        | s -> raise (new System.Exception($"invalid direction in enum, expected left/right, got {s}"))

    let flip x =
        match x with
        | Left -> Right
        | Right -> Left

    let ofGame x =
        match x with
        | Utils.Dir.Left -> Left
        | Utils.Dir.Right -> Right
        | x -> raise (new System.Exception($"got invalid direction from game, expected left or right, got {x}"))

    let toGame x =
        match x with
        | Left -> Utils.Dir.Left
        | Right -> Utils.Dir.Right

type Deck =
    | Main
    | Alt
    | Random

type Observatory<'T when 'T: equality>(func: unit -> 'T) =
    let mutable value = None

    member _.Changed() =
        let old = value
        let cur = Some(func ())
        value <- cur
        old <> cur

[<RequireQualifiedAccess>]
type Cheat =
    | Quick
    | SkipTitle
    | ShortLocations
    | Invulnerable
    | TimeScale of value: float32
    | Location of value: ProgressionEnums.LocationEnum option
    | InitialRoomIndex of value: int
    | CustomDay of value: int option
    | CustomHero of value: AgentEnums.HeroEnum option
    | Loadout of
        n_rewards: int option *
        tiles: TileEnums.AttackEnum array option *
        attack_effects: TileEnums.AttackEffectEnum array option *
        skills: SkillEnums.SkillEnum array option *
        consumables: PotionsManager.PotionEnum array option
    | Playground of
        grid: DeveloperUtilities.PlaygroundConfig.GridSize option *
        style: DeveloperUtilities.PlaygroundConfig.Style option *
        wave: AgentEnums.EnemyEnum array option *
        pool: AgentEnums.EnemyEnum array option
    | Next
    | Reset
    | Money
    | Skulls
    | Kill
    | Heal

type Actions =
    | Cheat of Cheat
    // available in combat when you can move
    // conditions
    // - AllowHeroAction
    //   Set in Shop.UnlockSequence and SwitchHeroCoroutine
    //   notes: this isnt easy to subscribe to but this is only triggered by the player, so it's easy to catch right after handling an action
    // - Globals.Hero.AgentStats.ice <= 0
    //   set in ApplyIceStatusCoroutine
    //   notes: ApplyIceStatus cant be called for hero, so does this ever happen?
    // - the cell in that dir is free
    //   updated after each player/enemy move and kill duh
    //   - SetUpNewAct
    //   - Agent.Die()
    //   - all too many ProcessTurns
    //   - HeroSelection.InitializeForNewHero
    //   - other than the last one it really
    //   notes: this can only update during turns so nobody cares
    // - CombatInProgress
    //   set in BeginCombat/OnEndOfCombat - subscribe for !camp  EventsManager.Instance.BeginningOfCombat, EventsManager.Instance.EndOfCombat
    //   stashed in MapManager.{Open,Close}map - subscribe EventsManager.Instance.MapOpened EventsManager.Instance.MapClosed
    //   set in GameOver - EventsManager.Instance.GameOver
    // - !TurnInProgress
    //   set in ProcessTurn, {Begin,End}ArtificialTurn
    //   artificial turn is:
    //   - after free turnaround
    //   - in RainOfMirrors.PerformEffect
    // mutability: set the directions enum
    | [<Action("move", "Move in a direction (ends your turn)")>] Move of direction: Direction
    // available in combat when theres an enemy in a direction
    // mutability: set the directions enum
    | [<Action("special_move", "Execute the special move in a particular direction (ends your turn)")>] SpecialMove of
        direction: Direction
    // available in combat always
    // mutability: set the directions enum
    | [<Action("turn", "Turn around and face in another direction")>] Turn of direction: Direction
    // available in combat always (ice forces a wait in update loop)
    | [<Action("wait", "Wait for a single turn (ends your turn)")>] Wait
    // available in combat when queued tiles
    | [<Action("attack",
               "Execute the attacks in your tile queue in a specified order. Any attacks that you have not listed will be discarded")>] Attack of
        tileNames: string list
    // available in combat when any unqueued tiles
    // mutability: list all tiles
    | [<Action("play_tile", "Add an attack to your tile queue (ends your turn)")>] PlayTile of tileName: string
    // available when PotionsManager.Instance.HeldPotions isnt empty
    // mutability: list all potions that CanBeUsed
    | [<Action("consume", "Consume an item (does not end your turn)")>] Consume of itemName: string
    // available: when ShopRoom.Shop contains SkillShopItemData
    // mutability: list all that CanBeSold
    | [<Action("buy_skill", "Purchase a skill for coins")>] BuySkill of skillName: string
    // available: when ShopRoom.Shop contains ConsumableShopItem
    // mutability: list all that CanBeSold
    | [<Action("buy_consumable", "Purchase a consumable for coins")>] BuyConsumable of consumableName: string
    // available: when Potion.CanBeSold
    // mutability: list all that CanBeSold
    | [<Action("sell_consumable", "Sell a consumable for coins")>] SellConsumable of consumableName: string
    // available: when ShopRoom.Shop contains ShopUpgradeShopItem
    // mutability: list all that CanBeSold
    | [<Action("unlock_shop_upgrade", "Unlock a shop upgrade for skulls")>] UnlockShopUpgrade of upgradeName: string
    // available: when ShopRoom.Shop contains UnlockShopItemData, or maybe campRoom.UnlocksShop
    // mutability: list all that CanBeSold
    | [<Action("unlock_consumable", "Unlock a consumable for skulls")>] UnlockConsumable of consumableName: string
    | [<Action("unlock_skill", "Unlock a skill for skulls")>] UnlockSkill of skillName: string
    // available: when RewardRoom.Reward.TileUpgrade is WarriorGambleUpgrade
    // note: _CanUpgradeTileAndWhy, CannotUpgradeText
    | [<Action("warriors_gamble", "Reroll a tile, changing its attack and randomizing its upgrades")>] RerollTile of
        tileName: string
    | [<Action("upgrade_tile_stats", "Upgrade a tile's stats: %s")>] UpgradeTile of tileName: string
    // available: when RewardRoom.Reward.TileUpgrade is AddAttackEffectTileUpgrade
    | [<Action("add_attack_effect",
               "Add an attack effect to a tile: %s. If the tile already has an attack effect, the previous effect will be overwritten!")>] AddAttackEffect of
        tileName: string
    // available: when RewardRoom.Reward.TileUpgrade is AddTileEffectTileUpgrade
    | [<Action("add_tile_effect",
               "Add a tile effect to a tile: %s. If the tile already has a tile effect, the previous effect will be overwritten!")>] AddTileEffect of
        tileName: string
    // available: when RewardRoom.Reward.TileUpgrade is SacrificeTileUpgrade
    | [<Action("sacrifice_tile", "Sacrifice a tile for %d coins")>] SacrificeTile of tileName: string
    // available: when RewardRoom.Reward is NewTileReward
    | [<Action("pick_tile_reward", "Pick a tile reward out of the options available")>] PickTileReward of
        tileName: string
    // available: when ShopServiceEnum.get5Coins or get10Coins
    | [<Action("get_coins", "Buy coins from the shop")>] GetCoins
    // available: when ShopServiceEnum.heal
    | [<Action("heal", "Fully heal for %d coins")>] Heal
    // available: when ShopServiceEnum.reroll
    //            when rewardRoom.rewardRerolling.rerollButton.Interactable
    // rewardRoom.RerollButtonPressed
    // shopRoom.Shop.RerollShopContent
    | [<Action("reroll_rewards", "Reroll the rewards for %d coins")>] RerollRewards
    // available: when !rewardRoom.Reward.InProgress && !rewardRoom.Busy
    //            when rewardRoom.skipButton.Interactable
    // rewardRoom.SkipButtonPressed
    | [<Action("skip_rewards", "Skip the rewards")>] SkipRewards
    // campRoom.HeroSelection   .StartingDeckSelection   .RerollDeck
    // IS ON A DELAY of openDelay
    | [<Action("select_hero", "Choose a hero to play as")>] SelectHero of heroName: string * deck: Deck
    // i guess: GetVisibleUncoveredLocations()
    // and Navigate(dir), which does horizontal/vertical nav
    | [<Action("choose_path", "Proceed to the next location")>] ChoosePath of pathIndex: int

type ConsumableContext =
    { name: string
      description: string
      buyPriceCoins: int option
      unlockPriceSkulls: int option
      sellPriceCoins: int option }

type ShopUpgradeContext =
    { name: string
      description: string
      unlockPriceSkulls: int option }

type SkillContext =
    { name: string
      description: string
      buyPriceCoins: int option
      unlockPriceSkulls: int option }

type TileContext =
    { name: string
      // desc
      attack: string
      attackEffect: string option
      tileEffect: string option
      buyPriceCoins: int option
      unlockPriceSkulls: int option }

type TileUpgradeContext =
    { upgradeCost: int option
      addedCooldown: int option
      removedCooldown: int option
      addedDamage: int option
      removedDamage: int option
      addedUpgradeSlots: int option
      removedUpgradeSlots: int option
      attackEffect: string option
      tileEffect: string option
      rerollPrice: int option }

type ShopContext =
    { consumables: ConsumableContext list option
      upgrades: ShopUpgradeContext list option
      skills: SkillContext list option
      tiles: TileContext list option
      healPrice: int option
      rerollPrice: int option }

// "{current}/{max}"
type HpContext = string

type PlayerContext =
    { coins: int option
      skullCurrency: int option
      consumables: ConsumableContext list
      tiles: TileContext list
      skills: SkillContext list
      hp: HpContext }

type LocationContext =
    { island: string
      name: string
      shop1: string
      shop2: string }

type PathsContext =
    { up: LocationContext
      down: LocationContext
      left: LocationContext
      right: LocationContext }

[<RequireQualifiedAccess>]
type Intention =
    | MoveRight
    | MoveLeft
    | TurnRight
    | TurnLeft
    | PlayTile
    | Attack

type EnemyContext =
    { name: string
      description: string
      traits: string list option
      elite: string
      attack_queue: Tile list
      intention: string option
      hp: HpContext
      boss: bool option
      bossPhase: int option
      onlyVulnerableInSpotlightCells: bool option
      flyingAndImmune: bool
      iceResistance: bool option }

type CellContext =
    { xPos: int
      // corrupted soul
      yPos: int option
      // nobunaga
      spotlight: bool option
      enemy: EnemyContext option
      youAreHere: bool option }

type MapContext =
    { currentLocation: LocationContext
      paths: PathsContext }

type Context =
    { player: PlayerContext
      shop: ShopContext option
      pickTileOptions: TileContext list option
      tileUpgrade: TileUpgradeContext option
      tileSacrificeRewardCoins: int option
      gridCells: CellContext list option }

type Game(plugin: MainClass) =
    inherit Game<Actions>()

    let stripTags s = Regex(@"\[[^\]]*\]").Replace(s, "")

    let chk (cond: bool) (error: string) res : Result<'T, string option> =
        res |> Result.bind (fun x -> if cond then Ok x else Error(Some error))

    let deck () : (string * Tile) seq =
        TilesManager.Instance.Deck
        |> Seq.map (fun tile -> (tile.Attack.Name, tile))
        |> (Map.empty
            |> Seq.mapFold (fun state (name, tile) ->
                match state.TryFind name with
                | Some count -> (($"{name} ({count + 1})", tile), Map.add name (count + 1) state)
                | None -> ((name, tile), Map.add name 1 state)))
        |> fst

    let combatError' () =
        Ok()
        |> chk
            CombatManager.Instance.CombatInProgress
            "You aren't in combat and can't perform combat actions at the moment"
        |> chk (not CombatManager.Instance.TurnInProgress) "Please wait for your turn"

    let combatError (isWait: bool) =
        combatError' ()
        |> chk CombatManager.Instance.AllowHeroAction "You are in a cutscene and can't perform any actions"
        |> if isWait then
               chk Globals.Hero.AllowWait "You can't wait in the camp room"
           else
               chk (Globals.Hero.AgentStats.ice <= 0) "You are frozen and can't perform any actions"

    let chkValid dir =
        let cell = Globals.Hero.Cell.Neighbour(Dir.toGame dir, 1)
        cell <> null

    let chkMove dir =
        let cell = Globals.Hero.Cell.Neighbour(Dir.toGame dir, 1)
        cell <> null && cell.Agent = null

    let chkSpecialMove dir =
        not (chkMove dir)
        && Globals.Hero.SpecialMove.Cooldown.IsCharged
        && Globals.Hero.SpecialMove.Allowed(Globals.Hero, Dir.toGame dir)

    let mutable initDone = false

    member this.Update() =
        if not initDone && EventsManager.Instance <> null then
            initDone <- true
            let man = EventsManager.Instance
            man.BeginRun.AddListener(fun () -> ())
            man.CoinsUpdate.AddListener(fun _coins -> ())
            man.MetaCurrencyUpdate.AddListener(fun _meta -> ())
            man.MetaCurrencyReceived.AddListener(fun _meta -> ())
            man.EndOfCombatTurn.AddListener(fun () -> ())
            man.BeginningOfCombatTurn.AddListener(fun () -> ())
            man.EnterRoom.AddListener(fun _room -> ())
            man.ExitRoom.AddListener(fun _room -> ())
            man.BeginningOfCombat.AddListener(fun _ -> ())
            man.EndOfCombat.AddListener(fun _ -> ())
            man.NewWaveSpawns.AddListener(fun _wave -> ())
            man.BeginBossFight.AddListener(fun () -> ())
            man.EndBossFight.AddListener(fun () -> ())
            man.IslandCleared.AddListener(fun struct (_island, _) -> ())
            man.HeroStampObtained.AddListener(fun _stamp -> ())
            man.GameOver.AddListener(fun _win -> ())
            man.ShogunDefeated.AddListener(fun () -> ())
            man.EnemyDied.AddListener(fun _enemy -> ())
            man.BossDied.AddListener(fun _boss -> ())
            man.EnemyFriendlyKill.AddListener(fun () -> ())
            man.ComboKill.AddListener(fun _enemy -> ())
            man.PreciseKill.AddListener(fun _enemy -> ())
            man.PickupCreated.AddListener(fun _pickup -> ())
            man.PickupPickedUp.AddListener(fun _pickup -> ())
            man.TileUpgraded.AddListener(fun _tile -> ())
            man.NewTilePicked.AddListener(fun _tile -> ())
            man.ShopBegin.AddListener(fun () -> ())
            man.UnlocksShopBegin.AddListener(fun () -> ())
            man.ShopEnd.AddListener(fun () -> ())
            man.Attack.AddListener(fun _attacker _attacked _hit -> ())
            man.SkillTriggered.AddListener(fun _skill -> ())
            man.SpecialMoveEffectOnTarget.AddListener(fun _hero _target -> ())
            man.PotionUsed.AddListener(fun _potion -> ())
            man.HeroIsHit.AddListener(fun struct (_hit, _attacker) -> ())
            man.HeroHPUpdate.AddListener(fun _hp -> ())
            man.HeroMaxHPUpdate.AddListener(fun _hp -> ())
            man.HeroRevived.AddListener(fun () -> ())
            man.RoomBegin.AddListener(fun _room -> ())
            man.RoomEnd.AddListener(fun _room -> ())
            man.MapOpened.AddListener(fun () -> ())
            man.MapCurrentLocationCleared.AddListener(fun () -> ())

        this.ReregisterActions()

    override this.ReregisterActions() =
        if
            CombatManager.Instance = null
            || Globals.Hero = null
            || TilesManager.Instance = null
            || CombatSceneManager.Instance = null
            || PotionsManager.Instance = null
        then
            ()
        else
            let mutable actions: Action list = []

            if
                CombatManager.Instance.CombatInProgress
                && not CombatManager.Instance.TurnInProgress
                && CombatManager.Instance.AllowHeroAction
            then
                if Globals.Hero.AllowWait then
                    actions <- this.Action Wait :: actions

                if Globals.Hero.AgentStats.ice <= 0 then
                    let move = this.Action Move

                    move.MutateProp "direction" (fun x ->
                        (x :?> StringSchema).RetainEnum(fun x -> chkMove (Dir.ofStr x)))

                    actions <- move :: actions

                    if Globals.Hero.AttackQueue.NTiles <> 0 then
                        let queue =
                            deck ()
                            |> Seq.filter (fun (_, v) -> v.TileContainer :? AttackQueueTileContainer)
                            |> Seq.map fst
                            |> Array.ofSeq

                        let atk = this.Action Attack

                        atk.MutateProp "tileNames" (fun x ->
                            ((x :?> ArraySchema).Items :?> StringSchema).SetEnum(queue))

                        actions <- atk :: actions

                    if Globals.Hero.SpecialMove.Cooldown.IsCharged then
                        let spMove = this.Action SpecialMove

                        spMove.MutateProp "direction" (fun x ->
                            (x :?> StringSchema).RetainEnum(fun x -> chkSpecialMove (Dir.ofStr x)))

                        actions <- spMove :: actions

                    let turn = this.Action Turn

                    turn.Description <-
                        turn.InitialDescription
                        + if Globals.Hero.TurnAroundIsFree then
                              " (does *not* end your turn)"
                          else
                              " (ends your turn)"

                    turn.MutateProp "direction" (fun x ->
                        (x :?> StringSchema)
                            .RetainEnum(fun x -> Dir.ofStr x = Dir.flip (Dir.ofGame Globals.Hero.FacingDir)))

                    actions <- turn :: actions

                    if
                        TilesManager.Instance.CanInteractWithTiles
                        && Globals.Hero.AttackQueue.CanAddTile
                        && CombatSceneManager.Instance.CurrentMode = CombatSceneManager.Mode.combat
                    then
                        let hand =
                            deck ()
                            |> Seq.filter (fun (_, tile) ->
                                tile.TileIsEnabled && tile.TileContainer :? HandTileContainer)
                            |> Seq.map fst
                            |> Array.ofSeq

                        let play = this.Action PlayTile

                        play.MutateProp "tileName" (fun x -> (x :?> StringSchema).SetEnum(hand))

                        actions <- play :: actions

            if actions |> List.exists (fun x -> x.Dirty) then
                ()

            this.RetainActions(actions |> List.map (fun x -> x))

    override _.Name = "test"

    override _.HandleAction(action: Actions) =
        match action with
        | Cheat cheat ->
            match cheat with
            | Cheat.Quick ->
                Globals.DeveloperUtils.Quick <- not Globals.DeveloperUtils.Quick
                Ok(Some $"{Globals.DeveloperUtils.Quick}")
            | Cheat.CustomDay None ->
                Globals.DeveloperUtils._customDay <- false
                Ok(None)
            | Cheat.CustomDay(Some n) ->
                Globals.DeveloperUtils._customDay <- true
                Globals.DeveloperUtils.day <- System.Math.Clamp(n, 1, Globals.CurrentlyImplementedMaxDay)
                Ok(Some($"{Globals.DeveloperUtils.day}"))
            | Cheat.CustomHero None ->
                Globals.DeveloperUtils._customHero <- false
                Ok(None)
            | Cheat.CustomHero(Some n) ->
                Globals.DeveloperUtils._customHero <- true
                Globals.DeveloperUtils.hero <- n
                Ok(Some $"{Globals.DeveloperUtils.CustomHero} {Globals.DeveloperUtils.hero}")
            | Cheat.InitialRoomIndex n ->
                Globals.DeveloperUtils.initialRoomIndex <- n
                Ok(Some $"{Globals.DeveloperUtils.initialRoomIndex}")
            | Cheat.Invulnerable ->
                Globals.DeveloperUtils._invulnerable <- not Globals.DeveloperUtils._invulnerable
                Ok(Some $"{Globals.DeveloperUtils.Invulnerable}")
            | Cheat.SkipTitle ->
                Globals.DeveloperUtils._skipTitleScreen <- not Globals.DeveloperUtils._skipTitleScreen
                Ok(Some $"{Globals.DeveloperUtils.SkipTitleScreen}")
            | Cheat.TimeScale x ->
                UnityEngine.Time.timeScale <- x
                Ok(Some $"{UnityEngine.Time.timeScale}")
            | Cheat.ShortLocations ->
                Globals.DeveloperUtils._shortLocations <- not Globals.DeveloperUtils._shortLocations
                Ok(Some $"{Globals.DeveloperUtils.ShortLocations}")
            | Cheat.Loadout(None, None, None, None, None) ->
                Globals.DeveloperUtils._customLoadout <- false
                Ok(Some "disabled")
            | Cheat.Loadout(n_rewards, tiles, attack_effects, skills, consumables) ->
                let l = Globals.DeveloperUtils.loadout
                let n_rewards = Option.defaultValue l.nRewards n_rewards
                let tiles = Option.defaultValue l.tiles tiles
                let effects = Option.defaultValue l.attackEffects attack_effects
                let skills = Option.defaultValue l.skills skills
                let consumables = Option.defaultValue l.consumables consumables
                l.nRewards <- n_rewards
                l.tiles <- tiles
                l.attackEffects <- effects
                l.skills <- skills
                l.consumables <- consumables
                Globals.DeveloperUtils._customLoadout <- true

                Ok(
                    Some
                        $"loadout {Globals.DeveloperUtils.CustomLoadout} {n_rewards}/{List.ofArray tiles}/{List.ofArray effects}/{List.ofArray skills}/{List.ofArray consumables}"
                )
            | Cheat.Location None ->
                Globals.DeveloperUtils._customLocation <- false
                Ok(None)
            | Cheat.Location(Some x) ->
                Globals.DeveloperUtils._customLocation <- true
                Globals.DeveloperUtils.location <- x
                Ok(Some $"{Globals.DeveloperUtils.CustomLocation} {Globals.DeveloperUtils.location}")
            | Cheat.Playground(grid, style, wave, pool) ->
                let c = Globals.DeveloperUtils.playgroundConfig
                let grid = Option.defaultValue c.gridSize grid
                let style = Option.defaultValue c.style style
                let wave = Option.defaultValue c.wave wave
                let pool = Option.defaultValue c.pool pool
                c.gridSize <- grid
                c.style <- style
                c.wave <- wave
                c.pool <- pool

                Ok(Some $"pg {grid}/{style}/{List.ofArray wave}/{List.ofArray pool}")
            | Cheat.Next ->
                if CombatManager.Instance.CombatInProgress then
                    CombatManager.Instance.KillEnemies()
                    EventsManager.Instance.EndOfCombat.Invoke()
                    Ok(Some "killed")
                else
                    match CombatSceneManager.Instance.Room with
                    | :? RewardRoom as room ->
                        room.SkipButtonPressed()
                        Ok(Some "skipped")
                    | _ -> Error(None)
            | Cheat.Reset ->
                UnityEngine.SceneManagement.SceneManager.LoadScene("ResetGameState")
                Ok(Some "reset")
            | Cheat.Money ->
                Globals.Coins <- Globals.Coins + 100
                Ok(Some "money")
            | Cheat.Skulls ->
                Globals.KillCount <- Globals.KillCount + 100
                Ok(Some "meta")
            | Cheat.Kill ->
                CombatManager.Instance.KillEnemies()
                CombatManager.Instance.TriggerTurn(CombatEnums.ActionEnum.Wait)
                Ok(Some "killed")
            | Cheat.Heal ->
                Globals.Hero.FullHeal()
                Ok(Some "healed")
        | Move dir ->
            combatError false
            |> chk (chkValid dir) "There is nothing in that direction"
            |> chk
                (chkMove dir)
                (if chkSpecialMove dir then
                     "You cannot currently move in that direction, you can try doing a special move instead"
                 else
                     "You cannot currently move in that direction")
            |> Result.map (fun () ->
                CombatManager.Instance.PlayerInputsCombatAction(
                    (match dir with
                     | Left -> CombatEnums.ActionEnum.MoveLeft
                     | Right -> CombatEnums.ActionEnum.MoveRight),
                    false
                )

                None)
        | SpecialMove dir ->
            combatError false
            |> chk (chkValid dir) "There is nothing in that direction"
            |> chk (not (chkMove dir)) "You can only do a special move into enemies, there's no enemy on that cell"
            |> chk Globals.Hero.SpecialMove.Cooldown.IsCharged "Your special move is on cooldown"
            |> chk
                (chkSpecialMove dir)
                (if
                     Globals.Hero.FacingDir <> Dir.toGame dir
                     && not Globals.Hero.SpecialMove.CanDoBackwards
                 then
                     "You can't do the special move there because you are facing in the opposite direction"
                 else
                     "You can't do the special move there, check the special move rules again")
            |> Result.map (fun () ->
                CombatManager.Instance.PlayerInputsCombatAction(
                    (match dir with
                     | Left -> CombatEnums.ActionEnum.MoveLeft
                     | Right -> CombatEnums.ActionEnum.MoveRight),
                    false
                )

                None)
        | Turn dir ->
            if Globals.Hero.TurnAroundIsFree then
                combatError' ()
            else
                combatError false
            |> chk (Globals.Hero.FacingDir <> Dir.toGame dir) "You are already facing in that direction"
            |> Result.map (fun () ->
                // this flag is only for UI purposes, force allow it
                Globals.Hero.AllowTurnAround <- true

                CombatManager.Instance.PlayerInputsCombatAction(
                    (match dir with
                     | Left -> CombatEnums.ActionEnum.FlipLeft
                     | Right -> CombatEnums.ActionEnum.FlipRight),
                    false
                )

                None)
        | Wait ->
            combatError true
            |> Result.map (fun () ->
                CombatManager.Instance.PlayerInputsCombatAction(CombatEnums.ActionEnum.Wait, false)
                None)
        | Attack names ->
            // TODO: reorder as specified in indices abcd
            combatError false
            |> chk (names.Length > 0) "Must choose at least one tile to attack with"
            |> Result.bind (fun () ->
                let deck = deck () |> Map.ofSeq

                Ok(List.empty, Set.empty)
                |> List.foldBack
                    (fun name state ->
                        match state with
                        | Ok(_, set) when set.Contains name -> Error(Some($"Duplicate tile provided: {name}"))
                        | Ok(list, set) ->
                            match Map.tryFind name deck with
                            | Some(tile) when (tile.TileContainer :? AttackQueueTileContainer) ->
                                Ok(tile :: list, set.Add name)
                            | Some(_) -> Error(Some $"Tile not in attack queue: {name}")
                            | None -> Error(Some $"Tile not found: {name}")
                        | Error err -> Error err)
                    names
                |> Result.map (fun (tiles, names) -> (deck, tiles, names)))
            |> Result.bind (fun (deck, tiles, names) ->
                let toRemove =
                    deck
                    |> Map.toSeq
                    |> Seq.map (fun (name, x) ->
                        ((name, x), x.TileContainer :? AttackQueueTileContainer && not (names.Contains name)))
                    |> Seq.choose (fun (x, b) -> if b then Some(x) else None)
                    |> List.ofSeq

                toRemove |> List.iter (fun (_, x) -> x.TileContainer.UponTileSubmit())

                let q = Globals.Hero.AttackQueue
                let containers = q.TCC.Containers |> Seq.filter (fun x -> x.HasTile) |> List.ofSeq

                let res =
                    if containers.Length = tiles.Length then
                        containers |> List.iter (fun c -> c.RemoveTile() |> ignore)

                        Seq.zip containers tiles |> Seq.iter (fun (c, t) -> c.AddTile t)
                        Ok()
                    else
                        Error(Some("Internal error: Tile container count mismatch"))

                res
                |> chk (Globals.Hero.AttackQueue.NTiles <> 0) "Your attack queue is empty"
                |> chk (not Globals.Hero.AttackQueue.TCC.IsFragmented) "Your attack queue has gaps"
                |> Result.map (fun () ->
                    CombatManager.Instance.PlayerInputsCombatAction(CombatEnums.ActionEnum.Attack, false)

                    let context =
                        if toRemove.IsEmpty then
                            None
                        else
                            Some(
                                $"Tiles that you haven't listed were removed from the attack queue: {List.map fst toRemove}. Note that adding tiles to the queue costs turns, so unplayed tiles are usually wasted turns"
                            )

                    context))
        | PlayTile name ->
            combatError false
            |> chk TilesManager.Instance.CanInteractWithTiles "You can't currently use tiles"
            |> chk Globals.Hero.AttackQueue.CanAddTile "Your attack queue is full"
            |> chk
                (CombatSceneManager.Instance.CurrentMode = CombatSceneManager.Mode.combat)
                "You are not currently in combat"
            |> Result.bind (fun () ->
                let deck = deck () |> Map.ofSeq

                match Map.tryFind name deck with
                | Some tile -> Ok tile
                | None -> Error(Some $"Tile not found: {name}"))
            |> Result.bind (fun tile ->
                Ok(tile)
                |> chk tile.TileIsEnabled "The tile is currently on cooldown"
                |> chk
                    (tile.TileContainer :? HandTileContainer)
                    (if (tile.TileContainer :? AttackQueueTileContainer) then
                         "The title is already in the attack queue"
                     else
                         "The title is not currently in your hand"))
            |> Result.map (fun tile ->
                tile.TileContainer.UponTileSubmit()
                None)

        | Consume _itemName -> Error(None)
        | BuyConsumable _consumableName -> Error(None)
        | BuySkill _skillName -> Error(None)
        | UnlockShopUpgrade _shopUpgradeName -> Error(None)
        | UnlockConsumable _consumableName -> Error(None)
        | UnlockSkill _skillName -> Error(None)
        | SellConsumable _consumableName -> Error(None)
        | AddAttackEffect _tileName -> Error(None)
        | AddTileEffect _tileName -> Error(None)
        | UpgradeTile _tileName -> Error(None)
        | RerollTile _tileName -> Error(None)
        | PickTileReward _tileName -> Error(None)
        | SacrificeTile _tileName -> Error(None)
        // services
        | RerollRewards -> Error(None)
        | Heal -> Error(None)
        | GetCoins -> Error(None)
        // skip
        | SkipRewards -> Error(None)
        | SelectHero(_heroName, _altDeck) -> Error(None)
        | ChoosePath _pathIndex -> Error(None)

    override _.LogError error = plugin.Logger.LogError $"{error}"
    override _.LogDebug error = plugin.Logger.LogInfo $"{error}"

and [<BepInPlugin("org.pavluk.neuroshogun", "NeuroShogun", "1.0.0")>] MainClass() =
    inherit BaseUnityPlugin()
    let mutable harmony = null
    let mutable initDone = false
    let mutable game = None
    let cts = new System.Threading.CancellationTokenSource()

    [<DefaultValue>]
    val mutable public Logger: ManualLogSource

    [<DefaultValue>]
    static val mutable private instance: MainClass

    static member Instance = MainClass.instance


    member this.Awake() =
        try
            MainClass.instance <- this
            harmony <- Harmony.CreateAndPatchAll(Assembly.GetExecutingAssembly())
            this.Logger <- base.Logger
            let cnt = Seq.fold (fun x _ -> x + 1) 0 (harmony.GetPatchedMethods())
            Globals.ForcePlayTutorial <- false
            Globals.Tutorial <- false
            Globals.SkipTitleScreen <- true

            game <-
                Some(
                    let game = new Game(this)
                    game.Start(Some("ws://127.0.0.1:8000"), cts.Token) |> ignore
                    game
                )

            this.Logger.LogInfo($"Plugin NeuroShogun is loaded with {cnt} patches!")
        with exc ->
            this.Logger.LogError($"ERROR {exc}")

    // wow i can't imagine this person is so lazy who would ever do so much on every frame smh my head
    member _.LateUpdate() =
        game |> Option.iter (fun x -> x.Update())

    member this.PreSceneLoad(name: string) =
        this.Logger.LogInfo($"Init {name}")

        if Globals.GameInitialized && not initDone then
            this.Logger.LogInfo("Initializing")
            initDone <- true
            Globals.DeveloperUtils._invulnerable <- true
            Globals.DeveloperUtils._customLocation <- true
            Globals.DeveloperUtils._quick <- true
            Globals.DeveloperUtils._shortLocations <- true
        else
            ()
