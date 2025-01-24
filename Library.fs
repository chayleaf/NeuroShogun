namespace NeuroShogun

open System
open System.Collections
open System.Reflection
open System.Text.RegularExpressions
open BepInEx
open BepInEx.Logging
open HarmonyLib
open NeuroFSharp

type Direction =
    | Left
    | Right

(*
wrap cutscenes:
ShopkeeperGiveFreeConsumableCoroutine
ItemBoughtSequenceBegin -> ItemBoughtSequenceOver
*)

type EnumeratorWrapper(obj: IEnumerator, onNext: int -> unit, onDone: unit -> unit) =
    let mutable count = 0

    interface IEnumerator with
        override _.Current = obj.Current

        override _.MoveNext() =
            if obj.MoveNext() then
                onNext count
                count <- count + 1
                true
            else
                onDone ()
                false

        override _.Reset() =
            count <- 0
            obj.Reset()

module Dir =
    let ofStr s =
        match s with
        | "left" -> Left
        | "right" -> Right
        | s -> raise (Exception($"invalid direction in enum, expected left/right, got {s}"))

    let flip x =
        match x with
        | Left -> Right
        | Right -> Left

    let ofGame x =
        match x with
        | Utils.Dir.Left -> Left
        | Utils.Dir.Right -> Right
        | x -> raise (Exception($"got invalid direction from game, expected left or right, got {x}"))

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

type Actions =
    | [<Action("cheat_quick", "Cheat")>] CheatQuick
    | [<Action("cheat_skip_title", "Cheat")>] CheatSkipTitle
    | [<Action("cheat_short_locs", "Cheat")>] CheatShortLocations
    | [<Action("cheat_invuln", "Cheat")>] CheatInvulnerable
    | [<Action("cheat_ts", "Cheat")>] CheatTimeScale of value: float32
    | [<Action("cheat_loc", "Cheat")>] CheatLocation of value: ProgressionEnums.LocationEnum option
    | [<Action("cheat_initroom", "Cheat")>] CheatInitialRoomIndex of value: int
    | [<Action("cheat_day", "Cheat")>] CheatCustomDay of value: int option
    | [<Action("cheat_hero", "Cheat")>] CheatCustomHero of value: AgentEnums.HeroEnum option
    | [<Action("cheat_loadout", "Cheat")>] CheatLoadout of
        nRewards: int option *
        tiles: TileEnums.AttackEnum array option *
        attackEffects: TileEnums.AttackEffectEnum array option *
        skills: SkillEnums.SkillEnum array option *
        consumables: PotionsManager.PotionEnum array option
    | [<Action("cheat_pg", "Cheat")>] CheatPlayground of
        grid: DeveloperUtilities.PlaygroundConfig.GridSize option *
        style: DeveloperUtilities.PlaygroundConfig.Style option *
        wave: AgentEnums.EnemyEnum array option *
        pool: AgentEnums.EnemyEnum array option
    | [<Action("cheat_next", "Cheat")>] CheatNext
    | [<Action("cheat_reset", "Cheat")>] CheatReset
    | [<Action("cheat_money", "Cheat")>] CheatMoney
    | [<Action("cheat_skulls", "Cheat")>] CheatSkulls
    | [<Action("cheat_kill", "Cheat")>] CheatKill
    | [<Action("cheat_heal", "Cheat")>] CheatHeal
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
    | [<Action("select_hero",
               "Choose a hero to play as. Day is the ascension level, higher games are harder but unlock more.")>] SelectHero of
        heroName: string *
        deck: Deck *
        day: int
    // i guess: GetVisibleUncoveredLocations()
    // and Navigate(dir), which does horizontal/vertical nav
    | [<Action("choose_path", "Proceed to the next location")>] ChoosePath of pathIndex: int

type ConsumableContext =
    { slot: int option
      name: string option
      description: string option
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

type EffectContext = { name: string; description: string }

type TileContext =
    { name: string
      // desc
      description: string
      attackEffect: EffectContext option
      tileEffect: EffectContext option
      buyPriceCoins: int option
      unlockPriceSkulls: int option
      cooldown: int option
      inAttackQueue: bool
      remainingCooldown: int option }

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

type SpecialMoveContext =
    { name: string
      // desc
      description: string
      cooldown: int
      remainingCooldown: int }

type PlayerContext =
    { name: string
      coins: int
      skullMetaCurrency: int
      consumables: ConsumableContext list
      skills: SkillContext list
      tiles: TileContext list
      specialMove: SpecialMoveContext
      attackQueue: string list
      hp: HpContext
      [<SkipSerializingIfEquals 0>]
      remainingFrozenDuration: int
      [<SkipSerializingIfEquals false>]
      shield: bool
      [<SkipSerializingIfEquals false>]
      remainingPoisonedDuration: int
      [<SkipSerializingIfEquals false>]
      cursed: bool }

type LocationContext =
    { island: string
      name: string
      shop1: string option
      shop2: string option }

[<RequireQualifiedAccess>]
type Intention =
    | MoveLeft
    | MoveRight
    | LeapLeft
    | LeapRight
    | TurnLeft
    | TurnRight
    | PlayTile
    | Attack
    | Wait

type EnemyContext =
    { name: string
      description: string
      traits: string list option
      elite: string option
      attackQueue: TileContext list
      intention: Intention
      hp: HpContext
      [<SkipSerializingIfEquals 0>]
      remainingFrozenDuration: int
      [<SkipSerializingIfEquals false>]
      shield: bool
      [<SkipSerializingIfEquals false>]
      remainingPoisonedDuration: int
      [<SkipSerializingIfEquals false>]
      cursed: bool
      [<SkipSerializingIfEquals false>]
      boss: bool
      [<SkipSerializingIfEquals false>]
      iceResistance: bool
      [<SkipSerializingIfEquals false>]
      pushResistance: bool }

type CellContext =
    { xPos: int
      // nobunaga
      spotlight: bool option
      enemy: EnemyContext option
      [<SkipSerializingIfEquals 0>]
      traps: int
      // corrupted soul
      flyingEnemy: EnemyContext option
      [<SkipSerializingIfEquals false>]
      youAreHere: bool }

type MapContext =
    { currentLocation: LocationContext
      paths: LocationContext list }

type Context =
    { player: PlayerContext
      shop: ShopContext option
      pickTileOptions: TileContext list option
      tileUpgrade: TileUpgradeContext option
      tileSacrificeRewardCoins: int option
      gridCells: CellContext list option }

module Context =
    let stripTags s = Regex(@"\[[^\]]*\]").Replace(s, "")

    let consumable (slot: int option) (potion: Potion) : ConsumableContext =
        { name = Some(stripTags potion.Name)
          description = Some(stripTags potion.Description)
          slot = slot
          buyPriceCoins = None
          unlockPriceSkulls = None
          sellPriceCoins =
            if Option.isSome slot && potion.CanBeSold then
                Some(
                    potion.BasePriceForHeroSelling
                    + if PotionsManager.Instance.RogueRetail then 1 else 0
                )
            else
                None }

    let deck () : (string * Tile) seq =
        TilesManager.Instance.Deck
        |> Seq.map (fun tile -> (stripTags tile.Attack.Name, tile))
        |> (Map.empty
            |> Seq.mapFold (fun state (name, tile) ->
                match state.TryFind name with
                | Some count -> (($"{name} ({count + 1})", tile), Map.add name (count + 1) state)
                | None -> ((name, tile), Map.add name 1 state)))
        |> fst

    let attackEffect (eff: TileEnums.AttackEffectEnum) : EffectContext option =
        if eff = TileEnums.AttackEffectEnum.None then
            None
        else
            Some
                { name = stripTags (TileEnums.TileEnumsUtils.LocalizedAttackEffectName(eff))
                  description = stripTags (TileEnums.TileEnumsUtils.LocalizedAttackEffectDescription(eff)) }

    let tileEffect (eff: TileEnums.TileEffectEnum) : EffectContext option =
        if eff = TileEnums.TileEffectEnum.None then
            None
        else
            Some
                { name = stripTags (TileEnums.TileEnumsUtils.LocalizedTileEffectName(eff))
                  description = stripTags (TileEnums.TileEnumsUtils.LocalizedTileEffectDescription(eff)) }

    let tile (name: string option) (tile: Tile) : TileContext =
        { name = stripTags (Option.defaultValue tile.Attack.Name name)
          description = stripTags tile.Attack.Description
          attackEffect = attackEffect tile.Attack.AttackEffect
          tileEffect = tileEffect tile.Attack.TileEffect
          buyPriceCoins = None
          unlockPriceSkulls = None
          cooldown = Some tile.Attack.Cooldown
          remainingCooldown = Some tile.TurnsBeforeCharged
          inAttackQueue = tile.TileContainer :? AttackQueueTileContainer }

    let skill (skill: Skill) : SkillContext =
        { name = skill.Name
          description = skill.Description
          buyPriceCoins = None
          unlockPriceSkulls = None }

    let hp (s: AgentStats) : HpContext = $"{s.HP}/{s.maxHP}"

    let specialMove (hero: Hero) : SpecialMoveContext =
        { name = stripTags hero.SpecialAbilityName
          description = stripTags hero.SpecialAbilityDescription
          cooldown = hero.SpecialMove.Cooldown.Cooldown
          remainingCooldown = hero.SpecialMove.Cooldown.Cooldown - hero.SpecialMove.Cooldown.Charge }

    let enemy (enemy: Enemy) : EnemyContext =
        let traits =
            typeof<Enemy>
                .GetProperty("EnemyTraits", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue(enemy)
            :?> AgentEnums.EnemyTraitsEnum array

        { name = stripTags enemy.Name
          description = stripTags enemy.Description
          traits =
            if Array.isEmpty traits then
                None
            else
                traits
                |> Array.map AgentEnums.AgentEnumsUtils.EnemyTraitDescription
                |> List.ofArray
                |> Some
          elite =
            if enemy.EliteType = AgentEnums.EliteTypeEnum.None then
                None
            else
                Some(AgentEnums.AgentEnumsUtils.EliteDescription enemy.EliteType)
          intention =
            match enemy.Action with
            | CombatEnums.ActionEnum.Wait -> Intention.Wait
            | CombatEnums.ActionEnum.MoveLeft when (enemy :? FumikoBoss || enemy :? StriderEnemy) -> Intention.LeapLeft
            | CombatEnums.ActionEnum.MoveLeft -> Intention.MoveLeft
            | CombatEnums.ActionEnum.MoveRight when (enemy :? FumikoBoss || enemy :? StriderEnemy) ->
                Intention.LeapRight
            | CombatEnums.ActionEnum.MoveRight -> Intention.MoveRight
            | CombatEnums.ActionEnum.Attack -> Intention.Attack
            | CombatEnums.ActionEnum.PlayTile -> Intention.PlayTile
            | CombatEnums.ActionEnum.FlipLeft -> Intention.TurnLeft
            | CombatEnums.ActionEnum.FlipRight -> Intention.TurnRight
            | _ -> Intention.Wait
          boss = enemy :? Boss
          attackQueue = enemy.AttackQueue.TCC.Tiles |> Seq.map (tile None) |> List.ofSeq
          iceResistance = not enemy.Freezable
          pushResistance = not enemy.Movable
          hp = hp enemy.AgentStats
          remainingFrozenDuration = enemy.AgentStats.ice
          shield = enemy.AgentStats.shield
          remainingPoisonedDuration = enemy.AgentStats.poison
          cursed = enemy.AgentStats.curse }

    let cell (nobunaga: Cell list) (traps: (Cell * Trap) list) (cell: Cell) : CellContext =
        let flying =
            match CombatSceneManager.Instance.Room with
            | :? CorruptedSoulBossRoom as room when room.Boss <> null ->
                let boss = room.Boss :?> CorruptedSoulBoss

                if cell = boss.PseudoCell && not (boss.Animator.GetBool("IsDown")) then
                    Some(enemy boss)
                else
                    None
            | _ -> None

        let enemy, you =
            match cell.Agent with
            | null -> None, false
            | :? Hero -> None, true
            | x -> Some(enemy (x :?> Enemy)), false

        { xPos = cell.IndexInGrid
          spotlight =
            if List.isEmpty nobunaga then
                None
            else
                Some(List.contains cell nobunaga)
          enemy = enemy
          traps = traps |> List.filter (fst >> (=) cell) |> List.fold (fun n _ -> n + 1) 0
          // corrupted soul
          flyingEnemy = flying
          youAreHere = you }

    let player () : PlayerContext =
        let hero = Globals.Hero
        let pm = PotionsManager.Instance
        let sm = SkillsManager.Instance
        let deck = deck ()

        { name = stripTags hero.Name
          coins = Globals.Coins
          skullMetaCurrency = Globals.KillCount
          consumables = pm.HeldPotions |> Array.mapi (fun i x -> consumable (Some i) x) |> List.ofArray
          tiles = deck |> Seq.map (fun (s, x) -> tile (Some s) x) |> List.ofSeq
          specialMove = specialMove hero
          attackQueue =
            deck
            |> Seq.filter (fun (_, x) -> x.TileContainer :? AttackQueueTileContainer)
            |> Seq.map fst
            |> List.ofSeq
          skills = sm.Skills |> Seq.map skill |> List.ofSeq
          hp = hp hero.AgentStats
          remainingFrozenDuration = hero.AgentStats.ice
          shield = hero.AgentStats.shield
          remainingPoisonedDuration = hero.AgentStats.poison
          cursed = hero.AgentStats.curse }

    let location (loc: MapLocation) : LocationContext =
        let loc = loc.location

        let shop =
            fun (comp: ShopComponent) -> Utils.LocalizationUtils.LocalizedString("Locations", comp.technicalName)

        let shop1, shop2 =
            match loc with
            | :? ShopLocation as loc -> Some(shop loc.leftShopComponent), Some(shop loc.rightShopComponent)
            | _ -> None, None

        { name = stripTags loc.Name
          island =
            match loc.island with
            | ProgressionEnums.IslandEnum.green -> "Green"
            | ProgressionEnums.IslandEnum.brown -> "Brown"
            | ProgressionEnums.IslandEnum.red -> "Red"
            | ProgressionEnums.IslandEnum.purple -> "Purple"
            | ProgressionEnums.IslandEnum.white -> "White"
            | ProgressionEnums.IslandEnum.gray -> "Gray"
            | ProgressionEnums.IslandEnum.darkGreen -> "Dark Green"
            | ProgressionEnums.IslandEnum.shogun -> "Shogun"
            | _ -> "???"
          shop1 = shop1
          shop2 = shop2 }

    let map (map: Map) : MapContext =
        { currentLocation = location map.CurrentMapLocation
          paths =
            map.MapLocations
            |> Seq.filter _.Reachable
            |> Seq.filter _.Uncovered
            |> Seq.map location
            |> List.ofSeq }

type Game(plugin: MainClass) =
    inherit Game<Actions>()

    let mutable inhibitForces = 0
    let stripTags = Context.stripTags

    let chk (cond: bool) (error: string) res : Result<'T, string option> =
        res |> Result.bind (fun x -> if cond then Ok x else Error(Some error))

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

    let dioramaLine (text: string) =
        let text, char =
            match text with
            | text when text.StartsWith "[L]" -> text.Substring 3, false
            | text when text.StartsWith "[R]" -> text.Substring 3, true
            | text -> text, false

        (if char then "Character 2: " else "Character 1: ") + stripTags text

    let mutable initDone = false
    let mutable nobunagaCells = List.empty
    let mutable trapCells = List.empty
    let mutable trapCell = null

    member _.InhibitForces
        with set value =
            if value then
                inhibitForces <- inhibitForces + 1
            else
                inhibitForces <- inhibitForces - 1

    member _.TrapGone(trap: Trap) =
        trapCells <- trapCells |> List.filter (snd >> (<>) trap)

    member _.TrapPlaced(trap: Trap) =
        trapCells <- (trapCell, trap) :: trapCells

    member _.TrapAttack(agent: Agent) =
        trapCell <- agent.Cell.Neighbour(agent.FacingDir, 1)

    member _.NobunagaCells(cells: Cell list) = nobunagaCells <- cells

    member this.ReceiveAttack(agent: Agent, hit: Hit, attacker: Agent) =
        let atkName =
            match attacker with
            | :? Hero -> "you"
            | null -> "poison/thorns/trap/shockwave/karma (figure it out yourself)"
            | _ -> attacker.Name

        let effects =
            (if agent.AgentStats.shield then
                 " The attack is nullified by the shield, the shield is now gone."
             elif agent.AgentStats.curse then
                 " The attack damage is doubled because of the curse."
             else
                 "")
            + (let mutable dmg = hit.Damage

               if agent.AgentStats.curse then
                   dmg <- dmg * 2

               if hit.IsNonLethal then
                   dmg <- min dmg (agent.AgentStats.HP - 1)

               if agent.AgentStats.shield then
                   dmg <- 0

               match dmg with
               | 0 -> ""
               | x when x < agent.AgentStats.HP -> $" HP: {agent.AgentStats.HP}->{agent.AgentStats.HP - x}"
               | _ -> " The hit is lethal.")

        match agent with
        | :? Hero -> $"You have been hit by {atkName} for {hit.Damage} damage.{effects}"
        | :? NobunagaBoss when nobunagaCells |> List.exists ((=) agent.Cell) |> not ->
            $"Nobunaga got hit by {atkName}, but the attack didn't seem to have any effect..."
        | _ -> $"{attacker.Name} has been hit by {atkName}.{effects}"
        |> this.Context false

    member this.ShowCatDialogue(text: string) =
        this.Context false $"The cat says: {stripTags text}"

    member this.ShowDialogue (agent: Agent) (text: string) =
        match agent with
        | :? Hero -> this.Context false $"You, {stripTags Globals.Hero.Name}, say: {stripTags text}"
        | _ -> this.Context false $"The enemy {stripTags agent.Name} says: {stripTags text}"

    member this.CreditsStart() =
        this.Context
            false
            "Congratulations, you've beaten the game on day 7, the highest day! The credits are now playing."

    member this.DioramaStart() =
        this.Context
            false
            ($"You're viewing the ending cutscene."
             + (if Globals.Day = 7 then
                    "\n"
                else
                    " Beating the game on day 7 will unlock the entirety of the cutscene.\n")
             + if Globals.Day = 1 then
                   let name =
                       Utils.LocalizationUtils.LocalizedString("ShopAndNPC", $"Diorama_Day_1_Title")

                   $"Day 1: {name}"
               else
                   { 1 .. Globals.Day - 1 }
                   |> Seq.map (fun day ->
                       let name =
                           Utils.LocalizationUtils.LocalizedString("ShopAndNPC", $"Diorama_Day_{day}_Title")

                       Seq.append
                           (seq { $"Day {day}: {name}" })
                           (DioramaData.DioramaUtils.GetConversationLines day |> Seq.map dioramaLine))
                   |> Seq.concat
                   |> String.concat "\n")

    member this.DioramaEnd() =
        this.Context
            false
            ("The cutscene has ended."
             + (if UnlocksManager.Instance.NewBestDayUnlockedThisRun() then
                    (if Globals.Day < 7 then
                         $" Day {Globals.Day + 1} unlocked."
                     else
                         "")
                    + (if Globals.Day = 1 then " New islands unlocked." else "")
                else
                    ""))

    member this.ShowDioramaDialogue(text: string) = this.Context false (dioramaLine text)

    member this.Update() =
        if not initDone && EventsManager.Instance <> null then
            initDone <- true
            // attacker can be null
            let man = EventsManager.Instance
            let ctx = this.Context false
            man.BeginRun.AddListener(fun () -> ctx "You have started a new game!")
            // man.CoinsUpdate.AddListener(fun _coins -> ())
            // man.MetaCurrencyUpdate.AddListener(fun _meta -> ())

            man.MetaCurrencyReceived.AddListener(fun meta ->
                ctx $"You got {meta} skulls as a reward for defeating the boss")

            man.EndOfCombatTurn.AddListener(fun () -> ctx "The enemies' turn has ended.")
            man.BeginningOfCombatTurn.AddListener(fun () -> ctx "It's the enemies' turn...")

            man.EnterRoom.AddListener(fun room ->
                ctx (
                    match room with
                    | :? CampRoom as room ->
                        "You are at the camp - a starting location. Here, you can access the metaprogression shop that unlocks new items for skulls, or you can start the game. A cat is lying around the metaprogression shop."
                        + (if room.UnlocksShop.CanBuyAnything() then
                               " You have items available for purchase in the shop."
                           else
                               " You can't currently purchase anything from the shop.")
                    | :? ShogunBossRoom ->
                        $"You, {Globals.Hero.Name}, have reached the final boss - this is the Shogun Showdown!"
                    | :? BossRoom as room -> $"You, {Globals.Hero.Name}, have encountered a boss - {room.Boss.Name}"
                    | :? CombatRoom -> $"You have entered a new location - prepare for a fight!"
                    | :? RewardRoom -> $"You can now claim your rewards (or skip them)"
                    | :? ShopRoom -> $"You have entered a shop."
                    | _ -> $"You have entered a new room"
                ))

            man.ExitRoom.AddListener(fun _room ->
                nobunagaCells <- List.empty
                trapCells <- List.empty)
            // man.BeginningOfCombat.AddListener(fun _ -> ())
            // man.EndOfCombat.AddListener(fun _ -> ())
            man.NewWaveSpawns.AddListener(fun wave -> ctx $"A new wave of {wave.NEnemies} enemies has spawned!")
            man.EndBossFight.AddListener(fun () -> ctx "You have defeated the boss!")

            man.HeroStampObtained.AddListener(fun stamp ->
                let level =
                    match Globals.Hero.CharacterSaveData.GetHeroStampRank(stamp) with
                    | ProgressionEnums.HeroStampRank.regular -> "Regular"
                    | ProgressionEnums.HeroStampRank.ultimate -> "Ultimate"
                    | _ -> "Unknown"

                let stamp =
                    match stamp with
                    | ProgressionEnums.HeroStamp.shogunSlayer -> "Shogun Slayer"
                    | ProgressionEnums.HeroStamp.totalOblitaration -> "Obliteration"
                    | ProgressionEnums.HeroStamp.swiftKiller -> "Swift Killer"
                    | ProgressionEnums.HeroStamp.comboMaster -> "Combo Master"
                    | ProgressionEnums.HeroStamp.strategist -> "Strategist"
                    | _ -> "???"

                ctx $"New {stamp} stamp level achieved: {level}")

            man.GameOver.AddListener(fun win ->
                nobunagaCells <- List.empty
                trapCells <- List.empty
                ctx (if win then "Congratulations, you won!" else "You died..."))

            man.ShogunDefeated.AddListener(fun () -> ctx "You have defeated the final boss!")
            // man.EnemyDied.AddListener(fun _enemy -> ())
            man.BossDied.AddListener(fun boss -> ctx $"You have defeated the boss {boss.Name}!")
            // man.EnemyFriendlyKill.AddListener(fun () -> ())
            // man.ComboKill.AddListener(fun _enemy -> ())
            // man.PreciseKill.AddListener(fun _enemy -> ())
            // man.PickupCreated.AddListener(fun _pickup -> ())
            // man.PickupPickedUp.AddListener(fun _pickup -> ())
            // man.TileUpgraded.AddListener(fun _tile -> ())
            // man.NewTilePicked.AddListener(fun _tile -> ())
            // man.ShopBegin.AddListener(fun () -> ())
            // man.UnlocksShopBegin.AddListener(fun () -> ())
            // man.ShopEnd.AddListener(fun () -> ())
            man.SkillTriggered.AddListener(fun skill ->
                let name =
                    Utils.LocalizationUtils.LocalizedString(
                        "Skills",
                        $"{Enum.GetName(typeof<SkillEnums.SkillEnum>, skill)}_Name"
                    )

                ctx $"The skill {name} has been triggered!")

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
            let mutable actions: Action list =
                [ (this.Action CheatQuick)
                  (this.Action CheatSkipTitle)
                  (this.Action CheatShortLocations)
                  (this.Action CheatInvulnerable)
                  (this.Action CheatTimeScale)
                  (this.Action CheatLocation)
                  (this.Action CheatInitialRoomIndex)
                  (this.Action CheatCustomDay)
                  (this.Action CheatCustomHero)
                  (this.Action CheatLoadout)
                  (this.Action CheatPlayground)
                  (this.Action CheatNext)
                  (this.Action CheatReset)
                  (this.Action CheatMoney)
                  (this.Action CheatSkulls)
                  (this.Action CheatKill)
                  (this.Action CheatHeal) ]

            if
                CombatManager.Instance.CombatInProgress
                && not CombatManager.Instance.TurnInProgress
                && CombatManager.Instance.AllowHeroAction
            then
                if Globals.Hero.AllowWait then
                    actions <- this.Action Wait :: actions

                if Globals.Hero.AgentStats.ice <= 0 then
                    let move = this.Action Move

                    move.MutateProp "direction" (fun x -> (x :?> StringSchema).RetainEnum(Dir.ofStr >> chkMove))

                    actions <- move :: actions

                    if Globals.Hero.AttackQueue.NTiles <> 0 then
                        let queue =
                            Context.deck ()
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
                            (x :?> StringSchema).RetainEnum(Dir.ofStr >> chkSpecialMove))

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
                            .RetainEnum(Dir.ofStr >> (=) (Dir.flip (Dir.ofGame Globals.Hero.FacingDir))))

                    actions <- turn :: actions

                    if
                        TilesManager.Instance.CanInteractWithTiles
                        && Globals.Hero.AttackQueue.CanAddTile
                        && CombatSceneManager.Instance.CurrentMode = CombatSceneManager.Mode.combat
                    then
                        let hand =
                            Context.deck ()
                            |> Seq.filter (fun (_, tile) ->
                                tile.TileIsEnabled && tile.TileContainer :? HandTileContainer)
                            |> Seq.map fst
                            |> Array.ofSeq

                        let play = this.Action PlayTile

                        play.MutateProp "tileName" (fun x -> (x :?> StringSchema).SetEnum(hand))

                        actions <- play :: actions

            if actions |> List.exists _.Dirty then
                ()

            this.RetainActions(actions |> List.map (fun x -> x))

    override _.Name = "test"

    override _.HandleAction(action: Actions) =
        match action with
        | CheatQuick ->
            Globals.DeveloperUtils.Quick <- not Globals.DeveloperUtils.Quick
            Ok(Some $"{Globals.DeveloperUtils.Quick}")
        | CheatCustomDay None ->
            Globals.DeveloperUtils._customDay <- false
            Ok(None)
        | CheatCustomDay(Some n) ->
            Globals.DeveloperUtils._customDay <- true
            Globals.DeveloperUtils.day <- System.Math.Clamp(n, 1, Globals.CurrentlyImplementedMaxDay)
            Ok(Some($"{Globals.DeveloperUtils.day}"))
        | CheatCustomHero None ->
            Globals.DeveloperUtils._customHero <- false
            Ok(None)
        | CheatCustomHero(Some n) ->
            Globals.DeveloperUtils._customHero <- true
            Globals.DeveloperUtils.hero <- n
            Ok(Some $"{Globals.Developer} {Globals.DeveloperUtils.CustomHero} {Globals.DeveloperUtils.hero}")
        | CheatInitialRoomIndex n ->
            Globals.DeveloperUtils.initialRoomIndex <- n
            Ok(Some $"{Globals.DeveloperUtils.initialRoomIndex}")
        | CheatInvulnerable ->
            Globals.DeveloperUtils._invulnerable <- not Globals.DeveloperUtils._invulnerable
            Ok(Some $"{Globals.DeveloperUtils.Invulnerable}")
        | CheatSkipTitle ->
            Globals.DeveloperUtils._skipTitleScreen <- not Globals.DeveloperUtils._skipTitleScreen
            Ok(Some $"{Globals.DeveloperUtils.SkipTitleScreen}")
        | CheatTimeScale x ->
            UnityEngine.Time.timeScale <- x
            Ok(Some $"{UnityEngine.Time.timeScale}")
        | CheatShortLocations ->
            Globals.DeveloperUtils._shortLocations <- not Globals.DeveloperUtils._shortLocations
            Ok(Some $"{Globals.DeveloperUtils.ShortLocations}")
        | CheatLoadout(None, None, None, None, None) ->
            Globals.DeveloperUtils._customLoadout <- false
            Ok(Some "disabled")
        | CheatLoadout(n_rewards, tiles, attack_effects, skills, consumables) ->
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
        | CheatLocation None ->
            Globals.DeveloperUtils._customLocation <- false
            Ok(None)
        | CheatLocation(Some x) ->
            Globals.DeveloperUtils._customLocation <- true
            Globals.DeveloperUtils.location <- x
            Ok(Some $"{Globals.DeveloperUtils.CustomLocation} {Globals.DeveloperUtils.location}")
        | CheatPlayground(grid, style, wave, pool) ->
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
        | CheatNext ->
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
        | CheatReset ->
            UnityEngine.SceneManagement.SceneManager.LoadScene("ResetGameState")
            Ok(Some "reset")
        | CheatMoney ->
            Globals.Coins <- Globals.Coins + 100
            Ok(Some "money")
        | CheatSkulls ->
            Globals.KillCount <- Globals.KillCount + 100
            Ok(Some "meta")
        | CheatKill ->
            CombatManager.Instance.KillEnemies()
            CombatManager.Instance.TriggerTurn(CombatEnums.ActionEnum.Wait)
            Ok(Some "killed")
        | CheatHeal ->
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
                let deck = Context.deck () |> Map.ofSeq

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
                let containers = q.TCC.Containers |> Seq.filter _.HasTile |> List.ofSeq

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
                let deck = Context.deck () |> Map.ofSeq

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
        | SelectHero(_heroName, _altDeck, _day) -> Error(None)
        | ChoosePath _pathIndex -> Error(None)

    override _.LogError error = plugin.Logger.LogError $"{error}"
    override _.LogDebug error = plugin.Logger.LogInfo $"{error}"

and [<BepInPlugin("org.pavluk.neuroshogun", "NeuroShogun", "1.0.0")>] MainClass() =
    inherit BaseUnityPlugin()
    let mutable harmony = null
    let mutable initDone = false
    let mutable game = None
    let cts = new Threading.CancellationTokenSource()

    [<DefaultValue>]
    val mutable public Logger: ManualLogSource

    [<DefaultValue>]
    static val mutable private instance: MainClass

    static member Instance = MainClass.instance
    member _.Game = game.Value

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
                    let game = Game(this)
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
            // Globals.DeveloperUtils._customLocation <- true
            Globals.DeveloperUtils._quick <- true
            Globals.DeveloperUtils._shortLocations <- true
        else
            ()
