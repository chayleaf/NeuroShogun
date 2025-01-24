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
    | [<Action("buy_coins", "Buy coins from the shop")>] BuyCoins
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

type ShopPrice =
    { [<SkipSerializingIfEquals(0)>]
      coins: int
      [<SkipSerializingIfEquals(0)>]
      skulls: int
      [<SkipSerializingIfEquals(0)>]
      hp: int
      [<SkipSerializingIfEquals(0)>]
      maxHp: int
      [<SkipSerializingIfEquals(false)>]
      free: bool }

type ConsumableContext =
    { [<SkipSerializingIfNone>]
      slot: int option
      [<SkipSerializingIfNone>]
      name: string option
      [<SkipSerializingIfNone>]
      description: string option
      [<SkipSerializingIfNone>]
      buyPrice: ShopPrice option
      [<SkipSerializingIfNone>]
      unlockPrice: ShopPrice option
      [<SkipSerializingIfNone>]
      sellPrice: ShopPrice option }

type ShopUpgradeContext =
    { name: string
      description: string
      unlockPrice: ShopPrice }

// "{current}/{max}"
type HpContext = string

type SkillContext =
    { name: string
      description: string
      level: HpContext
      [<SkipSerializingIfNone>]
      buyPrice: ShopPrice option
      [<SkipSerializingIfNone>]
      unlockPrice: ShopPrice option }

type EffectContext = { name: string; description: string }

type TileContext =
    { name: string
      // desc
      description: string
      damage: int
      [<SkipSerializingIfNone>]
      attackEffect: EffectContext option
      [<SkipSerializingIfNone>]
      tileEffect: EffectContext option
      [<SkipSerializingIfNone>]
      buyPrice: ShopPrice option
      [<SkipSerializingIfNone>]
      unlockPrice: ShopPrice option
      [<SkipSerializingIfNone>]
      cooldownCharge: HpContext option
      [<SkipSerializingIfNone>]
      inAttackQueue: bool option
      [<SkipSerializingIfNone>]
      upgradeSlotsUsed: HpContext option }

type TileUpgradeContext =
    { [<SkipSerializingIfNone>]
      addedCooldown: int option
      [<SkipSerializingIfNone>]
      removedCooldown: int option
      [<SkipSerializingIfNone>]
      addedDamage: int option
      [<SkipSerializingIfNone>]
      removedDamage: int option
      [<SkipSerializingIfNone>]
      addedUpgradeSlots: int option
      [<SkipSerializingIfNone>]
      removedUpgradeSlots: int option
      usesUpgradeSlots: int
      [<SkipSerializingIfNone>]
      attackEffect: EffectContext option
      [<SkipSerializingIfNone>]
      tileEffect: EffectContext option }

type ShopContext =
    { name: string
      [<SkipSerializingIfNone>]
      consumables: ConsumableContext list option
      [<SkipSerializingIfNone>]
      upgrades: ShopUpgradeContext list option
      [<SkipSerializingIfNone>]
      skills: SkillContext list option
      [<SkipSerializingIfNone>]
      tiles: TileContext list option
      [<SkipSerializingIfNone>]
      fullHealPrice: ShopPrice option
      [<SkipSerializingIfNone>]
      rerollPrice: ShopPrice option
      [<SkipSerializingIfNone>]
      get5CoinsPrice: ShopPrice option
      [<SkipSerializingIfNone>]
      get10CoinsPrice: ShopPrice option }

type SpecialMoveContext =
    { name: string
      // desc
      description: string
      cooldownCharge: HpContext }

type PlayerContext =
    { name: string
      coins: int
      skullMetaCurrency: int
      facingDirection: Direction
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
      [<SkipSerializingIfEquals 0>]
      remainingPoisonedDuration: int
      [<SkipSerializingIfEquals false>]
      cursed: bool }

type LocationContext =
    { island: string
      name: string
      [<SkipSerializingIfNone>]
      shop1: string option
      [<SkipSerializingIfNone>]
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
      facingDirection: Direction
      [<SkipSerializingIfNone>]
      traits: string list option
      [<SkipSerializingIfNone>]
      elite: string option
      attackQueue: TileContext list
      intention: Intention
      hp: HpContext
      [<SkipSerializingIfEquals 0>]
      remainingFrozenDuration: int
      [<SkipSerializingIfEquals false>]
      shield: bool
      [<SkipSerializingIfEquals 0>]
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
      [<SkipSerializingIfNone>]
      spotlight: bool option
      [<SkipSerializingIfNone>]
      enemy: EnemyContext option
      [<SkipSerializingIfEquals 0>]
      traps: int
      // corrupted soul
      [<SkipSerializingIfNone>]
      flyingEnemy: EnemyContext option
      [<SkipSerializingIfNone>]
      youAreHereAndFacing: Direction option
      [<SkipSerializingIfEquals false>]
      goHereToOpenShop: bool
      [<SkipSerializingIfEquals false>]
      goHereToStartNewGame: bool }

type MapContext =
    { currentLocation: LocationContext
      paths: LocationContext list }

type RewardContext =
    { [<SkipSerializingIfNone>]
      name: string option
      [<SkipSerializingIfNone>]
      description: string option
      [<SkipSerializingIfNone>]
      pickTileOptions: TileContext list option
      [<SkipSerializingIfNone>]
      tileUpgrade: TileUpgradeContext option
      [<SkipSerializingIfNone>]
      tileSacrificeReward: ShopPrice option
      [<SkipSerializingIfEquals(false)>]
      warriorsGamble: bool
      [<SkipSerializingIfNone>]
      rerollPrice: ShopPrice option
      [<SkipSerializingIfNone>]
      price: ShopPrice option }

type HeroContext =
    { name: string
      specialMove: SpecialMoveContext
      mainDeckUnlocked: bool
      altDeckUnlocked: bool
      randomDeckUnlocked: bool
      maxUnlockedDay: int }

type NewGameContext =
    { heroes: HeroContext list
      // Ascension.DescriptionOfBuffActivatedOnDay(2)
      dayBuffs: string list }

type Context =
    { player: PlayerContext
      [<SkipSerializingIfNone>]
      shop: ShopContext option
      [<SkipSerializingIfNone>]
      shop1: RewardContext option
      [<SkipSerializingIfNone>]
      shop2: ShopContext option
      [<SkipSerializingIfNone>]
      newGameOptions: NewGameContext option
      [<SkipSerializingIfNone>]
      reward: RewardContext option
      [<SkipSerializingIfNone>]
      gridCells: CellContext list option
      [<SkipSerializingIfNone>]
      map: MapContext option }

module Context =
    let stripTags s = Regex(@"\[[^\]]*\]").Replace(s, "")
    let stripHtml s = Regex(@"<[^>]*>").Replace(s, "")

    let coinPrice n =
        { coins = n
          skulls = 0
          hp = 0
          maxHp = 0
          free = (n = 0) }

    let consumable (slot: int option) (potion: Potion) : ConsumableContext =
        { name = Some(stripTags potion.Name)
          description = Some(stripTags potion.Description)
          slot = slot
          buyPrice = None
          unlockPrice = None
          sellPrice =
            if Option.isSome slot && potion.CanBeSold then
                Some(
                    potion.BasePriceForHeroSelling
                    + if PotionsManager.Instance.RogueRetail then 1 else 0
                    |> coinPrice
                )
            else
                None }

    let deckMap () : (string * Tile) seq * Map<string, int> =
        TilesManager.Instance.Deck
        |> Seq.mapFold
            (fun state tile ->
                let name = stripTags tile.Attack.Name

                match Map.tryFind name state with
                | Some count -> (($"{name} ({count + 1})", tile), Map.add name (count + 1) state)
                | None -> ((name, tile), Map.add name 1 state))
            Map.empty

    let deck () : (string * Tile) seq = fst (deckMap ())

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

    let tile (player: bool) (name: string option) (tile: Tile) : TileContext =
        { name = stripTags (Option.defaultValue tile.Attack.Name name)
          damage = tile.Attack.Value
          description = stripTags tile.Attack.Description
          attackEffect = attackEffect tile.Attack.AttackEffect
          tileEffect = tileEffect tile.Attack.TileEffect
          buyPrice = None
          unlockPrice = None
          cooldownCharge = Some $"{tile.CooldownCharge}/{tile.Attack.Cooldown}"
          inAttackQueue =
            if player then
                Some(tile.TileContainer :? AttackQueueTileContainer)
            else
                None
          upgradeSlotsUsed =
            if player then
                Some $"{tile.Attack.Level}/{tile.Attack.MaxLevel}"
            else
                None }

    let skill (skill: Skill) : SkillContext =
        { name = skill.Name
          description = skill.Description
          level = $"{skill.Level}/{skill.MaxLevel}"
          buyPrice = None
          unlockPrice = None }

    let hp (s: AgentStats) : HpContext = $"{s.HP}/{s.maxHP}"

    let specialMove (hero: Hero) : SpecialMoveContext =
        { name = stripTags hero.SpecialAbilityName
          description = stripHtml hero.SpecialAbilityDescription
          cooldownCharge = $"{hero.SpecialMove.Cooldown.Charge}/{hero.SpecialMove.Cooldown.Cooldown}" }

    let enemy (enemy: Enemy) : EnemyContext =
        let traits =
            typeof<Enemy>
                .GetProperty("EnemyTraits", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue(enemy)
            :?> AgentEnums.EnemyTraitsEnum array

        { name = stripTags enemy.Name
          description = stripTags enemy.Description
          facingDirection = Dir.ofGame enemy.FacingDir
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
          attackQueue = enemy.AttackQueue.TCC.Tiles |> Seq.map (tile false None) |> List.ofSeq
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
            | null -> None, None
            | :? Hero as hero -> None, Some(Dir.ofGame hero.FacingDir)
            | x -> Some(enemy (x :?> Enemy)), None

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
          youAreHereAndFacing = you
          goHereToOpenShop = false
          goHereToStartNewGame = false }

    let hero (hero: Hero) : HeroContext =
        { name = stripTags hero.Name
          specialMove = specialMove hero
          mainDeckUnlocked = hero.Unlocked
          altDeckUnlocked = hero.Unlocked && hero.AltDeckUnlocked
          randomDeckUnlocked = hero.Unlocked && hero.RandomDeckUnlocked
          maxUnlockedDay = hero.CharacterSaveData.bestDay + 1 }

    let player () : PlayerContext =
        let hero = Globals.Hero
        let pm = PotionsManager.Instance
        let sm = SkillsManager.Instance
        let deck = deck ()

        { name = stripTags hero.Name
          coins = Globals.Coins
          skullMetaCurrency = Globals.KillCount
          facingDirection = Dir.ofGame hero.FacingDir
          consumables = pm.HeldPotions |> Array.mapi (fun i x -> consumable (Some i) x) |> List.ofArray
          tiles = deck |> Seq.map (fun (s, x) -> tile true (Some s) x) |> List.ofSeq
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

    let map () : MapContext =
        let map = MapManager.Instance.map

        { currentLocation = location map.CurrentMapLocation
          paths =
            map.MapLocations
            |> Seq.filter _.Reachable
            |> Seq.filter _.Uncovered
            |> Seq.map location
            |> List.ofSeq }

    let price (price: Price) : ShopPrice =
        let ret =
            { coinPrice 0 with
                free = (price.Value = 0) }

        match price.Currency with
        | ShopStuff.CurrencyEnum.coins -> { ret with coins = price.Value }
        | ShopStuff.CurrencyEnum.meta -> { ret with skulls = price.Value }
        | ShopStuff.CurrencyEnum.hp -> { ret with hp = price.Value }
        | ShopStuff.CurrencyEnum.maxHP -> { ret with maxHp = price.Value }
        | _ -> ret

    let shop (shop: Shop) : ShopContext =
        let name =
            typeof<Shop>
                .GetProperty("Name", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue(shop)
            :?> string

        let uis =
            typeof<Shop>
                .GetField("shopItemUIs", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue(shop)
            :?> Generic.List<ShopItemUI>
            |> List.ofSeq

        uis
        // public property that returns Interactable && !Interactable which is just what i need
        |> List.filter _.InfoBoxEnabled
        |> List.fold
            (fun (ret: ShopContext) ui ->
                match ui.shopItemData with
                | :? ConsumableShopItem as item ->
                    let item =
                        { consumable None item.potionPickupPrefab.PotionPrefab with
                            buyPrice = Some(price ui.price) }

                    { ret with
                        consumables = Some(item :: Option.defaultValue List.empty ret.consumables) }
                | :? ServiceShopItem as item ->
                    match item.shopServiceEnum with
                    | ShopStuff.ShopServiceEnum.reroll ->
                        { ret with
                            rerollPrice = Some(price ui.price) }
                    | ShopStuff.ShopServiceEnum.heal ->
                        { ret with
                            rerollPrice = Some(price ui.price) }
                    | ShopStuff.ShopServiceEnum.get5Coins ->
                        { ret with
                            get5CoinsPrice = Some(price ui.price) }
                    | ShopStuff.ShopServiceEnum.get10Coins ->
                        { ret with
                            get10CoinsPrice = Some(price ui.price) }
                    // unknown service
                    | _ -> ret
                | :? ShopUpgradeShopItem as item ->
                    let key =
                        typeof<ShopUpgradeShopItem>
                            .GetField("SlotTypeLocalizationTableKey", BindingFlags.NonPublic ||| BindingFlags.Instance)
                            .GetValue(item)
                        :?> string

                    let item =
                        { name = stripTags (Utils.LocalizationUtils.LocalizedString("Terms", key))
                          description = stripTags item.Description
                          unlockPrice = price ui.price }

                    { ret with
                        upgrades = Some(item :: Option.defaultValue List.empty ret.upgrades) }
                | :? SkillShopItemData as item ->
                    let item =
                        typeof<SkillShopItemData>
                            .GetField("skill", BindingFlags.NonPublic ||| BindingFlags.Instance)
                            .GetValue(item)
                        :?> Skill

                    let item =
                        { skill item with
                            buyPrice = Some(price ui.price) }

                    { ret with
                        skills = Some(item :: Option.defaultValue List.empty ret.skills) }
                | :? UnlockConsumableShopItem as item ->
                    let item =
                        { consumable None item.potionPickupPrefab.PotionPrefab with
                            unlockPrice = Some(price ui.price) }

                    { ret with
                        consumables = Some(item :: Option.defaultValue List.empty ret.consumables) }
                | :? UnlockSkillShopItem as item ->
                    let item =
                        typeof<UnlockSkillShopItem>
                            .GetField("item", BindingFlags.NonPublic ||| BindingFlags.Instance)
                            .GetValue(item)
                        :?> Skill

                    let item =
                        { skill item with
                            unlockPrice = Some(price ui.price) }

                    { ret with
                        skills = Some(item :: Option.defaultValue List.empty ret.skills) }
                | :? UnlockTileShopItem as item ->
                    let item =
                        typeof<UnlockTileShopItem>
                            .GetField("tile", BindingFlags.NonPublic ||| BindingFlags.Instance)
                            .GetValue(item)
                        :?> Tile

                    let item =
                        { tile false None item with
                            unlockPrice = Some(price ui.price) }

                    { ret with
                        tiles = Some(item :: Option.defaultValue List.empty ret.tiles) }
                // unknown unlock
                | :? UnlockShopItemData -> ret
                // unknown item
                | _ -> ret)
            { upgrades = None
              name = name
              consumables = None
              skills = None
              tiles = None
              fullHealPrice = None
              rerollPrice = None
              get5CoinsPrice = None
              get10CoinsPrice = None }
        |> (fun (x: ShopContext) ->
            { x with
                consumables = Option.map List.rev x.consumables
                upgrades = Option.map List.rev x.upgrades
                skills = Option.map List.rev x.skills
                tiles = Option.map List.rev x.tiles })

    let reward (reward: Reward) : RewardContext option =
        if reward.Exausted then
            None
        else
            let room = CombatSceneManager.Instance.Room

            let rerollPrice, price =
                match room with
                | :? ShopRoom as room ->
                    let upgrade =
                        typeof<ShopRoom>
                            .GetField("tileUpgradeInShop", BindingFlags.NonPublic ||| BindingFlags.Instance)
                            .GetValue(room)
                        :?> TileUpgradeInShop

                    None, Some(price upgrade.price)
                | :? RewardRoom as room -> Some(coinPrice room.rewardRerolling.RerollPrice), None
                | _ -> None, None

            let name, description, pickTileOptions, tileUpgrade, tileSacrificeReward, warriorsGamble =
                let a n = if n > 0 then Some n else None
                let r n = if n < 0 then Some(-n) else None

                match reward with
                | :? NewTileReward as reward ->
                    let _, map = deckMap ()

                    let pickTileOptions =
                        reward.NewTilePedestals
                        |> Array.map _.Tile
                        |> Array.mapFold
                            (fun state tile ->
                                let name = stripTags tile.Attack.Name

                                match Map.tryFind name state with
                                | Some count -> (($"{name} ({count + 1})", tile), Map.add name (count + 1) state)
                                | None -> ((name, tile), Map.add name 1 state))
                            map
                        |> fst
                        |> Array.map (fun (s, x) -> tile true (Some s) x)
                        |> List.ofArray

                    None, None, Some pickTileOptions, None, None, false
                | :? TileUpgradeReward as reward ->
                    match reward.TileUpgrade with
                    | :? AddAttackEffectTileUpgrade as reward ->
                        let upg =
                            { addedCooldown = a reward.cooldownDelta
                              removedCooldown = r reward.cooldownDelta
                              removedDamage = None
                              addedDamage = None
                              removedUpgradeSlots = None
                              addedUpgradeSlots = None
                              usesUpgradeSlots = 1
                              attackEffect = attackEffect reward.effect
                              tileEffect = None }

                        None, None, None, Some upg, None, false
                    // description is redundant
                    | :? AddTileEffectTileUpgrade as reward ->
                        let upg =
                            { addedCooldown = a reward.cooldownDelta
                              removedCooldown = r reward.cooldownDelta
                              removedDamage = None
                              addedDamage = None
                              removedUpgradeSlots = None
                              addedUpgradeSlots = None
                              usesUpgradeSlots = 1
                              attackEffect = None
                              tileEffect = tileEffect reward.effect }

                        None, None, None, Some upg, None, false
                    // for stats, description is redundant, dont show it
                    | :? StatsTileUpgrade as reward ->
                        let f s =
                            typeof<StatsTileUpgrade>
                                .GetField(s, BindingFlags.NonPublic ||| BindingFlags.Instance)
                                .GetValue(reward)
                            :?> int

                        let cooldownDelta, maxLevelDelta, levelDelta, attackDelta =
                            f "cooldownDelta", f "maxLevelDelta", f "levelDelta", f "attackDelta"

                        let upg =
                            { addedCooldown = a cooldownDelta
                              removedCooldown = r cooldownDelta
                              removedDamage = r attackDelta
                              addedDamage = a attackDelta
                              removedUpgradeSlots = r maxLevelDelta
                              addedUpgradeSlots = a maxLevelDelta
                              usesUpgradeSlots = levelDelta
                              attackEffect = None
                              tileEffect = None }

                        None, None, None, Some upg, None, false
                    // for sacrifice, description is redundant i guess
                    | :? SacrificeTileUpgrade as reward ->
                        let coins =
                            typeof<SacrificeTileUpgrade>
                                .GetField("nCoins", BindingFlags.NonPublic ||| BindingFlags.Instance)
                                .GetValue(reward)
                            :?> int

                        None, None, None, None, Some(coinPrice coins), false
                    // for warriors gamble, description/details makes sense to show
                    | :? WarriorGambleUpgrade as reward ->
                        Some(stripTags reward.Description), Some(stripTags reward.Details), None, None, None, true
                    | reward ->
                        Some(stripTags reward.Description), Some(stripTags reward.Details), None, None, None, false
                | _ -> None, None, None, None, None, false

            Some
                { name = name
                  description = description
                  pickTileOptions = pickTileOptions
                  tileUpgrade = tileUpgrade
                  tileSacrificeReward = tileSacrificeReward
                  warriorsGamble = warriorsGamble
                  price = price
                  rerollPrice = rerollPrice }


    let context (nobunaga: Cell list) (traps: (Cell * Trap) list) : Context =
        let room = CombatSceneManager.Instance.Room

        let mutable cells =
            room.Grid.Cells |> Array.map (cell nobunaga traps) |> List.ofArray

        let shop, shop1, shop2, reward, ngc =
            match room with
            | :? CampRoom as room ->
                cells <-
                    cells
                    |> List.mapi (fun i x ->
                        if i = 0 then
                            { x with goHereToOpenShop = true }
                        elif i = cells.Length - 1 then
                            { x with goHereToStartNewGame = true }
                        else
                            x)

                let ngc =
                    if room.HeroSelection.goButton.Interactable then
                        let heroes = room.HeroSelection.heroes |> Array.map hero |> List.ofArray
                        let maxDay = heroes |> List.map _.maxUnlockedDay |> List.max

                        Some
                            { heroes = heroes
                              dayBuffs =
                                [ 2..maxDay ]
                                |> List.map (fun i ->
                                    $"Day {i} - {stripTags (Ascension.DescriptionOfBuffActivatedOnDay i)}") }
                    else
                        None

                let shop =
                    cells.[0].youAreHereAndFacing |> Option.map (fun _ -> shop room.UnlocksShop)

                shop, None, None, None, ngc
            | :? RewardRoom as room ->
                let reward = reward room.Reward

                None, None, None, reward, None
            | :? ShopRoom as room ->
                let reward = reward room.TileUpgradeReward
                let shop = shop room.Shop
                None, reward, Some shop, None, None
            | _ -> None, None, None, None, None

        { player = player ()
          shop = shop
          shop1 = shop1
          shop2 = shop2
          reward = reward
          newGameOptions = ngc
          gridCells =
            if CombatSceneManager.Instance.CurrentMode = CombatSceneManager.Mode.combat then
                Some cells
            else
                None
          map =
            if CombatSceneManager.Instance.CurrentMode = CombatSceneManager.Mode.mapSelection then
                Some(map ())
            else
                None }

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
    let mutable skipDioramaTime = None
    let mutable isForce = false
    let mutable forceNames = None

    member _.InhibitForces
        with set value =
            if value then
                inhibitForces <- inhibitForces + 1
            else
                inhibitForces <- inhibitForces - 1

    member _.ScheduleDioramaSkip() =
        skipDioramaTime <- Some(DateTime.UtcNow.AddSeconds(1))

    member _.TrapGone(trap: Trap) =
        trapCells <- trapCells |> List.filter (snd >> (<>) trap)

    member _.TrapPlaced(trap: Trap) =
        trapCells <- (trapCell, trap) :: trapCells

    member _.TrapAttack(agent: Agent) =
        trapCell <- agent.Cell.Neighbour(agent.FacingDir, 1)

    member _.NobunagaCells(cells: Cell list) = nobunagaCells <- cells

    member this.PerformForce(names: string list) =
        if inhibitForces = 0 then
            let ctx = Context.context nobunagaCells trapCells

            isForce <- true

            this.Force(
                { state = Some(this.Serialize(ctx))
                  ephemeral_context = true
                  query =
                    match CombatSceneManager.Instance.CurrentMode with
                    | CombatSceneManager.Mode.mapSelection -> "Please pick your next destination"
                    | CombatSceneManager.Mode.reward -> "Please pick your rewards"
                    | _ -> "Please pick your next action"
                  action_names = names }
            )

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
            $"{agent.Name} got hit by {atkName}, but the attack didn't seem to have any effect..."
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

            man.EndOfCombatTurn.AddListener(fun () ->
                if not (CombatSceneManager.Instance.Room :? CampRoom) then
                    ctx "The enemies' turn has ended.")

            man.BeginningOfCombatTurn.AddListener(fun () ->
                if not (CombatSceneManager.Instance.Room :? CampRoom) then
                    ctx "It's the enemies' turn...")

            man.EnterRoom.AddListener(fun room ->
                ctx (
                    match room with
                    | :? CampRoom as room ->
                        "You are at the camp - a starting location. Here, you can access the metaprogression shop that unlocks new items for skulls, or you can start the game. The metaprogression shop's owner has a cat."
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
            man.HeroRevived.AddListener(fun () -> ctx "You've been revived! There won't be a next time!")
            // man.RoomBegin.AddListener(fun _room -> ())

            man.RoomEnd.AddListener(fun room ->
                if room.BannerTextEnd <> "" && not (room :? CampRoom) then
                    ctx $"Fight result: {room.BannerTextEnd}")

            // man.MapOpened.AddListener(fun () -> ())
            // man.MapCurrentLocationCleared.AddListener(fun () -> ())
            man.RewardBusy.AddListener(fun () -> inhibitForces <- inhibitForces + 1)
            man.RewardReady.AddListener(fun () -> inhibitForces <- inhibitForces - 1)

        this.ReregisterActions()

        match forceNames with
        | Some names when not isForce -> this.PerformForce names
        | _ -> ()

        if skipDioramaTime |> Option.exists (fun x -> DateTime.UtcNow < x) then
            let chars =
                typeof<DioramaManager>
                    .GetField("dioramaCharacters", BindingFlags.NonPublic ||| BindingFlags.Instance)
                    .GetValue(DioramaManager.Instance)
                :?> DioramaCharacters

            chars.SkipPressed()
            skipDioramaTime <- None

    override this.ReregisterActions() =
        let mutable shouldForce = false

        if
            CombatManager.Instance = null
            || Globals.Hero = null
            || TilesManager.Instance = null
            || CombatSceneManager.Instance = null
            || PotionsManager.Instance = null
            || EventsManager.Instance = null
            // || DioramaManager.Instance = null
            || UnlocksManager.Instance = null
            || SkillsManager.Instance = null
            || MapManager.Instance = null
        then
            ()
        else
            let mutable actions: Action list =
                [
                (*(this.Action CheatQuick)
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
                  (this.Action CheatHeal)*) ]

            if
                CombatManager.Instance.CombatInProgress
                && not CombatManager.Instance.TurnInProgress
                && CombatManager.Instance.AllowHeroAction
            then

                if Globals.Hero.AllowWait then
                    actions <- this.Action Wait :: actions
                    shouldForce <- true

                if Globals.Hero.AgentStats.ice <= 0 then
                    shouldForce <- true
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

            let actions = actions |> List.filter _.Valid

            this.RetainActions(actions |> List.map (fun x -> x))

            if shouldForce then
                let newForce = actions |> List.map _.Name

                if not isForce then
                    this.LogDebug($"!isforce {newForce}")

                if
                    not isForce
                    || isForce
                       && not (
                           forceNames
                           |> Option.forall (fun x -> x.Length = newForce.Length && List.forall2 (=) x newForce)
                       )
                then
                    isForce <- false
                    forceNames <- Some newForce

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
        | BuyCoins -> Error(None)
        // skip
        | SkipRewards -> Error(None)
        | SelectHero(_heroName, _altDeck, _day) -> Error(None)
        | ChoosePath _pathIndex -> Error(None)
        |> (fun x ->
            if Result.isOk x then
                isForce <- false
                forceNames <- None

            x)

    override _.LogError error =
        let fff = "fff"
        plugin.Logger.LogError $"{DateTime.UtcNow}.{DateTime.UtcNow.ToString(fff)} {error}"

    override _.LogDebug error =
        let fff = "fff"
        plugin.Logger.LogInfo $"{DateTime.UtcNow}.{DateTime.UtcNow.ToString(fff)} {error}"

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
        // Globals.DeveloperUtils._shortLocations <- true
        else
            ()
