namespace NeuroShogun

open System
open System.Collections
open System.Reflection
open System.Text.RegularExpressions
open BepInEx
open BepInEx.Logging
open HarmonyLib
open NeuroFSharp

type Dir =
    | Left
    | Right

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

module Deck =
    let ofStr s =
        match s with
        | "main" -> Main
        | "alt" -> Alt
        | "random" -> Random
        | s -> raise (Exception($"invalid deck in enum, expected main/alt/random, got {s}"))


type Observatory<'T when 'T: equality>(func: unit -> 'T) =
    let mutable value = None

    member _.Changed() =
        let old = value
        let cur = Some(func ())
        value <- cur
        old <> cur

type Actions =
    (*| [<Action("cheat_quick", "Cheat")>] CheatQuick
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
    | [<Action("cheat_heal", "Cheat")>] CheatHeal*)
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
    | [<Action("move", "Move in a direction (ends your turn)")>] Move of direction: Dir
    // available in combat when theres an enemy in a direction
    // mutability: set the directions enum
    | [<Action("special_move", "Execute the special move in a particular direction (ends your turn)")>] SpecialMove of
        direction: Dir
    // available in combat always
    // mutability: set the directions enum
    | [<Action("turn",
               "Turn around and face in another direction. Your facing direction may affect attacks and special moves, but it doesn't affect regular movement, nor does it affect taking damage from enemies.")>] Turn of
        direction: Dir
    // available in combat always (ice forces a wait in update loop)
    | [<Action("wait", "Wait for a single turn (ends your turn)")>] Wait
    // available in combat when queued tiles
    | [<Action("attack",
               "*Immediately* execute some (or all) of the attacks in your tile queue in a specified order. Any attacks that you have not listed will be removed from the queue. Be careful - your facing direction and position affects what the attacks will do. Ends your turn.")>] Attack of
        tileNames: string list
    // available in combat when any unqueued tiles
    // mutability: list all tiles
    | [<Action("queue_tile", "Add an attack to your tile queue (ends your turn)")>] QueueTile of tileName: string
    // available when PotionsManager.Instance.HeldPotions isnt empty
    // mutability: list all potions that CanBeUsed
    | [<Action("consume", "Consume an item (does not end your turn)")>] Consume of consumableName: string
    // available: when ShopRoom.Shop contains SkillShopItemData
    // mutability: list all that CanBeSold
    | [<Action("buy_skill", "Purchase a skill")>] BuySkill of skillName: string
    // available: when ShopRoom.Shop contains ConsumableShopItem
    // mutability: list all that CanBeSold
    | [<Action("buy_consumable", "Purchase a consumable")>] BuyConsumable of consumableName: string
    // available: when Potion.CanBeSold
    // mutability: list all that CanBeSold
    | [<Action("sell_consumable", "Sell a consumable")>] SellConsumable of consumableName: string
    // available: when ShopRoom.Shop contains ShopUpgradeShopItem
    // mutability: list all that CanBeSold
    | [<Action("unlock_shop_upgrade", "Purchase a shop upgrade")>] UnlockShopUpgrade of upgradeName: string
    // available: when ShopRoom.Shop contains UnlockShopItemData, or maybe campRoom.UnlocksShop
    // mutability: list all that CanBeSold
    | [<Action("unlock_consumable", "Purchase a consumable unlock")>] UnlockConsumable of consumableName: string
    | [<Action("unlock_skill", "Purchase a skill unlock")>] UnlockSkill of skillName: string
    | [<Action("unlock_tile", "Purchase a tile unlock")>] UnlockTile of tileName: string
    // available: when RewardRoom.Reward.TileUpgrade is WarriorGambleUpgrade
    // note: _CanUpgradeTileAndWhy, CannotUpgradeText
    | [<Action("warriors_gamble", "Reroll a tile, changing its attack and randomizing its upgrades")>] GambleTile of
        tileName: string
    | [<Action("apply_upgrade", "Apply the upgrade to a tile")>] ApplyUpgrade of tileName: string
    // available: when RewardRoom.Reward.TileUpgrade is SacrificeTileUpgrade
    | [<Action("sacrifice_tile", "Sacrifice a tile")>] SacrificeTile of tileName: string
    // available: when RewardRoom.Reward is NewTileReward
    | [<Action("pick_tile_reward", "Pick a tile reward out of the options available")>] PickTileReward of
        tileName: string
    // available: when ShopServiceEnum.get5Coins or get10Coins
    | [<Action("buy_5_coins", "Purchase 5 coins from the shop")>] Buy5Coins
    | [<Action("buy_10_coins", "Purchase 5 coins from the shop")>] Buy10Coins
    // available: when ShopServiceEnum.heal
    | [<Action("heal", "Purchase a full heal")>] BuyHeal
    | [<Action("reroll_shop", "Reroll the shop contents")>] RerollShop
    | [<Action("reroll_rewards", "Reroll the rewards")>] RerollRewards
    // available: when !rewardRoom.Reward.InProgress && !rewardRoom.Busy
    //            when rewardRoom.skipButton.Interactable
    // rewardRoom.SkipButtonPressed
    | [<Action("skip_rewards", "Skip the rewards")>] SkipRewards
    | [<Action("continue", "Exit the shop and move on")>] Continue
    // campRoom.HeroSelection   .StartingDeckSelection   .RerollDeck
    // IS ON A DELAY of openDelay
    | [<Action("new_game",
               "Start a new game, choosing a hero to play as. Day is the ascension level, higher games are harder but unlock more.")>] NewGame of
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
      cooldownCharge: HpContext
      [<SkipSerializingIfNone>]
      canBePerformed: bool option }

type PlayerContext =
    { name: string
      coins: int
      skullMetaCurrency: int
      facingDirection: Dir
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
    { [<SkipSerializingIfNone>]
      pathIndex: int option
      island: string
      name: string
      [<SkipSerializingIfNone>]
      shop1: string option
      [<SkipSerializingIfNone>]
      shop2: string option }

[<RequireQualifiedAccess>]
type Intention =
    | MoveLeft
    | MoveRight
    | MoveUp
    | LeapLeft
    | LeapRight
    | TurnLeft
    | TurnRight
    | QueueTile
    | Attack
    | Wait

type EnemyContext =
    { name: string
      description: string
      facingDirection: Dir
      [<SkipSerializingIfNone>]
      traits: string list option
      [<SkipSerializingIfNone>]
      elite: string option
      attackQueue: TileContext list
      intention: Intention
      [<SkipSerializingIfNone>]
      hp: HpContext option
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
      onlyVulnerableInSpotlight: bool
      [<SkipSerializingIfEquals false>]
      confusionResistance: bool
      [<SkipSerializingIfEquals false>]
      iceResistance: bool
      [<SkipSerializingIfEquals false>]
      pushResistance: bool }

type CellState =
    { nobunaga: Cell list
      corrupted: Cell list
      warnings: (string * Cell) list
      traps: (Cell * Trap) list
      bombs: (Cell * Bomb) list }

type CellContext =
    { xPos: int
      // nobunaga
      [<SkipSerializingIfNone>]
      spotlight: bool option
      [<SkipSerializingIfNone>]
      warnings: string list option
      [<SkipSerializingIfNone>]
      entity: EnemyContext option
      [<SkipSerializingIfNone>]
      enemy: EnemyContext option
      [<SkipSerializingIfEquals 0>]
      trapsForEnemies: int
      // corrupted soul
      [<SkipSerializingIfNone>]
      flyingEnemy: EnemyContext option
      [<SkipSerializingIfNone>]
      youAreHereAndFacing: Dir option
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
      [<SkipSerializingIfEquals(0)>]
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
          unlockPrice = None
          cooldownCharge = Some $"{tile.CooldownCharge}/{tile.Attack.Cooldown}"
          inAttackQueue =
            if
                player
                && CombatSceneManager.Instance.CurrentMode = CombatSceneManager.Mode.combat
            then
                Some(tile.TileContainer :? AttackQueueTileContainer)
            else
                None
          upgradeSlotsUsed =
            if
                player
                && CombatSceneManager.Instance.CurrentMode <> CombatSceneManager.Mode.combat
            then
                Some $"{tile.Attack.Level}/{tile.Attack.MaxLevel}"
            else
                None }

    let skill (skill: Skill) : SkillContext =
        { name = stripTags skill.Name
          description = stripTags skill.Description
          level = $"{skill.Level}/{skill.MaxLevel}"
          buyPrice = None
          unlockPrice = None }

    let hp (s: AgentStats) : HpContext = $"{s.HP}/{s.maxHP}"

    let specialMove (combat: bool) (hero: Hero) : SpecialMoveContext =
        let chkMove dir =
            let cell = Globals.Hero.Cell.Neighbour(Dir.toGame dir, 1)
            cell <> null && cell.Agent = null

        let chkSpecialMove dir =
            not (chkMove dir)
            && Globals.Hero.SpecialMove.Cooldown.IsCharged
            && Globals.Hero.SpecialMove.Allowed(Globals.Hero, Dir.toGame dir)

        { name = stripTags hero.SpecialAbilityName
          description = stripHtml hero.SpecialAbilityDescription
          cooldownCharge = $"{hero.SpecialMove.Cooldown.Charge}/{hero.SpecialMove.Cooldown.Cooldown}"
          canBePerformed =
            if combat then
                Some(chkSpecialMove Left || chkSpecialMove Right)
            else
                None }

    let enemy (names: Map<string, int>) (enemy: Enemy) : EnemyContext * Map<string, int> =
        let traits =
            typeof<Enemy>
                .GetProperty("EnemyTraits", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue(enemy)
            :?> AgentEnums.EnemyTraitsEnum array

        let baseName = stripTags enemy.Name

        let name, map =
            match Map.tryFind baseName names with
            | Some n -> $"{baseName} ({n + 1})", Map.add baseName (n + 1) names
            | None ->
                (if enemy :? TwinBaseBoss then
                     $"{baseName} (1)"
                 else
                     baseName),
                Map.add baseName 1 names

        { name = name
          description = stripTags enemy.Description
          facingDirection = Dir.ofGame enemy.FacingDir
          traits =
            if Array.isEmpty traits then
                None
            else
                traits
                |> Array.map (AgentEnums.AgentEnumsUtils.EnemyTraitDescription >> stripTags)
                |> List.ofArray
                |> Some
          elite =
            if enemy.EliteType = AgentEnums.EliteTypeEnum.None then
                None
            else
                Some(stripTags (AgentEnums.AgentEnumsUtils.EliteDescription enemy.EliteType))
          intention =
            match enemy.Action with
            | CombatEnums.ActionEnum.Wait when (enemy :? CorruptedSoulBoss) ->
                let enemy = enemy :?> CorruptedSoulBoss

                let up =
                    typeof<CorruptedSoulBoss>
                        .GetField("goUpTrigger", BindingFlags.NonPublic ||| BindingFlags.Instance)
                        .GetValue(enemy)
                    :?> bool

                if up then Intention.MoveUp else Intention.Wait
            | CombatEnums.ActionEnum.Wait -> Intention.Wait
            | CombatEnums.ActionEnum.MoveLeft when (enemy :? FumikoBoss || enemy :? StriderEnemy) -> Intention.LeapLeft
            | CombatEnums.ActionEnum.MoveLeft -> Intention.MoveLeft
            | CombatEnums.ActionEnum.MoveRight when (enemy :? FumikoBoss || enemy :? StriderEnemy) ->
                Intention.LeapRight
            | CombatEnums.ActionEnum.MoveRight -> Intention.MoveRight
            | CombatEnums.ActionEnum.Attack -> Intention.Attack
            | CombatEnums.ActionEnum.PlayTile -> Intention.QueueTile
            | CombatEnums.ActionEnum.FlipLeft -> Intention.TurnLeft
            | CombatEnums.ActionEnum.FlipRight -> Intention.TurnRight
            | _ -> Intention.Wait
          boss = enemy :? Boss
          onlyVulnerableInSpotlight = enemy :? NobunagaBoss
          attackQueue = enemy.AttackQueue.TCC.Tiles |> Seq.map (tile false None) |> List.ofSeq
          confusionResistance = enemy :? CorruptedSoulBoss
          iceResistance = not enemy.Freezable
          pushResistance = not enemy.Movable
          hp =
            match enemy with
            | :? ShogunSummonerEnemy
            | :? SwapperEnemy -> None
            | _ -> Some(hp enemy.AgentStats)
          remainingFrozenDuration = enemy.AgentStats.ice
          shield = enemy.AgentStats.shield
          remainingPoisonedDuration = enemy.AgentStats.poison
          cursed = enemy.AgentStats.curse },
        map

    let cell (state: CellState) (names: Map<string, int>) (cell: Cell) : CellContext * Map<string, int> =
        let names, flying =
            match CombatSceneManager.Instance.Room with
            | :? CorruptedSoulBossRoom as room when room.Boss <> null ->
                let boss = room.Boss :?> CorruptedSoulBoss

                if cell = boss.PseudoCell && not (boss.Animator.GetBool("IsDown")) then
                    let boss, names = enemy names boss
                    names, Some boss
                else
                    names, None
            | _ -> names, None

        let names, enemy, entity, you =
            match cell.Agent with
            | null -> names, None, None, None
            | :? Hero as hero -> names, None, None, Some(Dir.ofGame hero.FacingDir)
            | :? ThornsEnemy
            | :? DummyEnemy
            | :? BarricadeEnemy as x ->
                let ctx, names = enemy names (x :?> Enemy)
                names, None, Some ctx, None
            | x ->
                let ctx, names = enemy names (x :?> Enemy)
                names, Some ctx, None, None

        { // use 1-based indexing because corrupted wave mentions "odd/even" cells with the first cell being odd (probably)
          xPos = cell.IndexInGrid + 1
          spotlight =
            if List.isEmpty state.nobunaga then
                None
            else
                Some(List.contains cell state.nobunaga)
          warnings =
            let warnings =
                (state.warnings |> List.filter (snd >> (=) cell) |> List.map fst)
                @ (if List.contains cell state.corrupted then
                       [ "Corruption strikes this cell on the next turn (damages units, heals bosses)" ]
                   else
                       [])
                @ (let bombs =
                    state.bombs
                    |> List.filter (fst >> (=) cell)
                    |> List.map (fun (_, bomb) ->
                        typeof<Bomb>
                            .GetField("nTurnsLeft", BindingFlags.NonPublic ||| BindingFlags.Instance)
                            .GetValue(bomb)
                        :?> int
                        + 1)
                    |> List.filter ((>) 0)
                    |> List.sort

                   match bombs with
                   | [] -> []
                   | [ bomb ] -> [ $"Bomb strikes this cell in {bomb} turns" ]
                   | _ -> [ $"{List.length bombs} bombs hit this cell in {List.last bombs} turns" ])

            if List.isEmpty warnings then None else Some warnings
          entity = entity
          enemy = enemy
          trapsForEnemies = state.traps |> List.filter (fst >> (=) cell) |> List.fold (fun n _ -> n + 1) 0
          // corrupted soul
          flyingEnemy = flying
          youAreHereAndFacing = you
          goHereToOpenShop = false
          goHereToStartNewGame = false },
        names

    let hero (combat: bool) (hero: Hero) : HeroContext =
        { name = stripTags hero.Name
          specialMove = specialMove combat hero
          mainDeckUnlocked = hero.Unlocked
          altDeckUnlocked = hero.Unlocked && hero.AltDeckUnlocked
          randomDeckUnlocked = hero.Unlocked && hero.RandomDeckUnlocked
          maxUnlockedDay =
            if hero.Unlocked then
                min (max 1 (hero.CharacterSaveData.bestDay + 1)) Globals.CurrentlyImplementedMaxDay
            else
                0 }

    let player () : PlayerContext =
        let hero = Globals.Hero
        let pm = PotionsManager.Instance
        let sm = SkillsManager.Instance
        let deck = deck ()

        { name = stripTags hero.Name
          coins = Globals.Coins
          skullMetaCurrency = Globals.KillCount
          facingDirection = Dir.ofGame hero.FacingDir
          consumables =
            (pm.HeldPotions |> Array.mapi (fun i x -> consumable (Some i) x) |> List.ofArray)
            @ (List.init (pm.NPotionsSlots - pm.NPotions) (fun i ->
                { slot = Some(pm.NPotions + i)
                  name = None
                  description = None
                  buyPrice = None
                  unlockPrice = None
                  sellPrice = None }))
          tiles = deck |> Seq.map (fun (s, x) -> tile true (Some s) x) |> List.ofSeq
          specialMove = specialMove true hero
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

    let location (pathIndex: int option) (loc: MapLocation) : LocationContext =
        let loc = loc.location

        let shop =
            fun (comp: ShopComponent) -> Utils.LocalizationUtils.LocalizedString("Locations", comp.technicalName)

        let shop1, shop2 =
            match loc with
            | :? ShopLocation as loc -> Some(shop loc.leftShopComponent), Some(shop loc.rightShopComponent)
            | _ -> None, None

        { pathIndex = pathIndex
          name = stripTags loc.Name
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

        { currentLocation = location None map.CurrentMapLocation
          paths =
            map.MapLocations
            |> Seq.filter _.Reachable
            |> Seq.filter _.Uncovered
            |> Seq.mapi (fun i x -> location (Some i) x)
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

    let shopUis (shop: Shop) : ShopItemUI list =
        typeof<Shop>
            .GetField("shopItemUIs", BindingFlags.NonPublic ||| BindingFlags.Instance)
            .GetValue(shop)
        :?> Generic.List<ShopItemUI>
        // public property that returns Interactable && !Interactable which is just what i need
        |> Seq.filter _.InfoBoxEnabled
        |> List.ofSeq

    let skillShopData (item: SkillShopItemData) =
        typeof<SkillShopItemData>
            .GetField("skill", BindingFlags.NonPublic ||| BindingFlags.Instance)
            .GetValue(item)
        :?> Skill

    let unlockSkillShopData (item: UnlockSkillShopItem) =
        typeof<UnlockSkillShopItem>
            .GetField("item", BindingFlags.NonPublic ||| BindingFlags.Instance)
            .GetValue(item)
        :?> Skill

    let unlockTileShopData (item: UnlockTileShopItem) =
        typeof<UnlockTileShopItem>
            .GetField("tile", BindingFlags.NonPublic ||| BindingFlags.Instance)
            .GetValue(item)
        :?> Tile

    let shopUpgradeName (item: ShopUpgradeShopItem) =
        let key =
            typeof<ShopUpgradeShopItem>
                .GetField("SlotTypeLocalizationTableKey", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue(item)
            :?> string

        stripTags (Utils.LocalizationUtils.LocalizedString("Terms", key))


    let shop (buyable: bool) (shop: Shop) : ShopContext =
        let name =
            typeof<Shop>
                .GetProperty("Name", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue(shop)
            :?> string

        let uis = shopUis shop

        uis
        |> List.filter (fun x -> not buyable || x.price.CanAfford)
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
                    let name = shopUpgradeName item

                    let item =
                        { name = name
                          description = stripTags item.Description
                          unlockPrice = price ui.price }

                    { ret with
                        upgrades = Some(item :: Option.defaultValue List.empty ret.upgrades) }
                | :? SkillShopItemData as item ->
                    let item = skillShopData item

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
                    let item = unlockSkillShopData item

                    let item =
                        { skill item with
                            unlockPrice = Some(price ui.price) }

                    { ret with
                        skills = Some(item :: Option.defaultValue List.empty ret.skills) }
                | :? UnlockTileShopItem as item ->
                    let item = unlockTileShopData item

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

    let reward (rewardRoom: bool) (reward: Reward) : RewardContext option =
        if rewardRoom && reward.Exausted then
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
                | :? NewTileReward as reward when reward.gameObject.activeSelf && reward.NewTilePedestals <> null ->
                    let _, map = deckMap ()

                    let pickTileOptions =
                        reward.NewTilePedestals
                        |> Array.filter (fun p ->
                            let b =
                                typeof<NewTilePedestal>
                                    .GetField("button", BindingFlags.NonPublic ||| BindingFlags.Instance)
                                    .GetValue(p)
                                :?> MyButton

                            b.Interactable)
                        |> Array.map _.Tile
                        |> Array.filter ((<>) null)
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

                    let pickTileOptions =
                        if List.isEmpty pickTileOptions then
                            None
                        else
                            Some pickTileOptions

                    None, None, pickTileOptions, None, None, false
                | :? TileUpgradeReward as reward when reward.TileUpgrade <> null ->
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

    let newGame (room: CampRoom) : NewGameContext option =
        let locked =
            typeof<HeroSelection>
                .GetField("transitionInProgress", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue(room.HeroSelection)
            :?> bool

        if room.HeroSelection.goButton.Interactable && not locked then
            let heroes = room.HeroSelection.heroes |> Array.map (hero false) |> List.ofArray
            let maxDay = heroes |> List.map _.maxUnlockedDay |> List.max

            Some
                { heroes = heroes
                  dayBuffs =
                    [ 2..maxDay ]
                    |> List.map (fun i -> $"Day {i} - {stripTags (Ascension.DescriptionOfBuffActivatedOnDay i)}") }
        else
            None

    let context (state: CellState) : Context =
        let room = CombatSceneManager.Instance.Room

        let mutable cells =
            room.Grid.Cells |> Array.mapFold (cell state) Map.empty |> fst |> List.ofArray

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

                let ngc = newGame room

                let shop =
                    cells.[0].youAreHereAndFacing
                    |> Option.map (fun _ -> shop false room.UnlocksShop)

                shop, None, None, None, ngc
            | :? RewardRoom as room ->
                let reward = reward true room.Reward

                None, None, None, reward, None
            | :? ShopRoom as room ->
                let reward = reward false room.TileUpgradeReward
                let shop = shop false room.Shop
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

        (if char then "???: " else "The Astronomer: ") + stripTags text

    let mutable initDone = false
    let mutable nobunagaCells = List.empty
    let mutable corruptedCells = List.empty
    let mutable trapCells = List.empty
    let mutable bombCells = List.empty
    let mutable warnings = List.empty
    let mutable trapCell = null
    let mutable skipDioramaTime = None
    let mutable isForce = false
    let mutable forceNames = None
    let mutable enemyNameMap: (Enemy * string) list = List.empty

    let enemyName (enemy: Enemy) =
        enemyNameMap
        |> List.tryFind (fst >> (=) enemy)
        |> Option.map snd
        |> Option.defaultValue enemy.Name
        |> stripTags

    let mutable nullAttackReason =
        "poison/thorns/trap/shockwave/karma (figure it out yourself)"

    let shouldAn (s: string) =
        s.Length > 0
        && List.contains s.[0] [ 'a'; 'e'; 'y'; 'u'; 'i'; 'o'; 'A'; 'E'; 'Y'; 'U'; 'I'; 'O' ]

    member this.InhibitForces
        with set value =
            if value then
                inhibitForces <- inhibitForces + 1
            else
                inhibitForces <- inhibitForces - 1

            this.LogDebug $"Inhibitiion level {inhibitForces}"

    member _.ScheduleDioramaSkip() =
        skipDioramaTime <- Some(DateTime.UtcNow.AddSeconds(1))

    member _.TrapGone(trap: Trap) =
        trapCells <- trapCells |> List.filter (snd >> (<>) trap)

    member _.TrapPlaced(trap: Trap) =
        trapCells <- (trapCell, trap) :: trapCells

    member _.TrapAttack(agent: Agent) =
        trapCell <- agent.Cell.Neighbour(agent.FacingDir, 1)

    member _.BombPlaced(bomb: Bomb) =
        let cell =
            typeof<Bomb>
                .GetField("cell", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue(bomb)
            :?> Cell

        bombCells <- (cell, bomb) :: bombCells
        let left = cell.Neighbour(Utils.Dir.Left, 1)
        let right = cell.Neighbour(Utils.Dir.Left, 1)

        if left <> null then
            bombCells <- (left, bomb) :: bombCells

        if right <> null then
            bombCells <- (right, bomb) :: bombCells

    member _.BombGone(bomb: Bomb) =
        bombCells <- bombCells |> List.filter (snd >> (<>) bomb)

    member _.NobunagaCells
        with set cells = nobunagaCells <- cells

    member _.CorruptedCells
        with get () = corruptedCells
        and set value = corruptedCells <- value

    member this.PerformForce(names: string list) =
        try
            if inhibitForces = 0 && not (List.isEmpty names) then
                let ctx =
                    Context.context
                        { nobunaga = nobunagaCells
                          traps = trapCells
                          warnings = warnings
                          bombs = bombCells
                          corrupted = corruptedCells }

                isForce <- true

                this.Force(
                    { state = Some(this.Serialize(ctx))
                      ephemeral_context = true
                      query =
                        match CombatSceneManager.Instance.CurrentMode with
                        | CombatSceneManager.Mode.mapSelection -> "Please pick your next destination"
                        | CombatSceneManager.Mode.reward -> "Please pick your rewards"
                        | _ ->
                            "It's your turn! Execute any actions, they will be performed immediately. After your turn is over, enemies' turn will begin, they will act *exactly* according to their current intentions."
                      action_names = names }
                )
        with _ ->
            ()

    member _.NullAttackReason
        with set value = nullAttackReason <- value

    member this.ReceiveAttack(agent: Agent, hit: Hit, attacker: Agent | null) =
        let atkName =
            match attacker with
            | :? Hero ->
                // hit.Damage <- hit.Damage * 100
                "you"
            | null -> nullAttackReason
            | :? Enemy as attacker -> enemyName attacker
            | attacker -> attacker.Name

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
               | _ ->
                   if atkName = "you" && CombatManager.Instance.KillStreak > 0 then
                       $" The hit is lethal ({CombatManager.Instance.KillStreak + 1} combo)."
                   else
                       " The hit is lethal.")

        match agent with
        | :? Hero -> $"You have been hit by {atkName} for {hit.Damage} damage.{effects}"
        | :? NobunagaBoss as agent when nobunagaCells |> List.exists ((=) agent.Cell) |> not ->
            $"{enemyName agent} got hit by {atkName}, but the attack didn't seem to have any effect..."
        | :? Enemy as agent -> $"{enemyName agent} has been hit by {atkName}.{effects}"
        | _ -> $"{agent.Name} has been hit by {atkName}.{effects}"
        |> this.Context false

    member this.ShowShopkeeperDialogue(text: string) =
        this.Context false $"The shopkeeper says: {stripTags text}"

    member this.ShowCatDialogue(text: string) =
        this.Context false $"The cat says: {stripTags text}"

    member this.ShowDialogue (agent: Agent) (text: string) =
        match agent with
        | :? Hero -> $"You, {stripTags Globals.Hero.Name}, say: {Context.stripHtml text}"
        | :? Boss as agent -> $"{enemyName agent} says: {Context.stripHtml text}"
        | _ ->
            if shouldAn (stripTags agent.Name) then
                $"An {stripTags agent.Name} says: {Context.stripHtml text}"
            else
                $"A {stripTags agent.Name} says: {Context.stripHtml text}"
        |> (this.Context false)

    member this.CurtainDown(title: string) =
        this.Context false $"The curtains have been pulled down {stripTags title}"

    member this.CurtainUp() =
        this.Context false $"The curtains are rising. The next act unfolds."

    member this.CreditsStart() =
        this.Context
            false
            "Congratulations, you've beaten the game on day 7, the highest day! The credits are now playing."

    member this.DioramaStart() =
        this.Context
            true
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
                   "Previous dialogue for reference:\n"
                   + ({ 1 .. Globals.Day - 1 }
                      |> Seq.map (fun day ->
                          let name =
                              Utils.LocalizationUtils.LocalizedString("ShopAndNPC", $"Diorama_Day_{day}_Title")

                          Seq.append
                              (seq { $"Day {day}: {name}" })
                              (DioramaData.DioramaUtils.GetConversationLines day |> Seq.map dioramaLine))
                      |> Seq.concat
                      |> String.concat "\n"))

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

    member _.AddWarning (text: string) (cell: Cell) = warnings <- (text, cell) :: warnings

    member this.EnemyTurnStart() =
        corruptedCells <- List.empty
        warnings <- List.empty

        if not (CombatSceneManager.Instance.Room :? CampRoom) then
            this.Context true "It's the enemies' turn..."

            enemyNameMap <-
                CombatManager.Instance.Enemies
                |> Seq.sortBy _.Cell.IndexInGrid
                |> Seq.mapFold
                    (fun s e ->
                        let c, s = Context.enemy s e
                        (e, c.name), s)
                    Map.empty
                |> fst
                |> List.ofSeq

    member this.EnemyTurnEnd() =
        if not (CombatSceneManager.Instance.Room :? CampRoom) then
            this.Context true "The enemies' turn has ended."

    member this.EnterRoom() =
        let combatPrompt =
            "Avoid enemy attacks whenever you can, healing is expensive! You can dodge, outmaneuver the enemies by predicting where they will go, or just kill them before they get the chance to deal damage. Sometimes you can even trick the enemies into attacking each other! Each cell can only contain a single entity, you can't normally move into enemies, but special moves allow you to do it in certain conditions. Enemies will only do what they *intend** to do, and will not do anything else on their turn. Keep track of attacks' direction."

        this.Context
            false
            (match CombatSceneManager.Instance.Room with
             | :? CampRoom as room ->
                 "You are at the camp - a starting location. Here, you can access the metaprogression shop that unlocks new items for skulls, or you can start the game. The metaprogression shop's owner has a cat."
                 + (if room.UnlocksShop.CanBuyAnything() then
                        " You have items available for purchase in the shop."
                    else
                        " You can't currently purchase anything from the shop.")
             | :? ShogunBossRoom ->
                 $"You, {stripTags Globals.Hero.Name}, have reached the final boss - this is the Shogun Showdown! {combatPrompt}"
             | :? BossRoom as room ->
                 $"You, {stripTags Globals.Hero.Name}, have encountered a boss - {enemyName room.Boss}. {combatPrompt}"
             | :? CombatRoom as room ->
                 $"You have entered a new location - {stripTags room.Name} - prepare for a fight! {combatPrompt}"
             | :? RewardRoom -> $"You can now claim your rewards (or skip them)"
             | :? ShopRoom -> $"You have entered a shop."
             | _ -> $"You have entered a new room")

    member this.ExitRoom(room: Room) =
        nobunagaCells <- List.empty
        trapCells <- List.empty
        corruptedCells <- List.empty
        warnings <- List.empty
        bombCells <- List.empty
        let win = CombatSceneManager.Instance.progression.IsLastLevel

        if win then
            this.Context false $"Congratulations, you won on day (difficulty/ascension level) {Globals.Day}/7!"
        elif room.BannerTextEnd <> "" && not (room :? CampRoom) then
            this.Context false $"Fight result: {room.BannerTextEnd}"

    member this.HeroDied() =
        nobunagaCells <- List.empty
        trapCells <- List.empty
        corruptedCells <- List.empty
        warnings <- List.empty
        bombCells <- List.empty

        match Globals.Hero.LastAttacker with
        | null -> "Game over. You died."
        | :? Hero -> "Game over. You committed seppuku..."
        | :? Boss as boss -> $"Game over. You were slain by {enemyName boss}."
        | enemy when shouldAn (stripTags enemy.Name) -> $"Game over. You were slain by an {stripTags enemy.Name}."
        | enemy -> $"Game over. You were slain by a {stripTags enemy.Name}."
        |> (this.Context false)

    member this.BossDied(boss: Boss) =
        let metaR =
            typeof<Boss>
                .GetProperty("MetaCurrencyReward", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue(boss)
            :?> int

        let coinR =
            typeof<Boss>
                .GetProperty("CoinReward", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue(boss)
            :?> int

        this.Context
            false
            $"You have defeated {stripTags boss.Name} and got a reward of {metaR} skulls and {coinR} coins!"

    member this.WaveSpawned(wave: Wave) =
        this.Context false $"A new wave of {wave.NEnemies} enemies has spawned!"

    member this.SkillTriggered(skill: Skill) =
        this.Context false $"The skill {stripTags skill.Name} has been triggered!"

    member this.Update() =
        if not initDone && EventsManager.Instance <> null then
            initDone <- true
            let man = EventsManager.Instance

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

                this.Context false $"New {stamp} stamp level achieved: {level}")

        // man.MapOpened.AddListener(fun () -> this.InhibitForces <- false)
        // man.MapCurrentLocationCleared.AddListener(fun () -> this.InhibitForces <- false)

        this.ReregisterActions' false

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

    override this.ReregisterActions() = this.ReregisterActions' true

    member this.ReregisterActions'(forceAnyway: bool) =
        let mutable shouldForce = forceAnyway

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

            let lock =
                typeof<CombatManager>
                    .GetField("heroPlayedTileInThisUpdate", BindingFlags.NonPublic ||| BindingFlags.Instance)
                    .GetValue(CombatManager.Instance)
                :?> bool

            if
                CombatManager.Instance.CombatInProgress
                && not CombatManager.Instance.TurnInProgress
                && CombatManager.Instance.AllowHeroAction
                && not lock
                && CombatSceneManager.Instance.CurrentMode = CombatSceneManager.Mode.combat
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
                            let x = (x :?> ArraySchema)
                            x.Unique <- true
                            x.MinItems <- 1
                            (x.Items :?> StringSchema).SetEnum(queue))

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

                        let play = this.Action QueueTile

                        play.MutateProp "tileName" (fun x -> (x :?> StringSchema).SetEnum(hand))

                        actions <- play :: actions

            let fmtPrice (price: ShopPrice) : string =
                match price with
                | { coins = coins } when coins > 0 -> $"{coins} coins"
                | { skulls = skulls } when skulls > 0 -> $"{skulls} skulls"
                | { hp = hp } when hp > 0 -> $"{hp} HP"
                | { maxHp = maxHp } when maxHp > 0 -> $"{maxHp} max HP"
                | _ -> "free"

            let mutable forceAllowShop = false

            let shop, reward =
                match CombatSceneManager.Instance.Room with
                | :? CampRoom as room ->
                    if Array.last room.Grid.Cells = Globals.Hero.Cell then
                        let ngc = Context.newGame room

                        match ngc with
                        | Some ngc ->
                            let maxDay = ngc.heroes |> List.map _.maxUnlockedDay |> List.max
                            let main = ngc.heroes |> List.exists _.mainDeckUnlocked
                            let alt = ngc.heroes |> List.exists _.altDeckUnlocked
                            let rand = ngc.heroes |> List.exists _.randomDeckUnlocked

                            let names =
                                ngc.heroes
                                |> Array.ofList
                                |> Array.filter _.mainDeckUnlocked
                                |> Array.map _.name

                            let decks =
                                (if main then [ Main ] else [])
                                @ (if alt then [ Alt ] else [])
                                @ (if rand then [ Random ] else [])

                            let act = this.Action NewGame
                            act.MutateProp "heroName" (fun x -> (x :?> StringSchema).SetEnum names)

                            act.MutateProp "deck" (fun x ->
                                (x :?> StringSchema).RetainEnum(fun s -> List.contains (Deck.ofStr s) decks))

                            act.MutateProp "day" (fun x ->
                                let x = x :?> IntegerSchema
                                x.Minimum <- Some 1
                                x.Maximum <- Some maxDay)

                            actions <- act :: actions

                            None, None
                        | None -> None, None
                    elif Array.head room.Grid.Cells = Globals.Hero.Cell then
                        Some room.UnlocksShop, None
                    else
                        None, None
                | _ when CombatSceneManager.Instance.CurrentMode <> CombatSceneManager.Mode.reward -> None, None
                | :? RewardRoom as room ->
                    if room.Reward.InProgress && room.skipButton.Interactable then
                        let act = this.Action SkipRewards
                        actions <- act :: actions

                    if room.rewardRerolling.rerollButton.Interactable then
                        let act = this.Action RerollRewards
                        actions <- act :: actions

                    if room.Busy || room.Reward = null then
                        None, None
                    else
                        None, Some(room.Reward, true)
                | :? ShopRoom as room ->
                    let goBtn =
                        typeof<ShopRoom>
                            .GetField("goButton", BindingFlags.NonPublic ||| BindingFlags.Instance)
                            .GetValue(room)
                        :?> MyButton

                    if goBtn.gameObject.activeSelf then
                        let act = this.Action Continue
                        actions <- act :: actions

                    let reward =
                        if room.TileUpgradeReward = null then
                            None
                        else
                            let upgrade =
                                typeof<ShopRoom>
                                    .GetField("tileUpgradeInShop", BindingFlags.NonPublic ||| BindingFlags.Instance)
                                    .GetValue(room)
                                :?> TileUpgradeInShop

                            if upgrade.price.CanAfford then
                                Some(room.TileUpgradeReward :> Reward, false)
                            else
                                forceAllowShop <- true
                                None

                    Some room.Shop, reward
                | _ -> None, None

            if
                CombatSceneManager.Instance.CurrentMode = CombatSceneManager.Mode.mapSelection
                && not MapManager.Instance.map.MovingInProgress
                && MapManager.Instance.map.LocationSelectionMode
                && not MapManager.Instance.mapScreen.IsInTransition
                && MapManager.Instance.Interactable
            then
                shouldForce <- true
                let map = Context.map ()
                let act = this.Action ChoosePath

                if not (List.isEmpty map.paths) then
                    let minIdx = map.paths |> List.map _.pathIndex.Value |> List.min
                    let maxIdx = map.paths |> List.map _.pathIndex.Value |> List.max

                    act.MutateProp "pathIndex" (fun x ->
                        let x = x :?> IntegerSchema
                        x.Minimum <- Some minIdx
                        x.Maximum <- Some maxIdx)

                    actions <- act :: actions

            shop
            |> Option.iter (fun shop ->
                let ctx = Context.shop true shop

                if Option.isSome reward || forceAllowShop then
                    shouldForce <- true

                ctx.get10CoinsPrice
                |> Option.iter (fun price ->
                    let act = this.Action Buy10Coins
                    act.Description <- act.InitialDescription + $" for {fmtPrice price}"
                    actions <- act :: actions)

                ctx.get5CoinsPrice
                |> Option.iter (fun price ->
                    let act = this.Action Buy5Coins
                    act.Description <- act.InitialDescription + $" for {fmtPrice price}"
                    actions <- act :: actions)

                ctx.fullHealPrice
                |> Option.iter (fun price ->
                    let act = this.Action BuyHeal
                    act.Description <- act.InitialDescription + $" for {fmtPrice price}"
                    actions <- act :: actions)

                ctx.rerollPrice
                |> Option.iter (fun price ->
                    let act = this.Action RerollShop
                    act.Description <- act.InitialDescription + $" for {fmtPrice price}"
                    actions <- act :: actions)

                ctx.tiles
                |> Option.iter (fun tiles ->
                    let names = tiles |> Array.ofList |> Array.map _.name
                    let act = this.Action UnlockTile
                    act.MutateProp "tileName" (fun x -> (x :?> StringSchema).SetEnum(names))
                    actions <- act :: actions)

                ctx.upgrades
                |> Option.iter (fun upgs ->
                    let names = upgs |> Array.ofList |> Array.map _.name
                    let act = this.Action UnlockShopUpgrade
                    act.MutateProp "upgradeName" (fun x -> (x :?> StringSchema).SetEnum(names))
                    actions <- act :: actions)

                let chooseMap x y =
                    Array.choose (fun z -> if Option.isSome (x z) then Some(y z) else None)

                ctx.skills
                |> Option.iter (fun skills ->
                    let n1 = skills |> Array.ofList |> chooseMap _.unlockPrice _.name
                    let n2 = skills |> Array.ofList |> chooseMap _.buyPrice _.name
                    let act = this.Action UnlockSkill
                    act.MutateProp "skillName" (fun x -> (x :?> StringSchema).SetEnum(n1))
                    actions <- act :: actions
                    let act = this.Action BuySkill
                    act.MutateProp "skillName" (fun x -> (x :?> StringSchema).SetEnum(n2))
                    actions <- act :: actions)

                ctx.consumables
                |> Option.iter (fun cs ->
                    let n1 = cs |> Array.ofList |> chooseMap _.unlockPrice _.name.Value
                    let n2 = cs |> Array.ofList |> chooseMap _.buyPrice _.name.Value
                    let act = this.Action UnlockConsumable
                    act.MutateProp "consumableName" (fun x -> (x :?> StringSchema).SetEnum(n1))
                    actions <- act :: actions
                    let act = this.Action BuyConsumable
                    act.MutateProp "consumableName" (fun x -> (x :?> StringSchema).SetEnum(n2))
                    actions <- act :: actions))

            reward
            |> Option.bind (fun x -> Option.map (fun y -> x, y) (Context.reward false (fst x)))
            |> Option.iter (fun ((reward, rewardRoom), ctx) ->
                let reward = reward

                match reward with
                | :? NewTileReward when reward.gameObject.activeSelf ->
                    ctx.pickTileOptions
                    |> Option.iter (fun opts ->
                        if rewardRoom then
                            shouldForce <- true

                        let names = Array.ofList opts |> Array.map _.name
                        let act = this.Action PickTileReward
                        act.MutateProp "tileName" (fun x -> (x :?> StringSchema).SetEnum(names))
                        actions <- act :: actions)
                | :? TileUpgradeReward as reward when reward.TileUpgrade <> null ->
                    let deck =
                        Context.deck ()
                        |> Seq.filter (snd >> reward.TileUpgrade.CanUpgradeTile)
                        |> Seq.map fst
                        |> Array.ofSeq

                    ctx.tileUpgrade
                    |> Option.iter (fun _ ->
                        if rewardRoom then
                            shouldForce <- true

                        let act = this.Action ApplyUpgrade
                        act.MutateProp "tileName" (fun x -> (x :?> StringSchema).SetEnum(deck))
                        actions <- act :: actions)

                    ctx.tileSacrificeReward
                    |> Option.iter (fun _ ->
                        if rewardRoom then
                            shouldForce <- true

                        let act = this.Action SacrificeTile
                        act.MutateProp "tileName" (fun x -> (x :?> StringSchema).SetEnum(deck))
                        actions <- act :: actions)

                    if ctx.warriorsGamble then
                        if rewardRoom then
                            shouldForce <- true

                        let act = this.Action GambleTile
                        act.MutateProp "tileName" (fun x -> (x :?> StringSchema).SetEnum(deck))
                        actions <- act :: actions
                | :? NewTileReward
                | :? TileUpgradeReward ->
                    if rewardRoom && not reward.Exausted then
                        shouldForce <- false
                // this.LogDebug "Reward not ready"
                | null -> this.LogError "Null reward"
                | _ -> this.LogError "Unknown reward")

            if CombatSceneManager.Instance.CurrentMode <> CombatSceneManager.Mode.mapSelection then
                let potions' =
                    try
                        PotionsManager.Instance.HeldPotions |> Array.filter (_.AlreadyUsed >> not)
                    with _ ->
                        [||]

                let potions =
                    potions' |> Array.filter _.CanBeUsed |> Array.map (_.Name >> stripTags)

                let usePotion = this.Action Consume
                usePotion.MutateProp "consumableName" (fun x -> (x :?> StringSchema).SetEnum(potions))
                actions <- usePotion :: actions

                let potions =
                    potions' |> Array.filter _.CanBeSold |> Array.map (_.Name >> stripTags)

                let sellPotion = this.Action SellConsumable
                sellPotion.MutateProp "consumableName" (fun x -> (x :?> StringSchema).SetEnum(potions))
                actions <- sellPotion :: actions

            let actions = actions |> List.filter _.Valid

            this.RetainActions(actions |> List.map (fun x -> x))

            if shouldForce && inhibitForces = 0 then
                let newForce = actions |> List.map _.Name

                if
                    forceAnyway
                    || not isForce
                    || isForce
                       && not (
                           forceNames
                           |> Option.forall (fun x -> x.Length = newForce.Length && List.forall2 (=) x newForce)
                       )
                then
                    isForce <- false
                    forceNames <- Some newForce

    override _.Name = "Shogun Showdown"

    override this.HandleAction(action: Actions) =
        let copyPrice (price: Price) : (ShopStuff.CurrencyEnum * int) = (price.Currency, price.Value)

        let fmtPrice'' (infix: string) (price: (ShopStuff.CurrencyEnum * int)) : string =
            let value = snd price

            match fst price with
            | ShopStuff.CurrencyEnum.coins -> $"{value} coins, you{infix} have {Globals.Coins} coins"
            | ShopStuff.CurrencyEnum.meta -> $"{value} skulls, you{infix} have {Globals.KillCount} skulls"
            | ShopStuff.CurrencyEnum.hp -> $"{value} HP, you{infix} have {Globals.Hero.AgentStats.HP} HP"
            | ShopStuff.CurrencyEnum.maxHP -> $"{value} max HP, you{infix} have {Globals.Hero.AgentStats.maxHP} max HP"
            | _ -> $"{value} ???"

        let fmtPrice' = fmtPrice'' " now"
        let fmtPrice = copyPrice >> fmtPrice'' ""

        let shop =
            match CombatSceneManager.Instance.Room with
            | :? CampRoom as room -> Ok room.UnlocksShop
            | :? ShopRoom as room -> Ok room.Shop
            | _ -> Error(Some "There's no shop in this room!")

        let finalizePurchase (name: string) (x: Result<ShopItemUI list, string option>) =
            x
            |> Result.bind (fun thisItem ->
                let toBuy = thisItem |> List.tryFind _.price.CanAfford

                match toBuy with
                | Some ui ->
                    let price = copyPrice ui.price
                    let shop = (Result.toOption shop).Value
                    UINavigation.UINavigationHelper.SelectNewTarget(shop, ui)
                    ui.Submit()
                    Ok(Some $"Bought {name} for {fmtPrice' price}")
                | None ->
                    Error(Some $"This {name} is too expensive - it costs {(List.head thisItem).price |> fmtPrice}!"))

        let findService ty =
            Result.bind (fun shop ->
                let items =
                    Context.shopUis shop
                    |> List.choose (fun ui ->
                        match ui.shopItemData with
                        | :? ServiceShopItem as item -> Some(ui, item)
                        | _ -> None)

                let thisItem = items |> List.filter (snd >> _.shopServiceEnum >> (=) ty)

                if List.isEmpty thisItem then
                    Error(Some "You can't do that in this shop")
                else
                    Ok(List.map fst thisItem))

        let reward =
            match CombatSceneManager.Instance.Room with
            | :? RewardRoom as room -> Ok room.Reward
            | :? ShopRoom as room ->
                let upgrade =
                    typeof<ShopRoom>
                        .GetField("tileUpgradeInShop", BindingFlags.NonPublic ||| BindingFlags.Instance)
                        .GetValue(room)
                    :?> TileUpgradeInShop

                if upgrade.price.CanAfford then
                    Ok room.TileUpgradeReward
                else
                    Error(Some $"You can't afford this, it requires {fmtPrice (upgrade.price)}!")
            | _ -> Error(Some "There's no shops or rewards in this room!")

        match action with
        (*| CheatQuick ->
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
        | CheatLoadout(n_rewards, tiles, attackEffects, skills, consumables) ->
            let l = Globals.DeveloperUtils.loadout
            let n_rewards = Option.defaultValue l.nRewards n_rewards
            let tiles = Option.defaultValue l.tiles tiles
            let effects = Option.defaultValue l.attackEffects attackEffects
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
            Ok(Some "healed")*)
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
                                $"Tiles that you haven't listed were removed from the attack queue: {List.map fst toRemove |> this.Serialize}. Note that adding tiles to the queue costs turns, so unplayed tiles are usually wasted turns"
                            )

                    context))
        | QueueTile tileName ->
            combatError false
            |> chk TilesManager.Instance.CanInteractWithTiles "You can't currently use tiles"
            |> chk Globals.Hero.AttackQueue.CanAddTile "Your attack queue is full"
            |> chk
                (CombatSceneManager.Instance.CurrentMode = CombatSceneManager.Mode.combat)
                "You are not currently in combat"
            |> Result.bind (fun () ->
                match Context.deck () |> Seq.tryFind (fst >> (=) tileName) with
                | Some tile -> Ok(snd tile)
                | None -> Error(Some $"This tile is not in your deck: {tileName}"))
            |> Result.bind (fun tile ->
                Ok(tile)
                |> chk tile.TileIsEnabled "The tile is currently on cooldown"
                |> chk
                    (tile.TileContainer :? HandTileContainer)
                    (if (tile.TileContainer :? AttackQueueTileContainer) then
                         "The tile is already in the attack queue"
                     else
                         "The tile is not currently in your hand"))
            |> Result.map (fun tile ->
                tile.TileContainer.UponTileSubmit()
                None)

        | Consume consumableName ->
            let room = CombatSceneManager.Instance.Room

            match
                PotionsManager.Instance.HeldPotions
                |> Array.filter (_.AlreadyUsed >> not)
                |> Array.tryFind (_.Name >> stripTags >> (=) consumableName)
            with
            | Some potion when potion.CanBeUsed ->
                let hadShield = Globals.Hero.AgentStats.shield
                let prevHp = Context.hp Globals.Hero.AgentStats
                potion.Submit()

                match potion with
                | :? D6Potion ->
                    match room with
                    | :? CombatRoom -> "Confusing all enemies..."
                    | :? RewardRoom -> "Rerolling the reward..."
                    | :? ShopRoom -> "Rerolling the shop content..."
                    | _ -> "???"
                | :? FreezeTimePotion ->
                    $"Freezing every enemy for {Parameters.GameParams.iceEffectTurnsDuration} turns..."
                | :? HealSmallPotion ->
                    let curHp = Context.hp Globals.Hero.AgentStats
                    $"Healed from {prevHp} to {curHp}"
                | :? MassCursePotion -> "Cursing every enemy, doubling their next taken damage"
                | :? PoisonPotion ->
                    $"Poisoning every enemy for {Parameters.GameParams.poisonEffectTurnsDuration} turns, they will take 1 damage each turn"
                | :? RainOfMirrorsPotion -> "Mirroring every enemy and their intentions..."
                | :? ShieldPotion ->
                    if hadShield then
                        "You already had a shield, so the potion did nothing, whoops..."
                    else
                        "Added a shield, completely nullifying your next damage taken"
                | :? CoolUpPotion -> "Recharged all cooldowns"
                | _ -> "Potion used"
                |> (Some >> Ok)
            | Some _ ->
                if CombatManager.Instance.CombatInProgress && CombatManager.Instance.TurnInProgress then
                    "Please wait for your turn first"
                else
                    "This consumable can't be used in this location"
                |> (Some >> Error)
            | None ->
                $"This consumable doesn't exist, existing consumables: {PotionsManager.Instance.HeldPotions
                                                                        |> Array.map (_.Name >> stripTags)
                                                                        |> List.ofArray
                                                                        |> this.Serialize}"
                |> (Some >> Error)
        | BuyConsumable consumableName ->
            shop
            |> Result.bind (fun shop ->
                let items =
                    Context.shopUis shop
                    |> List.choose (fun ui ->
                        match ui.shopItemData with
                        | :? ConsumableShopItem as item -> Some(ui, item)
                        | _ -> None)

                let thisItem =
                    items
                    |> List.filter (snd >> _.potionPickupPrefab.PotionPrefab.Name >> stripTags >> (=) consumableName)

                if List.isEmpty thisItem then
                    let names =
                        items
                        |> List.map (snd >> _.potionPickupPrefab.PotionPrefab.Name >> stripTags)
                        |> this.Serialize

                    Error(Some $"Consumable with this name was not found in the shop, available consumables: {names}")
                else
                    Ok(List.map fst thisItem))
            |> finalizePurchase "consumable"
        | BuySkill skillName ->
            shop
            |> Result.bind (fun shop ->
                let items =
                    Context.shopUis shop
                    |> List.choose (fun ui ->
                        match ui.shopItemData with
                        | :? SkillShopItemData as item -> Some(ui, item)
                        | _ -> None)

                let thisItem =
                    items
                    |> List.filter (snd >> Context.skillShopData >> _.Name >> stripTags >> (=) skillName)

                if List.isEmpty thisItem then
                    let names =
                        items
                        |> List.map (snd >> Context.skillShopData >> _.Name >> stripTags)
                        |> this.Serialize

                    Error(Some $"Skill with this name was not found in the shop, available skills: {names}")
                else
                    Ok(List.map fst thisItem))
            |> finalizePurchase "skill"
        | UnlockShopUpgrade shopUpgradeName ->
            shop
            |> Result.bind (fun shop ->
                let items =
                    Context.shopUis shop
                    |> List.choose (fun ui ->
                        match ui.shopItemData with
                        | :? ShopUpgradeShopItem as item -> Some(ui, item)
                        | _ -> None)

                let thisItem =
                    items |> List.filter (snd >> Context.shopUpgradeName >> (=) shopUpgradeName)

                if List.isEmpty thisItem then
                    let names = items |> List.map (snd >> Context.shopUpgradeName) |> this.Serialize

                    Error(Some $"Shop upgrade with this name was not found in the shop, available upgrades: {names}")
                else
                    Ok(List.map fst thisItem))
            |> finalizePurchase "shop upgrade"
        | UnlockConsumable consumableName ->
            shop
            |> Result.bind (fun shop ->
                let items =
                    Context.shopUis shop
                    |> List.choose (fun ui ->
                        match ui.shopItemData with
                        | :? UnlockConsumableShopItem as item -> Some(ui, item)
                        | _ -> None)

                let thisItem =
                    items
                    |> List.filter (snd >> _.potionPickupPrefab.PotionPrefab.Name >> stripTags >> (=) consumableName)

                if List.isEmpty thisItem then
                    let names =
                        items
                        |> List.map (snd >> _.potionPickupPrefab.PotionPrefab.Name >> stripTags)
                        |> this.Serialize

                    Error(Some $"Consumable with this name was not found in the shop, available consumables: {names}")
                else
                    Ok(List.map fst thisItem))
            |> finalizePurchase "consumable"
        | UnlockSkill skillName ->
            shop
            |> Result.bind (fun shop ->
                let items =
                    Context.shopUis shop
                    |> List.choose (fun ui ->
                        match ui.shopItemData with
                        | :? UnlockSkillShopItem as item -> Some(ui, item)
                        | _ -> None)

                let thisItem =
                    items
                    |> List.filter (snd >> Context.unlockSkillShopData >> _.Name >> stripTags >> (=) skillName)

                if List.isEmpty thisItem then
                    let names =
                        items
                        |> List.map (snd >> Context.unlockSkillShopData >> _.Name >> stripTags)
                        |> this.Serialize

                    Error(Some $"Skill with this name was not found in the shop, available skills: {names}")
                else
                    Ok(List.map fst thisItem))
            |> finalizePurchase "skill"
        | UnlockTile tileName ->
            shop
            |> Result.bind (fun shop ->
                let items =
                    Context.shopUis shop
                    |> List.choose (fun ui ->
                        match ui.shopItemData with
                        | :? UnlockTileShopItem as item -> Some(ui, item)
                        | _ -> None)

                let thisItem =
                    items
                    |> List.filter (snd >> Context.unlockTileShopData >> _.Attack.Name >> stripTags >> (=) tileName)

                if List.isEmpty thisItem then
                    let names =
                        items
                        |> List.map (snd >> Context.unlockTileShopData >> _.Attack.Name >> stripTags)
                        |> this.Serialize

                    Error(Some $"Tile with this name was not found in the shop, available tiles: {names}")
                else
                    Ok(List.map fst thisItem))
            |> finalizePurchase "tile"
        | RerollShop ->
            shop
            |> findService ShopStuff.ShopServiceEnum.reroll
            |> finalizePurchase "shop reroll"
        | BuyHeal ->
            shop
            |> findService ShopStuff.ShopServiceEnum.heal
            |> finalizePurchase "full heal"
        | Buy5Coins ->
            shop
            |> findService ShopStuff.ShopServiceEnum.get5Coins
            |> finalizePurchase "5 coins"
        | Buy10Coins ->
            shop
            |> findService ShopStuff.ShopServiceEnum.get10Coins
            |> finalizePurchase "10 coins"
        | SellConsumable consumableName ->
            match
                PotionsManager.Instance.HeldPotions
                |> Array.filter (_.AlreadyUsed >> not)
                |> Array.tryFind (_.Name >> stripTags >> (=) consumableName)
            with
            | Some potion when potion.CanBeSold ->
                let price =
                    potion.BasePriceForHeroSelling
                    + if PotionsManager.Instance.RogueRetail then 1 else 0

                potion.SellConsumable()
                Ok(Some $"Sold consumable for {price} coins, you now have {Globals.Coins} coins")
            | Some _ ->
                match CombatSceneManager.Instance.CurrentMode with
                | CombatSceneManager.Mode.transition
                | CombatSceneManager.Mode.mapSelection -> "You can't sell consumables right now"
                | _ -> "You can only sell consumables in shops"
                |> (Some >> Error)
            | None ->
                $"This consumable doesn't exist, existing consumables: {PotionsManager.Instance.HeldPotions
                                                                        |> Array.map (_.Name >> stripTags)
                                                                        |> List.ofArray
                                                                        |> this.Serialize}"
                |> (Some >> Error)
        | Continue ->
            match CombatSceneManager.Instance.Room with
            | :? ShopRoom as room ->
                let goBtn =
                    typeof<ShopRoom>
                        .GetField("goButton", BindingFlags.NonPublic ||| BindingFlags.Instance)
                        .GetValue(room)
                    :?> MyButton

                goBtn.Click()
                Ok None
            | _ -> Error(Some "You are not currently in a shop")
        // rewards
        | ApplyUpgrade tileName ->
            reward
            |> chk TilesManager.Instance.CanInteractWithTiles "You can't currently use tiles"
            |> chk
                (CombatSceneManager.Instance.CurrentMode = CombatSceneManager.Mode.reward)
                "You are not currently in reward mode"
            |> Result.bind (fun reward ->
                match Context.deck () |> Seq.tryFind (fst >> (=) tileName) with
                | Some(_, tile) ->
                    Ok(reward, tile)
                    |> chk tile.TileIsEnabled "The tile is currently on cooldown"
                    |> chk
                        (tile.TileContainer :? HandTileContainer)
                        (if (tile.TileContainer :? AttackQueueTileContainer) then
                             "The tile is already in the attack queue"
                         else
                             "The tile is not currently in your hand")
                | None -> Error(Some $"This tile is not in your deck: {tileName}"))
            |> Result.bind (fun (reward, tile) ->
                match reward with
                | :? TileUpgradeReward as reward ->
                    match reward.TileUpgrade with
                    | :? AddAttackEffectTileUpgrade
                    | :? AddTileEffectTileUpgrade
                    | :? StatsTileUpgrade ->
                        if reward.TileUpgrade.CanUpgradeTile tile then
                            Ok(reward, tile)
                        else
                            let txt = reward.TileUpgrade.CannotUpgradeText tile
                            Error(Some(stripTags txt))
                    | _ -> Error(Some "You can't use this action in this location")
                | _ -> Error(Some "You can't use this action in this location"))
            |> Result.bind (fun (reward, tile) ->
                tile.TileContainer.UponTileSubmit()
                // CanBeNavigatedTo is cc.HasTile here
                if reward.CanBeNavigatedTo then
                    reward.DecisionTaken()
                    Ok None
                else
                    Error(Some "Unknown mod error"))
        | GambleTile tileName ->
            reward
            |> chk TilesManager.Instance.CanInteractWithTiles "You can't currently use tiles"
            |> chk
                (CombatSceneManager.Instance.CurrentMode = CombatSceneManager.Mode.reward)
                "You are not currently in reward mode"
            |> Result.bind (fun reward ->
                match Context.deck () |> Seq.tryFind (fst >> (=) tileName) with
                | Some(_, tile) ->
                    Ok(reward, tile)
                    |> chk tile.TileIsEnabled "The tile is currently on cooldown"
                    |> chk
                        (tile.TileContainer :? HandTileContainer)
                        (if (tile.TileContainer :? AttackQueueTileContainer) then
                             "The tile is already in the attack queue"
                         else
                             "The tile is not currently in your hand")
                | None -> Error(Some $"This tile is not in your deck: {tileName}"))
            |> Result.bind (fun (reward, tile) ->
                match reward with
                | :? TileUpgradeReward as reward ->
                    match reward.TileUpgrade with
                    | :? WarriorGambleUpgrade ->
                        if reward.TileUpgrade.CanUpgradeTile tile then
                            Ok(reward, tile)
                        else
                            let txt = reward.TileUpgrade.CannotUpgradeText tile
                            Error(Some(stripTags txt))
                    | _ -> Error(Some "You can't use this action in this location")
                | _ -> Error(Some "You can't use this action in this location"))
            |> Result.bind (fun (reward, tile) ->
                tile.TileContainer.UponTileSubmit()
                // CanBeNavigatedTo is cc.HasTile here
                if reward.CanBeNavigatedTo then
                    reward.DecisionTaken()
                    Ok None
                else
                    Error(Some "Unknown mod error"))
        | SacrificeTile tileName ->
            reward
            |> chk TilesManager.Instance.CanInteractWithTiles "You can't currently use tiles"
            |> chk
                (CombatSceneManager.Instance.CurrentMode = CombatSceneManager.Mode.reward)
                "You are not currently in reward mode"
            |> Result.bind (fun reward ->
                match Context.deck () |> Seq.tryFind (fst >> (=) tileName) with
                | Some(_, tile) ->
                    Ok(reward, tile)
                    |> chk tile.TileIsEnabled "The tile is currently on cooldown"
                    |> chk
                        (tile.TileContainer :? HandTileContainer)
                        (if (tile.TileContainer :? AttackQueueTileContainer) then
                             "The tile is already in the attack queue"
                         else
                             "The tile is not currently in your hand")
                | None -> Error(Some $"This tile is not in your deck: {tileName}"))
            |> Result.bind (fun (reward, tile) ->
                match reward with
                | :? TileUpgradeReward as reward ->
                    match reward.TileUpgrade with
                    | :? SacrificeTileUpgrade ->
                        if reward.TileUpgrade.CanUpgradeTile tile then
                            Ok(reward, tile)
                        else
                            let txt = reward.TileUpgrade.CannotUpgradeText tile
                            Error(Some(stripTags txt))
                    | _ -> Error(Some "You can't use this action in this location")
                | _ -> Error(Some "You can't use this action in this location"))
            |> Result.bind (fun (reward, tile) ->
                tile.TileContainer.UponTileSubmit()
                // CanBeNavigatedTo is cc.HasTile here
                if reward.CanBeNavigatedTo then
                    reward.DecisionTaken()
                    Ok None
                else
                    Error(Some "Unknown mod error"))
        | PickTileReward tileName ->
            reward
            |> chk
                (CombatSceneManager.Instance.CurrentMode = CombatSceneManager.Mode.reward)
                "You are not currently in reward mode"
            |> Result.bind (fun reward ->
                match reward with
                | :? NewTileReward as reward ->
                    let _, map = Context.deckMap ()

                    let pickTileOptions =
                        reward.NewTilePedestals
                        |> Array.mapFold
                            (fun state p ->
                                let name = stripTags p.Tile.Attack.Name

                                match Map.tryFind name state with
                                | Some count -> (($"{name} ({count + 1})", p), Map.add name (count + 1) state)
                                | None -> ((name, p), Map.add name 1 state))
                            map
                        |> fst

                    match pickTileOptions |> Array.tryFind (fst >> (=) tileName) with
                    | Some(_, tile) -> Ok tile
                    | None ->
                        Error(
                            Some
                                $"This tile is not available here, available options are: {this.Serialize(Array.map fst pickTileOptions)}"
                        )
                | _ -> Error(Some "You can't use this action in this location"))
            |> Result.map (fun tile ->
                tile.Select()
                tile.OnButtonClick()
                None)
        | RerollRewards ->
            match CombatSceneManager.Instance.Room with
            | :? RewardRoom as room ->
                if room.rewardRerolling.rerollButton.Interactable then
                    let price = room.rewardRerolling.RerollPrice
                    room.rewardRerolling.RerollButtonPressed()
                    Ok(Some $"Rerolled reward for {price} coins, you now have {Globals.Coins} coins")
                elif Globals.Coins < room.rewardRerolling.RerollPrice then
                    Error(
                        Some
                            $"Rerolling currently costs {room.rewardRerolling.RerollPrice} coins, while you only have {Globals.Coins} coins"
                    )
                else
                    Error(Some "Can't reroll at the moment")
            | _ -> Error(Some "There's no reward to reroll")
        | SkipRewards ->
            match CombatSceneManager.Instance.Room with
            | :? RewardRoom as room ->
                if room.Reward.InProgress && room.skipButton.Interactable then
                    room.SkipButtonPressed()
                    Ok None
                else
                    Error(Some "Can't skip, the reward is probably already being skipped")
            | _ -> Error(Some "There's no reward to skip")
        // nav
        | NewGame(heroName, deck, day) ->
            match CombatSceneManager.Instance.Room with
            | :? CampRoom as room ->
                let sel = room.HeroSelection

                let locked =
                    typeof<HeroSelection>
                        .GetField("transitionInProgress", BindingFlags.NonPublic ||| BindingFlags.Instance)
                        .GetValue(sel)
                    :?> bool

                match
                    sel.heroes
                    |> Array.mapi (fun i x -> i, x)
                    |> Array.tryFind (snd >> _.Name >> stripTags >> (=) heroName)
                with
                | _ when locked -> Error(Some "A new game is already starting, please wait...")
                | Some(i, hero) when hero.Unlocked ->
                    let deckUnlocked =
                        match deck with
                        | Main -> true
                        | Alt -> hero.AltDeckUnlocked
                        | Random -> hero.RandomDeckUnlocked

                    let dayUnlocked =
                        min (max 1 (hero.CharacterSaveData.bestDay + 1)) Globals.CurrentlyImplementedMaxDay


                    if day > dayUnlocked then
                        Error(
                            Some
                                $"Max unlocked day for this hero is {dayUnlocked}! Beat the game on a lower day to unlock higher days."
                        )
                    elif not deckUnlocked then
                        Error(
                            Some
                                "You haven't unlocked this deck! Alt deck is unlocked by collecting 3 hero stamps, random deck is unlocked by beating the game on day 4"
                        )
                    else
                        sel.UpdateDay(day - Globals.Day)

                        let cur =
                            typeof<HeroSelection>
                                .GetField("iHero", BindingFlags.NonPublic ||| BindingFlags.Instance)
                                .GetValue(sel)
                            :?> int

                        sel.HeroIndexUpdate(i - cur)
                        Ok None
                | Some(_, hero) ->
                    Error(
                        Some
                            $"This hero is not yet unlocked, the unlock quest is: {stripTags hero.QuestForUnlocking.Description}"
                    )
                | None ->
                    let names = sel.heroes |> Array.map (_.Name >> stripTags) |> this.Serialize
                    Error(Some $"No hero with this name! Available heroes: {names}")
            | _ -> Error(Some "Can't start a new game, you are not in the camp")
        | ChoosePath pathIndex ->
            if
                CombatSceneManager.Instance.CurrentMode = CombatSceneManager.Mode.mapSelection
                && not MapManager.Instance.map.MovingInProgress
                && MapManager.Instance.map.LocationSelectionMode
            then
                let loc =
                    MapManager.Instance.map.MapLocations
                    |> Seq.filter _.Reachable
                    |> Seq.filter _.Uncovered
                    |> Seq.mapi (fun i x -> i, x)
                    |> Seq.tryFind (fst >> (=) pathIndex)
                    |> Option.map snd

                match loc with
                | Some loc ->
                    if MapManager.Instance.map.SelectLocation loc then
                        Ok None
                    else
                        Error(Some "Unknown mod error")
                | None ->
                    let map = Context.map ()
                    let minIdx = map.paths |> List.map _.pathIndex.Value |> List.min
                    let maxIdx = map.paths |> List.map _.pathIndex.Value |> List.max

                    Error(
                        Some
                            $"Index out of bounds, the currently available range is from {minIdx} to {maxIdx} (inclusive)"
                    )
            else
                Error(Some "You can't yet select the next location")
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
    let mutable logger: ManualLogSource | null = null
    let cts = new Threading.CancellationTokenSource()

    [<DefaultValue>]
    static val mutable private instance: MainClass

    static member Instance = MainClass.instance
    member _.Game = game.Value

    member _.Logger: ManualLogSource =
        match logger with
        | null -> raise (Exception "this isn't supposed to happen")
        | x -> x

    member this.Awake() =
        MainClass.instance <- this
        harmony <- Harmony.CreateAndPatchAll(Assembly.GetExecutingAssembly())
        logger <- base.Logger
        let cnt = Seq.fold (fun x _ -> x + 1) 0 (harmony.GetPatchedMethods())
        Globals.ForcePlayTutorial <- false
        Globals.Tutorial <- false
        Globals.SkipTitleScreen <- true

        game <-
            Some(
                let game = Game(this)
                game.Start(None, cts.Token) |> ignore
                game
            )

        this.Logger.LogInfo($"Plugin NeuroShogun is loaded with {cnt} patches!")

    // wow i can't imagine this person is so lazy who would ever do so much on every frame smh my head
    member _.LateUpdate() =
        game |> Option.iter (fun x -> x.Update())

    member this.PreSceneLoad(name: string) =
        this.Logger.LogInfo($"Init {name}")

        if Globals.GameInitialized && not initDone then
            this.Logger.LogInfo("Initializing")
            initDone <- true

            if SaveDataManager.Instance.runSaveData.hasRunInProgress then
                Globals.ContinueRun <- true
        // Globals.DeveloperUtils._invulnerable <- true
        // Globals.DeveloperUtils._customLocation <- true
        // Globals.DeveloperUtils._quick <- true
        // Globals.DeveloperUtils._shortLocations <- true
        else
            ()
