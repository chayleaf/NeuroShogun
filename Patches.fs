namespace NeuroShogun

open System.Collections
open System.Reflection
open HarmonyLib
open UnityEngine.SceneManagement

type EnumeratorSeq(a: IEnumerator, b: IEnumerator) =
    let mutable current = a

    interface IEnumerator with
        override _.Current = current.Current

        override _.MoveNext() =
            if current.MoveNext() then
                true
            elif current = a then
                current <- b
                current.MoveNext()
            else
                false

        override _.Reset() =
            a.Reset()
            b.Reset()
            current <- a

type EnumeratorSingle(func: unit -> obj) =
    let mutable init = true

    interface IEnumerator with
        override _.Current = func ()

        override _.MoveNext() =
            let ret = init
            init <- false
            ret

        override _.Reset() = init <- true

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

[<HarmonyPatch>]
type public Patches() =
    static let mutable lastPickup: Pickup | null = null
    static let mutable csmInstance: CreditsSceneManager | null = null

    [<HarmonyPatch(typeof<SceneManager>, nameof (SceneManager.LoadScene: string -> unit), [| typeof<string> |])>]
    [<HarmonyPrefix>]
    static member public PreSceneLoad(sceneName: string) =
        MainClass.Instance.PreSceneLoad(sceneName)

    // [<HarmonyPatch(typeof<Globals>, "Developer", MethodType.Getter)>]
    // [<HarmonyPostfix>]
    // static member GlobalsDeveloper(__result: bool byref) = __result <- true

    [<HarmonyPatch(typeof<Shop>, nameof Unchecked.defaultof<Shop>.InstantiateAndThrowPickupAtHero)>]
    [<HarmonyPostfix>]
    static member ThrowPickupAtHero(__result: Pickup byref) = lastPickup <- __result

    [<HarmonyPatch(typeof<Shop>, "ShopkeeperGiveFreeConsumableCoroutine")>]
    [<HarmonyPostfix>]
    static member ShopkeeperGiveFreeConsumableHijack(__result: IEnumerator byref) =
        MainClass.Instance.Logger.LogInfo "freec start"
        MainClass.Instance.Game.InhibitForces <- true

        __result <-
            EnumeratorWrapper(
                __result,
                ignore,
                (fun () ->
                    // dont wait for it to update, just force update it
                    typeof<Pickup>
                        .GetField("playerOverlapping", BindingFlags.NonPublic ||| BindingFlags.Instance)
                        .SetValue(lastPickup, true)

                    typeof<Pickup>
                        .GetMethod("Update", BindingFlags.NonPublic ||| BindingFlags.Instance)
                        .Invoke(lastPickup, [||])
                    |> ignore

                    MainClass.Instance.Logger.LogInfo "freec end"
                    MainClass.Instance.Game.InhibitForces <- false)
            )

    [<HarmonyPatch(typeof<RainOfMirrorsEffect>, "PerformEffect")>]
    [<HarmonyPostfix>]
    static member PerformRainOfMirrors(__result: IEnumerator byref) =
        MainClass.Instance.Logger.LogInfo "rom start"
        MainClass.Instance.Game.InhibitForces <- true

        __result <-
            EnumeratorWrapper(
                __result,
                ignore,
                (fun () ->
                    MainClass.Instance.Logger.LogInfo "rom end"
                    MainClass.Instance.Game.InhibitForces <- false)
            )

    [<HarmonyPatch(typeof<Shop>, "ItemBoughtSequenceBegin")>]
    [<HarmonyPostfix>]
    static member ShopLock() =
        MainClass.Instance.Logger.LogInfo "ibs start"
        MainClass.Instance.Game.InhibitForces <- true

    [<HarmonyPatch(typeof<Shop>, "ItemBoughtSequenceOver")>]
    [<HarmonyPostfix>]
    static member ShopUnlock() =
        MainClass.Instance.Logger.LogInfo "ibs end"
        MainClass.Instance.Game.InhibitForces <- false

    [<HarmonyPatch(typeof<DioramaManager>, "Start")>]
    [<HarmonyPrefix>]
    static member DioramaStart() = MainClass.Instance.Game.DioramaStart()

    [<HarmonyPatch(typeof<DioramaManager>, "LeaveScene")>]
    [<HarmonyPrefix>]
    static member DioramaEnd() = MainClass.Instance.Game.DioramaEnd()

    [<HarmonyPatch(typeof<DioramaCharacters>, "ShowDialogueText")>]
    [<HarmonyPrefix>]
    static member DioramaLine(text: string) =
        MainClass.Instance.Game.ShowDioramaDialogue text

    [<HarmonyPatch(typeof<Agent>, nameof Unchecked.defaultof<Agent>.ShowDialogue)>]
    [<HarmonyPrefix>]
    static member ShowDialogue(__instance: Agent, text: string) =
        MainClass.Instance.Game.ShowDialogue __instance text

    [<HarmonyPatch(typeof<CreditsSceneManager>, "Awake")>]
    [<HarmonyPostfix>]
    static member GetCreditsSceneManagerInstance(__instance: CreditsSceneManager) = csmInstance <- __instance

    [<HarmonyPatch(typeof<ScrollingCredits>, "CreditsRollSequence")>]
    [<HarmonyPostfix>]
    static member CreditsSeq(__result: IEnumerator byref, __instance: ScrollingCredits) =
        MainClass.Instance.Game.CreditsStart()
        Globals.SkipTitleScreen <- true

        match csmInstance with
        | null -> ()
        | csmInstance ->
            __result <-
                EnumeratorWrapper(
                    EnumeratorSeq(
                        __result,
                        EnumeratorSingle(fun () ->
                            let canvas: UnityEngine.RectTransform =
                                typeof<ScrollingCredits>
                                    .GetField("canvas", BindingFlags.NonPublic ||| BindingFlags.Instance)
                                    .GetValue(__instance)
                                :?> UnityEngine.RectTransform

                            let scrollSpeed =
                                typeof<ScrollingCredits>
                                    .GetField("scrollSpeed", BindingFlags.NonPublic ||| BindingFlags.Instance)
                                    .GetValue(__instance)
                                :?> float32

                            let time = (canvas.sizeDelta.y * canvas.localScale.y - 4.0f) / scrollSpeed
                            UnityEngine.WaitForSeconds(time))
                    ),
                    ignore,
                    csmInstance.Continue
                )

    [<HarmonyPatch(typeof<Cat>, "WaitAndMaoAndPurr")>]
    [<HarmonyPostfix>]
    static member MeowMeowLol(__result: IEnumerator byref) =
        __result <-
            EnumeratorWrapper(
                __result,
                (fun i ->
                    match i with
                    | 0 -> ()
                    | 1 ->
                        MainClass.Instance.Game.ShowCatDialogue(
                            Utils.LocalizationUtils.LocalizedString("ShopAndNPC", "Cat_Mao")
                        )
                    | 2 ->
                        MainClass.Instance.Game.ShowCatDialogue(
                            Utils.LocalizationUtils.LocalizedString("ShopAndNPC", "Cat_Purr")
                        )
                    | _ -> MainClass.Instance.Logger.LogWarning "What is the cat doing?"),
                id
            )

    [<HarmonyPatch(typeof<ShopKeeper>, nameof Unchecked.defaultof<ShopKeeper>.BeginInteraction)>]
    [<HarmonyPrefix>]
    static member ShopkeeperTalked(text: string) =
        MainClass.Instance.Game.ShowShopkeeperDialogue text

    [<HarmonyPatch(typeof<NobunagaBoss>, "AddVulnerableCells")>]
    [<HarmonyPostfix>]
    static member NobunagaAttacked(___vulnerableCells: Generic.List<Cell>) =
        MainClass.Instance.Game.NobunagaCells <- ___vulnerableCells |> List.ofSeq

    [<HarmonyPatch(typeof<NobunagaBoss>, nameof Unchecked.defaultof<NobunagaBoss>.ReceiveAttack)>]
    [<HarmonyPrefix>]
    static member NobunagaAttacked(hit: Hit, attacker: Agent | null, __instance: NobunagaBoss) =
        MainClass.Instance.Game.ReceiveAttack(__instance, hit, attacker)

    [<HarmonyPatch(typeof<Agent>, nameof Unchecked.defaultof<Agent>.ReceiveAttack)>]
    [<HarmonyPrefix>]
    static member AgentAttacked(hit: Hit, attacker: Agent | null, __instance: Agent) =
        if not (__instance :? NobunagaBoss) then
            MainClass.Instance.Game.ReceiveAttack(__instance, hit, attacker)

    [<HarmonyPatch(typeof<TrapAttack>, nameof Unchecked.defaultof<TrapAttack>.Begin)>]
    [<HarmonyPrefix>]
    static member TrapInit(__instance: Trap) =
        MainClass.Instance.Game.TrapPlaced __instance

    [<HarmonyPatch(typeof<TrapAttack>, nameof Unchecked.defaultof<TrapAttack>.Begin)>]
    [<HarmonyPrefix>]
    static member TrapAttack(attacker: Agent) =
        MainClass.Instance.Game.TrapAttack attacker

    [<HarmonyPatch(typeof<Trap>, "OnTriggerEnter2D")>]
    [<HarmonyPostfix>]
    static member TrapWentOff(__instance: Trap, ___alreadyTriggered: bool) =
        if ___alreadyTriggered then
            MainClass.Instance.Game.TrapGone __instance

    [<HarmonyPatch(typeof<DioramaManager>, nameof Unchecked.defaultof<DioramaManager>.SetPressAnyKeyVisible)>]
    [<HarmonyPostfix>]
    static member DioramaAnyKeyVis(value: bool) =
        if value then
            MainClass.Instance.Game.ScheduleDioramaSkip()

    [<HarmonyPatch(typeof<HeroSelection>, "SwitchHeroCoroutine")>]
    [<HarmonyPostfix>]
    static member HeroSwitched(__result: IEnumerator byref, __instance: HeroSelection) =
        __result <- EnumeratorWrapper(__result, ignore, (fun () -> __instance.GoToNextRoom()))

    [<HarmonyPatch(typeof<HeroSelection>, "AgentEntersCoroutine")>]
    [<HarmonyPostfix>]
    static member HeroSelectionStart(__result: IEnumerator byref) =
        MainClass.Instance.Game.InhibitForces <- true
        MainClass.Instance.Logger.LogInfo "hero start"

        __result <-
            EnumeratorWrapper(
                __result,
                ignore,
                (fun () ->
                    MainClass.Instance.Logger.LogInfo "hero end"
                    MainClass.Instance.Game.InhibitForces <- false)
            )

    [<HarmonyPatch(typeof<DaySelection>, nameof Unchecked.defaultof<DaySelection>.SetInitiallySelectedDayForHero)>]
    [<HarmonyPrefix>]
    static member DisableDaySwitchingOnHeroSelection() = false

    [<HarmonyPatch(typeof<Boss>, nameof Unchecked.defaultof<Boss>.Die)>]
    [<HarmonyPostfix>]
    static member BossDied(__instance: Boss) =
        MainClass.Instance.Game.BossDied(__instance)

    [<HarmonyPatch(typeof<Hero>, nameof Unchecked.defaultof<Hero>.Die)>]
    [<HarmonyPostfix>]
    static member Hero() = MainClass.Instance.Game.HeroDied()

    [<HarmonyPatch(typeof<CombatManager>, "ProcessTurn")>]
    [<HarmonyPostfix>]
    static member ProcessTurn(__result: IEnumerator byref) =
        MainClass.Instance.Game.EnemyTurnStart()
        __result <- EnumeratorWrapper(__result, ignore, MainClass.Instance.Game.EnemyTurnEnd)

    [<HarmonyPatch(typeof<CombatSceneManager>, "EnterRoomCoroutine")>]
    [<HarmonyPostfix>]
    static member EnterRoom() = MainClass.Instance.Game.EnterRoom()

    [<HarmonyPatch(typeof<CombatSceneManager>, "ExitRoomCoroutine")>]
    [<HarmonyPostfix>]
    static member ExitRoom(__instance: CombatSceneManager) =
        MainClass.Instance.Game.ExitRoom __instance.Room

    [<HarmonyPatch(typeof<Wave>, nameof Unchecked.defaultof<Wave>.Spawn)>]
    [<HarmonyPostfix>]
    static member WaveSpawn(__instance: Wave) =
        MainClass.Instance.Game.WaveSpawned __instance

    [<HarmonyPatch(typeof<Skill>, "InvokeSkillTriggeredEvent")>]
    [<HarmonyPostfix>]
    static member SkillTriggered(__instance: Skill) =
        MainClass.Instance.Game.SkillTriggered __instance

    [<HarmonyPatch(typeof<GameOverScreen>, "SequenceCoroutine")>]
    [<HarmonyPostfix>]
    static member GameOverSeq(__instance: GameOverScreen, __result: IEnumerator byref) =
        __result <- EnumeratorWrapper(__result, ignore, (fun () -> __instance.Continue()))

    [<HarmonyPatch(typeof<ThornsEnemy>, nameof Unchecked.defaultof<ThornsEnemy>.ReceiveAttack)>]
    [<HarmonyPostfix>]
    static member ThornsAttackRec() =
        MainClass.Instance.Game.NullAttackReason <- "thorns"

    [<HarmonyPatch(typeof<Agent>, "ProcessPoisonStatus")>]
    [<HarmonyPostfix>]
    static member ProcessPoisonStatus() =
        MainClass.Instance.Game.NullAttackReason <- "poison"

    [<HarmonyPatch(typeof<Trap>, "OnTriggerEnter2D")>]
    [<HarmonyPostfix>]
    static member TrapTriggerEnter() =
        MainClass.Instance.Game.NullAttackReason <- "a trap"

    [<HarmonyPatch(typeof<ShockwaveEffect>, nameof Unchecked.defaultof<ShockwaveEffect>.Initialize)>]
    [<HarmonyPostfix>]
    static member ShockwaveInit() =
        MainClass.Instance.Game.NullAttackReason <- "a shockwave"

    [<HarmonyPatch(typeof<KarmaSkill>, "OnHeroIsHit")>]
    [<HarmonyPostfix>]
    static member KarmaHit() =
        MainClass.Instance.Game.NullAttackReason <- "karma"

    [<HarmonyPatch(typeof<CorruptionAttackCombatTask>, "AddWarningToCells")>]
    [<HarmonyPostfix>]
    static member AddCorruptedWarnings(cells: Generic.List<Cell>) =
        MainClass.Instance.Game.CorruptedCells <- (cells |> List.ofSeq) @ MainClass.Instance.Game.CorruptedCells

    (*[<HarmonyPatch(typeof<CorruptionAttackCombatTask>, "DestroyCellWarning")>]
    [<HarmonyPostfix>]
    static member RemoveCorruptedWarnings() =
        MainClass.Instance.Game.CorruptedCells <- []*)

    [<HarmonyPatch(typeof<Bomb>, nameof Unchecked.defaultof<Bomb>.Initialize)>]
    [<HarmonyPostfix>]
    static member BombInit(__instance: Bomb) =
        MainClass.Instance.Game.BombPlaced __instance

    [<HarmonyPatch(typeof<Bomb>, "Explode")>]
    [<HarmonyPostfix>]
    static member BombGone(__instance: Bomb) =
        MainClass.Instance.Game.BombGone __instance

    [<HarmonyPatch(typeof<MakuEffect>, nameof Unchecked.defaultof<MakuEffect>.CurtainDown)>]
    [<HarmonyPostfix>]
    static member CurtainDown(title: string) =
        MainClass.Instance.Game.CurtainDown title

    [<HarmonyPatch(typeof<MakuEffect>, "CurtainUp")>]
    [<HarmonyPostfix>]
    static member CurtainUp() = MainClass.Instance.Game.CurtainUp()

    [<HarmonyPatch(typeof<Enemy>, "TelegraphAction")>]
    [<HarmonyPostfix>]
    static member TelegraphAction(__instance: Enemy) =
        match __instance with
        | :? SniperEnemy as x ->
            if x.Action = CombatEnums.ActionEnum.Attack then
                MainClass.Instance.Game.AddWarning "Sniper is targeting this cell" Globals.Hero.Cell
        | :? BaruBoss as x ->
            if
                x.Action = CombatEnums.ActionEnum.Attack
                && x.HasInAttackStack TileEnums.AttackEnum.Volley
            then
                MainClass.Instance.Game.AddWarning "Baru is targeting this cell with volley" Globals.Hero.Cell
        | _ -> ()

    [<HarmonyPatch(typeof<SaveDataManager>, nameof Unchecked.defaultof<SaveDataManager>.LoadSaveData)>]
    [<HarmonyPostfix>]
    static member DisableForceTutorial() =
        Globals.ForcePlayTutorial <- false
        Globals.Tutorial <- false
        Globals.SkipTitleScreen <- true
