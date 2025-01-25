namespace NeuroShogun

open System.Collections
open System.Reflection
open HarmonyLib
open UnityEngine.SceneManagement

[<HarmonyPatch>]
type public Patches() =
    static let mutable lastPickup: Pickup = null

    [<HarmonyPatch(typeof<SceneManager>, nameof (SceneManager.LoadScene: string -> unit), [| typeof<string> |])>]
    [<HarmonyPrefix>]
    static member public PreSceneLoad(sceneName: string) =
        MainClass.Instance.PreSceneLoad(sceneName)

    [<HarmonyPatch(typeof<Globals>, "Developer", MethodType.Getter)>]
    [<HarmonyPostfix>]
    static member GlobalsDeveloper(__result: bool byref) = __result <- true

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

    [<HarmonyPatch(typeof<ScrollingCredits>, "Start")>]
    [<HarmonyPrefix>]
    static member CreditsStart() = MainClass.Instance.Game.CreditsStart()

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
        MainClass.Instance.Game.NobunagaCells(___vulnerableCells |> List.ofSeq)

    [<HarmonyPatch(typeof<NobunagaBoss>, nameof Unchecked.defaultof<NobunagaBoss>.ReceiveAttack)>]
    [<HarmonyPrefix>]
    static member NobunagaAttacked(hit: Hit, attacker: Agent, __instance: NobunagaBoss) =
        MainClass.Instance.Game.ReceiveAttack(__instance, hit, attacker)

    [<HarmonyPatch(typeof<Agent>, nameof Unchecked.defaultof<Agent>.ReceiveAttack)>]
    [<HarmonyPrefix>]
    static member AgentAttacked(hit: Hit, attacker: Agent, __instance: Agent) =
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
