namespace NeuroShogun

open System.Collections
open HarmonyLib
open UnityEngine.SceneManagement

[<HarmonyPatch>]
type public Patches() =
    // __instance, __result
    [<HarmonyPatch(typeof<Shop>, nameof Unchecked.defaultof<Shop>.Begin)>]
    [<HarmonyPrefix>]
    static member PreShopBegin() =
        MainClass.Instance.Logger.LogInfo("Shop Begin")

    [<HarmonyPatch(typeof<SceneManager>, nameof (SceneManager.LoadScene: string -> unit), [| typeof<string> |])>]
    [<HarmonyPrefix>]
    static member public PreSceneLoad(sceneName: string) =
        MainClass.Instance.PreSceneLoad(sceneName)

    [<HarmonyPatch(typeof<Globals>, "Developer", MethodType.Getter)>]
    [<HarmonyPostfix>]
    static member GlobalsDeveloper(__result: bool byref) = __result <- true

    [<HarmonyPatch(typeof<Shop>, "ShopkeeperGiveFreeConsumableCoroutine")>]
    [<HarmonyPostfix>]
    static member PostShopkeeperGiveFreeConsumableCoroutine(__result: IEnumerator byref) =
        MainClass.Instance.Game.InhibitForces <- true
        __result <- EnumeratorWrapper(__result, (fun () -> MainClass.Instance.Game.InhibitForces <- false))

    [<HarmonyPatch(typeof<DioramaManager>, "Start")>]
    [<HarmonyPrefix>]
    static member DioramaStart() = MainClass.Instance.Game.DioramaStart()

    [<HarmonyPatch(typeof<DioramaManager>, "LeaveScene")>]
    [<HarmonyPrefix>]
    static member DioramaEnd() = MainClass.Instance.Game.DioramaEnd()

    [<HarmonyPatch(typeof<ScrollingCredits>, "Start")>]
    [<HarmonyPrefix>]
    static member CreditsStart() = MainClass.Instance.Game.CreditsStart()
