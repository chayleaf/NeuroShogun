namespace NeuroShogun

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
