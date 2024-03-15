import { Effects } from "../types"
import { ExecuteAction } from "../../../core/startos/bindings/ExecuteAction"
import { CreateOverlayedImageParams } from "../../../core/startos/bindings/CreateOverlayedImageParams"
import { DestroyOverlayedImageParams } from "../../../core/startos/bindings/DestroyOverlayedImageParams"
import { BindParams } from "../../../core/startos/bindings/BindParams"
import { GetHostInfoParams } from "../../../core/startos/bindings/GetHostInfoParams"
import { ParamsPackageId } from "../../../core/startos/bindings/ParamsPackageId"
import { ParamsMaybePackageId } from "../../../core/startos/bindings/ParamsMaybePackageId"
import { SetConfigured } from "../../../core/startos/bindings/SetConfigured"
import { SetHealth } from "../../../core/startos/bindings/SetHealth"
import { ExposeForDependentsParams } from "../../../core/startos/bindings/ExposeForDependentsParams"
import { ExposeUiParams } from "../../../core/startos/bindings/ExposeUiParams"
import { GetSslCertificateParams } from "../../../core/startos/bindings/GetSslCertificateParams"
import { GetSslKeyParams } from "../../../core/startos/bindings/GetSslKeyParams"
import { GetServiceInterfaceParams } from "../../../core/startos/bindings/GetServiceInterfaceParams"
import { SetDependenciesParams } from "../../../core/startos/bindings/SetDependenciesParams"

function typeEquality<ExpectedType>(_a: ExpectedType) {}
describe("startosTypeValidation ", () => {
  test(`checking the params match`, () => {
    const testInput: any = {}
    typeEquality<{
      [K in keyof Effects &
        (
          | "gitInfo"
          | "echo"
          | "chroot"
          | "exists"
          | "executeAction"
          | "getConfigured"
          | "stopped"
          | "running"
          | "restart"
          | "shutdown"
          | "setConfigured"
          | "setMainStatus"
          | "setHealth"
          | "getStore"
          | "setStore"
          | "exposeForDependents"
          | "exposeUi"
          | "createOverlayedImage"
          | "destroyOverlayedImage"
          | "getSslCertificate"
          | "getSslKey"
          | "getServiceInterface"
          | "clearBindings"
          | "bind"
          | "getHostInfo"
          | "setDependencies"
        )]: Effects[K] extends Function ? Parameters<Effects[K]>[0] : never
    }>({
      executeAction: {} as ExecuteAction,
      createOverlayedImage: {} as CreateOverlayedImageParams,
      destroyOverlayedImage: {} as DestroyOverlayedImageParams,
      clearBindings: undefined,
      bind: {} as BindParams,
      getHostInfo: {} as GetHostInfoParams,
      exists: {} as ParamsPackageId,
      getConfigured: undefined,
      stopped: {} as ParamsMaybePackageId,
      running: {} as ParamsPackageId,
      restart: undefined,
      shutdown: undefined,
      setConfigured: {} as SetConfigured,
      setHealth: {} as SetHealth,
      exposeForDependents: {} as ExposeForDependentsParams,
      exposeUi: {} as ExposeUiParams,
      getSslCertificate: {} as GetSslCertificateParams,
      getSslKey: {} as GetSslKeyParams,
      getServiceInterface: {} as GetServiceInterfaceParams,
      setDependencies: {} as SetDependenciesParams,
    })
    typeEquality<Parameters<Effects["executeAction"]>[0]>(
      testInput as ExecuteAction,
    )
  })
})
