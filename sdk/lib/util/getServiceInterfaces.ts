import { Effects } from "../types"
import {
  ServiceInterfaceFilled,
  filledAddress,
  getHostname,
} from "./getServiceInterface"

const makeManyInterfaceFilled = async ({
  effects,
  packageId,
  callback,
}: {
  effects: Effects
  packageId: string | null
  callback: () => void
}) => {
  const serviceInterfaceValues = await effects.listServiceInterfaces({
    packageId,
    callback,
  })

  const serviceInterfacesFilled: ServiceInterfaceFilled[] = await Promise.all(
    Object.values(serviceInterfaceValues).map(async (serviceInterfaceValue) => {
      const host = await effects.getHostInfo({
        kind: null,
        packageId,
        hostId: serviceInterfaceValue.hostInfo.id,
        callback,
      })
      const primaryUrl = await effects
        .getPrimaryUrl({
          serviceInterfaceId: serviceInterfaceValue.id,
          hostId: serviceInterfaceValue.hostInfo.id,
          packageId,
          callback,
        })
        .catch(() => null)
      return {
        ...serviceInterfaceValue,
        primaryUrl: primaryUrl,
        host,
        addressInfo: filledAddress(host, serviceInterfaceValue.addressInfo),
        get primaryHostname() {
          if (primaryUrl == null) return null
          return getHostname(primaryUrl)
        },
      }
    }),
  )
  return serviceInterfacesFilled
}

export class GetServiceInterfaces {
  constructor(
    readonly effects: Effects,
    readonly opts: { packageId: string | null },
  ) {}

  /**
   * Returns the value of Store at the provided path. Restart the service if the value changes
   */
  async const() {
    const { packageId } = this.opts
    const callback = this.effects.restart
    const interfaceFilled: ServiceInterfaceFilled[] =
      await makeManyInterfaceFilled({
        effects: this.effects,
        packageId,
        callback,
      })

    return interfaceFilled
  }
  /**
   * Returns the value of ServiceInterfacesFilled at the provided path. Does nothing if the value changes
   */
  async once() {
    const { packageId } = this.opts
    const callback = () => {}
    const interfaceFilled: ServiceInterfaceFilled[] =
      await makeManyInterfaceFilled({
        effects: this.effects,
        packageId,
        callback,
      })

    return interfaceFilled
  }

  /**
   * Watches the value of ServiceInterfacesFilled at the provided path. Takes a custom callback function to run whenever the value changes
   */
  async *watch() {
    const { packageId } = this.opts
    while (true) {
      let callback: () => void = () => {}
      const waitForNext = new Promise<void>((resolve) => {
        callback = resolve
      })
      yield await makeManyInterfaceFilled({
        effects: this.effects,
        packageId,
        callback,
      })
      await waitForNext
    }
  }
}
export function getServiceInterfaces(
  effects: Effects,
  opts: { packageId: string | null },
) {
  return new GetServiceInterfaces(effects, opts)
}
