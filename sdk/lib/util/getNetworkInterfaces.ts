import { Effects, HostName } from "../types"
import {
  HostId,
  NetworkInterfaceFilled,
  filledAddress,
  networkInterfaceFilled,
} from "./getNetworkInterface"

const makeManyInterfaceFilled = async ({
  effects,
  packageId,
  callback,
}: {
  effects: Effects
  packageId: string | undefined
  callback: () => void
}) => {
  const interfaceValues = await effects.listInterface({
    packageId,
    callback,
  })
  const hostIdsRecord = Object.fromEntries(
    await Promise.all(
      Array.from(
        new Set(
          interfaceValues.flatMap((x) => x.addresses).map((x) => x.hostId),
        ),
      ).map(
        async (hostId) =>
          [
            hostId,
            await effects.getHostnames({
              packageId,
              hostId,
              callback,
            }),
          ] as const,
      ),
    ),
  )
  const fillAddress = filledAddress.bind(null, hostIdsRecord)

  const interfacesFilled: NetworkInterfaceFilled[] = await Promise.all(
    interfaceValues.map(async (interfaceValue) =>
      networkInterfaceFilled(
        interfaceValue,
        await effects.getPrimaryUrl({
          interfaceId: interfaceValue.interfaceId,
          packageId,
          callback,
        }),
        interfaceValue.addresses.map(fillAddress),
      ),
    ),
  )
  return interfacesFilled
}

export class GetNetworkInterfaces {
  constructor(
    readonly effects: Effects,
    readonly opts: { packageId?: string },
  ) {}

  /**
   * Returns the value of Store at the provided path. Restart the service if the value changes
   */
  async const() {
    const { packageId } = this.opts
    const callback = this.effects.restart
    const interfaceFilled: NetworkInterfaceFilled[] =
      await makeManyInterfaceFilled({
        effects: this.effects,
        packageId,
        callback,
      })

    return interfaceFilled
  }
  /**
   * Returns the value of NetworkInterfacesFilled at the provided path. Does nothing if the value changes
   */
  async once() {
    const { packageId } = this.opts
    const callback = () => {}
    const interfaceFilled: NetworkInterfaceFilled[] =
      await makeManyInterfaceFilled({
        effects: this.effects,
        packageId,
        callback,
      })

    return interfaceFilled
  }

  /**
   * Watches the value of NetworkInterfacesFilled at the provided path. Takes a custom callback function to run whenever the value changes
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
export function getNetworkInterfaces(
  effects: Effects,
  opts: { packageId?: string },
) {
  return new GetNetworkInterfaces(effects, opts)
}
