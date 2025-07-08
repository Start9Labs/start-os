import { Effects } from "../Effects"
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
  packageId?: string
  callback?: () => void
}) => {
  const serviceInterfaceValues = await effects.listServiceInterfaces({
    packageId,
    callback,
  })

  const serviceInterfacesFilled: ServiceInterfaceFilled[] = await Promise.all(
    Object.values(serviceInterfaceValues).map(async (serviceInterfaceValue) => {
      const hostId = serviceInterfaceValue.addressInfo.hostId
      const host = await effects.getHostInfo({
        packageId,
        hostId,
        callback,
      })
      if (!host) {
        throw new Error(`host ${hostId} not found!`)
      }
      return {
        ...serviceInterfaceValue,
        host,
        addressInfo: filledAddress(host, serviceInterfaceValue.addressInfo),
      }
    }),
  )
  return serviceInterfacesFilled
}

export class GetServiceInterfaces {
  constructor(
    readonly effects: Effects,
    readonly opts: { packageId?: string },
  ) {}

  /**
   * Returns the service interfaces for the package. Reruns the context from which it has been called if the underlying value changes
   */
  async const() {
    const { packageId } = this.opts
    const callback =
      this.effects.constRetry &&
      (() => this.effects.constRetry && this.effects.constRetry())
    const interfaceFilled: ServiceInterfaceFilled[] =
      await makeManyInterfaceFilled({
        effects: this.effects,
        packageId,
        callback,
      })

    return interfaceFilled
  }
  /**
   * Returns the service interfaces for the package. Does nothing if the value changes
   */
  async once() {
    const { packageId } = this.opts
    const interfaceFilled: ServiceInterfaceFilled[] =
      await makeManyInterfaceFilled({
        effects: this.effects,
        packageId,
      })

    return interfaceFilled
  }

  /**
   * Watches the service interfaces for the package. Returns an async iterator that yields whenever the value changes
   */
  async *watch() {
    const { packageId } = this.opts
    const resolveCell = { resolve: () => {} }
    this.effects.onLeaveContext(() => {
      resolveCell.resolve()
    })
    while (this.effects.isInContext) {
      let callback: () => void = () => {}
      const waitForNext = new Promise<void>((resolve) => {
        callback = resolve
        resolveCell.resolve = resolve
      })
      yield await makeManyInterfaceFilled({
        effects: this.effects,
        packageId,
        callback,
      })
      await waitForNext
    }
  }

  /**
   * Watches the service interfaces for the package. Takes a custom callback function to run whenever the value changes
   */
  onChange(
    callback: (
      value: ServiceInterfaceFilled[] | null,
      error?: Error,
    ) => void | Promise<void>,
  ) {
    ;(async () => {
      for await (const value of this.watch()) {
        try {
          await callback(value)
        } catch (e) {
          console.error(
            "callback function threw an error @ GetServiceInterfaces.onChange",
            e,
          )
        }
      }
    })()
      .catch((e) => callback(null, e))
      .catch((e) =>
        console.error(
          "callback function threw an error @ GetServiceInterfaces.onChange",
          e,
        ),
      )
  }

  /**
   * Watches the service interfaces for the package. Returns when the predicate is true
   */
  async waitFor(pred: (value: ServiceInterfaceFilled[] | null) => boolean) {
    const { packageId } = this.opts
    const resolveCell = { resolve: () => {} }
    this.effects.onLeaveContext(() => {
      resolveCell.resolve()
    })
    while (this.effects.isInContext) {
      let callback: () => void = () => {}
      const waitForNext = new Promise<void>((resolve) => {
        callback = resolve
        resolveCell.resolve = resolve
      })
      const res = await makeManyInterfaceFilled({
        effects: this.effects,
        packageId,
        callback,
      })
      if (pred(res)) {
        resolveCell.resolve()
        return res
      }
      await waitForNext
    }
    return null
  }
}
export function getServiceInterfaces(
  effects: Effects,
  opts: { packageId?: string },
) {
  return new GetServiceInterfaces(effects, opts)
}
