import { T } from "@start9labs/start-sdk"

const CallbackIdCell = { inc: 0 }

const callbackRegistry = new FinalizationRegistry(
  async (options: { cbs: Map<number, Function>; effects: T.Effects }) => {
    await options.effects.clearCallbacks({
      only: Array.from(options.cbs.keys()),
    })
  },
)

export class CallbackHolder {
  constructor(private effects?: T.Effects) {}

  private callbacks = new Map<number, Function>()
  private children: WeakRef<CallbackHolder>[] = []
  private newId() {
    return CallbackIdCell.inc++
  }
  addCallback(callback?: Function) {
    if (!callback) {
      return
    }
    const id = this.newId()
    this.callbacks.set(id, callback)
    if (this.effects)
      callbackRegistry.register(this, {
        cbs: this.callbacks,
        effects: this.effects,
      })
    return id
  }
  child(): CallbackHolder {
    const child = new CallbackHolder()
    this.children.push(new WeakRef(child))
    return child
  }
  removeChild(child: CallbackHolder) {
    this.children = this.children.filter((c) => {
      const ref = c.deref()
      return ref && ref !== child
    })
  }
  private getCallback(index: number): Function | undefined {
    let callback = this.callbacks.get(index)
    if (callback) this.callbacks.delete(index)
    else {
      for (let i = 0; i < this.children.length; i++) {
        callback = this.children[i].deref()?.getCallback(index)
        if (callback) return callback
      }
    }
    return callback
  }
  callCallback(index: number, args: any[]): Promise<unknown> {
    const callback = this.getCallback(index)
    if (!callback) return Promise.resolve()
    return Promise.resolve().then(() => callback(...args))
  }
}
