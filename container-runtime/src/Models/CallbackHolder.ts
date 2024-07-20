export class CallbackHolder {
  constructor() {}
  private inc = 0
  private callbacks = new Map<number, Function>()
  private newId() {
    return this.inc++
  }
  addCallback(callback: Function) {
    const id = this.newId()
    this.callbacks.set(id, callback)
    return id
  }
  callCallback(index: number, args: any[]): Promise<unknown> {
    const callback = this.callbacks.get(index)
    if (!callback) throw new Error(`Callback ${index} does not exist`)
    this.callbacks.delete(index)
    return Promise.resolve().then(() => callback(...args))
  }
}
