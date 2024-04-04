export class CallbackHolder {
  constructor() {}
  private root = (Math.random() + 1).toString(36).substring(7)
  private inc = 0
  private callbacks = new Map<string, Function>()
  private newId() {
    return this.root + (this.inc++).toString(36)
  }
  addCallback(callback: Function) {
    const id = this.newId()
    this.callbacks.set(id, callback)
    return id
  }
  callCallback(index: string, args: any[]): Promise<unknown> {
    const callback = this.callbacks.get(index)
    if (!callback) throw new Error(`Callback ${index} does not exist`)
    this.callbacks.delete(index)
    return Promise.resolve().then(() => callback(...args))
  }
}