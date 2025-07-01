export abstract class Drop {
  private static weak: { [id: number]: Drop } = {}
  private static registry = new FinalizationRegistry((id: number) => {
    const weak = Drop.weak[id]
    if (weak) weak.drop()
  })
  private static idCtr: number = 0
  private dropId?: number
  private dropRef?: { id: number } | WeakRef<{ id: number }>
  protected constructor() {
    this.dropId = Drop.idCtr++
    this.dropRef = { id: this.dropId }
    const weak = this.weak()
    Drop.weak[this.dropId] = weak
    Drop.registry.register(this.dropRef, this.dropId, this.dropRef)

    return new Proxy(this, {
      set(target: any, prop, value) {
        if (prop === "dropRef" || prop == "dropId") return false
        target[prop] = value
        ;(weak as any)[prop] = value
        return true
      },
    })
  }
  protected register() {}
  protected weak(): this {
    const weak = Object.assign(Object.create(Object.getPrototypeOf(this)), this)
    if (this.dropRef) weak.ref = new WeakRef(this.dropRef)
    return weak
  }
  abstract onDrop(): void
  drop(): void {
    if (!this.dropRef || !this.dropId) return
    this.onDrop()
    this.leak()
  }
  leak(): this {
    if (!this.dropRef || !this.dropId) return this
    Drop.registry.unregister(this.dropRef)
    delete Drop.weak[this.dropId]
    delete this.dropRef
    delete this.dropId
    return this
  }
}
