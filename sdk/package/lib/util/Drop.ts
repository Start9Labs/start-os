export abstract class Drop {
  private static weak: { [id: number]: Drop } = {}
  private static registry = new FinalizationRegistry((id: number) => {
    const weak = Drop.weak[id]
    if (weak) weak.drop()
  })
  private static idCtr: number = 0
  private id: number
  private ref: { id: number } | WeakRef<{ id: number }>
  protected constructor() {
    this.id = Drop.idCtr++
    this.ref = { id: this.id }
    const weak = this.weak()
    Drop.weak[this.id] = weak
    Drop.registry.register(this.ref, this.id, this.ref)

    return new Proxy(this, {
      set(target: any, prop, value) {
        if (prop === "ref") return false
        target[prop] = value
        ;(weak as any)[prop] = value
        return true
      },
    })
  }
  protected register() {}
  protected weak(): this {
    const weak = Object.assign(Object.create(Object.getPrototypeOf(this)), this)
    weak.ref = new WeakRef(this.ref)
    return weak
  }
  abstract onDrop(): void
  drop(): void {
    this.onDrop()
    Drop.registry.unregister(this.ref)
    delete Drop.weak[this.id]
  }
}
