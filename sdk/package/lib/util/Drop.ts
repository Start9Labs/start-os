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
    Drop.weak[this.id] = this.weak()
    Drop.registry.register(this, this.id, this)
  }
  protected weak(): this {
    const weak = Object.assign(Object.create(Object.getPrototypeOf(this)), this)
    weak.ref = new WeakRef(this.ref)
    return weak
  }
  abstract onDrop(): void
  drop(): void {
    this.onDrop()
    Drop.registry.unregister(this)
    delete Drop.weak[this.id]
  }
}
