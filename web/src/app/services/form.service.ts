export abstract class FormService<T> {
  abstract load(): Promise<T>
  abstract save(data: T): Promise<boolean>
  abstract reset(): T
}
