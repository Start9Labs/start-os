export type GetDependency<K extends string, T> = {
  [OtherK in K]: () => PromiseLike<T>
}
