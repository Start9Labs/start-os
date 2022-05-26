export interface RpcErrorDetails<T> {
  code: number
  message: string
  data?:
    | {
        details: string
        revision?: T | null
      }
    | string
}
