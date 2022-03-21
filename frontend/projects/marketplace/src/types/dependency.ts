export interface Dependency<T> {
  version: string
  requirement:
    | {
        type: 'opt-in'
        how: string
      }
    | {
        type: 'opt-out'
        how: string
      }
    | {
        type: 'required'
      }
  description: string | null
  config: T
}
