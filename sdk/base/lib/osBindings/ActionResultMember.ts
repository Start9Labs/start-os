// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.

export type ActionResultMember = {
  name: string
  description: string | null
} & (
  | {
      type: "single"
      value: string
      copyable: boolean
      qr: boolean
      masked: boolean
    }
  | { type: "group"; value: Array<ActionResultMember> }
)
