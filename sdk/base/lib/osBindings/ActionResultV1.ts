// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.

export type ActionResultV1 =
  | {
      type: "string"
      value: string
      description: string | null
      copyable: boolean
      qr: boolean
      masked: boolean
    }
  | {
      type: "object"
      value: { [key: string]: ActionResultV1 }
      description?: string
    }
