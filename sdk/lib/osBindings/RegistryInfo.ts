// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { Category } from "./Category"
import type { DataUrl } from "./DataUrl"

export type RegistryInfo = {
  name: string | null
  icon: DataUrl | null
  categories: { [key: string]: Category }
}
