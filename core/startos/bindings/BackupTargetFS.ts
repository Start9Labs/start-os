// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { BlockDev } from "./BlockDev";
import type { Cifs } from "./Cifs";

export type BackupTargetFS =
  | ({ type: "disk" } & BlockDev)
  | ({ type: "cifs" } & Cifs);
