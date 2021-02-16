import { AppInstalledFull } from "../app-types";
import { S9Server } from "../server-model";

export type DataModel = {
  server: S9Server
  apps: { [appId: string]: AppInstalledFull }
}