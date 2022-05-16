
import {Effects, Config, ConfigRes, SetResult} from './types';

export function properties(effects: Effects): unknown | Promise<unknown>;
export function getConfig(effects: Effects): ConfigRes | Promise<ConfigRes>;
export function setConfig(effects: Effects, input: Config): SetResult | Promise<SetResult>;