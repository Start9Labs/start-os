
import {Effects, Config, ConfigRes, SetResult, Properties} from './types';


export function properties(effects: Effects): Properties | Promise<Properties>;
export function getConfig(effects: Effects): ConfigRes | Promise<ConfigRes>;
export function setConfig(effects: Effects, input: Config): SetResult | Promise<SetResult>;