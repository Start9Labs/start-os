import * as E from "./embassy.ts";
import * as T from "./types.d.ts";
type Maybe<T> = T | undefined;

const exportables: {
    setConfig: Maybe<T.SetConfigProcedure> ,
    dependencies: Maybe<T.Dependencies> ,
    properties: Maybe<T.PropertiesProcedure> ,
    getConfig: Maybe<T.GetConfigProcedure> ,
} = E

export const setConfig: Maybe<T.SetConfigProcedure> = exportables.setConfig;
export const dependencies: Maybe<T.Dependencies> =  exportables.dependencies;
export const getConfig: Maybe<T.GetConfigProcedure> = exportables.getConfig;
export const properties: Maybe<T.PropertiesProcedure> =  exportables.properties;
