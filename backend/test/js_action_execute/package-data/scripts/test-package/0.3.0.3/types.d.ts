export type Effects = {
    writeFile(input: {path: string, volumeId: string, toWrite: string}): Promise<void>,
    readFile(input: {volumeId: string,path: string}): Promise<string>,
    createDir(input: {volumeId: string,path: string}): Promise<string>,
    removeDir(input: {volumeId: string,path: string}): Promise<string>,
    removeFile(input: {volumeId: string,path: string}): Promise<void>,
    writeJsonFile(input: {volumeId: string,path: string, toWrite: object}): void,
    readJsonFile(input:{volumeId: string,path: string}): object,
    trace(whatToPrin: string),
    warn(whatToPrin: string),
    error(whatToPrin: string),
    debug(whatToPrin: string),
    info(whatToPrin: string),
    is_sandboxed(): boolean,
}

export type ActionResult = {
    version: "0",
    message: string,
    value?: string,
    copyable: boolean,
    qr: boolean,
}

export type ConfigRes = {
     config?: Config,
     spec: ConfigSpec,
 }
export type Config = {
   [value: string]: any
 }

export type ConfigSpec = {      
   [value: string]: ValueSpecAny
 }
 export type WithDefault<T, Default> = T & {
    default?: Default
 }

export type WithDescription<T> = T & {
    description?: String,
    name: string,
    warning?: string,
}

export type ListSpec<T> = {
    spec: T,
    range: string
}

export type Tag<T extends string, V> = V & {
    type: T
} 

export type Subtype<T extends string, V> = V & {
    subtype: T
} 

export type Target<T extends string, V> = V & {
    "target": T
} 

export type UniqueBy =
|{
    any: UniqueBy[],
}
| {
    all: UniqueBy[]
}
| string
| null

export type WithNullable<T> = T & {
    nullable: boolean
}
export type DefaultString = String | {
    charset?: string,
    len: number
}

export type ValueSpecString= ({} | {
    pattern: string,
    'pattern-description': string
}) & {
    copyable?: boolean,
    masked?: boolean,
    placeholder?: string

}
export type ValueSpecNumber = {
    range?: string,
    integral?: boolean,
    units?: string,
    placeholder?: number,
}
export type ValueSpecBoolean = {}
export type ValueSpecAny = 
| Tag<'boolean', WithDescription<WithDefault<ValueSpecBoolean, boolean>>> 
| Tag<'string', WithDescription<WithDefault<WithNullable<ValueSpecString>, DefaultString>>>
| Tag<'number', WithDescription<WithDefault<WithNullable<ValueSpecNumber>, number>>>
| Tag<'enum', WithDescription<WithDefault<{
    values: string[],
    "value-names": {
        [key: string]: string
    }
}, string>>>
| Tag<'list', ValueSpecList>
| Tag<'object', WithDescription<WithDefault<ValueSpecObject, Config>>>
| Tag<'union', WithDescription<WithDefault<ValueSpecUnion, string>>>
| Tag<'pointer', WithDescription<
    | Subtype<'package', 
        | Target<'tor-key', {
            'package-id': string
            interface: string
        }>
        | Target<'tor-address', {
            'package-id': string,
            interface: string
        } >
        | Target<'lan-address',{
            'package-id': string,
            interface: string
        } >
        | Target<'config', {
            'package-id': string,
            selector: string,
            multi: boolean
        }>
    >
    | Subtype<'system', {}>
>>
export type ValueSpecUnion = {
    tag: {
        id: string,
        name: string,
        description?: string,
        "variant-names": {
            [key: string]: string,
        }
    },
    variants: {
        [key: string]: ConfigSpec
    },
    "display-as"?: string,
    "unique-by"?: UniqueBy

}
export type ValueSpecObject = {
    spec : ConfigSpec,
    'display-as'?: string,
    "unique-by"?: UniqueBy

}
export type ValueSpecList = 
| Subtype<'boolean', WithDescription<WithDefault<ListSpec<ValueSpecBoolean>, boolean>>> 
| Subtype<'string', WithDescription<WithDefault<ListSpec<ValueSpecString>, string>>>
| Subtype<'number', WithDescription<WithDefault<ListSpec<ValueSpecNumber>, number>>>
| Subtype<'enum', WithDescription<WithDefault<{
    values: string[],
    "value-names": {
        [key: string]: string
    }
}, string>>>

export type SetResult = {
    signal?: string,
    'depends-on': {
        [packageId: string]: string[]
    }
}
export type PackagePropertiesV2 = {
    [name: string]: PackagePropertyObject | PackagePropertyString
}
export type PackagePropertyString = {
    type: 'string',
    description?: string,
    value: string,
    copyable?: boolean,
    qr?: boolean,
    masked?: boolean,
}
export type PackagePropertyObject = {
    value: PackagePropertiesV2;
    type: "object";
    description: string;
}

export type Properties = {
    version: 2,
    data: PackagePropertiesV2
}
