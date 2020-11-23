export type Replace<T, key extends keyof T, withKey extends string> = Omit<T, key> & { [k in withKey]: T[key] }
