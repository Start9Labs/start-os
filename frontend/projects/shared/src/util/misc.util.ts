export function isObject(val: any): boolean {
  return val && typeof val === 'object' && !Array.isArray(val)
}

export function isEmptyObject(obj: object): boolean {
  return obj === undefined || !Object.keys(obj).length
}

export function pauseFor(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms))
}

export function debounce(delay: number = 300): MethodDecorator {
  return function (
    target: any,
    propertyKey: string | symbol,
    descriptor: PropertyDescriptor,
  ) {
    const timeoutKey = Symbol()

    const original = descriptor.value

    descriptor.value = function (this: any, ...args: any[]) {
      clearTimeout(this[timeoutKey])
      this[timeoutKey] = setTimeout(() => original.apply(this, args), delay)
    }

    return descriptor
  }
}

export function sameUrl(
  u1: string | null | undefined,
  u2: string | null | undefined,
): boolean {
  return toUrl(u1) === toUrl(u2)
}

export function isValidHttpUrl(url: string): boolean {
  try {
    const _ = new URL(url)
    return true
  } catch (_) {
    return false
  }
}

export function toUrl(text: string | null | undefined): string {
  try {
    const url = new URL(text as string)
    return url.toString()
  } catch {
    return ''
  }
}

export type WithId<T> = T & { id: string }
