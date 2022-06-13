export function isObject(val: any): boolean {
  return val && typeof val === 'object' && !Array.isArray(val)
}

export function isEmptyObject(obj: object): boolean {
  return obj === undefined || !Object.keys(obj).length
}

export function pauseFor(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms))
}

export function capitalizeFirstLetter(string: string): string {
  return string.charAt(0).toUpperCase() + string.slice(1)
}

export function exists<T>(t: T | undefined): t is T {
  return t !== undefined
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
