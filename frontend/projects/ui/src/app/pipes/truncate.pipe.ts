import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
  name: 'truncateCenter',
})
export class TruncateCenterPipe implements PipeTransform {
  transform (value: string, front: number, back: number, fullOnDesktop: boolean = false): unknown {
    if (value.length <= front + back + 3) return value
    if (fullOnDesktop && screen.width > 500) return value
    return value.slice(0, front) + '...' + value.slice(value.length - back, value.length)
  }
}

@Pipe({
  name: 'truncateEnd',
})
export class TruncateEndPipe implements PipeTransform {
  transform (val: string, length: number): unknown {
    if (val.length <= length) return val
    return val.slice(0, length) + '...'
  }
}

@Pipe({
  name: 'truncateTail',
})
export class TruncateTailPipe implements PipeTransform {
  transform (val: string, length: number): unknown {
    if (val.length <= length) return val
    return '...' + val.substr(length * -1)
  }
}

