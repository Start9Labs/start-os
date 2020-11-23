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


// 4 and 4

// 12345678 => 12345678
// 123456789 => 123456789
// 1234567890 => 1234567890
// 12345678901 => 12345678901
// 1234...9012 => 1234...9012
// 1234...0123 => 1234...0123