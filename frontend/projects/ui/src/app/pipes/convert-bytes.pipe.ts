import { Pipe, PipeTransform } from '@angular/core'

// converts bytes to gigabytes
@Pipe({
  name: 'convertBytes',
})
export class ConvertBytesPipe implements PipeTransform {
  transform (bytes: number): string {
    if (bytes === 0) return '0 Bytes'

    const k = 1024
    const sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB']

    const i = Math.floor(Math.log(bytes) / Math.log(k))

    return parseFloat((bytes / Math.pow(k, i)).toFixed(1)) + ' ' + sizes[i]
  }
}
