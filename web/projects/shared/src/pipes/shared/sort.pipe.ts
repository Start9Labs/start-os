import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
  name: 'sort',
})
export class SortPipe implements PipeTransform {
  transform(
    value: any[],
    column: string = '',
    direction: string = 'asc',
  ): any[] {
    // If the value is not an array or is empty, return the original value
    if (!Array.isArray(value) || value.length === 0) {
      return value
    }

    // Clone the array to avoid modifying the original value
    const sortedValue = [...value]

    // Define the sorting function based on the column and direction parameters
    const sortingFn = (a: any, b: any): number => {
      if (a[column] < b[column]) {
        return direction === 'asc' ? -1 : 1
      } else if (a[column] > b[column]) {
        return direction === 'asc' ? 1 : -1
      } else {
        return 0
      }
    }

    // Sort the array and return the result
    return sortedValue.sort(sortingFn)
  }
}
