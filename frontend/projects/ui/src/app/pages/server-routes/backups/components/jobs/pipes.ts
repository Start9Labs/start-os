import { Pipe, PipeTransform } from '@angular/core'
import cronstrue from 'cronstrue'

@Pipe({
  name: 'toHumanCron',
})
export class ToHumanCronPipe implements PipeTransform {
  transform(cron: string): { message: string; color: string } {
    try {
      return {
        message: cronstrue.toString(cron, {
          verbose: true,
          throwExceptionOnParseError: true,
        }),
        color: 'success',
      }
    } catch (e) {
      return {
        message: e as string,
        color: 'danger',
      }
    }
  }
}
