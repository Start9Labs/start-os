import { Pipe, PipeTransform } from '@angular/core'
import cronstrue from 'cronstrue'

@Pipe({
  name: 'toHumanCron',
})
export class ToHumanCronPipe implements PipeTransform {
  transform(cron: string): { message: string; color: string } {
    const toReturn = {
      message: '',
      color: 'success',
    }

    try {
      const human = cronstrue.toString(cron, {
        verbose: true,
        throwExceptionOnParseError: true,
      })
      const zero = Number(cron[0])
      const one = Number(cron[1])
      if (Number.isNaN(zero) || Number.isNaN(one)) {
        throw new Error(
          `${human}. Cannot run cron jobs more than once per hour`,
        )
      }
      toReturn.message = human
    } catch (e) {
      toReturn.message = e as string
      toReturn.color = 'danger'
    }

    return toReturn
  }
}
