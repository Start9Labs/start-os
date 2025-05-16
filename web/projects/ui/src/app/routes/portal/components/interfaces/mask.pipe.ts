import { inject, Pipe, PipeTransform } from '@angular/core'
import { InterfaceComponent } from './interface.component'

@Pipe({
  standalone: true,
  name: 'mask',
})
export class MaskPipe implements PipeTransform {
  private readonly interface = inject(InterfaceComponent)

  transform(value: string): string {
    return this.interface.interface().masked
      ? '●'.repeat(Math.min(64, value.length))
      : value
  }
}
