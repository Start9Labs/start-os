import { Directive, HostListener, inject, Input } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { filter } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { SnekComponent } from './snek.component'

@Directive({
  standalone: true,
  selector: 'img[snek]',
})
export class SnekDirective {
  private readonly dialogs = inject(TuiDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)

  @Input()
  snek = 0

  @HostListener('click')
  async onClick() {
    this.dialogs
      .open<number>(new PolymorpheusComponent(SnekComponent), {
        label: 'Snake!',
        closeable: false,
        dismissible: false,
        data: this.snek,
      })
      .pipe(filter(score => score > this.snek))
      .subscribe(async score => {
        const loader = this.loader.open('Saving high score').subscribe()

        try {
          await this.api.setDbValue<number>(
            ['gaming', 'snake', 'high-score'],
            score,
          )
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }
}
