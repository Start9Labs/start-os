import { Directive, HostListener, inject, Input } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { filter } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { HeaderSnekComponent } from './snek.component'

@Directive({
  standalone: true,
  selector: 'img[appSnek]',
})
export class HeaderSnekDirective {
  private readonly dialogs = inject(TuiDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)

  @Input()
  appSnek = 0

  @HostListener('click')
  async onClick() {
    this.dialogs
      .open<number>(new PolymorpheusComponent(HeaderSnekComponent), {
        label: 'Snake!',
        closeable: false,
        dismissible: false,
        data: this.appSnek,
      })
      .pipe(filter(score => score > this.appSnek))
      .subscribe(async score => {
        const loader = this.loader.open('Saving high score...').subscribe()

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
