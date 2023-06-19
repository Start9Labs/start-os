import { Directive, HostListener, Input } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { TuiDialogService } from '@taiga-ui/core'
import { filter } from 'rxjs'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { SnakePage } from './snake.page'

@Directive({
  selector: 'img[appSnek]',
})
export class SnekDirective {
  @Input()
  appSnekHighScore = 0

  constructor(
    private readonly dialogs: TuiDialogService,
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly embassyApi: ApiService,
  ) {}

  @HostListener('click')
  async onClick() {
    this.dialogs
      .open<number>(new PolymorpheusComponent(SnakePage), {
        label: 'Snake!',
        closeable: false,
        dismissible: false,
        data: {
          highScore: this.appSnekHighScore,
        },
      })
      .pipe(filter(score => score > this.appSnekHighScore))
      .subscribe(async score => {
        const loader = this.loader.open('Saving high score...').subscribe()

        try {
          await this.embassyApi.setDbValue<number>(
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
