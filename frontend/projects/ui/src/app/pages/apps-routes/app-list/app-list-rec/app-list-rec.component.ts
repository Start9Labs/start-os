import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'
import { AlertController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { from, merge, OperatorFunction, pipe, Subject } from 'rxjs'
import { catchError, mapTo, startWith, switchMap, tap } from 'rxjs/operators'
import { RecoveredInfo } from 'src/app/util/parse-data-model'
import { MarketplaceService } from 'src/app/pages/marketplace-routes/marketplace.service'

@Component({
  selector: 'app-list-rec',
  templateUrl: 'app-list-rec.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppListRecComponent {
  // Asynchronous actions initiators
  readonly install$ = new Subject<RecoveredInfo>()
  readonly delete$ = new Subject<RecoveredInfo>()

  @Input()
  rec: RecoveredInfo

  @Output()
  readonly deleted = new EventEmitter<void>()

  // Installing package
  readonly installing$ = this.install$.pipe(
    switchMap(({ id, version }) =>
      // Mapping each installation to API request
      from(
        this.marketplaceService.installPackage({
          id,
          'version-spec': `>=${version}`,
          'version-priority': 'min',
        }),
      ).pipe(
        // Mapping operation to true/false loading indication
        loading(this.errToast),
      ),
    ),
  )

  // Deleting package
  readonly deleting$ = this.delete$.pipe(
    switchMap(({ id }) =>
      // Mapping each deletion to API request
      from(this.api.deleteRecoveredPackage({ id })).pipe(
        // Notifying parent component that package is removed from recovered items
        tap(() => this.deleted.emit()),
        // Mapping operation to true/false loading indication
        loading(this.errToast),
      ),
    ),
  )

  // Merging both true/false loading indicators to a single stream
  readonly loading$ = merge(this.installing$, this.deleting$)

  constructor(
    private readonly api: ApiService,
    private readonly errToast: ErrorToastService,
    private readonly alertCtrl: AlertController,
    private readonly marketplaceService: MarketplaceService,
  ) {}

  async deleteRecovered(pkg: RecoveredInfo): Promise<void> {
    const alert = await this.alertCtrl.create({
      header: 'Delete Data',
      message: `This action will permanently delete all data associated with ${pkg.title}.`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          handler: () => {
            // Initiate deleting of 'pkg'
            this.delete$.next(pkg)
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }
}

// Custom RxJS operator to turn asynchronous operation into a true/false loading indicator
function loading(
  errToast: ErrorToastService,
): OperatorFunction<unknown, boolean> {
  return pipe(
    // Show notification on error
    catchError(e => from(errToast.present(e))),
    // Map any result to false to stop loading inidicator
    mapTo(false),
    // Start operation with true
    startWith(true),
  )
}
