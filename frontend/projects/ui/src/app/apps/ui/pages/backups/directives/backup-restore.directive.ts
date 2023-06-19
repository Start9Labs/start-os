import { Directive, HostListener } from '@angular/core'
import { NavController } from '@ionic/angular'
import { TuiDialogService } from '@taiga-ui/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { BackupInfo, BackupTarget } from 'src/app/services/api/api.types'
import * as argon2 from '@start9labs/argon2'
import { TargetSelectPage } from '../modals/target-select/target-select.page'
import {
  RecoverData,
  RecoverSelectPage,
} from '../modals/recover-select/recover-select.page'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import {
  PROMPT,
  PromptOptions,
} from 'src/app/apps/ui/modals/prompt/prompt.component'
import {
  catchError,
  EMPTY,
  exhaustMap,
  map,
  Observable,
  of,
  switchMap,
  take,
  tap,
} from 'rxjs'

@Directive({
  selector: '[backupRestore]',
})
export class BackupRestoreDirective {
  constructor(
    private readonly errorService: ErrorService,
    private readonly dialogs: TuiDialogService,
    private readonly navCtrl: NavController,
    private readonly embassyApi: ApiService,
    private readonly loader: LoadingService,
  ) {}

  @HostListener('click') onClick() {
    this.presentModalTarget()
  }

  async presentModalTarget() {
    this.dialogs
      .open<BackupTarget>(new PolymorpheusComponent(TargetSelectPage), {
        label: 'Select Backup Source',
        data: { type: 'restore' },
      })
      .subscribe(data => {
        this.presentModalPassword(data)
      })
  }

  presentModalPassword(target: BackupTarget) {
    const data: PromptOptions = {
      message:
        'Enter the master password that was used to encrypt this backup. On the next screen, you will select the individual services you want to restore.',
      label: 'Master Password',
      placeholder: 'Enter master password',
      useMask: true,
    }

    this.dialogs
      .open<string>(PROMPT, {
        label: 'Password Required',
        data,
      })
      .pipe(
        exhaustMap(password =>
          this.getRecoverData(
            target.id,
            password,
            target['embassy-os']?.['password-hash'] || '',
          ),
        ),
        take(1),
        switchMap(data => this.presentModalSelect(data)),
      )
      .subscribe(() => {
        this.navCtrl.navigateRoot('/services')
      })
  }

  private getRecoverData(
    targetId: string,
    password: string,
    hash: string,
  ): Observable<RecoverData> {
    return of(password).pipe(
      tap(() => argon2.verify(hash, password)),
      switchMap(() => this.getBackupInfo(targetId, password)),
      catchError(e => {
        this.errorService.handleError(e)

        return EMPTY
      }),
      map(backupInfo => ({ targetId, password, backupInfo })),
    )
  }

  private async getBackupInfo(
    targetId: string,
    password: string,
  ): Promise<BackupInfo> {
    const loader = this.loader.open('Decrypting drive...').subscribe()

    return this.embassyApi
      .getBackupInfo({
        'target-id': targetId,
        password,
      })
      .finally(() => loader.unsubscribe())
  }

  private presentModalSelect(data: RecoverData): Observable<void> {
    return this.dialogs.open(new PolymorpheusComponent(RecoverSelectPage), {
      label: 'Select Services to Restore',
      data,
    })
  }
}
