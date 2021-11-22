import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from "@angular/core";
import { AlertController } from "@ionic/angular";
import { ApiService } from "src/app/services/api/embassy-api.service";
import { ErrorToastService } from "src/app/services/error-toast.service";
import { from, merge, OperatorFunction, pipe, Subject } from "rxjs";
import { catchError, mapTo, startWith, switchMap, tap } from "rxjs/operators";
import { RecoveredInfo } from "src/app/util/parse-data-model";

@Component({
  selector: "app-list-rec",
  templateUrl: "app-list-rec.component.html",
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppListRecComponent {
  readonly install$ = new Subject<RecoveredInfo>();
  readonly delete$ = new Subject<RecoveredInfo>();

  @Input()
  rec: RecoveredInfo;

  @Output()
  readonly deleted = new EventEmitter<void>();

  readonly installing$ = this.install$.pipe(
    switchMap(({ id }) =>
      from(this.api.installPackage({ id })).pipe(
        tap(() => this.deleted.emit()),
        loading(this.errToast)
      )
    )
  );

  readonly deleting$ = this.delete$.pipe(
    switchMap(({ id }) =>
      from(this.api.deleteRecoveredPackage({ id })).pipe(loading(this.errToast))
    )
  );

  readonly loading$ = merge(this.installing$, this.deleting$);

  constructor(
    private readonly api: ApiService,
    private readonly errToast: ErrorToastService,
    private readonly alertCtrl: AlertController
  ) {}

  async deleteRecovered(pkg: RecoveredInfo): Promise<void> {
    const alert = await this.alertCtrl.create({
      header: "Delete Data",
      message: `This action will permanently delete all data associated with ${pkg.title}.`,
      buttons: [
        {
          text: "Cancel",
          role: "cancel",
        },
        {
          text: "Delete",
          handler: () => {
            this.delete$.next(pkg);
          },
          cssClass: "enter-click",
        },
      ],
    });
    await alert.present();
  }
}

function loading(
  errToast: ErrorToastService
): OperatorFunction<unknown, boolean> {
  return pipe(
    startWith(true),
    catchError((e) => from(errToast.present(e))),
    mapTo(false)
  );
}
