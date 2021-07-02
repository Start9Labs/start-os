import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { IonContent } from '@ionic/angular'
import { Subscription } from 'rxjs'
import { concatMap, take, tap } from 'rxjs/operators'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'
import { ApiService } from 'src/app/services/api/api.service'

@Component({
  selector: 'app-instructions',
  templateUrl: './app-instructions.page.html',
  styleUrls: ['./app-instructions.page.scss'],
})
export class AppInstructionsPage {
  instructions: string
  loading = true
  error = ''

  @ViewChild(IonContent) content: IonContent
  subs: Subscription[] = []

  constructor (
    private readonly route: ActivatedRoute,
    private readonly apiService: ApiService,
    private readonly patch: PatchDbModel,
  ) { }

  async ngOnInit () {
    const pkgId = this.route.snapshot.paramMap.get('pkgId')
    this.patch.watch$('package-data', pkgId)
    .pipe(
      concatMap(pkg => this.apiService.getStatic(pkg['static-files'].instructions)),
      tap(instructions => {
        this.instructions = instructions
      }),
      take(1),
    )
    .subscribe(
      () => { this.loading = false },
      e => {
        this.error = e.message
        this.loading = false
      },
      () => console.log('COMPLETE'),
    )
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }
}
