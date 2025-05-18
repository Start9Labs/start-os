import {
  Directive,
  EmbeddedViewRef,
  inject,
  Injectable,
  OnDestroy,
  OnInit,
  TemplateRef,
  ViewContainerRef,
} from '@angular/core'
import { tuiButtonOptionsProvider } from '@taiga-ui/core'

@Injectable({
  providedIn: 'root',
})
export class TitleService {
  private vcr?: ViewContainerRef

  register(vcr: ViewContainerRef) {
    this.vcr = vcr
  }

  add(template: TemplateRef<any>) {
    return this.vcr?.createEmbeddedView(template)
  }
}

@Directive({
  standalone: true,
  selector: 'ng-template[title]',
  providers: [tuiButtonOptionsProvider({ appearance: '' })],
})
export class TitleDirective implements OnInit, OnDestroy {
  private readonly template = inject(TemplateRef)
  private readonly service = inject(TitleService)
  private view?: EmbeddedViewRef<any>

  ngOnInit() {
    this.view = this.service.add(this.template)
  }

  ngOnDestroy() {
    this.view?.destroy()
  }
}
