import { DOCUMENT } from "@angular/common";
import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
} from "@angular/core";
import {
  PackageMainStatus,
  PackageDataEntry, Manifest,
} from "src/app/services/patch-db/data-model";
import { ConfigService } from "src/app/services/config.service";
import { PkgInfo } from "src/app/util/get-package-info";

@Component({
  selector: "app-list-pkg",
  templateUrl: "app-list-pkg.component.html",
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppListPkgComponent {
  @Input()
  pkg: PkgInfo;

  @Input()
  connectionFailure = false;

  constructor(
    @Inject(DOCUMENT) private readonly document: Document,
    private readonly config: ConfigService
  ) {}

  get status(): PackageMainStatus {
    return this.pkg.entry.installed?.status.main.status;
  }

  get manifest(): Manifest {
    return this.pkg.entry.manifest;
  }

  launchUi(pkg: PackageDataEntry): void {
    this.document.defaultView.open(
      this.config.launchableURL(pkg),
      "_blank",
      "noreferrer"
    );
  }
}
