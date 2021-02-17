import { Injectable } from "@angular/core";
import { SeqUpdate, Source } from "patch-db-client";
import { Observable, Subject } from "rxjs";
import { filter } from "rxjs/operators";
import { DataModel } from "./data-model";

@Injectable({
  providedIn: 'root'
})
export class ImperativeSource implements Source<DataModel> {
  private readonly trigger = new Subject<SeqUpdate<DataModel>>()
  private running = false

  sync(s: SeqUpdate<DataModel>): void {
    this.trigger.next(s)
  }

  watch(): Observable<SeqUpdate<DataModel>> {
    return this.trigger.asObservable().pipe(filter(() => this.running))
  }
  start(): void {
    this.running = true
  }
  stop(): void {
    this.running = false
  }
}