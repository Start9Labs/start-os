import { Http, SeqReplace, SeqUpdate } from "patch-db-client";
import { DataModel } from "./data-model";
import { mockApiAppInstalledFull, mockServer } from "../../services/api/mock-app-fixures";

export class MockPatchDbHttp implements Http<DataModel> {
  sequence = 0
  async getUpdates(startSequence: number, finishSequence?: number): Promise<SeqUpdate<DataModel>[]> {
    return this.getDump().then(a => [a])
  }
  async getDump(): Promise<SeqReplace<DataModel>> {
    this.sequence ++
    return {
      id: this.sequence,
      value: {
        server: mockServer,
        apps: mockApiAppInstalledFull,
      }
    }
  }
}