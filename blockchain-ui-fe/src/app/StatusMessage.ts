export enum MessageLevel {
  INFO, ERROR
}

export class StatusMessage {
  constructor(public level: MessageLevel,
              public message: string) {
  }
}
