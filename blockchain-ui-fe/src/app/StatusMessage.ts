export enum MessageLevel {
  INFO, ERROR
}

export class StatusMessage {
  level: MessageLevel;
  message: string;
}
