import {Injectable} from '@angular/core';
import {MessageLevel, StatusMessage} from "./StatusMessage";
import {Subject} from "rxjs/Subject";

const toastTimeout: number = 5000;

@Injectable()
export class MessageService {
  private messageSubject: Subject<StatusMessage> = new Subject();

  constructor() {
    this.messageSubject.subscribe(this.handleMessage);
  }

  add(message: StatusMessage): void {
    this.messageSubject.next(message);
  }

  private handleMessage(message: StatusMessage) {
    var style: string;
    switch(message.level) {
      case MessageLevel.ERROR:
        style = 'red darken-4 white-text';
        break;
      case MessageLevel.INFO:
        style = 'green darken-4 white-text';
        break;
    }
    (<any>window).Materialize.toast(message.message, toastTimeout, style);
  }
}
