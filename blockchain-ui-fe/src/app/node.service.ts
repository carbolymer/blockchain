import {Injectable} from '@angular/core';
import {Observable} from "rxjs/Observable";
import {Node} from "./Node"
import {HttpClient} from "@angular/common/http";
import {catchError, map} from "rxjs/operators";
import {handleError} from "./util";
import {MessageLevel, StatusMessage} from "./StatusMessage";
import {Status} from "tslint/lib/runner";

@Injectable()
export class NodeService {

  private nodesUrl = 'api/nodes';

  constructor(private httpService: HttpClient) {
  }

  getNodes(): Observable<Node[]> {
    return this.httpService.get<Node[]>(this.nodesUrl)
      .pipe(
        catchError(handleError('getNodes', []))
      );
  }

  mine(node: Node): Observable<StatusMessage> {
    return this.httpService.post<StatusMessage>(`${this.nodesUrl}/mine/${node.id}`, null)
      .pipe(
        map((messageDto: any) => {
            return new StatusMessage(
              MessageLevel[(<string>messageDto.level)],
              messageDto.message
            );
          }
        ),
        catchError(handleError<StatusMessage>('mine', {
          level: MessageLevel.ERROR,
          message: 'Could not send mining request'
        }))
      );
  }
}
