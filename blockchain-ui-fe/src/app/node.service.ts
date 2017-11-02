import {Injectable} from '@angular/core';
import {Observable} from "rxjs/Observable";
import {Node} from "./Node"
import {HttpClient} from "@angular/common/http";
import {catchError} from "rxjs/operators";
import {handleError} from "./util";

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

  mine(node: Node) {
    return this.httpService.post(`${this.nodesUrl}/mine/${node.id}`, null)
      .pipe(
        catchError(handleError('mine', []))
      );
  }
}
