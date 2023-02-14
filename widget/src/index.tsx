import * as React from "react";
import { Position } from "vscode-languageserver-protocol";
import {
  InteractiveCode,
  useAsync,
  RpcContext,
  CodeWithInfos,
  RpcSessionAtPos,
  DocumentPosition,
} from "@leanprover/infoview";

type ItemResult = {
  "error": {
    cmd?: string;
    error: string;
  }
} | {
  "done": { cmd: string }
} | {
  "change": { cmd: string, target: CodeWithInfos }
} | {
  "noChange": { cmd: string }
}

function isSuccessful(result: ItemResult): boolean {
  return "change" in result || "done" in result;
}

interface Item {
  name: string;
  score: number;
  expr?: CodeWithInfos;
  error?: string;
  result?: ItemResult;
}

interface Props {
  items: Item[];
  pos: DocumentPosition;
}

interface GetItemArgs {
  item: Item;
  pos: DocumentPosition;
}

async function getItem(rs: RpcSessionAtPos, args: GetItemArgs): Promise<Item> {
  const items = rs.call<GetItemArgs, Item>("PremiseSelection.getItem", args);
  // await new Promise(r => setTimeout(r, 2000));
  return items;
}

interface Update {
  kind: "update";
  index: number;
  item: Item;
}

interface Reset {
  kind: "reset";
  items: Item[];
}

function reducer(state: Item[], action: Update | Reset) {
  if (action.kind === "update") {
    state = [...state];
    state[action.index] = action.item;
    return state;
  } else {
    return action.items;
  }
}

export default function (props: Props) {
  const pos = props.pos;
  if (props.items.length === 0) {
    return <div>No premises found!</div>;
  }
  const [items, update] = React.useReducer(reducer, props.items || []);
  const [status, setStatus] = React.useState("init");
  const r = React.useRef(0);
  const rs = React.useContext(RpcContext);
  const [showFailed, setshowFailed] = React.useState(true)
  async function e() {
    r.current += 1;
    const id = r.current;
    update({ kind: "reset", items: props.items });
    setStatus(`loading ${id}`);
    for (let i = 0; i < items.length; i++) {
      const item = await getItem(rs, { item: props.items[i], pos });
      if (r.current !== id) {
        return;
      }
      update({ kind: "update", index: i, item });
      setStatus(`checked ${i} items`)
    }
    setStatus(`finished checking ${items.length} items`);
  }
  React.useEffect(() => {
    e();
  }, [rs, props.items.map((i) => `${i.name}-${i.score}`).join(",")]);
  let msg: any | undefined = <>{status}</>;
  return (
    <div>
      <label htmlFor=""><input type="checkbox" checked={showFailed} onChange={() => setshowFailed(x => !x)} /> Show failed suggestions. </label>
      <table>
        <tbody>
          {items.filter(x => showFailed || (x.result && isSuccessful(x.result))).map((x) => (
            <ViewItem key={x.name} {...x} />
          ))}
        </tbody>
      </table>
      {msg && <span>{msg}</span>}
    </div>
  );
}

function ViewResult(props: ItemResult) {
  if ("error" in props) {
    return <span>❌ {props.error.error}</span>;
  } else if ("done" in props) {
    const { cmd } = props.done;
    return <span>🎉 <code>{cmd}</code></span>;
  } else if ("change" in props) {
    const { cmd, target } = props.change;
    return (
      <span>✅ <code>{cmd}</code> &nbsp;
        <code style={{ color: "var(--vscode-textLink-activeForeground)" }}>
          ⊢ <InteractiveCode fmt={target} />
        </code></span>
    );
  } else if ("noChange" in props) {
    return <span>❌</span>
  } else {
    return <span>Unknown result type. {JSON.stringify(props)}</span>;
  }
}


function ViewItem(props: Item) {
  let n;
  if (props.expr) {
    n = (
      <code style={{ color: "var(--vscode-textLink-activeForeground)" }}>
        <InteractiveCode fmt={props.expr} />
      </code>
    );
  } else {
    n = <code style={{ overflowWrap: "anywhere" }}>{props.name}</code>;
  }
  return (
    <tr>
      <td>{n}</td>
      <td>
        {props.result && <ViewResult {...props.result} />}
      </td>
      <td>{props.error ?? ""}</td>
    </tr>
  );
}
