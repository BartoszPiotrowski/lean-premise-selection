import * as React from "react";
import {
  Position,
  Range,
  DocumentUri,
  TextEdit,
  WorkspaceEdit,
} from "vscode-languageserver-protocol";
import {
  InteractiveCode,
  useAsync,
  RpcContext,
  CodeWithInfos,
  RpcSessionAtPos,
  DocumentPosition,
  EditorContext,
  EditorApi,
} from "@leanprover/infoview";

type ItemResult =
  | {
    error: {
      cmd?: string;
      error: string;
    };
  }
  | {
    done: { cmd: string };
  }
  | {
    change: { cmd: string; target: CodeWithInfos };
  }
  | {
    noChange: { cmd: string };
  };

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
  uri: DocumentUri;
  stx: Range;
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
  const { pos, uri, stx } = props;
  if (props.items.length === 0) {
    return <div>No premises found!</div>;
  }
  const [items, update] = React.useReducer(reducer, props.items || []);
  const [status, setStatus] = React.useState("init");
  const r = React.useRef(0);
  const rs = React.useContext(RpcContext);
  const [showFailed, setshowFailed] = React.useState(true);
  async function e() {
    r.current += 1;
    const id = r.current;
    update({ kind: "reset", items: props.items });
    setStatus(`loading ${id}`);
    for (let i = 0; i < props.items.length; i++) {
      const item = await getItem(rs, { item: props.items[i], pos });
      if (r.current !== id) {
        return;
      }
      update({ kind: "update", index: i, item });
      setStatus(`checked ${i} items`);
    }
    setStatus(`finished checking ${props.items.length} items`);
  }
  React.useEffect(() => {
    e();
  }, [rs, props.items.map((i) => `${i.name}-${i.score}`).join(",")]);
  let msg: any | undefined = <>{status}</>;
  return (
    <div>
      <label htmlFor="">
        <input
          type="checkbox"
          checked={showFailed}
          onChange={() => setshowFailed((x) => !x)}
        />{" "}
        Show failed suggestions.{" "}
      </label>
      <table>
        <tbody>
          {items
            .filter((x) => showFailed || (x.result && isSuccessful(x.result)))
            .map((x) => (
              <ViewItem key={x.name} item={x} stx={stx} uri={uri} />
            ))}
        </tbody>
      </table>
      {msg && <span>{msg}</span>}
    </div>
  );
}

async function applyEdit(
  stx: Range,
  edit: string,
  uri: DocumentUri,
  e: EditorApi
) {
  const te: TextEdit = {
    range: stx,
    newText: edit,
  };
  const changes: any = {};
  changes[uri] = [te];
  const we: WorkspaceEdit = { changes };
  // @ts-ignore
  await e.applyEdit(we);
  console.log(`Applied ${edit}.`);
}

function ApplyLink(props: {
  stx: Range;
  cmd: string;
  uri: DocumentUri;
  emoji: string;
}) {
  const { stx, uri, cmd, emoji } = props;
  const ec = React.useContext(EditorContext);
  return (
    <a
      href="#"
      className="link grow pointer"
      title="apply!"
      onClick={() => applyEdit(stx, cmd, uri, ec.api)}
    >
      {emoji} <code>{cmd}</code>
    </a>
  );
}

function ViewResult(props: {
  result: ItemResult;
  stx: Range;
  uri: DocumentUri;
}) {
  const { result, stx, uri } = props;

  if ("error" in result) {
    return <span>❌ {result.error.error}</span>;
  } else if ("done" in result) {
    const { cmd } = result.done;
    return <ApplyLink emoji="🎉" stx={stx} uri={uri} cmd={cmd} />;
  } else if ("change" in result) {
    const { cmd, target } = result.change;
    return (
      <span>
        <ApplyLink emoji="✅" stx={stx} uri={uri} cmd={cmd} />
        &nbsp;
        <code style={{ color: "var(--vscode-textLink-activeForeground)" }}>
          ⊢ <InteractiveCode fmt={target} />
        </code>
      </span>
    );
  } else if ("noChange" in result) {
    return <span>❌</span>;
  } else {
    return <span>Unknown result type. {JSON.stringify(result)}</span>;
  }
}

function ViewItem(props: { item: Item, stx: Range, uri: DocumentUri }) {
  const { item, stx, uri } = props
  let n;
  if (item.expr) {
    n = (
      <code style={{ color: "var(--vscode-textLink-activeForeground)" }}>
        <InteractiveCode fmt={item.expr} />
      </code>
    );
  } else {
    n = <code style={{ overflowWrap: "anywhere" }}>{item.name}</code>;
  }
  return (
    <tr>
      <td>{n}</td>
      <td>{item.result && <ViewResult result={item.result} stx={stx} uri={uri} />}</td>
      <td>{item.error ?? ""}</td>
    </tr>
  );
}
