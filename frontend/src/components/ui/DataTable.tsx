import type { JSX } from "solid-js";
import { createEffect, createMemo, createSignal, For, on, Show } from "solid-js";

export type DataTableColumn<T> = {
  key: string;
  header: string;
  accessor?: (row: T) => unknown;
  render?: (row: T) => JSX.Element;
  sortable?: boolean;
  align?: "left" | "right" | "center";
  class?: string;
};

type SortState = {
  key: string;
  direction: "asc" | "desc";
} | null;

export default function DataTable<T>(props: {
  columns: DataTableColumn<T>[];
  rows: T[];
  loading?: boolean;
  error?: string;
  emptyText?: string;
  pageSize?: number;
  rowKey?: (row: T, index: number) => string | number;
}) {
  const [sort, setSort] = createSignal<SortState>(null);
  const [page, setPage] = createSignal(1);
  const pageSize = () => props.pageSize ?? 10;

  createEffect(on(() => props.rows, () => setPage(1), { defer: true }));

  const sortedRows = createMemo(() => {
    const current = sort();
    if (!current) {
      return props.rows;
    }
    const column = props.columns.find((item) => item.key === current.key);
    if (!column) {
      return props.rows;
    }
    const valueOf = (row: T) => column.accessor?.(row) ?? (row as Record<string, unknown>)[column.key];
    return [...props.rows].sort((left, right) => compareValues(valueOf(left), valueOf(right), current.direction));
  });

  const totalPages = createMemo(() => Math.max(1, Math.ceil(sortedRows().length / pageSize())));
  const visibleRows = createMemo(() => {
    const start = (page() - 1) * pageSize();
    return sortedRows().slice(start, start + pageSize());
  });

  const rangeText = createMemo(() => {
    const total = sortedRows().length;
    if (total === 0) {
      return "无记录";
    }
    const start = (page() - 1) * pageSize() + 1;
    const end = Math.min(total, page() * pageSize());
    return `当前 ${start} 条到 ${end} 条，共 ${total} 条`;
  });

  const toggleSort = (column: DataTableColumn<T>) => {
    if (!column.sortable) {
      return;
    }
    setSort((current) => {
      if (!current || current.key !== column.key) {
        return { key: column.key, direction: "asc" };
      }
      if (current.direction === "asc") {
        return { key: column.key, direction: "desc" };
      }
      return null;
    });
  };

  return (
    <section class="overflow-hidden rounded-lg border border-slate-200 bg-white shadow-sm">
      <Show when={props.error}>
        <div class="border-b border-amber-100 bg-amber-50 px-4 py-3 text-sm text-amber-700">{props.error}</div>
      </Show>
      <div class="overflow-x-auto">
        <table class="min-w-full divide-y divide-slate-200 text-sm">
          <thead class="bg-slate-50 text-left text-xs font-semibold uppercase tracking-wide text-slate-500">
            <tr>
              <For each={props.columns}>
                {(column) => (
                  <th class={cellClass(column.align, "px-4 py-3")}>
                    <button
                      type="button"
                      class={column.sortable ? "inline-flex items-center gap-1 hover:text-slate-950" : "cursor-default"}
                      onClick={() => toggleSort(column)}
                    >
                      {column.header}
                      <Show when={sort()?.key === column.key}>
                        <span>{sort()?.direction === "asc" ? "↑" : "↓"}</span>
                      </Show>
                    </button>
                  </th>
                )}
              </For>
            </tr>
          </thead>
          <tbody class="divide-y divide-slate-100">
            <Show
              when={visibleRows().length > 0}
              fallback={
                <tr>
                  <td class="px-4 py-8 text-center text-slate-400" colSpan={Math.max(props.columns.length, 1)}>
                    {props.loading ? "加载中..." : props.emptyText ?? "未查到数据"}
                  </td>
                </tr>
              }
            >
              <For each={visibleRows()}>
                {(row, index) => (
                  <tr class="hover:bg-slate-50" data-row-key={props.rowKey?.(row, index())}>
                    <For each={props.columns}>
                      {(column) => (
                        <td class={cellClass(column.align, `px-4 py-3 text-slate-600 ${column.class ?? ""}`)}>
                          {column.render ? column.render(row) : String(column.accessor?.(row) ?? (row as Record<string, unknown>)[column.key] ?? "")}
                        </td>
                      )}
                    </For>
                  </tr>
                )}
              </For>
            </Show>
          </tbody>
        </table>
      </div>
      <div class="flex flex-wrap items-center justify-between gap-3 border-t border-slate-100 px-4 py-3 text-sm text-slate-500">
        <span>{rangeText()}</span>
        <div class="flex items-center gap-1">
          <PageButton disabled={page() === 1} onClick={() => setPage(1)}>首页</PageButton>
          <PageButton disabled={page() === 1} onClick={() => setPage((value) => Math.max(1, value - 1))}>上一页</PageButton>
          <span class="px-2">{page()} / {totalPages()}</span>
          <PageButton disabled={page() === totalPages()} onClick={() => setPage((value) => Math.min(totalPages(), value + 1))}>下一页</PageButton>
          <PageButton disabled={page() === totalPages()} onClick={() => setPage(totalPages())}>尾页</PageButton>
        </div>
      </div>
    </section>
  );
}

function PageButton(props: { children: JSX.Element; disabled?: boolean; onClick: () => void }) {
  return (
    <button
      type="button"
      disabled={props.disabled}
      onClick={() => props.onClick()}
      class="rounded-md border border-slate-200 px-2 py-1 text-xs text-slate-600 hover:bg-slate-50 disabled:cursor-not-allowed disabled:opacity-40"
    >
      {props.children}
    </button>
  );
}

function cellClass(align: "left" | "right" | "center" | undefined, base: string) {
  const alignClass = align === "right" ? "text-right" : align === "center" ? "text-center" : "text-left";
  return `${base} ${alignClass}`;
}

function compareValues(left: unknown, right: unknown, direction: "asc" | "desc") {
  const modifier = direction === "asc" ? 1 : -1;
  const leftNumber = Number(left);
  const rightNumber = Number(right);
  if (Number.isFinite(leftNumber) && Number.isFinite(rightNumber)) {
    return (leftNumber - rightNumber) * modifier;
  }
  return String(left ?? "").localeCompare(String(right ?? ""), "zh-CN") * modifier;
}
