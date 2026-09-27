import { createMemo, createResource, createSignal } from "solid-js";
import Badge from "../components/ui/Badge";
import Button from "../components/ui/Button";
import DataTable, { type DataTableColumn } from "../components/ui/DataTable";
import { getFinanceRecords, type FinanceRecord } from "../lib/api/finance";
import { daysAgo, toApiDateTime, toInputDateTime } from "../lib/datetime";

const columns: DataTableColumn<FinanceRecord>[] = [
  { key: "tradeTime", header: "时间", sortable: true },
  { key: "sourceType", header: "来源", sortable: true, render: (record) => <Badge tone="blue">{sourceLabel(record.sourceType)}</Badge> },
  { key: "inOrOut", header: "收支", sortable: true },
  { key: "tradeType", header: "类型", sortable: true },
  { key: "amount", header: "金额", sortable: true, align: "right", class: "font-medium text-slate-950" }
];

export default function FinancePage() {
  const [sourceType, setSourceType] = createSignal("0");
  const [inOrOut, setInOrOut] = createSignal("0");
  const [startTime, setStartTime] = createSignal(toInputDateTime(daysAgo(30)));
  const [endTime, setEndTime] = createSignal(toInputDateTime(new Date()));
  const [query, setQuery] = createSignal(currentQuery());
  const [records] = createResource(query, getFinanceRecords);
  const rows = createMemo(() => records()?.data.items ?? []);
  const totalAmount = createMemo(() => rows().reduce((sum, item) => sum + Number(item.amount || 0), 0));

  function currentQuery() {
    return {
      sourceType: sourceType(),
      inOrOut: inOrOut(),
      startTime: toApiDateTime(startTime()),
      endTime: toApiDateTime(endTime())
    };
  }

  const handleSearch = (event: SubmitEvent) => {
    event.preventDefault();
    setQuery(currentQuery());
  };

  return (
    <div class="space-y-6">
      <section class="flex flex-wrap items-end justify-between gap-3">
        <div>
          <h2 class="text-2xl font-semibold tracking-tight">财务数据</h2>
          <p class="mt-1 text-sm text-slate-500">按来源、收支类型和交易时间查询账单流水。</p>
        </div>
        <div class="rounded-md bg-white px-3 py-2 text-sm text-slate-500 shadow-sm">
          {rows().length} 条，合计 {totalAmount().toFixed(2)}
        </div>
      </section>

      <form class="grid gap-3 rounded-lg border border-slate-200 bg-white p-4 shadow-sm xl:grid-cols-[140px_140px_1fr_1fr_auto]" onSubmit={handleSearch}>
        <select value={sourceType()} onChange={(event) => setSourceType(event.currentTarget.value)} class="h-10 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950">
          <option value="0">全部来源</option>
          <option value="1">支付宝</option>
          <option value="2">微信</option>
          <option value="3">银行</option>
        </select>
        <select value={inOrOut()} onChange={(event) => setInOrOut(event.currentTarget.value)} class="h-10 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950">
          <option value="0">全部收支</option>
          <option value="1">收入</option>
          <option value="2">支出</option>
          <option value="3">其他</option>
        </select>
        <input type="datetime-local" value={startTime()} onInput={(event) => setStartTime(event.currentTarget.value)} class="h-10 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950" />
        <input type="datetime-local" value={endTime()} onInput={(event) => setEndTime(event.currentTarget.value)} class="h-10 rounded-md border border-slate-300 px-3 text-sm outline-none focus:border-slate-950" />
        <Button type="submit">查询</Button>
      </form>

      <DataTable
        rows={rows()}
        columns={columns}
        loading={records.loading}
        error={records()?.success === false ? records()?.message || "财务数据加载失败" : undefined}
        emptyText="暂无财务数据"
        rowKey={(row) => row.id}
      />
    </div>
  );
}

function sourceLabel(sourceType: number) {
  return ({ 1: "支付宝", 2: "微信", 3: "银行" } as Record<number, string>)[sourceType] ?? String(sourceType);
}
