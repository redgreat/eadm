import { createResource, createSignal, For, Show } from "solid-js";
import Button from "../components/ui/Button";
import { getFinanceRecords } from "../lib/api/finance";

export default function FinancePage() {
  const [sourceType, setSourceType] = createSignal("0");
  const [inOrOut, setInOrOut] = createSignal("0");
  const [startTime, setStartTime] = createSignal(toInputDateTime(daysAgo(30)));
  const [endTime, setEndTime] = createSignal(toInputDateTime(new Date()));
  const [query, setQuery] = createSignal(currentQuery());
  const [records] = createResource(query, getFinanceRecords);

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
          <p class="mt-1 text-sm text-slate-500">已接入新的 `/api/finance` 查询接口。</p>
        </div>
        <div class="rounded-md bg-white px-3 py-2 text-sm text-slate-500 shadow-sm">
          共 {records()?.data.total ?? "--"} 条流水
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

      <Show when={records()?.success === false}>
        <div class="rounded-md bg-amber-50 px-4 py-3 text-sm text-amber-700">
          {records()?.message || "财务数据加载失败"}
        </div>
      </Show>

      <section class="overflow-hidden rounded-lg border border-slate-200 bg-white shadow-sm">
        <div class="overflow-x-auto">
          <table class="min-w-full divide-y divide-slate-200 text-sm">
            <thead class="bg-slate-50 text-left text-xs font-semibold uppercase tracking-wide text-slate-500">
              <tr>
                <th class="px-4 py-3">时间</th>
                <th class="px-4 py-3">来源</th>
                <th class="px-4 py-3">收支</th>
                <th class="px-4 py-3">类型</th>
                <th class="px-4 py-3 text-right">金额</th>
              </tr>
            </thead>
            <tbody class="divide-y divide-slate-100">
              <Show
                when={(records()?.data.items.length ?? 0) > 0}
                fallback={
                  <tr>
                    <td class="px-4 py-8 text-center text-slate-400" colSpan={5}>
                      {records.loading ? "加载中..." : "暂无财务数据"}
                    </td>
                  </tr>
                }
              >
                <For each={records()?.data.items ?? []}>
                  {(record) => (
                    <tr class="hover:bg-slate-50">
                      <td class="px-4 py-3 text-slate-500">{record.tradeTime}</td>
                      <td class="px-4 py-3">{sourceLabel(record.sourceType)}</td>
                      <td class="px-4 py-3">{record.inOrOut}</td>
                      <td class="px-4 py-3 text-slate-500">{record.tradeType}</td>
                      <td class="px-4 py-3 text-right font-medium">{record.amount}</td>
                    </tr>
                  )}
                </For>
              </Show>
            </tbody>
          </table>
        </div>
      </section>
    </div>
  );
}

function sourceLabel(sourceType: number) {
  return ({ 1: "支付宝", 2: "微信", 3: "银行" } as Record<number, string>)[sourceType] ?? String(sourceType);
}

function daysAgo(days: number) {
  return new Date(Date.now() - days * 24 * 60 * 60 * 1000);
}

function toInputDateTime(date: Date) {
  const pad = (value: number) => String(value).padStart(2, "0");
  return `${date.getFullYear()}-${pad(date.getMonth() + 1)}-${pad(date.getDate())}T${pad(date.getHours())}:${pad(date.getMinutes())}`;
}

function toApiDateTime(value: string) {
  return `${value.replace("T", " ")}:00`;
}
